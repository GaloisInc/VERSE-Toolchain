open! Base

(* LSP *)
module LspDiagnostic = Lsp.Types.Diagnostic
module LspDocumentUri = Lsp.Types.DocumentUri

module CnM = struct
  type error = Cn.TypeErrors.t
  type 'a m = 'a Cn.Resultat.t

  let ( let* ) (a : 'a m) (f : 'a -> 'b m) : 'b m = Cn.Resultat.bind a f

  (* No reason in principle not to have `return`, it just hasn't been used so
     far in practice *)

  let run (x : 'a m) : ('a, error) Result.t = x
end

type error =
  | CnError of CnM.error
  | CerbError of LspCerb.error

let error_to_string (err : error) : string =
  match err with
  | CerbError (loc, cause) -> LspCerb.error_to_string (loc, cause)
  | CnError e ->
    let report = Cn.TypeErrors.pp_message e.msg in
    let short = Cn.Pp.plain report.short in
    let desc = Option.value (Option.map report.descr ~f:Cn.Pp.plain) ~default:"<none>" in
    "CN Error: loc = "
    ^ Cn.Locations.to_string e.loc
    ^ ", short = "
    ^ short
    ^ ", desc = "
    ^ desc
;;

(** Convert an error to an LSP diagnostic and the URI to which it applies *)
let error_to_diagnostic (err : error) : (LspDocumentUri.t * LspDiagnostic.t) option =
  match err with
  | CerbError (loc, cause) -> LspCerb.error_to_diagnostic (loc, cause)
  | CnError e ->
    let report = Cn.TypeErrors.pp_message e.msg in
    let short = Cn.Pp.plain report.short in
    let message =
      match report.descr with
      | None -> short
      | Some d -> short ^ "\n" ^ Cn.Pp.plain d
    in
    let source = "CN" in
    (match LspCerb.loc_to_source_range e.loc with
     | None -> None
     | Some (path, range) ->
       Some (LspDocumentUri.of_path path, LspDiagnostic.create ~message ~range ~source ()))
;;

let errors_to_diagnostics (errs : error list)
  : (LspDocumentUri.t, LspDiagnostic.t list) Hashtbl.t
  =
  let diags = Hashtbl.create (module Uri) in
  let add err =
    match error_to_diagnostic err with
    | None -> ()
    | Some (uri, diag) -> Hashtbl.add_multi diags ~key:uri ~data:diag
  in
  List.iter errs ~f:add;
  diags
;;

type 'a m = ('a, error) Result.t

let ( let* ) (a : 'a m) (f : 'a -> 'b m) : 'b m = Result.bind a ~f
let return (a : 'a) : 'a m = Ok a
let run (a : 'a m) : ('a, error) Result.t = a

type cerb_env = LspCerb.env

let lift_cerb (x : 'a LspCerb.m) : 'a m =
  Result.map_error (LspCerb.run x) ~f:(fun (l, c) -> CerbError (l, c))
;;

let lift_cn (x : 'a CnM.m) : ('a, error) Result.t =
  Result.map_error (CnM.run x) ~f:(fun e -> CnError e)
;;

let setup () : cerb_env m = lift_cerb (LspCerb.setup ())

let run_cn (cerb_env : cerb_env) (uri : LspDocumentUri.t) : error list m =
  (* CLI flag? *)
  let inherit_loc : bool = true in
  let path = LspDocumentUri.to_path uri in
  let* prog, (markers_env, ail_prog), _statement_locs =
    lift_cerb (LspCerb.frontend cerb_env path)
  in
  let* errors =
    lift_cn
      CnM.(
        let* prog' =
          Cn.Core_to_mucore.normalise_file ~inherit_loc (markers_env, ail_prog) prog
        in
        Cn.Typing.(
          run
            Cn.Context.empty
            (let@ wellformedness_result, _ =
               Cn.Check.check_decls_lemmata_fun_specs prog'
             in
             Cn.Check.check_c_functions_all wellformedness_result)))
  in
  return (List.map errors ~f:(fun (_fn, e) -> CnError e))
;;
