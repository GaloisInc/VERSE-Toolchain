open! Base
module Diagnostic = Lsp.Types.Diagnostic

module Error = struct
  type t =
    | CnError of LspCn.Error.t
    | CerbError of LspCerb.Error.t

  let to_string (err : t) : string =
    match err with
    | CerbError (loc, cause) -> LspCerb.Error.to_string (loc, cause)
    | CnError e -> LspCn.Error.to_string e
  ;;

  (** Convert an error to an LSP diagnostic and the URI to which it applies *)
  let to_diagnostic (err : t) : (Uri.t * Diagnostic.t) option =
    match err with
    | CerbError (loc, cause) -> LspCerb.Error.to_diagnostic (loc, cause)
    | CnError e -> LspCn.Error.to_diagnostic e
  ;;

  let to_diagnostics (errs : t list) : (Uri.t, Diagnostic.t list) Hashtbl.t =
    let diags = Hashtbl.create (module Uri) in
    let add err =
      match to_diagnostic err with
      | None -> ()
      | Some (uri, diag) -> Hashtbl.add_multi diags ~key:uri ~data:diag
    in
    List.iter errs ~f:add;
    diags
  ;;
end

type 'a m = ('a, Error.t) Result.t

let ( let* ) (a : 'a m) (f : 'a -> 'b m) : 'b m = Result.bind a ~f
let return (a : 'a) : 'a m = Ok a

type cerb_env = LspCerb.env

let lift_cerb (x : 'a LspCerb.m) : 'a m =
  Result.map_error (LspCerb.run x) ~f:(fun (l, c) -> Error.CerbError (l, c))
;;

let lift_cn (x : 'a LspCn.m) : ('a, Error.t) Result.t =
  Result.map_error x ~f:(fun e -> Error.CnError e)
;;

let setup () : cerb_env m = lift_cerb (LspCerb.setup ())

let run_cn (cerb_env : cerb_env) (uri : Uri.t) : Error.t list m =
  (* CLI flag? *)
  let inherit_loc : bool = true in
  let path = Uri.to_path uri in
  let* prog, (markers_env, ail_prog), _statement_locs =
    lift_cerb (LspCerb.frontend cerb_env path)
  in
  let* errors =
    lift_cn
      LspCn.(
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
  return (List.map errors ~f:(fun (_fn, e) -> Error.CnError e))
;;
