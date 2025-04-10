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

  (** Convert an error to an LSP diagnostic and the URI to which it applies.

      If [html_report_dir] is provided, try to generate an HTML-formatted report
      of the error, store it in that directory, and include a link to it in the
      diagnostic. *)
  let to_diagnostic (err : t) ~(html_report_dir : string option)
    : (Uri.t * Diagnostic.t) option
    =
    match err with
    | CerbError (loc, cause) -> LspCerb.Error.to_diagnostic (loc, cause)
    | CnError e ->
      let html_report =
        match html_report_dir with
        | None -> None
        | Some output_dir ->
          (match LspCn.Error.to_html_report e ~fn_name:None ~output_dir with
           | None ->
             Log.d
               (Printf.sprintf
                  "to_diagnostic: unable to generate HTML report for CN error: %s"
                  (LspCn.Error.to_string e));
             None
           | Some report -> Some report)
      in
      LspCn.Error.to_diagnostic e ~html_report
  ;;

  (** Convert many CN errors to many LSP diagnostics, indexed by the URIs in
      which they apply.

      If [html_report_dir] is provided, try to generate HTML-formatted reports
      of each error, store them in that directory, and include links to each in
      their respective diagnostics. *)
  let to_diagnostics (errs : t list) ~(html_report_dir : string option)
    : (Uri.t, Diagnostic.t list) Hashtbl.t
    =
    let diags = Hashtbl.create (module Uri) in
    let add err =
      match to_diagnostic err ~html_report_dir with
      | None ->
        Log.e
          (Printf.sprintf "to_diagnostics: unable to interpret error: %s" (to_string err))
      | Some (uri, diag) -> Hashtbl.add_multi diags ~key:uri ~data:diag
    in
    List.iter errs ~f:add;
    diags
  ;;
end

(** The type of the verification "monad" *)
type 'a m = ('a, Error.t) Result.t

let ( let* ) (a : 'a m) (f : 'a -> 'b m) : 'b m = Result.bind a ~f
let return (a : 'a) : 'a m = Ok a

(** A reusable "environment" needed to run CN *)
type cerb_env = LspCerb.env

(** Lift a cerberus action into our monad *)
let lift_cerb (x : 'a LspCerb.m) : 'a m =
  Result.map_error (LspCerb.run x) ~f:(fun (l, c) -> Error.CerbError (l, c))
;;

(** Lift a CN action into our monad *)
let lift_cn (x : 'a LspCn.m) : ('a, Error.t) Result.t =
  Result.map_error x ~f:(fun e -> Error.CnError e)
;;

(** Create the environment needed to run CN *)
let setup () : cerb_env m =
  let* env = lift_cerb (LspCerb.setup ()) in
  Cn.Check.fail_fast := false;
  return env
;;

(** In the given document, run CN (on [fn] if it's provided, or on every
    function in the file if it's not) to potentially produce errors. *)
let run_cn (cerb_env : cerb_env) (uri : Uri.t) ~(fn : string option) : Error.t list m =
  (* See https://github.com/GaloisInc/VERSE-Toolchain/issues/142 and
     https://github.com/rems-project/cerberus/pull/833 *)
  Cn.Solver.reset_model_evaluator_state ();
  (* CLI flag? *)
  let inherit_loc : bool = true in
  let path = Uri.to_path uri in
  let* prog, (markers_env, ail_prog), _statement_locs =
    lift_cerb (LspCerb.frontend cerb_env path)
  in
  Cn.Check.skip_and_only := [], Option.to_list fn;
  let* errors =
    lift_cn
      LspCn.(
        let* prog' =
          Cn.Core_to_mucore.normalise_file ~inherit_loc (markers_env, ail_prog) prog
        in
        Cn.Typing.(
          run
            Cn.Context.empty
            (let@ wellformedness_result, global_var_constraints, _lemmata =
               Cn.Check.check_decls_lemmata_fun_specs prog'
             in
             Cn.Check.time_check_c_functions
               (global_var_constraints, wellformedness_result))))
  in
  return (List.map errors ~f:(fun (_fn, e) -> Error.CnError e))
;;
