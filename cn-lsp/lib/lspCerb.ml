open! Base

(* Cerberus *)
module CB = Cerb_backend
module CF = Cerb_frontend
module CG = Cerb_global

(* LSP *)
module Diagnostic = Lsp.Types.Diagnostic

let loc_to_source_range (loc : Cerb_location.t) : (string * Range.t) option =
  let path_opt =
    match Cn.Locations.start_pos loc, Cn.Locations.end_pos loc with
    | Some pos, _ | _, Some pos -> Some (Cerb_position.file pos)
    | _ -> None
  in
  let range_opt = Range.of_cerb_loc loc in
  match path_opt, range_opt with
  | Some path, Some range -> Some (path, range)
  | _ -> None
;;

module Error = struct
  type t = CF.Errors.error

  let to_string ((loc, cause) : t) : string = CF.Pp_errors.to_string (loc, cause)

  let to_diagnostic ((loc, cause) : t) : (Uri.t * Diagnostic.t) option =
    let message = to_string (loc, cause) in
    let source = "Cerberus" in
    match loc_to_source_range loc with
    | None -> None
    | Some (path, range) ->
      Some (Uri.of_path path, Diagnostic.create ~message ~range ~source ())
  ;;
end

type 'a m = ('a, Error.t) CF.Exception.exceptM

let ( let* ) (a : 'a m) (f : 'a -> 'b m) : 'b m = CF.Exception.except_bind a f
let return (a : 'a) : 'a m = CF.Exception.except_return a

let run (x : 'a m) : ('a, Error.t) Result.t =
  match x with
  | CF.Exception.Exception (loc, cause) -> Error (loc, cause)
  | CF.Exception.Result r -> Ok r
;;

type conf = CB.Pipeline.configuration
type impl = CF.Core.impl
type stdlib = (string, CF.Symbol.sym) Pmap.map * unit CF.Core.fun_map
type env = conf * impl * stdlib

let setup () : env m =
  let backend_name = "Cn" in
  let exec = false in
  let exec_mode = CG.Random in
  let concurrency = false in
  let error_verbosity = CG.Basic in
  let defacto = false in
  let permissive = false in
  let agnostic = false in
  let ignore_bitfields = false in
  let astprints : CB.Pipeline.language list = [] in
  let incl_dirs : string list = [] in
  let incl_files : string list = [] in
  let macros : (string * string option) list = [] in
  let () =
    CG.set_cerb_conf
      ~backend_name
      ~exec
      ~concurrency
      ~defacto
      ~permissive
      ~agnostic
      ~ignore_bitfields
      exec_mode
      error_verbosity;
    CF.Ocaml_implementation.set CF.Ocaml_implementation.HafniumImpl.impl;
    CF.Switches.set [ "inner_arg_temps"; "at_magic_comments" ]
  in
  let* stdlib = CB.Pipeline.load_core_stdlib () in
  let* impl = CB.Pipeline.load_core_impl stdlib Cn.Setup.impl_name in
  let cpp_location_info_file = None in
  let conf = Cn.Setup.conf macros incl_dirs incl_files astprints cpp_location_info_file in
  return (conf, impl, stdlib)
;;

let frontend ((conf, impl, stdlib) : env) (filename : string) =
  let cn_init_scope : CF.Cn_desugaring.init_scope =
    { predicates = [ Cn.Alloc.Predicate.(str, sym, Some loc) ]
    ; functions = List.map Cn.Builtins.fun_names ~f:(fun (str, sym) -> str, sym, None)
    ; idents = [ Cn.Alloc.History.(str, sym, None) ]
    }
  in
  let* tunit_opt, ail_prog_opt, prog0 =
    CB.Pipeline.c_frontend_and_elaboration
      ~cn_init_scope
      (conf, Cn.Setup.io)
      (stdlib, impl)
      ~filename
  in
  let* () =
    if conf.typecheck_core
    then
      let* _ = CF.Core_typing.typecheck_program prog0 in
      return ()
    else return ()
  in
  let tunit = Option.value_exn tunit_opt in
  let markers_env, ail_prog = Option.value_exn ail_prog_opt in
  let () = CF.Tags.set_tagDefs prog0.CF.Core.tagDefs in
  let prog1 = CF.Remove_unspecs.rewrite_file prog0 in
  let prog2 = CF.Milicore.core_to_micore__file Cn.Locations.update prog1 in
  let prog3 = CF.Milicore_label_inline.rewrite_file prog2 in
  let statement_locs = Cn.CStatements.search (snd ail_prog) in
  return (tunit, prog3, (markers_env, ail_prog), statement_locs)
;;
