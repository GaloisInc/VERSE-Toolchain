(* Cerberus *)
module CB = Cerb_backend
module CF = Cerb_frontend
module CG = Cerb_global

(* LSP *)
module LspDocumentUri = Lsp.Types.DocumentUri
module LspPosition = Lsp.Types.Position
module LspRange = Lsp.Types.Range
module LspDiagnostic = Lsp.Types.Diagnostic

module CerbM = struct
  type error = CF.Errors.error
  type 'a m = ('a, error) CF.Exception.exceptM

  let ( let* ) (a : 'a m) (f : 'a -> 'b m) : 'b m = CF.Exception.except_bind a f
  let return (a : 'a) : 'a m = CF.Exception.except_return a

  let run (x : 'a m) : ('a, error) result =
    match x with
    | CF.Exception.Exception (loc, cause) -> Error (loc, cause)
    | CF.Exception.Result r -> Ok r
  ;;
end

module Cerb = struct
  open CerbM

  let error_to_string ((loc, _cause) : error) : string =
    "Cerberus Error: loc = " ^ Cerb_location.location_to_string loc
  ;;

  let error_to_diagnostic (_err : error) : LspDiagnostic.t =
    let message = "cerberus error" in
    let zero = Lsp.Types.Position.create ~character:0 ~line:0 in
    let range = Lsp.Types.Range.create ~start:zero ~end_:zero in
    let source = "Cerberus" in
    LspDiagnostic.create ~message ~range ~source ()
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
    let bmc = false in
    let astprints : CB.Pipeline.language list = [] in
    let incl_dirs : string list = [] in
    let incl_files : string list = [] in
    let () =
      CG.set_cerb_conf
        backend_name
        exec
        exec_mode
        concurrency
        error_verbosity
        defacto
        permissive
        agnostic
        ignore_bitfields
        bmc;
      CF.Ocaml_implementation.set CF.Ocaml_implementation.HafniumImpl.impl;
      CF.Switches.set [ "inner_arg_temps"; "at_magic_comments" ];
      CF.Core_peval.config_unfold_stdlib := Cn.Sym.has_id_with Cn.Setup.unfold_stdlib_name
    in
    let* stdlib = CB.Pipeline.load_core_stdlib () in
    let* impl = CB.Pipeline.load_core_impl stdlib Cn.Setup.impl_name in
    let conf = Cn.Setup.conf incl_dirs incl_files astprints in
    return (conf, impl, stdlib)
  ;;

  let frontend ((conf, impl, stdlib) : env) (filename : string) =
    let* _, ail_prog_opt, prog0 =
      CB.Pipeline.c_frontend_and_elaboration
        ~cnnames:Cn.Builtins.cn_builtin_fun_names
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
    let markers_env, (_, ail_prog) = Option.get ail_prog_opt in
    let () = CF.Tags.set_tagDefs prog0.CF.Core.tagDefs in
    let prog1 = CF.Remove_unspecs.rewrite_file prog0 in
    let prog2 = CF.Milicore.core_to_micore__file Cn.Locations.update prog1 in
    let prog3 = CF.Milicore_label_inline.rewrite_file prog2 in
    let statement_locs = Cn.CStatements.search ail_prog in
    return (prog3, (markers_env, ail_prog), statement_locs)
  ;;
end

module CnM = struct
  type error = Cn.TypeErrors.t
  type 'a m = 'a Cn.Resultat.t

  let ( let* ) (a : 'a m) (f : 'a -> 'b m) : 'b m = Cn.Resultat.bind a f
  let return (a : 'a) : 'a m = Ok a
  let run (x : 'a m) : ('a, error) result = x
end

type error =
  | CnError of CnM.error
  | CerbError of CerbM.error

let error_to_string (err : error) : string =
  match err with
  | CerbError (loc, cause) -> Cerb.error_to_string (loc, cause)
  | CnError e ->
    let report = Cn.TypeErrors.pp_message e.msg in
    let short = Cn.Pp.plain report.short in
    let desc = Option.value (Option.map Cn.Pp.plain report.descr) ~default:"<none>" in
    "CN Error: loc = "
    ^ Cn.Locations.to_string e.loc
    ^ ", short = "
    ^ short
    ^ ", desc = "
    ^ desc
;;

let error_to_diagnostic (err : error) : LspDiagnostic.t =
  match err with
  | CerbError (loc, cause) -> Cerb.error_to_diagnostic (loc, cause)
  | CnError e ->
    let report = Cn.TypeErrors.pp_message e.msg in
    let short = Cn.Pp.plain report.short in
    let message =
      match report.descr with
      | None -> short
      | Some d -> short ^ "\n" ^ Cn.Pp.plain d
    in
    let posn_to_lsp (p : Lexing.position) : LspPosition.t =
      LspPosition.create ~line:(p.pos_lnum - 1) ~character:(p.pos_cnum - p.pos_bol)
    in
    let range =
      match Cn.Locations.start_pos e.loc, Cn.Locations.end_pos e.loc with
      | Some s, Some e -> LspRange.create ~start:(posn_to_lsp s) ~end_:(posn_to_lsp e)
      | Some x, None | None, Some x ->
        LspRange.create ~start:(posn_to_lsp x) ~end_:(posn_to_lsp x)
      | None, None ->
        let zero = LspPosition.create ~character:0 ~line:0 in
        LspRange.create ~start:zero ~end_:zero
    in
    let source = "CN" in
    LspDiagnostic.create ~message ~range ~source ()
;;

type 'a m = ('a, error) result

let ( let* ) (a : 'a m) (f : 'a -> 'b m) : 'b m = Result.bind a f
let return (a : 'a) : 'a m = Ok a
let run (a : 'a m) : ('a, error) result = a

type cerb_env = Cerb.env

let lift_cerb (x : 'a CerbM.m) : 'a m =
  Result.map_error (fun (l, c) -> CerbError (l, c)) (CerbM.run x)
;;

let lift_cn (x : 'a CnM.m) : ('a, error) result =
  Result.map_error (fun e -> CnError e) (CnM.run x)
;;

let setup () : cerb_env m = lift_cerb (Cerb.setup ())

let run_cn (cerb_env : cerb_env) (uri : LspDocumentUri.t) : unit m =
  (* CLI flag? *)
  let lemmata : string option = None in
  (* CLI flag? *)
  let inherit_loc : bool = true in
  let path = LspDocumentUri.to_path uri in
  let* prog, (markers_env, ail_prog), statement_locs =
    lift_cerb (Cerb.frontend cerb_env path)
  in
  lift_cn
    CnM.(
      let* prog' =
        Cn.Core_to_mucore.normalise_file ~inherit_loc (markers_env, ail_prog) prog
      in
      Cn.Typing.run Cn.Context.empty (Cn.Check.check prog' statement_locs lemmata))
;;
