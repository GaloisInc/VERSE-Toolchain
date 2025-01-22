open! Base
module CodeLens = Lsp.Types.CodeLens
module CodeLensOptions = Lsp.Types.CodeLensOptions
module Command = Lsp.Types.Command

let gillian_lenses_for ?(report_dir : string option) (uri : Uri.t) : CodeLens.t list =
  let report_dir =
    Option.value report_dir ~default:(Uri.to_path uri |> Stdlib.Filename.dirname)
  in
  match Parse.parse_document_file uri with
  | Error e ->
    Log.e (Printf.sprintf "Unable to parse file %s: %s" (Uri.to_path uri) e);
    []
  | Ok decls ->
    let fns = Parse.function_declarations decls in
    let lens fn =
      let procedure_name = Parse.function_name fn in
      let loc =
        Cerb_location.point Lexing.{ dummy_pos with pos_fname = Uri.to_path uri }
      in
      let state_trace_file =
        Cn.TypeErrors.mk_report_file_name ~fn_name:procedure_name report_dir loc
      in
      (* Schema derived from Gillian's `debug-ui/src/activateCodeLens.ts` *)
      let argument =
        `Assoc
          [ "type", `String "CN"
          ; "request", `String "launch"
          ; "program", `String (Uri.to_path uri)
          ; "procedureName", `String procedure_name
          ; "reportDir", `String report_dir
          ]
      in
      let command =
        Command.create
          ~command:"extension.gillian-debug.debugProcedure"
          ~title:"Debug with Gillian"
          ~arguments:[ argument ]
          ()
      in
      match Parse.function_loc fn with
      | None ->
        Log.d
          (Printf.sprintf
             "No location available for Gillian lens for function `%s`"
             procedure_name);
        None
      | Some _ when not (Stdlib.Sys.file_exists state_trace_file) ->
        Log.d
          (Printf.sprintf
             "No state trace available for Gillian lens for function `%s`"
             procedure_name);
        None
      | Some range -> Some (CodeLens.create ~command ~range ())
    in
    List.filter_map fns ~f:lens
;;
