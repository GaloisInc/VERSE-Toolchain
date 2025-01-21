open! Base
module CodeLens = Lsp.Types.CodeLens
module CodeLensOptions = Lsp.Types.CodeLensOptions
module Command = Lsp.Types.Command

let gillian_lenses_for (uri : Uri.t) : CodeLens.t list =
  match Parse.parse_document_file uri with
  | Error e ->
    Log.e (Printf.sprintf "Unable to parse file %s: %s" (Uri.to_path uri) e);
    []
  | Ok decls ->
    let fns = Parse.function_declarations decls in
    let lens fn =
      let procedure_name = Parse.function_name fn in
      (* Schema derived from Gillian's `debug-ui/src/activateCodeLens.ts` *)
      let argument =
        `Assoc
          [ "type", `String "CN"
          ; "request", `String "launch"
          ; "program", `String (Uri.to_path uri)
          ; "procedureName", `String procedure_name
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
      | Some range -> Some (CodeLens.create ~command ~range ())
    in
    List.filter_map fns ~f:lens
;;