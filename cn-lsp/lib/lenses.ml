open! Base
module CodeLens = Lsp.Types.CodeLens
module CodeLensOptions = Lsp.Types.CodeLensOptions
module Command = Lsp.Types.Command
module Cabs = Cerb_frontend.Cabs

let mk_verify_lens (fundef : Cabs.function_definition) : CodeLens.t option =
  let procedure_name = Parse.function_name fundef in
  let arguments = [ `String procedure_name ] in
  let command =
    Command.create ~command:"CN.runOnFunction" ~title:"Verify with CN" ~arguments ()
  in
  match Parse.function_loc fundef with
  | None ->
    Log.d
      (Printf.sprintf
         "No location available for verification lens for function '%s'"
         procedure_name);
    None
  | Some range -> Some (CodeLens.create ~command ~range ())
;;

let lenses_for (uri : Uri.t) : CodeLens.t list =
  match Parse.parse_document_file uri with
  | Error e ->
    Log.e (Printf.sprintf "Unable to parse file %s: %s" (Uri.to_path uri) e);
    []
  | Ok decls ->
    let fns = Parse.function_declarations decls in
    List.filter_map fns ~f:mk_verify_lens
;;
