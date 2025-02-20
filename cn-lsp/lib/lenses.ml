open! Base
module CodeLens = Lsp.Types.CodeLens
module CodeLensOptions = Lsp.Types.CodeLensOptions
module Command = Lsp.Types.Command
module Cabs = Cerb_frontend.Cabs

module Error = struct
  type t =
    | Parse of Parse.Error.t
    | FunctionLocation of
        { fn : string
        ; loc : Cerb_location.t
        }

  let to_string (err : t) : string =
    match err with
    | Parse e -> Printf.sprintf "parse error: %s" (Parse.Error.to_string e)
    | FunctionLocation e ->
      let buf = Buffer.create 1024 in
      let loc = Cerb_location.print_location e.loc in
      let () = PPrint.ToBuffer.compact buf loc in
      Printf.sprintf
        "error interpreting location for %s (original: %s)"
        e.fn
        (Buffer.contents buf)
  ;;
end

let mk_verify_lens (fundef : Cabs.function_definition) : (CodeLens.t, Error.t) Result.t =
  let procedure_name = Parse.function_name fundef in
  match Range.of_cerb_loc (Parse.function_loc fundef) with
  | None ->
    Log.d
      (Printf.sprintf
         "No location available for verification lens for function '%s'"
         procedure_name);
    Error (FunctionLocation { fn = procedure_name; loc = Parse.function_loc fundef })
  | Some range ->
    let arguments = [ `String procedure_name; Range.to_yojson range ] in
    let title = Printf.sprintf "Verify %s with CN" procedure_name in
    let command = Command.create ~command:"CN.runOnFunction" ~title ~arguments () in
    Ok (CodeLens.create ~command ~range ())
;;

let lenses_for (uri : Uri.t) : CodeLens.t list * Error.t list =
  match Parse.parse_document_file uri with
  | Error e -> [], [ Error.Parse e ]
  | Ok decls ->
    let fns = Parse.function_declarations decls ~only_from:(Some uri) in
    List.partition_result (List.map fns ~f:mk_verify_lens)
;;
