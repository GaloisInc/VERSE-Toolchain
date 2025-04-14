open! Base
module CodeLens = Lsp.Types.CodeLens
module CodeLensOptions = Lsp.Types.CodeLensOptions
module Command = Lsp.Types.Command
module Cabs = Cerb_frontend.Cabs

module Error = struct
  type t =
    | FunctionLocation of
        { fn : string
        ; loc : Cerb_location.t
        }

  let to_string (err : t) : string =
    match err with
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

(** For the given function at the given location, create a [CodeLens.t] to issue
    the "CN.testFunction" command *)
let mk_test_lens (fn_name : string) (fn_range : Range.t) : CodeLens.t =
  let arguments = [ `String fn_name; Range.to_yojson fn_range ] in
  let command =
    Command.create
      ~command:"CN.testFunction"
      ~title:(Printf.sprintf "Test %s with CN" fn_name)
      ~arguments
      ()
  in
  CodeLens.create ~command ~range:fn_range ()
;;

(** For the given function at the given location, create a [CodeLens.t] to issue
    the "CN.verifyFunction" command *)
let mk_verify_lens (fn_name : string) (fn_range : Range.t) : CodeLens.t =
  let arguments = [ `String fn_name; Range.to_yojson fn_range ] in
  let command =
    Command.create
      ~command:"CN.verifyFunction"
      ~title:(Printf.sprintf "Verify %s with CN" fn_name)
      ~arguments
      ()
  in
  CodeLens.create ~command ~range:fn_range ()
;;

(** Try to create code lenses for this function *)
let mk_lenses (fundef : Cabs.function_definition) : (CodeLens.t list, Error.t) Result.t =
  let procedure_name = Parse.function_name fundef in
  match Range.of_cerb_loc (Parse.function_loc fundef) with
  | None ->
    Log.d
      (Printf.sprintf
         "mk_lenses: no location available for lens for function '%s'"
         procedure_name);
    Error (FunctionLocation { fn = procedure_name; loc = Parse.function_loc fundef })
  | Some range ->
    let test_lens = mk_test_lens procedure_name range in
    let verify_lens = mk_verify_lens procedure_name range in
    Ok [ test_lens; verify_lens ]
;;

(** Try to create code lenses for functions in this file *)
let lenses_for (uri : Uri.t) : (CodeLens.t list * Error.t list, Parse.Error.t) Result.t =
  match Parse.parse_document_file uri with
  | Error e -> Error e
  | Ok decls ->
    let fns = Parse.function_declarations decls ~only_from:(Some uri) in
    let lenseses, errors = List.partition_result (List.map fns ~f:mk_lenses) in
    Ok (List.concat lenseses, errors)
;;
