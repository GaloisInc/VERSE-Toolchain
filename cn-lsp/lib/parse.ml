open! Base
module CF = Cerb_frontend
module Cabs = Cerb_frontend.Cabs

let rec declarator_name (declarator : Cabs.declarator) : string =
  match declarator with
  | Cabs.Declarator (_pdecl, ddecl) -> direct_declarator_name ddecl

and direct_declarator_name (declarator : Cabs.direct_declarator) : string =
  match declarator with
  | Cabs.DDecl_identifier (_attrs, Identifier (_loc, s)) -> s
  | Cabs.DDecl_declarator decl -> declarator_name decl
  | Cabs.DDecl_array (ddecl, _adecl) -> direct_declarator_name ddecl
  | Cabs.DDecl_function (ddecl, _params) -> direct_declarator_name ddecl
;;

let function_name (fd : Cabs.function_definition) : string =
  match fd with
  | Cabs.FunDef (_loc, _attrs, _specifiers, declarator, _stmt) ->
    declarator_name declarator
;;

let function_loc
  (Cabs.FunDef (loc, _attrs, _specifiers, _declarator, _stmt) : Cabs.function_definition)
  : Cerb_location.t
  =
  loc
;;

let function_declarations
  (decls : Cabs.external_declaration list)
  ~(only_from : Uri.t option)
  : Cabs.function_definition list
  =
  let only_from = Option.map only_from ~f:Uri.to_path in
  List.filter_map decls ~f:(fun decl ->
    match decl with
    | Cabs.EDecl_func fd ->
      (match only_from, Cerb_location.get_filename (function_loc fd) with
       | None, _ -> Some fd
       | Some _, None -> None
       | Some specified_source, Some actual_source ->
         if String.equal specified_source actual_source then Some fd else None)
    | Cabs.EDecl_decl _
    | Cabs.EDecl_magic _
    | Cabs.EDecl_funcCN _
    | Cabs.EDecl_lemmaCN _
    | Cabs.EDecl_predCN _
    | Cabs.EDecl_datatypeCN _
    | Cabs.EDecl_type_synCN _
    | Cabs.EDecl_fun_specCN _ -> None)
;;

let c_preprocessor : string = "cc"

let c_preprocessor_arguments : string list =
  [ (* Set standard to C11 *)
    "-std=c11"
  ; (* Run in preprocessor mode *)
    "-E"
  ; (* ??? *)
    "-CC"
  ; (* Disable standard inclusions *)
    "-nostdinc"
  ; (* ??? *)
    "-undef"
  ; (* TODO: should be able to do these only once, or avoid them entirely? *)
    "-I"
  ; Cerb_runtime.in_runtime "libc/include"
  ; "-I"
  ; Cerb_runtime.in_runtime "libcore"
  ; "-include"
  ; Cerb_runtime.in_runtime "libc/include/builtins.h"
  ; "-DDEBUG"
  ; "-DCN_MODE"
  ]
;;

module Error = struct
  type t =
    | Parse of
        { loc : Cerb_location.t
        ; cause : CF.Errors.cause
        }
    | Preprocess of
        { uri : Uri.t
        ; result : Process.proc_out
        }
    | Process of Process.Error.t

  let to_diagnostic (err : t) : (Uri.t * Lsp.Types.Diagnostic.t) option =
    match err with
    | Preprocess { uri = _; result = _ } | Process _ -> None
    | Parse { loc; cause } -> LspCerb.Error.to_diagnostic (loc, cause)
  ;;

  let to_string (err : t) : string =
    match err with
    | Parse e ->
      Printf.sprintf "parse error: %s" (CF.Pp_errors.to_string (e.loc, e.cause))
    | Preprocess e ->
      Printf.sprintf
        "preprocessing file %s failed with exit code %i - see logs"
        (Uri.to_string e.uri)
        e.result.exit_code
    | Process e -> Process.Error.to_string e
  ;;
end

let preprocess_file (uri : Uri.t) : (Process.proc_out, Error.t) Result.t =
  let ( let* ) x f = Result.bind x ~f in
  let path = Uri.to_path uri in
  let args = Array.of_list ((c_preprocessor :: c_preprocessor_arguments) @ [ path ]) in
  let* result =
    Result.map_error (Process.read_process c_preprocessor args) ~f:(fun e ->
      Error.Process e)
  in
  match result.exit_code with
  | 0 -> Ok result
  | code ->
    Log.e (Printf.sprintf "preprocessing failed with exit code %i" code);
    Log.e "stdout:";
    List.iter (String.split_lines result.stdout) ~f:(fun line -> Log.e ("  " ^ line));
    Log.e "stderr:";
    List.iter (String.split_lines result.stderr) ~f:(fun line -> Log.e ("  " ^ line));
    Error (Preprocess { uri; result })
;;

let parse_document_source (uri : Uri.t) (source : string)
  : (Cabs.external_declaration list, Error.t) Result.t
  =
  let () = CF.Switches.set [ "inner_arg_temps"; "at_magic_comments" ] in
  let path = Uri.to_path uri in
  match C_parser_driver.parse_from_string ~filename:path source with
  | CF.Exception.Result (TUnit decls) -> Ok decls
  | CF.Exception.Exception (loc, cause) -> Error (Parse { loc; cause })
;;

let parse_document_file (uri : Uri.t) : (Cabs.external_declaration list, Error.t) Result.t
  =
  match preprocess_file uri with
  | Ok out -> parse_document_source uri out.stdout
  | Error s -> Error s
;;

let enumerate (xs : 'a list) : (int * 'a) list = List.mapi xs ~f:(fun i x -> i, x)

(** The portion of the numbered line, if any, that's within the provided range *)
let in_range (range : Range.t) (linenum : int) (line : string) : string option =
  if linenum > range.start.line && linenum < range.end_.line
  then Some line
  else if linenum = range.start.line && linenum = range.end_.line
  then
    Some
      (String.drop_prefix (String.prefix line range.end_.character) range.start.character)
  else if linenum = range.start.line
  then Some (String.drop_prefix line range.start.character)
  else if linenum = range.end_.line
  then Some (String.prefix line range.end_.character)
  else None
;;

(** Extract the portion of [source] within [range], where the first character of
    [source] is considered to be at line 0, character 0 *)
let extract_from_source (range : Range.t) (source : string) : string =
  let lines = enumerate (String.split_lines source) in
  String.concat
    (List.filter_map lines ~f:(fun (linenum, line) -> in_range range linenum line))
    ~sep:"\n"
;;

(** Extract the portion of the provided file within [range] *)
let extract_from_file (range : Range.t) (uri : Uri.t) : string =
  let rec from_channel ic linenum acc =
    match In_channel.input_line ic with
    | None -> acc
    | Some line ->
      (match in_range range linenum line, acc with
       | None, [] -> from_channel ic (linenum + 1) acc
       | None, _ -> acc
       | Some portion, _ -> from_channel ic (linenum + 1) (portion :: acc))
  in
  let portions =
    In_channel.with_open_text (Uri.to_path uri) (fun ic -> from_channel ic 0 [])
  in
  String.concat (List.rev portions) ~sep:"\n"
;;
