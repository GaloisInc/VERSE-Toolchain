open! Base
module CF = Cerb_frontend
module Cabs = Cerb_frontend.Cabs

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

type proc_out =
  { exit_code : int
  ; stdout : string
  ; stderr : string
  }

let read_process (cmd : string) (args : string array) : (proc_out, string) Result.t =
  try
    let out_read, out_write = Unix.pipe () in
    Unix.set_close_on_exec out_read;
    let out_ic = Unix.in_channel_of_descr out_read in
    let err_read, err_write = Unix.pipe () in
    Unix.set_close_on_exec err_read;
    let err_ic = Unix.in_channel_of_descr err_read in
    let pid = Unix.create_process cmd args Unix.stdin out_write err_write in
    Unix.close out_write;
    Unix.close err_write;
    Stdlib.flush_all ();
    let out = In_channel.input_all out_ic in
    Stdlib.close_in out_ic;
    let err = In_channel.input_all err_ic in
    Stdlib.close_in err_ic;
    match Unix.waitpid [] pid with
    | _, WEXITED exit_code | _, WSIGNALED exit_code | _, WSTOPPED exit_code ->
      Ok { exit_code; stdout = out; stderr = err }
  with
  | Unix.Unix_error (err, fn, _param) -> Error (Unix.error_message err ^ ": " ^ fn)
;;

let preprocess_file (uri : Uri.t) : (proc_out, string) Result.t =
  let path = Uri.to_path uri in
  let args = Array.of_list (c_preprocessor_arguments @ [ path ]) in
  read_process c_preprocessor args
;;

(** Transform a comment "content" range, which Cerberus's lexer produces, to a
    comment "block" range, which is the span from a cursor positioned
    immediately before a comment's opening slash to one positioned immediately
    after its closing slash *)
let comment_block_range (loc : Cerb_location.t) : Range.t =
  (* Comment content, exclusive of start/stop markers *)
  let comment_content_range =
    Option.value_exn
      ~message:"impossible: non-range magic comment"
      (Range.of_cerb_loc loc)
  in
  (* Subtract 3 to account for the length of the comment start marker, "/*@"
     or "/*$" *)
  let comment_block_start =
    { comment_content_range.start with
      character = comment_content_range.start.character - 3
    }
  in
  (* We would add 3 to account for the length of the comment end marker
     ("@*/" or "$*/"), but the lexer recognizes the ends of magic comments
     as "*/", so the position only needs to account for 2 characters
  *)
  let comment_block_end =
    { comment_content_range.end_ with
      character = comment_content_range.end_.character + 2
    }
  in
  { start = comment_block_start; end_ = comment_block_end }
;;

let document_source_comments (uri : Uri.t) (source : string)
  : (Range.t list, string) Result.t
  =
  let lexbuf = Lexing.from_string source in
  Lexing.set_filename lexbuf (Uri.to_path uri);
  CF.Switches.set [ "at_magic_comments" ];
  let (`LEXER lexer) = C_lexer.create_lexer ~inside_cn:true in
  let rec lex ranges =
    try
      match lexer lexbuf with
      | Tokens.CERB_MAGIC (comment_loc, _) ->
        let range = comment_block_range comment_loc in
        lex (range :: ranges)
      | Tokens.EOF -> Ok (List.rev ranges)
      | _ -> lex ranges
    with
    | C_lexer.Error err -> Error (CF.Pp_errors.string_of_cparser_cause err)
  in
  lex []
;;

let document_file_comments (uri : Uri.t) : (Range.t list, string) Result.t =
  match preprocess_file uri with
  | Ok out ->
    Log.d (Printf.sprintf "cpp exit code: %i" out.exit_code);
    Log.d (Printf.sprintf "cpp stderr: \n%s" out.stderr);
    document_source_comments uri out.stdout
  | Error s -> Error s
;;
