module Error = struct
  (** This mirrors the [Unix.Unix_error] exception - see it for details on what
      these fields represent. *)
  type t =
    { code : Unix.error
    ; fn : string
    ; param : string
    }

  let to_string (err: t): string =
    Printf.sprintf
        "error in process: %s: %s(%s)"
        (Unix.error_message err.code)
        err.fn
        err.param
end

type proc_out =
  { exit_code : int
  ; stdout : string
  ; stderr : string
  }

(** Run the command [cmd] with arguments [args] and wait for it to finish. Note
    that, like [Unix.create_process], the first argument ([args.(0)]) should
    be "the filename of the program being executed". In most cases, this means
    it should be [cmd]. *)
let read_process (cmd : string) (args : string array) : (proc_out, Error.t) Result.t =
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
  | Unix.Unix_error (code, fn, param) -> Error { code; fn; param }
;;
