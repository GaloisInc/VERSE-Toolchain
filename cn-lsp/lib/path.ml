open! Base

(** Like [mkdir -p] - create the specified directory, while creating any
    intermediate directories as required, and without failing if the
    specified directory or any intermediate directories already exist. *)
let rec mkdir_p (path : string) : unit =
  try Unix.mkdir path 0o755 with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
    mkdir_p (Stdlib.Filename.dirname path);
    mkdir_p path
;;
