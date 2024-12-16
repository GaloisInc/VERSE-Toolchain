open Base
module Filename = Stdlib.Filename

type t = { root_dir : string }
type err = string

(** Convert a possibly-relative filepath into an absolute one *)
let canonicalize (path : string) : string =
  if Filename.is_relative path
  then (
    let current_dir = Stdlib.Sys.getcwd () in
    Filename.concat current_dir path)
  else path
;;

(** Like [mkdir -p] - create the specified directory, while creating any
    intermediate directories as required, and without failing if any directory
    already exists *)
let rec mkdir_p (path : string) : unit =
  try Unix.mkdir path 0o755 with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
    mkdir_p (Filename.dirname path);
    mkdir_p path
;;

let create (root_dir : string) : (t, err) Result.t =
  let canonical_root_dir = canonicalize root_dir in
  mkdir_p canonical_root_dir;
  Ok { root_dir = Unix.realpath canonical_root_dir }
;;

let list_sessions (storage : t) : Session.t list =
  let paths = Array.to_list (Stdlib.Sys.readdir storage.root_dir) in
  List.filter_map paths ~f:Session.of_filename
;;

(** The directory in which an event will be stored *)
let event_dir (storage : t) (session : Session.t) (source : string) : string =
  let session_dir = Session.to_filename session in
  let source_dir = source in
  String.concat [ storage.root_dir; session_dir; source_dir ] ~sep:Filename.dir_sep
;;

let event_file (storage : t) (session : Session.t) (source : string) : string =
  let dir = event_dir storage session source in
  let file = "events.json" in
  Filename.concat dir file
;;

type 'data stored_event =
  { event_data : 'data
  ; time : float
  }
[@@deriving yojson]

let stored_event_to_event
  (session : Session.t)
  (source : string)
  (stored : 'data stored_event)
  : 'data Event.t
  =
  Event.{ event_data = stored.event_data; session; source; time = stored.time }
;;

let store (storage : t) ~(event : 'data Event.t) ~(serialize : 'data -> Yojson.Safe.t)
  : (unit, err) Result.t
  =
  let dir = event_dir storage event.session event.source in
  mkdir_p dir;
  let event' = { event_data = event.event_data; time = event.time } in
  let js = stored_event_to_yojson serialize event' in
  let path = event_file storage event.session event.source in
  Out_channel.with_open_gen
    [ Open_wronly; Open_append; Open_creat; Open_text ]
    0o644
    path
    (fun oc ->
       Yojson.Safe.to_channel ~suf:"\n" oc js;
       Ok ())
;;

let load_session
  (storage : t)
  ~(source : string)
  ~(session : Session.t)
  ~(deserialize : Yojson.Safe.t -> ('data, string) Result.t)
  : ('data Event.t list, err) Result.t
  =
  let event_path = event_file storage session source in
  if not (Stdlib.Sys.file_exists event_path)
  then Error "not found"
  else (
    let records = Stdlib.List.of_seq (Yojson.Safe.seq_from_file event_path) in
    let load_results = List.map records ~f:(stored_event_of_yojson deserialize) in
    let stored_events, errors = List.partition_result load_results in
    let events = List.map stored_events ~f:(stored_event_to_event session source) in
    match errors with
    | [] -> Ok events
    | _ -> Error "errors")
;;
