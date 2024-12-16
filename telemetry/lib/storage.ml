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
let event_dir (storage : t) (session : Session.t) : string =
  let session_dir = Session.to_filename session in
  String.concat [ storage.root_dir; session_dir ] ~sep:Filename.dir_sep
;;

let event_file (storage : t) (session : Session.t) : string =
  let dir = event_dir storage session in
  let file = "events.json" in
  Filename.concat dir file
;;

type 'data stored_event =
  { event_data : 'data
  ; source : string
  ; time : float
  }
[@@deriving yojson]

let stored_event_to_event (session : Session.t) (stored : 'data stored_event)
  : 'data Event.t
  =
  Event.
    { event_data = stored.event_data
    ; session
    ; source = stored.source
    ; time = stored.time
    }
;;

let event_to_stored_event (event : 'data Event.t) : 'data stored_event =
  { event_data = event.event_data; source = event.source; time = event.time }
;;

let store (storage : t) ~(event : 'data Event.t) ~(serialize : 'data -> Yojson.Safe.t)
  : (unit, err) Result.t
  =
  let dir = event_dir storage event.session in
  mkdir_p dir;
  let js = stored_event_to_yojson serialize (event_to_stored_event event) in
  let path = event_file storage event.session in
  Out_channel.with_open_gen
    [ Open_wronly; Open_append; Open_creat; Open_text ]
    0o644
    path
    (fun oc ->
       Yojson.Safe.to_channel ~suf:"\n" oc js;
       Ok ())
;;

let from_source (source : string) (event_js : Yojson.Safe.t) : Yojson.Safe.t option =
  try
    match Yojson.Safe.Util.member "source" event_js with
    | `String s when String.equal s source -> Some event_js
    | _ -> None
  with
  | _ -> None
;;

let load_session
  (storage : t)
  ~(source : string)
  ~(session : Session.t)
  ~(deserialize : Yojson.Safe.t -> ('data, string) Result.t)
  : ('data Event.t list, err) Result.t
  =
  let event_path = event_file storage session in
  if not (Stdlib.Sys.file_exists event_path)
  then Error "not found"
  else (
    let all_records = Stdlib.List.of_seq (Yojson.Safe.seq_from_file event_path) in
    let relevant_records = List.filter_map all_records ~f:(from_source source) in
    let load_results =
      List.map relevant_records ~f:(stored_event_of_yojson deserialize)
    in
    let stored_events, errors = List.partition_result load_results in
    let events = List.map stored_events ~f:(stored_event_to_event session) in
    match errors with
    | [] -> Ok events
    | _ -> Error "errors")
;;
