open Base

type cfg = { root_dir : string }

module M (EventData : EventData.S) :
  Storage.S with type config = cfg and type event = Event.M(EventData).t = struct
  module Filename = Stdlib.Filename
  module Event = Event.M (EventData)

  type config = cfg
  type t = { root_dir : string }
  type event = Event.t

  type err =
    | Deserialization of string
    | Multiple of err list
    | Serialization of string
    | FileNotFound of string

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

  let create (config : config) : (t, err) Result.t =
    let canonical_root_dir = canonicalize config.root_dir in
    mkdir_p canonical_root_dir;
    Ok { root_dir = Unix.realpath canonical_root_dir }
  ;;

  let session_dir (storage : t) (session : Session.t) : string =
    Filename.concat storage.root_dir (Session.to_filename session)
  ;;

  let source_dir (storage : t) (session : Session.t) : string =
    let session_d = session_dir storage session in
    let source_d = Int.Hex.to_string (Hashable.hash EventData.source) in
    Filename.concat session_d source_d
  ;;

  let source_file (storage : t) (session : Session.t) : string =
    let source_d = source_dir storage session in
    Filename.concat source_d "source.json"
  ;;

  let event_file (storage : t) (session : Session.t) : string =
    let source_d = source_dir storage session in
    Filename.concat source_d "events.json"
  ;;

  type stored_event =
    { event_data : EventData.t
    ; time : float
    }
  [@@deriving yojson]

  let stored_event_to_event (session : Session.t) (stored : stored_event) : event =
    Event.
      { event_data = stored.event_data
      ; session
      ; source = EventData.source
      ; time = stored.time
      }
  ;;

  let event_to_stored_event (event : event) : stored_event =
    { event_data = event.event_data; time = event.time }
  ;;

  let store_source (storage : t) (event : event) : unit =
    let source_d = source_dir storage event.session in
    mkdir_p source_d;
    let source_f = source_file storage event.session in
    Yojson.Safe.to_file source_f (`String EventData.source)
  ;;

  let store_event (storage : t) (event : event) : (unit, err) Result.t =
    let source_d = source_dir storage event.session in
    mkdir_p source_d;
    let event_f = event_file storage event.session in
    let storable_event = event_to_stored_event event in
    let json = stored_event_to_yojson storable_event in
    Out_channel.with_open_gen
      [ Open_wronly; Open_append; Open_creat; Open_text ]
      0o644
      event_f
      (fun out_chan ->
         try
           Yojson.Safe.to_channel ~suf:"\n" out_chan json;
           Ok ()
         with
         | Yojson.Json_error s -> Error (Serialization s))
  ;;

  let store (storage : t) ~(event : event) : (unit, err) Result.t =
    store_source storage event;
    store_event storage event
  ;;

  let load_session (storage : t) ~(session : Session.t) : (event list, err) Result.t =
    let event_f = event_file storage session in
    if not (Stdlib.Sys.file_exists event_f)
    then Error (FileNotFound event_f)
    else (
      let objs = Stdlib.List.of_seq (Yojson.Safe.seq_from_file event_f) in
      let load_results =
        List.mapi objs ~f:(fun i obj ->
          match stored_event_of_yojson obj with
          | Ok e -> Ok e
          | Error s -> Error (Deserialization (Printf.sprintf "object %i: %s" i s)))
      in
      let stored_events, errors = List.partition_result load_results in
      let events = List.map stored_events ~f:(stored_event_to_event session) in
      match errors with
      | [] -> Ok events
      | [ err ] -> Error err
      | _ -> Error (Multiple errors))
  ;;
end
