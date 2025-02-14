open Base

type cfg = { root_dir : string }

(** An extension of [Session] that supports a filename-friendly encoding of
    session values. *)
module Session = struct
  include Session

  (** Losslessly encode a session as a filename. *)
  let to_filename (session : t) : string =
    let tag =
      match session with
      | Day day -> Printf.sprintf "%i-%i-%i" day.year day.month day.day
      | Custom { id } -> Int64.to_string id
    in
    "session-" ^ tag
  ;;

  (** Attempt to decode a filename as a session. *)
  let of_filename (path : string) : t option =
    try
      Stdlib.Scanf.sscanf
        (Stdlib.Filename.basename path)
        "session-%i-%i-%i"
        (fun year month day -> Some (Day { year; month; day }))
    with
    | Stdlib.Scanf.Scan_failure _ ->
      (try
         Stdlib.Scanf.sscanf (Stdlib.Filename.basename path) "session-%Li" (fun id ->
           Some (Custom { id }))
       with
       | Stdlib.Scanf.Scan_failure _ -> None)
  ;;
end

module Error = struct
  type t =
    | Deserialization of string
    | FileNotFound of string
    | Multiple of t list
    | Serialization of string

  let rec to_string (err : t) : string =
    match err with
    | Deserialization s -> Printf.sprintf "deserialization error: %s" s
    | FileNotFound file -> Printf.sprintf "file '%s' not found" file
    | Multiple errs ->
      Printf.sprintf
        "multiple errors: %s"
        (String.concat (List.map errs ~f:to_string) ~sep:", ")
    | Serialization s -> Printf.sprintf "serialization error: %s" s
  ;;
end

(** Disk-backed storage of events that contain the given [EventData]. *)
module M (EventData : EventData.S) (ProfileData : ProfileData.S) :
  Storage.S
  with type config = cfg
   and type event = Event.M(EventData).t
   and type profile = ProfileData.t
   and type err = Error.t = struct
  (** With a root directory called "sample", here's how events in two different
      sessions from two different sources are organized:

      {v
      sample
      ├── profile.json
      ├── session-1
      │   ├── source-1-id
      │   │   ├── events.json
      │   │   └── source.json
      │   └── source-2-id
      │       ├── events.json
      │       └── source.json
      └── session-2
          ├── source-1-id
          │   ├── events.json
          │   └── source.json
          └── source-2-id
              ├── events.json
              └── source.json
      v}

      [profile.json]: contains user profile information provided in storage
      configuration.

      [session-*]: a filename-friendly encoding of an individual session.

      [source-*-id]: a filename-friendly encoding of an individual source.

      [events.json]: contains a sequence of JSON-encoded events.

      [source.json]: contains the [EventData]-provided source name, since the
      filename-friendly encoding is not reversible.

      This is subject to change - it's meant to be informative, not normative. *)

  module Filename = Stdlib.Filename
  module Event = Event.M (EventData)

  type t = { root_dir : string }
  type config = cfg
  type event = Event.t
  type profile = ProfileData.t
  type err = Error.t

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

  (** Create a storage instance rooted in the [config]-specified directory,
      creating the directory if it doesn't exist and succeeding if it does. *)
  let create (config : config) : (t, err) Result.t =
    let canonical_root_dir = canonicalize config.root_dir in
    mkdir_p canonical_root_dir;
    Ok { root_dir = Unix.realpath canonical_root_dir }
  ;;

  (** The file in which we store profile information. *)
  let profile_file (storage : t) : string =
    Filename.concat storage.root_dir "profile.json"
  ;;

  (** The directory that defines a session. *)
  let session_dir (storage : t) (session : Session.t) : string =
    Filename.concat storage.root_dir (Session.to_filename session)
  ;;

  (** The directory that defines our [EventData]'s source within the given
      session. *)
  let source_dir (storage : t) (session : Session.t) : string =
    let session_d = session_dir storage session in
    let source_d = Int.Hex.to_string (Hashable.hash EventData.source) in
    Filename.concat session_d source_d
  ;;

  (** The file in which we store our original, [EventData]-provided source name. *)
  let source_file (storage : t) (session : Session.t) : string =
    let source_d = source_dir storage session in
    Filename.concat source_d "source.json"
  ;;

  (** The file in which we store events within the given session from our
      [EventData]'s source. *)
  let event_file (storage : t) (session : Session.t) : string =
    let source_d = source_dir storage session in
    Filename.concat source_d "events.json"
  ;;

  (** A more minimal event representation suitable for on-disk storage. *)
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

  (** Write our [EventData]'s source name to file, creating any intermediate
      directories necessary to do so. *)
  let write_source_to_file (storage : t) (event : event) : unit =
    let source_d = source_dir storage event.session in
    mkdir_p source_d;
    let source_f = source_file storage event.session in
    Yojson.Safe.to_file source_f (`String EventData.source)
  ;;

  (** Write an event to file, creating any intermediate directories necessary to
      do so. *)
  let write_event_to_file (storage : t) (event : event) : (unit, err) Result.t =
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
         | Yojson.Json_error s -> Error (Error.Serialization s))
  ;;

  (** Store the original source string, if necessary, then store the event. *)
  let store_event (storage : t) ~(event : event) : (unit, err) Result.t =
    write_source_to_file storage event;
    write_event_to_file storage event
  ;;

  (** Load the events associated with a given session. *)
  let load_events (storage : t) ~(session : Session.t) : (event list, err) Result.t =
    let event_f = event_file storage session in
    if not (Stdlib.Sys.file_exists event_f)
    then Error (FileNotFound event_f)
    else (
      let objs = Stdlib.List.of_seq (Yojson.Safe.seq_from_file event_f) in
      let load_results =
        List.mapi objs ~f:(fun i obj ->
          match stored_event_of_yojson obj with
          | Ok e -> Ok e
          | Error s -> Error (Error.Deserialization (Printf.sprintf "object %i: %s" i s)))
      in
      let stored_events, errors = List.partition_result load_results in
      let events = List.map stored_events ~f:(stored_event_to_event session) in
      match errors with
      | [] -> Ok events
      | [ err ] -> Error err
      | _ -> Error (Multiple errors))
  ;;

  (** Attempt to read profile information from disk. *)
  let load_profile (storage : t) : (profile option, err) Result.t =
    let ( let@ ) x f = Result.bind x ~f in
    let profile_f = profile_file storage in
    if not (Stdlib.Sys.file_exists profile_f)
    then Ok None
    else
      let@ json =
        try Ok (Yojson.Safe.from_file profile_f) with
        | Yojson.Json_error e ->
          Error (Error.Deserialization (Printf.sprintf "profile file: %s" e))
      in
      match ProfileData.of_yojson json with
      | Error e -> Error (Error.Deserialization (Printf.sprintf "profile object: %s" e))
      | Ok profile -> Ok (Some profile)
  ;;

  (** Write profile information to disk. *)
  let store_profile (storage : t) ~(profile : profile) : (profile option, err) Result.t =
    let existing_profile =
      match load_profile storage with
      | Ok (Some existing) -> Some existing
      | Ok None | Error _ -> None
    in
    let profile_f = profile_file storage in
    let json = ProfileData.to_yojson profile in
    Yojson.Safe.to_file profile_f json;
    Ok existing_profile
  ;;
end
