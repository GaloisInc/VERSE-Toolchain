open Base

module type S = functor (EventData : EventData.S) -> sig
  type t
  type err

  val create : root_dir:string -> (t, err) Result.t
  val store : t -> event:Event.M(EventData).t -> (unit, err) Result.t
  val load_session : t -> session:Session.t -> (Event.M(EventData).t list, err) Result.t
end

module M : S =
functor
  (EventData : EventData.S)
  ->
  struct
    module Filename = Stdlib.Filename
    module Event = Event.M (EventData)

    type t = { root_dir : string }

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

    let create ~(root_dir : string) : (t, err) Result.t =
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

    type stored_event =
      { event_data : EventData.t
      ; source : string
      ; time : float
      }
    [@@deriving yojson]

    let stored_event_to_event (session : Session.t) (stored : stored_event) : Event.t =
      Event.
        { event_data = stored.event_data
        ; session
        ; source = stored.source
        ; time = stored.time
        }
    ;;

    let event_to_stored_event (event : Event.t) : stored_event =
      { event_data = event.event_data; source = event.source; time = event.time }
    ;;

    let store (storage : t) ~(event : Event.t) : (unit, err) Result.t =
      let dir = event_dir storage event.session in
      mkdir_p dir;
      let js = stored_event_to_yojson (event_to_stored_event event) in
      let path = event_file storage event.session in
      Out_channel.with_open_gen
        [ Open_wronly; Open_append; Open_creat; Open_text ]
        0o644
        path
        (fun oc ->
           try
             Yojson.Safe.to_channel ~suf:"\n" oc js;
             Ok ()
           with
           | Yojson.Json_error s -> Error (Serialization s))
    ;;

    let from_source (event_js : Yojson.Safe.t) : Yojson.Safe.t option =
      try
        match Yojson.Safe.Util.member "source" event_js with
        | `String s when String.equal s EventData.source -> Some event_js
        | _ -> None
      with
      | _ -> None
    ;;

    let load_session (storage : t) ~(session : Session.t) : (Event.t list, err) Result.t =
      let event_path = event_file storage session in
      if not (Stdlib.Sys.file_exists event_path)
      then Error (FileNotFound event_path)
      else (
        let all_objs = Stdlib.List.of_seq (Yojson.Safe.seq_from_file event_path) in
        let relevant_objs = List.filter_map all_objs ~f:from_source in
        let load_results =
          List.mapi relevant_objs ~f:(fun i obj ->
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
