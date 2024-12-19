open Base

(** In-memory storage of events that contain the given [EventData]. Basically a
    glorified list. *)
module M (EventData : EventData.S) :
  Storage.S
  with type config = unit
   and type event = Event.M(EventData).t
   and type profile = unit = struct
  module Event = Event.M (EventData)

  type t =
    { events : (Session.t, Event.t list) Hashtbl.t
    ; profile : unit option ref
    }

  type config = unit
  type err
  type event = Event.t
  type profile = unit

  let create (() : config) : (t, err) Result.t =
    Ok { events = Hashtbl.create (module Session); profile = ref None }
  ;;

  let store_event (storage : t) ~(event : event) : (unit, err) Result.t =
    Hashtbl.add_multi storage.events ~key:event.session ~data:event;
    Ok ()
  ;;

  let load_events (storage : t) ~(session : Session.t) : (event list, err) Result.t =
    Ok (List.rev (Hashtbl.find_multi storage.events session))
  ;;

  let store_profile (storage : t) ~profile:(_ : profile) : (profile option, err) Result.t =
    match !(storage.profile) with
    | None ->
      storage.profile := Some ();
      Ok None
    | Some () -> Ok (Some ())
  ;;

  let load_profile (storage : t) : (profile option, err) Result.t = Ok !(storage.profile)
end
