open Base

module M (EventData : EventData.S) :
  Storage.S with type config = unit and type event = Event.M(EventData).t = struct
  module Event = Event.M (EventData)

  type event = Event.t
  type config = unit
  type t = { events : (Session.t, Event.t list) Hashtbl.t }
  type err

  let create (() : config) : (t, err) Result.t =
    Ok { events = Hashtbl.create (module Session) }
  ;;

  let store (storage : t) ~(event : event) : (unit, err) Result.t =
    Hashtbl.add_multi storage.events ~key:event.session ~data:event;
    Ok ()
  ;;

  let load_session (storage : t) ~(session : Session.t) : (event list, err) Result.t =
    Ok (List.rev (Hashtbl.find_multi storage.events session))
  ;;
end
