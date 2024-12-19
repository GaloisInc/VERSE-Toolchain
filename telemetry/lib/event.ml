open Base

(** A general-purpose event type with some generic metadata, parametric over the
    type of application-specific data the event carries. *)
module type S = functor (EventData : EventData.S) -> sig
  type t =
    { event_data : EventData.t
    ; session : Session.t
    ; source : string (** Source of event, as provided by [EventData.source] *)
    ; time : float (** Time of event creation (via [create]) *)
    }

  val equal : t -> t -> bool
  val pp : Stdlib.Format.formatter -> t -> unit
  val show : t -> string

  (** Generate a timestamp of the current time, and initialize a new event with
      it, as well as with the given session and event data. *)
  val create : session:Session.t -> event_data:EventData.t -> t
end

module M : S =
functor
  (EventData : EventData.S)
  ->
  struct
    type t =
      { event_data : EventData.t
      ; session : Session.t
      ; source : string
      ; time : float
      }
    [@@deriving eq, show]

    let create ~(session : Session.t) ~(event_data : EventData.t) : t =
      let source = EventData.source in
      let time = Unix.gettimeofday () in
      { event_data; session; source; time }
    ;;
  end
