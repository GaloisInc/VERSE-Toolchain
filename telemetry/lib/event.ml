open Base

module type S = functor (EventData : EventData.S) -> sig
  type t =
    { event_data : EventData.t
    ; session : Session.t
    ; source : string
    ; time : float
    }

  val equal : t -> t -> bool
  val pp : Stdlib.Format.formatter -> t -> unit
  val show : t -> string
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