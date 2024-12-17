open Base

module type S = sig
  type t
  type config
  type err
  type event

  val create : config -> (t, err) Result.t
  val store_event : t -> event:event -> (unit, err) Result.t
  val load_events : t -> session:Session.t -> (event list, err) Result.t
end
