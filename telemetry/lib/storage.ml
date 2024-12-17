open Base

module type S = sig
  type config
  type event
  type t
  type err

  val create : config -> (t, err) Result.t
  val store : t -> event:event -> (unit, err) Result.t
  val load_session : t -> session:Session.t -> (event list, err) Result.t
end
