open Base

module type S = sig
  type t

  val equal : t -> t -> bool
  val pp : Stdlib.Format.formatter -> t -> unit
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) Result.t
  val source : string
end
