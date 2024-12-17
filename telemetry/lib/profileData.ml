open Base

(** The interface we require for custom profile data. *)
module type S = sig
  (** The type of the profile data. *)
  type t

  (** Equality comparison. Consider generating via [[@@deriving eq]]. *)
  val equal : t -> t -> bool

  (** Pretty-printing. Consider generating via [[@@deriving show]]. *)
  val pp : Stdlib.Format.formatter -> t -> unit

  (** JSON serialization. Consider generating via [[@@deriving yojson]]. *)
  val to_yojson : t -> Yojson.Safe.t

  (** JSON deserialization. Consider generating via [[@@deriving yojson]]. *)
  val of_yojson : Yojson.Safe.t -> (t, string) Result.t
end
