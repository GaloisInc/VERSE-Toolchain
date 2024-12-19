open! Base

type t = Lsp.Types.Range.t

val create : start:Position.t -> end_:Position.t -> t
val to_string : t -> string

(** Convenience notation: [(begin_line, begin_char) |--| (end_line, end_char)] *)
val ( |--| ) : int * int -> int * int -> t

val of_cerb_loc : Cerb_location.t -> t option
