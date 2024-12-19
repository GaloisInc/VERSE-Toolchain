open! Base

type t = Lsp.Types.Position.t

val create : line:int -> character:int -> t
val compare : t -> t -> int
val lt : t -> t -> bool
val le : t -> t -> bool
val eq : t -> t -> bool
val ge : t -> t -> bool
val gt : t -> t -> bool
val to_string : t -> string
