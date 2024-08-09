open Base

(** A mapping from intervals (here, [Range.t]s) to data *)
type 'a t

(** The empty interval tree. *)
val empty : 'a t

(** Map a range to an item, replacing any item already indexed by the range. *)
val insert : Range.t -> 'a -> 'a t -> 'a t

(** Convert a list of ranged items to a tree. This is more efficient than
    repeated calls to [insert], but is semantically equivalent. *)
val from_list : (Range.t * 'a) list -> 'a t

(** Convert a tree to a list of ranged items. *)
val to_list : 'a t -> (Range.t * 'a) list

(** Get the item at the given range, if it exists. *)
val at : 'a t -> Range.t -> 'a option

(** Get any items whose ranges overlap with the given position. *)
val intersecting : 'a t -> Position.t -> (Range.t * 'a) list
