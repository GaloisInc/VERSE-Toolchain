(** Sessions are the primary indexer of [Event]s. A single session can index
    different event types from different event sources. *)

open Base

type t =
  | Day of
      { year : int
      ; month : int
      ; day : int
      } (** A session that encompasses the entire specified day. *)
  | Custom of { id : Int64.t } (** A session with custom meaning/duration. *)
[@@deriving eq, hash, ord, sexp, show]

(** The session that encompasses today's calendar date. *)
let today () : t =
  let tm = Unix.(gmtime (time ())) in
  Day { year = tm.tm_year + 1900; month = tm.tm_mon + 1; day = tm.tm_mday }
;;

(** A session guaranteed to be different from any previously-constructed
    session. *)
let custom () : t = Custom { id = Random.int64_incl Int64.zero Int64.max_value }
