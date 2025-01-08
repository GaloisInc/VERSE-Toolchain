open! Base

(** Logging *)

(** General-purpose logger at Debug *)
val debug : 'a Logs.log

(** General-purpose logger at Info *)
val info : 'a Logs.log

(** General-purpose logger at Warning *)
val warn : 'a Logs.log

(** General-purpose logger at Error *)
val err : 'a Logs.log

(** Log a string at Debug *)
val d : string -> unit

(** Log a string at Info *)
val i : string -> unit

(** Log a string at Warning *)
val w : string -> unit

(** Log a string at Error *)
val e : string -> unit
