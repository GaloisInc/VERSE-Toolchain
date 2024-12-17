open Base

(** A module that can store and load events. *)
module type S = sig
  (** The type of the storage instance. *)
  type t

  (** The type used to configure the instance. *)
  type config

  (** Errors associated with the instance. *)
  type err

  (** The type of events stored in this instance. *)
  type event

  (** Create a new storage instance *)
  val create : config -> (t, err) Result.t

  (** Store an event for later loading. The event should persist as long as the
      storage object exists, but whether it persists for longer is
      implementation-defined. *)
  val store_event : t -> event:event -> (unit, err) Result.t

  (** Load previously-stored events associated with a given session. *)
  val load_events : t -> session:Session.t -> (event list, err) Result.t
end
