open Base

type t
type err

val create : root_dir:string -> (t, err) Result.t

val store
  :  t
  -> event:'data Event.t
  -> serialize:('data -> Yojson.Safe.t)
  -> (unit, err) Result.t

val load_session
  :  t
  -> source:string
  -> session:Session.t
  -> deserialize:(Yojson.Safe.t -> ('data, string) Result.t)
  -> ('data Event.t list, err) Result.t
