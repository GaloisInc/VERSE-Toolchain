type event_type =
  | ServerStart
  | ServerStop
  | VerifyFunction of string
[@@deriving eq, show, yojson]

type event_result =
  | Success
  | PartialSuccess of
      { successes : int
      ; failures : int
      }
  | Failure
[@@deriving eq, show, yojson]

type t =
  { event_type : event_type
  ; event_result : event_result option
  }
[@@deriving eq, show, yojson]

let startup : t = { event_type = ServerStart; event_result = None }
let shutdown : t = { event_type = ServerStop; event_result = None }
