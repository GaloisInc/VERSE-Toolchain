open Base

module EventData = struct
  let source : string = "cn-lsp-server"

  type event_type =
    | ServerStart
    | OpenFile of string
    | CloseFile of string
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
end

module ProfileData = struct
  type t = { id : string } [@@deriving eq, show, yojson]
end
