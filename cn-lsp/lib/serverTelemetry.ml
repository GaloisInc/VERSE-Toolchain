open Base

module EventData = struct
  let source : string = "cn-lsp-server"

  (* Note: when creating (non-singleton) constructors of any sum types in this
     module, make them contain record data (e.g. [Foo of { thing : int }]),
     rather than bare values (e.g. [Foo of int]). This will hopefully simplify
     third-party consumption of any JSON-serialized values of this type. *)

  type event_type =
    | ServerStart
    | ServerStop
    | BeginVerify of { file : string }
    | EndVerify of { file : string }
    | OpenFile of { file : string }
    | CloseFile of { file : string }
    | ChangeConfiguration of { cfg : ServerConfig.t }
  [@@deriving eq, show, yojson]

  type event_result =
    | Success
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
