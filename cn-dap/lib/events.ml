open! Base

module DebugStateUpdate = struct
  let type_ = "debugStateUpdate"

  module Payload = struct
    type t = Debugger.WireState.t [@@deriving yojson]
  end
end
