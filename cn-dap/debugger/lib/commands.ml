open Base
module Dap = Debug_protocol

module DebuggerState = struct
  let type_ = "debuggerState"

  module Arguments = struct
    type t = unit

    let of_yojson = function
      | `Assoc [] -> Ok ()
      | _ -> Error "non-null"
    ;;

    let to_yojson () = `Assoc []
  end

  module Result = struct
    type t = Debugger.WireState.t [@@deriving yojson]
  end
end

module Launch = struct
  let type_ = Dap.Launch_command.type_

  module Arguments = struct
    type t =
      { program : string
      ; stop_on_entry : bool [@default false] [@key "stopOnEntry"]
      ; procedure_name : string option [@default None] [@key "procedureName"]
      }
    [@@deriving yojson { strict = false }]
  end

  module Result = Dap.Launch_command.Result
end
