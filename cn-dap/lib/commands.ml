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

module Jump = struct
  let type_ = "jump"

  module Arguments = struct
    type t = { id : int } [@@deriving yojson]
  end

  (* TODO: deduplicate with `StepSpecific.Result` *)
  module Result = struct
    type t =
      { success : bool
      ; err : string option [@default None]
      }
    [@@deriving yojson]
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

module StepSpecific = struct
  let type_ = "stepSpecific"

  module Arguments = struct
    type t =
      { prev_id : int [@key "prevId"]
      ; branch_case : Yojson.Safe.t option [@key "branchCase"]
      }
    [@@deriving yojson]
  end

  (* TODO: deduplicate with `Jump.Result` *)
  module Result = struct
    type t =
      { success : bool
      ; err : string option [@default None]
      }
    [@@deriving yojson]
  end
end
