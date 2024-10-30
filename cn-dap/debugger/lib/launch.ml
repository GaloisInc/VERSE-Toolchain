open Base
module Dap = Debug_protocol

module Command = struct
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
