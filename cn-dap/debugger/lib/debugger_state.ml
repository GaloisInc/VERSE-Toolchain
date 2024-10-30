open Base

module Command = struct
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
