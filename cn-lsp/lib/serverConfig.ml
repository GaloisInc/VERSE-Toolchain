open! Base

(** The client controls these options, and sends them at a server's request *)
type t =
  { run_CN_on_save : bool [@key "runOnSave"]
  ; telemetry_dir : string option [@default None] [@key "telemetryDir"]
  ; user_id : string option [@default None] [@key "userID"]
  }
[@@deriving yojson { strict = false }]

(* `strict = false` to account for extra configuration fields the client
   defines but which the server doesn't care about (e.g., at the moment,
   "cerbRuntime"). There's probably a more idiomatic way to handle this - have
   the client put such fields in a different "section", perhaps? *)

(** The name of the configuration "section" the client uses to identify
    CN-specific settings *)
let section : string = "CN"

let default : t = { run_CN_on_save = false; telemetry_dir = None; user_id = None }
