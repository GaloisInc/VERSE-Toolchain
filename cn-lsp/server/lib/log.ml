let src : Logs.src = Logs.Src.create ~doc:"CN Language Server" "cn-language-server"

include (val Logs.src_log src)

let d (msg : string) : unit = debug (fun m -> m "%s" msg)
let i (msg : string) : unit = info (fun m -> m "%s" msg)
let w (msg : string) : unit = warn (fun m -> m "%s" msg)
let e (msg : string) : unit = err (fun m -> m "%s" msg)
