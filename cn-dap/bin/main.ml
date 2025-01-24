open Cndap

let log_setup (log_path : string option) : unit =
  let log_file = Option.value log_path ~default:"/tmp/cn-debug-log.txt" in
  let log_channel = Out_channel.open_text log_file in
  let formatter = Format.formatter_of_out_channel log_channel in
  let reporter = Logs.format_reporter ~app:formatter ~dst:formatter () in
  let () = Logs.set_level (Some Logs.Debug) in
  let () = Logs.set_reporter reporter in
  ()
;;

type options = { log_path : string option }

let parse_arguments () : options =
  let log_file_ref : string option ref = ref None in
  let populate r s = r := Some s in
  let usage = String.concat " " [ "Usage: cn-debug"; "--log <log-file>" ] in
  let arglist = [ "--log", Arg.String (populate log_file_ref), "Path to log file" ] in
  let handle_positional _ = () in
  let () = Arg.parse arglist handle_positional usage in
  { log_path = !log_file_ref }
;;

let main () : unit =
  let options = parse_arguments () in
  log_setup options.log_path;
  Adapter.run_stdio ()
;;

let () = main ()
