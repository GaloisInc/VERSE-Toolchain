open Cnlsp

let log_setup () : unit =
  let log_file = "log.txt" in
  let log_channel = Out_channel.open_text log_file in
  let formatter = Format.formatter_of_out_channel log_channel in
  let reporter = Logs.format_reporter ~app:formatter ~dst:formatter () in
  let () = Logs.set_level (Some Logs.Debug) in
  let () = Logs.set_reporter reporter in
  ()
;;

let main () : unit =
  log_setup ();
  Server.run ()
;;

let () = main ()
