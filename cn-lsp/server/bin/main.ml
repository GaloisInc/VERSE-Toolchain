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

type options = { pipe_path : string }

let rec unwords strs =
  match strs with
  | [] -> ""
  | s :: ss -> s ^ " " ^ unwords ss
;;

let parse_arguments () : options =
  let pipe_path_ref : string option ref = ref None in
  let populate r s = r := Some s in
  let usage = unwords [ "Usage: cn-lsp-server"; "--pipe <pipe-file>" ] in
  let arglist = [ "--pipe", Arg.String (populate pipe_path_ref), "Path to pipe file" ] in
  let handle_positional _ = () in
  let () = Arg.parse arglist handle_positional usage in
  match !pipe_path_ref with
  | Some pipe_path -> { pipe_path }
  | None ->
    print_endline usage;
    exit 1
;;

let main () : unit =
  log_setup ();
  let options = parse_arguments () in
  Server.run options.pipe_path
;;

let () = main ()
