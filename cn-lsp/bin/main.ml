open Cnlsp

let log_setup (log_path : string option) : unit =
  let log_channel =
    match log_path with
    | None -> Out_channel.stdout
    | Some log_file -> Out_channel.open_text log_file
  in
  let formatter = Format.formatter_of_out_channel log_channel in
  let reporter = Logs.format_reporter ~app:formatter ~dst:formatter () in
  let () = Logs.set_level (Some Logs.Debug) in
  let () = Logs.set_reporter reporter in
  ()
;;

type options =
  { log_path : string option
  ; pipe_path : string
  }

let rec unwords strs =
  match strs with
  | [] -> ""
  | s :: ss -> s ^ " " ^ unwords ss
;;

let parse_arguments () : options =
  let pipe_path_ref : string option ref = ref None in
  let log_file_ref : string option ref = ref None in
  let populate r s = r := Some s in
  let usage =
    unwords [ "Usage: cn-lsp-server"; "[--log <log-file>]"; "--pipe <pipe-file>" ]
  in
  let arglist =
    [ ( "--log"
      , Arg.String (populate log_file_ref)
      , "Path to log file - defaults to stdout if not specified" )
    ; ( "--pipe"
      , Arg.String (populate pipe_path_ref)
      , "Path to pipe file to use for communication" )
    ]
  in
  let handle_positional _ = () in
  let () = Arg.parse arglist handle_positional usage in
  match !pipe_path_ref with
  | Some pipe_path -> { log_path = !log_file_ref; pipe_path }
  | None ->
    print_endline usage;
    exit 1
;;

let main () : unit =
  let options = parse_arguments () in
  log_setup options.log_path;
  Server.run ~socket_path:options.pipe_path
;;

let () = main ()
