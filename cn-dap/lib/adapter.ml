open Base
module Dap = Debug_protocol

let capabilities (_init_args : Dap.Initialize_command.Arguments.t) : Dap.Capabilities.t =
  Dap.Capabilities.make ()
;;

let handle_initialize (init_args : Dap.Initialize_command.Arguments.t)
  : Dap.Capabilities.t Lwt.t
  =
  Lwt.return (capabilities init_args)
;;

let serve ~(in_ : Lwt_io.input_channel) ~(out : Lwt_io.output_channel) : unit Lwt.t =
  let rpc = Debug_rpc.create ~in_ ~out () in
  Debug_rpc.set_command_handler rpc (module Dap.Initialize_command) handle_initialize;
  Debug_rpc.start rpc
;;

let run_stdio () : unit =
  let task = serve ~in_:Lwt_io.stdin ~out:Lwt_io.stdout in
  match Lwt_main.run task with
  | () -> ()
  | exception e ->
    Log.e (Exn.to_string e);
    Stdlib.exit 1
;;
