open Base
module Dap = Debug_protocol
module Rpc = Debug_rpc

let capabilities_from_args (_init_args : Dap.Initialize_command.Arguments.t)
  : Dap.Capabilities.t
  =
  Dap.Capabilities.make ()
;;

let handle_initialize (rpc : Rpc.t) : Dap.Capabilities.t Lwt.t =
  let promise, resolver = Lwt.task () in
  Debug_rpc.set_command_handler
    rpc
    (module Dap.Initialize_command)
    (fun init_args ->
      let caps = capabilities_from_args init_args in
      (* Declare that `promise` will resolve to `caps` *)
      Lwt.wakeup_later resolver caps;
      (* Send our capabilities back to the client *)
      Lwt.return caps);
  (* Future access to capabilities, whenever they're created *)
  promise
;;

let startup (rpc : Rpc.t) : unit Lwt.t =
  let open Lwt.Syntax in
  let* _capabilities = handle_initialize rpc in
  Lwt.return_unit
;;

let serve ~(in_ : Lwt_io.input_channel) ~(out : Lwt_io.output_channel) : unit Lwt.t =
  let rpc = Debug_rpc.create ~in_ ~out () in
  Lwt.async (fun () -> startup rpc);
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
