open Base
module Dap = Debug_protocol
module Rpc = Debug_rpc

(** Multiple threads? Unheard of! *)
let thread_id = 0

let send_stopped_event rpc reason =
  let stopped = Dap.Stopped_event.Payload.make ~reason ~thread_id:(Some thread_id) () in
  Rpc.send_event rpc (module Dap.Stopped_event) stopped
;;

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

let handle_launch (rpc : Rpc.t) : Debugger.t Lwt.t =
  let promise, resolver = Lwt.task () in
  Rpc.set_command_handler
    rpc
    (module Commands.Launch)
    (fun launch_args ->
      match launch_args.procedure_name with
      | None -> Lwt.fail_with "no procedure name!"
      | Some procedure_name ->
        (* does debugger initialization need to be in Lwt? *)
        (match Debugger.make launch_args.program procedure_name with
         | Error s -> Lwt.fail_with s
         | Ok debugger ->
           Lwt.wakeup_later resolver debugger;
           Lwt.return_unit));
  promise
;;

let handle_debugger_state (rpc : Rpc.t) (dbg : Debugger.t) : unit =
  Rpc.set_command_handler
    rpc
    (module Commands.DebuggerState)
    (fun () -> Lwt.return (Debugger.wire_state dbg))
;;

let handle_jump (rpc : Rpc.t) (dbg : Debugger.t) : unit =
  let open Lwt.Syntax in
  Rpc.set_command_handler
    rpc
    (module Commands.Jump)
    (fun args ->
      Debugger.go_to_node dbg args.id;
      let state = Debugger.wire_state dbg in
      let* () = Rpc.send_event rpc (module Events.DebugStateUpdate) state in
      let* () = send_stopped_event rpc Step in
      Lwt.return Commands.Jump.Result.{ success = true; err = None })
;;

let handle_step_specific (rpc : Rpc.t) (dbg : Debugger.t) : unit =
  let open Lwt.Syntax in
  Rpc.set_command_handler
    rpc
    (module Commands.StepSpecific)
    (fun args ->
      match Debugger.go_to_next dbg args.prev_id with
      | Error s ->
        Lwt.return Commands.StepSpecific.Result.{ success = false; err = Some s }
      | Ok () ->
        let state = Debugger.wire_state dbg in
        let* () = Rpc.send_event rpc (module Events.DebugStateUpdate) state in
        let* () = send_stopped_event rpc Step in
        Lwt.return Commands.StepSpecific.Result.{ success = true; err = None })
;;

let handle_stack_trace (rpc : Rpc.t) (dbg : Debugger.t) =
  Rpc.set_command_handler
    rpc
    (module Dap.Stack_trace_command)
    (fun _args ->
      let line, column, end_line, end_column =
        match Debugger.current_location dbg with
        | None -> 0, 0, None, None
        | Some loc ->
          loc.start_line, loc.start_column, Some loc.end_line, Some loc.end_column
      in
      let source = Some (Dap.Source.make ~path:(Some dbg.source_file) ()) in
      let frame =
        Dap.Stack_frame.make
          ~id:0
          ~name:dbg.procedure_name
          ~source
          ~line
          ~column
          ~end_line
          ~end_column
          ()
      in
      Lwt.return
        (Dap.Stack_trace_command.Result.make
           ~stack_frames:[ frame ]
           ~total_frames:(Some 1)
           ()))
;;

let handle_threads (rpc : Rpc.t) : unit =
  Rpc.set_command_handler
    rpc
    (module Dap.Threads_command)
    (fun () ->
      Lwt.return
        (Dap.Threads_command.Result.make
           ~threads:[ Dap.Thread.make ~id:thread_id ~name:"main" ]
           ()))
;;

let startup (rpc : Rpc.t) : unit Lwt.t =
  let open Lwt.Syntax in
  let _capabilities = handle_initialize rpc in
  let* debugger = handle_launch rpc in
  Log.d "Initialized debugger";
  let* () = Rpc.send_event rpc (module Dap.Initialized_event) () in
  handle_debugger_state rpc debugger;
  handle_step_specific rpc debugger;
  handle_jump rpc debugger;
  handle_stack_trace rpc debugger;
  handle_threads rpc;
  let* () = send_stopped_event rpc Entry in
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
