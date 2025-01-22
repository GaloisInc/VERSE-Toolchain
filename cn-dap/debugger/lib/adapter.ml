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
        (match
           Debugger.make launch_args.report_dir launch_args.program procedure_name
         with
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

module Scope = struct
  type t =
    | Unproven
    | Requested
    | Constraints
    | Resources
    | Terms
  [@@deriving enumerate]

  let to_string (scope : t) : string =
    match scope with
    | Unproven -> "Goals: Unproven"
    | Requested -> "Goals: Requested"
    | Constraints -> "Constraints"
    | Resources -> "Resources"
    | Terms -> "Terms"
  ;;

  let all_scopes : (int, t) Hashtbl.t =
    let map = Hashtbl.create (module Int) in
    (* `idx + 1` because, per the spec, 0 is reserved as a reference to an
       "unstructured" variable whose value can't be explored. Because scope and
       variable references occupy the same namespace, using it here leads a
       client to treat whatever scope 0 is assigned to as an unstructured scope,
       which prevents its variables from even being listed. *)
    List.iteri all ~f:(fun idx scope -> Hashtbl.add_exn map ~key:(idx + 1) ~data:scope);
    map
  ;;

  (** Get the scope (as [t]) associated with a given variable reference *)
  let find_scope (variable_reference : int) : t option =
    Hashtbl.find all_scopes variable_reference
  ;;
end

let handle_scopes (rpc : Rpc.t) : unit =
  Rpc.set_command_handler
    rpc
    (module Dap.Scopes_command)
    (fun _args ->
      let scope (idx, scope) =
        Dap.Scope.make
          ~name:(Scope.to_string scope)
          ~variables_reference:idx
          ~expensive:false
          ()
      in
      let scopes = List.map (Hashtbl.to_alist Scope.all_scopes) ~f:scope in
      Lwt.return (Dap.Scopes_command.Result.make ~scopes ()))
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

let handle_variables (rpc : Rpc.t) (dbg : Debugger.t) : unit =
  Rpc.set_command_handler
    rpc
    (module Dap.Variables_command)
    (fun args ->
      let variable_scope = args.variables_reference in
      (*
         Per the spec: "[i]f `variablesReference` is > 0, the variable is
         structured and its children can be retrieved by passing
         `variablesReference` to the `variables` request"
      *)
      let unstructured_variable_reference = 0 in
      let variable_of_string s =
        Dap.Variable.make
          ~name:s
          ~value:""
          ~variables_reference:unstructured_variable_reference
          ()
      in
      let variable_of_term (t : Debugger.term) =
        Dap.Variable.make
          ~name:t.name
          ~value:t.value
          ~variables_reference:unstructured_variable_reference
          ()
      in
      let variables =
        match Scope.find_scope variable_scope with
        | None ->
          Log.e (Printf.sprintf "Scope %i not recognized!" variable_scope);
          []
        | Some Unproven ->
          (match Debugger.current_unproven dbg with
           | None -> []
           | Some s -> [ variable_of_string s ])
        | Some Requested ->
          (match Debugger.current_requested dbg with
           | None -> []
           | Some s -> [ variable_of_string s ])
        | Some Constraints ->
          (match Debugger.current_constraints dbg with
           | None ->
             Log.d "No constraints found";
             []
           | Some constraints -> List.map constraints ~f:variable_of_string)
        | Some Resources ->
          (match Debugger.current_resources dbg with
           | None ->
             Log.d "No resources found";
             []
           | Some resources -> List.map resources ~f:variable_of_string)
        | Some Terms ->
          (match Debugger.current_terms dbg with
           | None ->
             Log.d "No terms found";
             []
           | Some terms -> List.map terms ~f:variable_of_term)
      in
      Lwt.return (Dap.Variables_command.Result.make ~variables ()))
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
  handle_scopes rpc;
  handle_variables rpc debugger;
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
