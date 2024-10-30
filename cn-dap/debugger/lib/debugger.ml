open Base
module Dap = Debug_protocol
module Exec_map = Gillian.Debugger.Utils.Exec_map

module FrameMap = struct
  type t =
    { exec_map : Exec_map.Packaged.t
    ; order : Order.t
    }

  type id = int

  let id_of_int (i : int) : Logging.Report_id.t =
    Result.ok_or_failwith (Logging.Report_id.of_yojson (`Int i))
  ;;

  type cmd_data = Exec_map.Packaged.cmd_data

  let cmd_data_of_state_report (id : id) (state_report : Cn.Report.state_report)
    : cmd_data
    =
    let locstr ((l1, c1), (l2, c2)) = Printf.sprintf "%i:%i - %i:%i" l1 c1 l2 c2 in
    { id = id_of_int id
    ; all_ids = [ id_of_int id ]
    ; display =
        Option.value_map state_report.where.loc_cartesian ~default:"<none>" ~f:locstr
    ; matches = []
    ; errors = []
    ; submap = NoSubmap
    }
  ;;

  let add_node (map : t) (node_id : id) : unit =
    match Order.frame map.order node_id with
    | None -> ()
    | Some frame ->
      let data = cmd_data_of_state_report node_id frame in
      let next =
        match Order.next map.order node_id with
        | None -> None
        | Some Zero -> None
        | Some (One _) -> Some (Exec_map.Single (None, "next"))
        | Some (Many _) -> Some (Exec_map.Branch [])
      in
      let node = Exec_map.{ data; next } in
      Exec_map.insert
        map.exec_map
        ~all_ids:[ id_of_int node_id ]
        ~id:(id_of_int node_id)
        node
  ;;

  let reveal_next (map : t) (prev_id : id) : unit =
    match Order.next map.order prev_id with
    | None -> Log.d (Printf.sprintf "Node %i had no `next`" prev_id)
    | Some Zero -> Log.d (Printf.sprintf "Node %i was terminal" prev_id)
    | Some (One next_id) ->
      Log.d (Printf.sprintf "Node %i had one `next`, %i " prev_id next_id);
      add_node map next_id;
      let update_prev prev_node =
        let new_next =
          match Exec_map.(prev_node.next) with
          | Some (Exec_map.Single (_, x)) ->
            Some (Exec_map.Single (Some (id_of_int next_id), x))
          | n ->
            Log.d (Printf.sprintf "Unexpected existing `next` for node %i" prev_id);
            n
        in
        { prev_node with next = new_next }
      in
      let _ = Exec_map.map_node map.exec_map (id_of_int prev_id) update_prev in
      ()
    | Some (Many next_ids) ->
      List.iter next_ids ~f:(add_node map);
      Log.d "Unimplemented: revealing `Many` next nodes"
  ;;

  let of_order (order : Order.t) : t =
    let exec_map = Exec_map.make () in
    exec_map.root <- Some (id_of_int order.root);
    let map = { exec_map; order } in
    let () = add_node map order.root in
    map
  ;;
end

type t =
  { frame_map : FrameMap.t
  ; procedure_name : string
  ; source_file : string
  }

let report_from_file (file : string) : (Cn.Report.report, string) Result.t =
  let js = Yojson.Safe.from_file file in
  Cn.Report.report_of_yojson js
;;

let from_launch_args (launch_args : Launch.Command.Arguments.t) : (t, string) Result.t =
  let ( let* ) x f = Result.bind x ~f in
  let source_file = launch_args.program in
  (* TODO: this improperly relies on the fact that
     `Cn.TypeErrors.mk_report_file_name` happens to only care about the filename
     in its error location parameter *)
  let source_loc =
    Cerb_location.point { Lexing.dummy_pos with pos_fname = source_file }
  in
  let source_dir = Stdlib.Filename.dirname source_file in
  let* procedure_name =
    Result.of_option launch_args.procedure_name ~error:"no procedure name"
  in
  let report_file =
    Cn.TypeErrors.mk_report_file_name ~fn_name:procedure_name source_dir source_loc
  in
  Log.d (Printf.sprintf "Looking for report file at %s" report_file);
  let* report = report_from_file report_file in
  let* order =
    Result.of_option
      (Order.of_state_reports report.trace)
      ~error:"unable to create ordering"
  in
  let frame_map = FrameMap.of_order order in
  Result.Ok { frame_map; procedure_name; source_file }
;;

module WireState = struct
  type state =
    { exec_map : Exec_map.Packaged.t [@key "execMap"]
    ; lifted_exec_map : Exec_map.Packaged.t [@key "liftedExecMap"]
    ; current_cmd_id : int [@key "currentCmdId"]
    ; matches : Exec_map.matching list
    ; proc_name : string [@key "procName"]
    }
  [@@deriving yojson]

  let procs_to_yosjon (procs : (string, state) Hashtbl.t) : Yojson.Safe.t =
    let entries = Hashtbl.to_alist procs in
    `Assoc (List.map entries ~f:(fun (s, p) -> s, state_to_yojson p))
  ;;

  let procs_of_yojson (json : Yojson.Safe.t)
    : ((string, state) Hashtbl.t, string) Result.t
    =
    let ( let@ ) x f = Result.bind x ~f in
    let pairs = Yojson.Safe.Util.to_assoc json in
    let of_pair (name, proc_json) =
      let@ proc = state_of_yojson proc_json in
      Ok (name, proc)
    in
    let@ entries = Result.all (List.map pairs ~f:of_pair) in
    let tbl = Hashtbl.of_alist_or_error (module String) entries in
    Result.map_error tbl ~f:Error.to_string_hum
  ;;

  type t =
    { main_proc : string [@key "mainProc"]
    ; current_proc : string [@key "currentProc"]
    ; procs : (string, state) Hashtbl.t
         [@to_yojson procs_to_yosjon] [@of_yojson procs_of_yojson]
    }
  [@@deriving yojson]

  let of_exec_map (exec_map : Exec_map.Packaged.t) (proc_name : string) : t =
    let procs = Hashtbl.create (module String) ~size:1 in
    let proc_state =
      { exec_map = Exec_map.make ()
      ; lifted_exec_map = exec_map
      ; current_cmd_id = 0
      ; matches = []
      ; proc_name
      }
    in
    let _ = Hashtbl.add procs ~key:proc_name ~data:proc_state in
    { main_proc = proc_name; current_proc = proc_name; procs }
  ;;
end

let wire_state (dbg : t) : WireState.t =
  WireState.of_exec_map dbg.frame_map.exec_map dbg.procedure_name
;;
