open Base
module Dap = Debug_protocol
module Exec_map = Gillian.Debugger.Utils.Exec_map

type id = FrameMap.id

type t =
  { mutable current_node : id
  ; frame_map : FrameMap.t
  ; procedure_name : string
  ; source_file : string
  }

let report_from_file (file : string) : (Cn.Report.report, string) Result.t =
  let js = Yojson.Safe.from_file file in
  Cn.Report.report_of_yojson js
;;

let make (source_file : string) (procedure_name : string) : (t, string) Result.t =
  let ( let* ) x f = Result.bind x ~f in
  (* TODO: this improperly relies on the fact that
     `Cn.TypeErrors.mk_report_file_name` happens to only care about the filename
     in its error location parameter *)
  let source_loc =
    Cerb_location.point { Lexing.dummy_pos with pos_fname = source_file }
  in
  let source_dir = Stdlib.Filename.dirname source_file in
  let report_file =
    Cn.TypeErrors.mk_report_file_name ~fn_name:procedure_name source_dir source_loc
  in
  Log.d (Printf.sprintf "Looking for report file at %s" report_file);
  let* report = report_from_file report_file in
  let with_locations =
    List.filter report.trace ~f:(fun t -> Option.is_some t.where.loc_cartesian)
  in
  let* frame_map =
    Result.of_option
      (FrameMap.of_state_reports with_locations)
      ~error:"unable to create frame map"
  in
  Result.Ok { current_node = frame_map.root; frame_map; procedure_name; source_file }
;;

let go_to_node (dbg : t) (node_id : id) : unit =
  Log.d (Printf.sprintf "Setting current node to %i" node_id);
  (* TODO: worthwhile to keep a set of nodes we've seen? Erroneous to go to one
     we haven't? *)
  dbg.current_node <- node_id
;;

let go_to_next (dbg : t) (prev_id : id) : (unit, string) Result.t =
  match FrameMap.reveal_next dbg.frame_map prev_id with
  | None -> Error "unable to step"
  | Some next_id ->
    go_to_node dbg next_id;
    Ok ()
;;

type location =
  { start_line : int
  ; start_column : int
  ; end_line : int
  ; end_column : int
  }

let current_location (dbg : t) : location option =
  let ( let* ) x f = Option.bind x ~f in
  let* state_report = FrameMap.find_frame dbg.frame_map dbg.current_node in
  let* (start_line, start_column), (end_line, end_column) =
    state_report.where.loc_cartesian
  in
  Some
    { start_line = start_line + 1
    ; start_column = start_column + 1
    ; end_line = end_line + 1
    ; end_column = end_column + 1
    }
;;

let current_constraints (dbg : t) : string list option =
  let ( let* ) x f = Option.bind x ~f in
  let* state_report = FrameMap.find_frame dbg.frame_map dbg.current_node in
  let* interesting = Cn.Report.(get_labeled state_report.constraints lab_interesting) in
  Some (List.map interesting ~f:(fun view -> Cn.Pp.plain view.original))
;;

let current_resources (dbg : t) : string list option =
  let ( let* ) x f = Option.bind x ~f in
  let* state_report = FrameMap.find_frame dbg.frame_map dbg.current_node in
  let* interesting = Cn.Report.(get_labeled state_report.resources lab_interesting) in
  Some (List.map interesting ~f:(fun view -> Cn.Pp.plain view.original))
;;

type term =
  { name : string
  ; value : string
  }

let term_of_term_entry (entry : Cn.Report.term_entry) : term =
  { name = Cn.Pp.plain entry.term; value = Cn.Pp.plain entry.value }
;;

let current_terms (dbg : t) : term list option =
  let ( let* ) x f = Option.bind x ~f in
  let* state_report = FrameMap.find_frame dbg.frame_map dbg.current_node in
  let* interesting = Cn.Report.(get_labeled state_report.terms lab_interesting) in
  Some (List.map interesting ~f:term_of_term_entry)
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

  let of_exec_map
    (exec_map : Exec_map.Packaged.t)
    (proc_name : string)
    (current_cmd_id : int)
    : t
    =
    let procs = Hashtbl.create (module String) ~size:1 in
    let proc_state =
      { exec_map = Exec_map.make ()
      ; lifted_exec_map = exec_map
      ; current_cmd_id
      ; matches = []
      ; proc_name
      }
    in
    let _ = Hashtbl.add procs ~key:proc_name ~data:proc_state in
    { main_proc = proc_name; current_proc = proc_name; procs }
  ;;
end

(* TODO: the abstraction boundaries between this module, WireState, and FrameMap
   feel off... *)
let wire_state (dbg : t) : WireState.t =
  WireState.of_exec_map dbg.frame_map.revealed dbg.procedure_name dbg.current_node
;;