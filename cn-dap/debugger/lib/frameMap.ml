open Base
module Utils = Gillian.Debugger.Utils
module Exec_map = Utils.Exec_map

type id = int

type 'a next =
  | Zero
  | One of 'a
  | Many of 'a list

type t =
  { frames : (id, Cn.Report.state_report) Hashtbl.t
  ; ordering : (id, id next) Hashtbl.t
  ; revealed : Exec_map.Packaged.t
  ; root : id
  }

let find_frame (order : t) (id : id) : Cn.Report.state_report option =
  Hashtbl.find order.frames id
;;

let next_frames (map : t) (id : id) : id next option = Hashtbl.find map.ordering id

let id_of_int (i : int) : Logging.Report_id.t =
  Result.ok_or_failwith (Logging.Report_id.of_yojson (`Int i))
;;

type cmd_data = Exec_map.Packaged.cmd_data

let cmd_data_of_state_report (id : id) (state_report : Cn.Report.state_report) : cmd_data =
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

(* let get_node (map : t) (node_id : id) : Cn.Report.state_report option =
   Order.frame map.ordering node_id
   ;; *)

let reveal_node (map : t) (node_id : id) : unit =
  match find_frame map node_id with
  | None -> ()
  | Some frame ->
    let data = cmd_data_of_state_report node_id frame in
    let next =
      match next_frames map node_id with
      | None -> None
      | Some Zero -> None
      | Some (One _) -> Some (Exec_map.Single (None, "next"))
      | Some (Many _) -> Some (Exec_map.Branch [])
    in
    let node = Exec_map.{ data; next } in
    Exec_map.insert
      map.revealed
      ~all_ids:[ id_of_int node_id ]
      ~id:(id_of_int node_id)
      node
;;

let reveal_next (map : t) (prev_id : id) : id option =
  match next_frames map prev_id with
  | None ->
    Log.d (Printf.sprintf "Node %i had no `next`" prev_id);
    None
  | Some Zero ->
    Log.d (Printf.sprintf "Node %i was terminal" prev_id);
    None
  | Some (One next_id) ->
    Log.d (Printf.sprintf "Node %i had one `next`, %i " prev_id next_id);
    reveal_node map next_id;
    (* Go in and set `prev` to point to `next_id` as its next node *)
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
    let _ = Exec_map.map_node map.revealed (id_of_int prev_id) update_prev in
    Some next_id
  | Some (Many _next_ids) ->
    Log.d "Unimplemented: revealing `Many` next nodes";
    None
;;

let of_state_reports (state_reports : Cn.Report.state_report list) : t option =
  (* Secret kept: the ordering is just the order of the list *)
  let frames = Hashtbl.create (module Int) in
  let ordering = Hashtbl.create (module Int) in
  let identified_reports = List.mapi state_reports ~f:(fun idx rep -> idx, rep) in
  let root = 0 in
  let rec go reps =
    match reps with
    | [] -> ()
    | [ (id, rep) ] ->
      Hashtbl.add_exn frames ~key:id ~data:rep;
      Hashtbl.add_exn ordering ~key:id ~data:Zero
    | (id1, rep1) :: (id2, rep2) :: rest ->
      Hashtbl.add_exn frames ~key:id1 ~data:rep1;
      Hashtbl.add_exn ordering ~key:id1 ~data:(One id2);
      go ((id2, rep2) :: rest)
  in
  match identified_reports with
  | [] -> None
  | _ ->
    go identified_reports;
    let revealed = Exec_map.make () in
    revealed.root <- Some (id_of_int root);
    let map = { frames; ordering; revealed; root } in
    let () = reveal_node map root in
    Some map
;;
