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
  ; order : (id, id next) Hashtbl.t
  ; root : id
  }

let frame (order : t) (id : id) : Cn.Report.state_report option =
  Hashtbl.find order.frames id
;;

let next (order : t) (id : id) : id next option = Hashtbl.find order.order id

let of_state_reports (state_reports : Cn.Report.state_report list) : t option =
  (* Secret kept: the ordering is just the order of the list *)
  let frames = Hashtbl.create (module Int) in
  let order = Hashtbl.create (module Int) in
  let identified_reports = List.mapi state_reports ~f:(fun idx rep -> idx, rep) in
  let root = 0 in
  let rec go reps =
    match reps with
    | [] -> ()
    | [ (id, rep) ] ->
      Hashtbl.add_exn frames ~key:id ~data:rep;
      Hashtbl.add_exn order ~key:id ~data:Zero
    | (id1, rep1) :: (id2, rep2) :: rest ->
      Hashtbl.add_exn frames ~key:id1 ~data:rep1;
      Hashtbl.add_exn order ~key:id1 ~data:(One id2);
      go ((id2, rep2) :: rest)
  in
  match identified_reports with
  | [] -> None
  | _ ->
    go identified_reports;
    Some { frames; order; root }
;;
