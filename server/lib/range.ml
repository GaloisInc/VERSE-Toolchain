open! Base

type t = Lsp.Types.Range.t

let create ~(start : Position.t) ~(end_ : Position.t) : t =
  Lsp.Types.Range.create ~start ~end_
;;

let to_string (range : t) : string =
  Printf.sprintf "%s-%s" (Position.to_string range.start) (Position.to_string range.end_)
;;

let ( |--| ) ((l1, c1) : int * int) ((l2, c2) : int * int) : t =
  create
    ~start:(Position.create ~line:l1 ~character:c1)
    ~end_:(Position.create ~line:l2 ~character:c2)
;;

let of_cerb_loc (loc : Cerb_location.t) : t option =
  match Cerb_location.to_cartesian loc with
  | None -> None
  | Some ((l1, c1), (l2, c2)) ->
    let start = Position.create ~line:l1 ~character:c1 in
    let end_ = Position.create ~line:l2 ~character:c2 in
    Some (create ~start ~end_)
;;
