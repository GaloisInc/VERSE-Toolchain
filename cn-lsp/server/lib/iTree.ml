open Base

type 'a ranged =
  { data : 'a
  ; range : Range.t
  }

let enrange (range : Range.t) (data : 'a) : 'a ranged = { data; range }
let derange (ranged : 'a ranged) : 'a = ranged.data

(** Assumes [items] is sorted *)
let rec insert_by (comp : 'a -> 'a -> int) (item : 'a) (items : 'a list) : 'a list =
  match items with
  | x :: xs when comp item x < 0 -> x :: insert_by comp item xs
  | _ -> item :: items
;;

module Intersections = struct
  (* TODO: we probably don't need to keep two copies around... *)
  type 'a t =
    { by_start : 'a ranged list (** Sorted by start, ascending *)
    ; by_end : 'a ranged list (** Sorted by end, descending *)
    }

  let empty : 'a t = { by_start = []; by_end = [] }

  let from_list (elements : 'a ranged list) : 'a t =
    let by_start =
      List.sort elements ~compare:(fun r1 r2 ->
        Position.compare r1.range.start r2.range.start)
    in
    (* The reverse comparison is intentional *)
    let by_end =
      List.sort elements ~compare:(fun r1 r2 ->
        Position.compare r2.range.end_ r1.range.end_)
    in
    { by_start; by_end }
  ;;

  let add (element : 'a ranged) (intersections : 'a t) : 'a t =
    let by_start =
      insert_by
        (fun r1 r2 -> Position.compare r1.range.start r2.range.start)
        element
        intersections.by_start
    in
    (* The reverse comparison is intentional *)
    let by_end =
      insert_by
        (fun r1 r2 -> Position.compare r2.range.end_ r1.range.end_)
        element
        intersections.by_end
    in
    { by_start; by_end }
  ;;

  let to_list (intersections : 'a t) : 'a ranged list = intersections.by_start
end

type 'a t =
  | Leaf
  | Node of
      { before : 'a t
      ; center : Position.t
      ; intersections : 'a Intersections.t
      ; after : 'a t
      }

let empty : 'a t = Leaf

let line_bounds (start : Position.t) (elements : 'a ranged list) : int * int =
  let update (min, max) e =
    Int.min e.range.start.line min, Int.max e.range.end_.line max
  in
  List.fold_left elements ~init:(start.line, start.line) ~f:update
;;

let triage (center : Position.t) (elements : 'a ranged list)
  : 'a ranged list * 'a ranged list * 'a ranged list
  =
  let assign (to_left, overlapping, to_right) e =
    if Position.lt e.range.end_ center
    then e :: to_left, overlapping, to_right
    else if Position.gt e.range.start center
    then to_left, overlapping, e :: to_right
    else to_left, e :: overlapping, to_right
  in
  List.fold_left elements ~init:([], [], []) ~f:assign
;;

let rec from_ranged_list ?(center : Position.t option) (elements : 'a ranged list) : 'a t =
  match elements with
  | [] -> Leaf
  | e :: _ ->
    let min_line, max_line = line_bounds e.range.start elements in
    let center_line = min_line + ((max_line - min_line) / 2) in
    let center =
      Option.value
        center
        ~default:
          (Lsp.Types.Position.create ~character:e.range.start.character ~line:center_line)
    in
    let to_left, overlapping, to_right = triage center elements in
    let intersections = Intersections.from_list overlapping in
    Node
      { before = from_ranged_list to_left
      ; center
      ; intersections
      ; after = from_ranged_list to_right
      }
;;

let from_list (elements : (Range.t * 'a) list) : 'a t =
  let to_ranged (r, a) = { data = a; range = r } in
  from_ranged_list (List.map elements ~f:to_ranged)
;;

let rec take_while (f : 'a -> bool) (xs : 'a list) : 'a list =
  match xs with
  | [] -> []
  | z :: zs when f z -> z :: take_while f zs
  | _ -> []
;;

let rec insert (range : Range.t) (data : 'a) (itree : 'a t) : 'a t =
  let elem = enrange range data in
  match itree with
  | Leaf ->
    let intersections = Intersections.add elem Intersections.empty in
    Node { before = Leaf; center = range.start; intersections; after = Leaf }
  | Node { before; center; intersections; after } ->
    if Position.lt range.end_ center
    then insert range data before
    else if Position.gt range.start center
    then insert range data after
    else (
      let intersections' = Intersections.add elem intersections in
      Node { before; center; intersections = intersections'; after })
;;

let rec ranged_intersecting (tree : 'a t) (posn : Position.t) : 'a ranged list =
  match tree with
  | Leaf -> []
  | Node { before; center; intersections; after } ->
    if Position.lt posn center
    then (
      let intersections =
        take_while (fun e -> Position.le e.range.start posn) intersections.by_start
      in
      intersections @ ranged_intersecting before posn)
    else if Position.gt posn center
    then (
      let intersections =
        take_while (fun e -> Position.ge e.range.end_ posn) intersections.by_end
      in
      intersections @ ranged_intersecting after posn)
    else intersections.by_start
;;

let intersecting (tree : 'a t) (posn : Position.t) : (Range.t * 'a) list =
  List.map (ranged_intersecting tree posn) ~f:(fun ranged -> ranged.range, ranged.data)
;;

let rec at (tree : 'a t) (range : Range.t) : 'a option =
  match tree with
  | Leaf -> None
  | Node { before; center; intersections; after } ->
    if Position.lt range.end_ center
    then at before range
    else if Position.gt range.start center
    then at after range
    else
      Option.map
        (List.find (Intersections.to_list intersections) ~f:(fun e -> Range.eq e.range range))
        ~f:derange
;;

let rec to_list (tree : 'a t) : (Range.t * 'a) list =
  match tree with
  | Leaf -> []
  | Node { before; center = _; intersections; after } ->
    let here =
      List.map (Intersections.to_list intersections) ~f:(fun ranged ->
        ranged.range, ranged.data)
    in
    to_list before @ here @ to_list after
;;
