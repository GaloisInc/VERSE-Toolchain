open Base
module ITree = Cnlsp.ITree
module Position = Cnlsp.Position
module Range = Cnlsp.Range

let range_t =
  let pp fmt r = Stdlib.Format.pp_print_string fmt (Range.to_string r) in
  let equal = Range.eq in
  Alcotest.testable pp equal
;;

let test_empty () =
  Alcotest.(check (list (pair range_t unit)))
    "empty tree to list"
    []
    (ITree.to_list ITree.empty);
  let posn = Position.create ~line:0 ~character:0 in
  Alcotest.(check (list (pair range_t unit)))
    "intersections in empty tree"
    []
    (ITree.intersecting ITree.empty posn)
;;

module Singleton = struct
  let start = Position.create ~line:1 ~character:1
  let middle = Position.create ~line:1 ~character:3
  let end_ = Position.create ~line:1 ~character:5
  let range = Range.create ~start ~end_

  (** A tree with a single element at 1:1-1:5 *)
  let tree : unit ITree.t = ITree.(insert range () empty)
end

let test_singleton_to_list () =
  Alcotest.(check (list (pair range_t unit)))
    "singleton to list"
    [ Singleton.range, () ]
    (ITree.to_list Singleton.tree)
;;

let test_singleton_at () =
  let tree = Singleton.tree in
  let range = Singleton.range in
  Alcotest.(check (option unit)) "at range" (Some ()) (ITree.at tree range);
  let range' = Range.create ~start:Singleton.start ~end_:Singleton.middle in
  Alcotest.(check (option unit)) "not at other range" None (ITree.at tree range')
;;

let test_singleton_intersecting () =
  let tree = Singleton.tree in
  let range = Singleton.range in
  Alcotest.(check (list (pair range_t unit)))
    "intersects with beginning"
    [ range, () ]
    (ITree.intersecting tree Singleton.start);
  Alcotest.(check (list (pair range_t unit)))
    "intersects with middle"
    [ range, () ]
    (ITree.intersecting tree Singleton.middle);
  Alcotest.(check (list (pair range_t unit)))
    "intersects with end"
    [ range, () ]
    (ITree.intersecting tree Singleton.end_)
;;

type expr =
  | Lit of int
  | Var of string
  | Add of (expr * expr)

let rec expr_eq (e1 : expr) (e2 : expr) =
  match e1, e2 with
  | Lit i1, Lit i2 -> i1 = i2
  | Var s1, Var s2 -> String.equal s1 s2
  | Add (e1l, e1r), Add (e2l, e2r) -> expr_eq e1l e2l && expr_eq e1r e2r
  | _ -> false
;;

type decl = Decl of (string * expr)

let decl_eq (Decl (s1, e1) : decl) (Decl (s2, e2) : decl) =
  String.equal s1 s2 && expr_eq e1 e2
;;

type elem =
  | Decl of decl
  | Expr of expr

let elem_eq (e1 : elem) (e2 : elem) =
  match e1, e2 with
  | Decl d1, Decl d2 -> decl_eq d1 d2
  | Expr ex1, Expr ex2 -> expr_eq ex1 ex2
  | _ -> false
;;

(** Defines a tree that maps locations to the [elem]s located there, as per this
    sample program, annotated with line and column numbers for reference:

    {v
 | 0         1    |
 | 01234567890123 |
1| int x = 3;     |
2| int y = 4;     |
3| int z = x + y; |
    v} *)
module Elems = struct
  (** A position that doesn't intersect with anything *)
  let nowhere = Position.create ~line:0 ~character:0

  let e_3 = Lit 3
  let e_3_start = Position.create ~line:1 ~character:8
  let e_3_end = Position.create ~line:1 ~character:9
  let e_3_range = Range.create ~start:e_3_start ~end_:e_3_end
  let e_4 = Lit 4
  let e_4_start = Position.create ~line:2 ~character:8
  let e_4_end = Position.create ~line:2 ~character:9
  let e_4_range = Range.create ~start:e_4_start ~end_:e_4_end
  let e_x = Var "x"
  let e_x_start = Position.create ~line:3 ~character:8
  let e_x_end = Position.create ~line:3 ~character:9
  let e_x_range = Range.create ~start:e_x_start ~end_:e_x_end
  let e_y = Var "y"
  let e_y_start = Position.create ~line:3 ~character:12
  let e_y_end = Position.create ~line:3 ~character:13
  let e_y_range = Range.create ~start:e_y_start ~end_:e_y_end
  let e_sum = Add (e_x, e_y)
  let e_sum_start = Position.create ~line:3 ~character:8
  let e_sum_end = Position.create ~line:3 ~character:13
  let e_sum_range = Range.create ~start:e_sum_start ~end_:e_sum_end
  let d_x : decl = Decl ("x", e_3)
  let d_x_start = Position.create ~line:1 ~character:0
  let d_x_end = Position.create ~line:1 ~character:9
  let d_x_range = Range.create ~start:d_x_start ~end_:d_x_end
  let d_y : decl = Decl ("y", e_4)
  let d_y_start = Position.create ~line:2 ~character:0
  let d_y_end = Position.create ~line:2 ~character:9
  let d_y_range = Range.create ~start:d_y_start ~end_:d_y_end
  let d_z : decl = Decl ("z", e_sum)
  let d_z_start = Position.create ~line:3 ~character:0
  let d_z_end = Position.create ~line:3 ~character:13
  let d_z_range = Range.create ~start:d_z_start ~end_:d_z_end

  let elements =
    [ e_3_range, Expr e_3
    ; e_4_range, Expr e_4
    ; e_x_range, Expr e_x
    ; e_y_range, Expr e_y
    ; e_sum_range, Expr e_sum
    ; d_x_range, Decl d_x
    ; d_y_range, Decl d_y
    ; d_z_range, Decl d_z
    ]
  ;;

  let tree = ITree.from_list elements
end

let elem_t =
  let rec expr_to_string (expr : expr) : string =
    match expr with
    | Lit i -> Int.to_string i
    | Var s -> s
    | Add (e1, e2) -> expr_to_string e1 ^ " + " ^ expr_to_string e2
  in
  let decl_to_string (Decl (v, e) : decl) : string =
    "int " ^ v ^ " = " ^ expr_to_string e
  in
  let elem_to_string (elem : elem) : string =
    match elem with
    | Decl d -> decl_to_string d
    | Expr e -> expr_to_string e
  in
  let pp fmt e = Stdlib.Format.pp_print_string fmt (elem_to_string e) in
  Alcotest.testable pp elem_eq
;;

let test_elems_to_list () =
  let expected = List.sort Elems.elements ~compare:Poly.compare in
  let actual = List.sort (ITree.to_list Elems.tree) ~compare:Poly.compare in
  Alcotest.(check (list (pair range_t elem_t))) "all to list" expected actual
;;

let test_elems_intersecting () =
  let tree = Elems.tree in
  let sum_start_intersecting_expected =
    List.sort
      [ Elems.e_sum_range, Expr Elems.e_sum
      ; Elems.d_z_range, Decl Elems.d_z
      ; Elems.e_x_range, Expr Elems.e_x
      ]
      ~compare:Poly.compare
  in
  let sum_start_intersecting_actual =
    List.sort (ITree.intersecting tree Elems.e_sum_start) ~compare:Poly.compare
  in
  let () =
    Alcotest.(check (list (pair range_t elem_t)))
      "intersecting with sum start"
      sum_start_intersecting_expected
      sum_start_intersecting_actual
  in
  let sum_end_intersecting_expected =
    List.sort
      [ Elems.e_sum_range, Expr Elems.e_sum
      ; Elems.d_z_range, Decl Elems.d_z
      ; Elems.e_y_range, Expr Elems.e_y
      ]
      ~compare:Poly.compare
  in
  let sum_end_intersecting_actual =
    List.sort (ITree.intersecting tree Elems.e_sum_end) ~compare:Poly.compare
  in
  let () =
    Alcotest.(check (list (pair range_t elem_t)))
      "intersecting with sum end"
      sum_end_intersecting_expected
      sum_end_intersecting_actual
  in
  let nowhere_expected = [] in
  let nowhere_actual = ITree.intersecting tree Elems.nowhere in
  let () =
    Alcotest.(check (list (pair range_t elem_t)))
      "nowhere"
      nowhere_expected
      nowhere_actual
  in
  ()
;;

let tests =
  let test_case = Alcotest.test_case in
  [ test_case "empty" `Quick test_empty
  ; test_case "singleton_to_list" `Quick test_singleton_to_list
  ; test_case "singleton_at" `Quick test_singleton_at
  ; test_case "singleton_intersecting" `Quick test_singleton_intersecting
  ; test_case "elems_to_list" `Quick test_elems_to_list
  ; test_case "elems_intersecting" `Quick test_elems_intersecting
  ]
;;
