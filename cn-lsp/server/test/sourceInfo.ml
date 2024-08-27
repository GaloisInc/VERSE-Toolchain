open Base
module SourceInfo = Cnlsp.SourceInfo
module Spec = Cnlsp.Spec

(* TODO: deduplicate *)
let spec_t =
  Alcotest.testable
    (fun fmt spec -> Stdlib.Format.pp_print_string fmt (Spec.to_string spec))
    (fun s1 s2 -> String.equal (Spec.to_string s1) (Spec.to_string s2))
;;

module Sample = struct
  (** Based on the same source as [Document.Sample] *)

  let source_info =
    Option.value_exn
      (Result.ok (SourceInfo.from_source Document.Sample.uri Document.Sample.source))
  ;;

  let doc_info = Option.value_exn (SourceInfo.document source_info Document.Sample.uri)
end

let check_decl_range_lookup () =
  let foo_decl_info =
    Option.value_exn
      (SourceInfo.info_at Sample.doc_info Document.Sample.foo_decl_range.start)
  in
  let bar_decl_info =
    Option.value_exn
      (SourceInfo.info_at Sample.doc_info Document.Sample.bar_decl_range.start)
  in
  Alcotest.(check (option spec_t))
    "foo spec by decl posn"
    Document.Sample.foo_spec
    foo_decl_info.spec;
  Alcotest.(check (option spec_t))
    "bar spec by decl posn"
    Document.Sample.bar_spec
    bar_decl_info.spec
;;

let check_use_specs () =
  List.iter Document.Sample.foo_use_ranges ~f:(fun (foo_use_range : Cnlsp.Range.t) ->
    let foo_start = foo_use_range.start in
    let foo_use_info = Option.value_exn (SourceInfo.info_at Sample.doc_info foo_start) in
    Alcotest.(check (option spec_t))
      (Printf.sprintf "foo spec by use posn (%s)" (Cnlsp.Position.to_string foo_start))
      Document.Sample.foo_spec
      foo_use_info.spec)
;;

let tests =
  let test_case = Alcotest.test_case in
  [ test_case "specs by decl range" `Quick check_decl_range_lookup
  ; test_case "specs by use range" `Quick check_use_specs
  ]
;;
