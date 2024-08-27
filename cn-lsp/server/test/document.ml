open Base
module DocumentUri = Lsp.Types.DocumentUri
module Document = Cnlsp.Document
module Range = Cnlsp.Range
module Spec = Cnlsp.Spec

(* TODO: deduplicate *)
let range_t =
  let pp fmt r = Stdlib.Format.pp_print_string fmt (Range.to_string r) in
  Alcotest.testable pp Range.eq
;;

(* TODO: deduplicate *)
let spec_t =
  Alcotest.testable
    (fun fmt spec -> Stdlib.Format.pp_print_string fmt (Spec.to_string spec))
    (fun s1 s2 -> String.equal (Spec.to_string s1) (Spec.to_string s2))
;;

(* TODO: deduplicate *)
let uri_t =
  Alcotest.testable
    (fun fmt spec -> Stdlib.Format.pp_print_string fmt (DocumentUri.to_string spec))
    (fun u1 u2 -> DocumentUri.equal u1 u2)
;;

module Sample = struct
  let uri = DocumentUri.of_path "sample.c"

  let source : string =
    {|int foo()
/*@
requires true;
@*/
{
  int x = 0;
  return x;
}

int bar()
{
  int y = foo();
  int z = foo();
  return z;
}
|}
  ;;

  let foo = `Ident "foo"
  let foo_decl_range = Cnlsp.Range.((0, 4) |--| (0, 7))
  let foo_spec = Some (Spec.of_string "\nrequires true;\n")

  let foo_use_ranges =
    [ Cnlsp.Range.((11, 10) |--| (11, 13)); Cnlsp.Range.((12, 10) |--| (12, 13)) ]
  ;;

  let bar = `Ident "bar"
  let bar_decl_range = Range.((9, 4) |--| (9, 7))
  let bar_spec = None

  let decls =
    match Document.parse_document_source uri source with
    | Error _ -> assert false
    | Ok decls -> decls
  ;;

  let doc = Document.process_external_declarations uri decls
end

let check_num_decls () =
  Alcotest.(check int) "# of declarations" 2 (List.length Sample.decls)
;;

let check_uris () =
  let foo_uri, _, _ = Option.value_exn (Document.find_ident Sample.doc Sample.foo) in
  let bar_uri, _, _ = Option.value_exn (Document.find_ident Sample.doc Sample.bar) in
  Alcotest.(check uri_t) "foo URI" Sample.uri foo_uri;
  Alcotest.(check uri_t) "bar URI" Sample.uri bar_uri
;;

let check_ranges () =
  let _, foo_decl_range, _ =
    Option.value_exn (Document.find_ident Sample.doc Sample.foo)
  in
  let _, bar_decl_range, _ =
    Option.value_exn (Document.find_ident Sample.doc Sample.bar)
  in
  Alcotest.(check range_t) "foo range" Sample.foo_decl_range foo_decl_range;
  Alcotest.(check range_t) "bar range" Sample.bar_decl_range bar_decl_range
;;

let check_decl_specs () =
  let _, _, foo_spec_opt = Option.value_exn (Document.find_ident Sample.doc Sample.foo) in
  let _, _, bar_spec_opt = Option.value_exn (Document.find_ident Sample.doc Sample.bar) in
  Alcotest.(check (option spec_t)) "foo spec" Sample.foo_spec foo_spec_opt;
  Alcotest.(check (option spec_t)) "bar spec" Sample.bar_spec bar_spec_opt
;;

let tests =
  let test_case = Alcotest.test_case in
  [ test_case "number of declarations" `Quick check_num_decls
  ; test_case "URIs" `Quick check_uris
  ; test_case "ranges" `Quick check_ranges
  ; test_case "specs at declarations" `Quick check_decl_specs
    (* ; test_case "specs by decl range" `Quick check_decl_range_lookup *)
    (* ; test_case "specs by use range" `Quick check_use_specs *)
  ]
;;
