open Base

let source : string =
  String.concat
    [ "#define X 42"
    ; "int foo() {"
    ; "  int x = X;"
    ; "  int y = x;"
    ; "  return x + y;"
    ; "}"
    ]
    ~sep:"\n"
;;

let file : Cnlsp.Uri.t =
  let filename = Stdlib.Filename.temp_file "prefix" ".c" in
  Out_channel.with_open_text filename (fun oc -> Out_channel.output_string oc source);
  Cnlsp.Uri.of_path filename
;;

let check_extract_zero_lines () =
  let range = Cnlsp.Range.((99, 0) |--| (100, 0)) in
  let expected = "" in
  let actual_source = Cnlsp.Parse.extract_from_source range source in
  let actual_file = Cnlsp.Parse.extract_from_file range file in
  Alcotest.(check string) "source correct" expected actual_source;
  Alcotest.(check string) "source matches file" actual_source actual_file
;;

let check_extract_one_line () =
  let range = Cnlsp.Range.((0, 1) |--| (0, 7)) in
  let expected = "define" in
  let actual_source = Cnlsp.Parse.extract_from_source range source in
  let actual_file = Cnlsp.Parse.extract_from_file range file in
  Alcotest.(check string) "source correct" expected actual_source;
  Alcotest.(check string) "source matches file" actual_source actual_file
;;

let check_extract_two_lines () =
  let range = Cnlsp.Range.((0, 1) |--| (1, 7)) in
  let expected = "define X 42\nint foo" in
  let actual_source = Cnlsp.Parse.extract_from_source range source in
  let actual_file = Cnlsp.Parse.extract_from_file range file in
  Alcotest.(check string) "source correct" expected actual_source;
  Alcotest.(check string) "source matches file" actual_source actual_file
;;

let check_extract_three_lines () =
  let range = Cnlsp.Range.((0, 1) |--| (2, 7)) in
  let expected = "define X 42\nint foo() {\n  int x" in
  let actual_source = Cnlsp.Parse.extract_from_source range source in
  let actual_file = Cnlsp.Parse.extract_from_file range file in
  Alcotest.(check string) "source correct" expected actual_source;
  Alcotest.(check string) "source matches file" actual_source actual_file
;;

let check_preprocess () =
  let expected =
    String.concat
      [ "int foo() {"; "  int x = 42;"; "  int y = x;"; "  return x + y;"; "}" ]
      ~sep:"\n"
  in
  match Cnlsp.Parse.read_process "cc" [| "cc"; "-E"; Cnlsp.Uri.to_path file |] with
  | Error e -> failwith ("failed to preprocess: " ^ Cnlsp.Parse.Error.to_string e)
  | Ok out ->
    Alcotest.(check int) "exit code zero" 0 out.exit_code;
    let stdout_lines = String.split_lines out.stdout in
    let actual = List.rev (List.take (List.rev stdout_lines) 5) in
    Alcotest.(check string)
      "preprocessed correct"
      expected
      (String.concat actual ~sep:"\n")
;;

let tests =
  let tc = Alcotest.test_case in
  [ tc "zero-line range extraction" `Quick check_extract_zero_lines
  ; tc "one-line range extraction" `Quick check_extract_one_line
  ; tc "two-line range extraction" `Quick check_extract_two_lines
  ; tc "three-line range extraction" `Quick check_extract_three_lines
  ; tc "preprocessing" `Quick check_preprocess
  ]
;;
