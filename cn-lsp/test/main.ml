open! Base

let () =
  let lwtify tests =
    List.map tests ~f:(fun (n, s, f) -> Alcotest_lwt.test_case_sync n s f)
  in
  Lwt_main.run
    (Alcotest_lwt.run
       "cn-lsp"
       ~verbose:true
       [ "Parse", lwtify Parse.tests; "Server", Server.tests ])
;;
