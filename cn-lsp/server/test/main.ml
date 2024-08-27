let () =
  Alcotest.run
    "cn-lsp-server"
    [ "Document", Document.tests; "ITree", ITree.tests; "SourceInfo", SourceInfo.tests ]
;;
