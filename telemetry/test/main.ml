let () = Alcotest.run "telemetry" [ "Disk", Disk.tests; "Memory", Memory.tests ]
