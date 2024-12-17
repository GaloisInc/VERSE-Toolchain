open Base
module Filename = Stdlib.Filename
module Event = Telemetry.Event.M (SampleEvent)
module Session = Telemetry.Session
module Storage = Telemetry.Disk.M (SampleEvent)

let ok (r : ('ok, _) Result.t) : 'ok = Option.value_exn (Result.ok r)

let storage =
  let tmpdir = Filename.temp_dir "telemetry-test" "" in
  ok Storage.(create { root_dir = tmpdir })
;;

let test_store_load () =
  let session = Session.custom () in
  let event = Event.create ~session ~event_data:SampleEvent.startup in
  let () = ok (Storage.store_event storage ~event) in
  let events = ok (Storage.load_events storage ~session) in
  Alcotest.(check (list (module Event))) "loaded vs. stored" [ event ] events
;;

let test_store_load_twice () =
  let session = Session.custom () in
  let event1 = Event.create ~session ~event_data:SampleEvent.startup in
  let event2 = Event.create ~session ~event_data:SampleEvent.shutdown in
  let () = ok (Storage.store_event storage ~event:event1) in
  let () = ok (Storage.store_event storage ~event:event2) in
  let events = ok (Storage.load_events storage ~session) in
  Alcotest.(check (list (module Event))) "loaded vs. stored" [ event1; event2 ] events
;;

let test_store_load_two_sessions () =
  let session1 = Session.custom () in
  let session2 = Session.today () in
  let event1 = Event.create ~session:session1 ~event_data:SampleEvent.startup in
  let event2 = Event.create ~session:session2 ~event_data:SampleEvent.shutdown in
  let () = ok (Storage.store_event storage ~event:event1) in
  let () = ok (Storage.store_event storage ~event:event2) in
  let session1_events = ok (Storage.load_events storage ~session:session1) in
  Alcotest.(check (list (module Event))) "loaded vs. stored" [ event1 ] session1_events;
  let session2_events = ok (Storage.load_events storage ~session:session2) in
  Alcotest.(check (list (module Event))) "loaded vs. stored" [ event2 ] session2_events
;;

let tests =
  let tc = Alcotest.test_case in
  [ tc "store once then load" `Quick test_store_load
  ; tc "store twice then load" `Quick test_store_load_twice
  ; tc "store in two sessions then load" `Quick test_store_load_two_sessions
  ]
;;
