open Base
module Filename = Stdlib.Filename
module Event = Telemetry.Event
module Session = Telemetry.Session
module Storage = Telemetry.Storage

let ok (r : ('ok, _) Result.t) : 'ok = Option.value_exn (Result.ok r)

let storage =
  let tmpdir = Filename.temp_dir "telemetry-test" "" in
  ok (Storage.create ~root_dir:tmpdir)
;;

let source = "testing"

let event_t =
  let pp = Event.pp SampleEvent.pp in
  let equal = Event.equal SampleEvent.equal in
  Alcotest.testable pp equal
;;

let test_store_load () =
  let session = Session.custom () in
  let event = Event.create ~source ~session ~event_data:SampleEvent.startup in
  let () = ok (Storage.store storage ~event ~serialize:SampleEvent.to_yojson) in
  let load_result =
    ok (Storage.load_session storage ~source ~session ~deserialize:SampleEvent.of_yojson)
  in
  Alcotest.(check (list event_t)) "loaded vs. stored" [ event ] load_result
;;

let test_store_load_twice () =
  let session = Session.custom () in
  let event1 = Event.create ~source ~session ~event_data:SampleEvent.startup in
  let event2 = Event.create ~source ~session ~event_data:SampleEvent.shutdown in
  let () = ok (Storage.store storage ~event:event1 ~serialize:SampleEvent.to_yojson) in
  let () = ok (Storage.store storage ~event:event2 ~serialize:SampleEvent.to_yojson) in
  let load_result =
    ok
      (Storage.load_session
         storage
         ~source:"testing"
         ~session
         ~deserialize:SampleEvent.of_yojson)
  in
  Alcotest.(check (list event_t)) "loaded vs. stored" [ event1; event2 ] load_result
;;

let tests =
  let tc = Alcotest.test_case in
  [ tc "store once then load" `Quick test_store_load
  ; tc "store twice then load" `Quick test_store_load_twice
  ]
;;
