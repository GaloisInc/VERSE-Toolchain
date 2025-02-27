open Base
module Filename = Stdlib.Filename
module Event = Telemetry.Event.M (SampleEvent)
module Session = Telemetry.Session
module Storage = Telemetry.Disk.M (SampleEvent) (SampleProfile)

let ok (r : ('ok, _) Result.t) : 'ok = Option.value_exn (Result.ok r)

(* A rough copy of [Filename.temp_dir] to allow building with OCaml versions
   prior to 5.1 *)
let make_temp_dir (prefix : string) (suffix : string) : string =
  let temp_dir = Filename.get_temp_dir_name () in
  let temp_file_name prefix suffix =
    let rnd = Stdlib.Random.bits () land 0xFFFFFF in
    Filename.concat temp_dir (Printf.sprintf "%s%06x%s" prefix rnd suffix)
  in
  let rec try_make tries =
    let name = temp_file_name prefix suffix in
    let perms = 0o700 in
    try
      Stdlib.Sys.mkdir name perms;
      name
    with
    | Sys_error _ as e -> if tries >= 20 then raise e else try_make (tries + 1)
  in
  try_make 0
;;

let storage =
  let tmpdir = make_temp_dir "telemetry-test" "" in
  ok Storage.(create { root_dir = tmpdir })
;;

let test_store_load_event () =
  let session = Session.custom () in
  let event = Event.create ~session ~event_data:SampleEvent.startup in
  let () = ok (Storage.store_event storage ~event) in
  let events = ok (Storage.load_events storage ~session) in
  Alcotest.(check (list (module Event))) "loaded vs. stored" [ event ] events
;;

let test_store_load_event_twice () =
  let session = Session.custom () in
  let event1 = Event.create ~session ~event_data:SampleEvent.startup in
  let event2 = Event.create ~session ~event_data:SampleEvent.shutdown in
  let () = ok (Storage.store_event storage ~event:event1) in
  let () = ok (Storage.store_event storage ~event:event2) in
  let events = ok (Storage.load_events storage ~session) in
  Alcotest.(check (list (module Event))) "loaded vs. stored" [ event1; event2 ] events
;;

let test_store_load_event_two_sessions () =
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

let test_load_store_overwrite_profile () =
  match ok (Storage.load_profile storage) with
  | Some _existing -> Alcotest.fail "found existing profile"
  | None ->
    let profile1 = SampleProfile.{ id = "id1" } in
    let profile2 = SampleProfile.{ id = "id2" } in
    (match ok (Storage.store_profile storage ~profile:profile1) with
     | Some _existing -> Alcotest.fail "found existing profile"
     | None ->
       (match ok (Storage.store_profile storage ~profile:profile2) with
        | None -> Alcotest.fail "failed to find existing profile"
        | Some existing ->
          Alcotest.(check (module SampleProfile)) "loaded vs. stored" existing profile1))
;;

let tests =
  let tc = Alcotest.test_case in
  [ tc "store event once then load" `Quick test_store_load_event
  ; tc "store event twice then load" `Quick test_store_load_event_twice
  ; tc "store event in two sessions then load" `Quick test_store_load_event_two_sessions
  ; tc "store profile then overwrite" `Quick test_load_store_overwrite_profile
  ]
;;
