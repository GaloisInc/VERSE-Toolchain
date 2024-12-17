# `telemetry`

`telemetry` is a lightweight package for storing and loading telemetric events
that is parametric over the type of stored event data and the implementation of
its storage.

Here's an annotated unit test of a single store-load workflow to illustrate how
a client might interact with this package:
```ocaml
(* Instantiate [Event] and [Storage] modules to hold events with data described
   by the [SampleEvent] module and profile data described by the [SampleProfile]
   module *)
module Event = Telemetry.Event.M (SampleEvent)
module Session = Telemetry.Session
module Storage = Telemetry.Disk.M (SampleEvent) (SampleProfile)

let test_store_load_event () =
  (* Create [session], a new [Session.t] to index events *)
  let session = Session.custom () in
  
  (* Create a new event in this session, containing the user-provided payload
     [SampleEvent.startup] *)
  let event = Event.create ~session ~event_data:SampleEvent.startup in
  
  (* Store the event *)
  let () = ok (Storage.store_event storage ~event) in
  
  (* Load all events associated with [session] *)
  let events = ok (Storage.load_events storage ~session) in
  
  (* Check that the sole inhabitant of [session] is the event we just created
     (paraphrasing the unit-test-specific operation) *)
  assert (events == [event])
;;
```

See [`test`](./test) for further interaction examples.
