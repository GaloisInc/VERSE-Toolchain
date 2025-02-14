open Base

module EventData = struct
  let source : string = "cn-lsp-server"

  (* Note: when creating (non-singleton) constructors of any sum types in this
     module, make them contain record data (e.g. [Foo of { thing : int }]),
     rather than bare values (e.g. [Foo of int]). This will hopefully simplify
     third-party consumption of any JSON-serialized values of this type. *)

  type event_type =
    | ServerStart
    | ServerStop
    | BeginVerify of
        { file : string
        ; fn_name : string option
        ; fn_body : string option
        }
    | EndVerify of
        { file : string
        ; fn_name : string option
        }
    | OpenFile of { file : string }
    | CloseFile of { file : string }
    | ChangeConfiguration of { cfg : ServerConfig.t }
  [@@deriving eq, show, yojson]

  type event_result =
    | Success
    | Failure of { causes : string list }
  [@@deriving eq, show, yojson]

  type t =
    { event_type : event_type
    ; event_result : event_result option
    }
  [@@deriving eq, show, yojson]
end

module ProfileData = struct
  type t = { id : string } [@@deriving eq, show, yojson]
end

module Event = Telemetry.Event.M (EventData)
module Storage = Telemetry.Disk.M (EventData) (ProfileData)
module StorageError = Telemetry.Disk.Error

module Error = struct
  type t =
    | CreateTelemetryEvent of exn
    | InitializeTelemetry of StorageError.t
    | RecordTelemetry of StorageError.t

  let to_string (err : t) : string =
    match err with
    | CreateTelemetryEvent exn ->
      Printf.sprintf "unable to create telemetry event: %s" (Exn.to_string exn)
    | InitializeTelemetry e ->
      Printf.sprintf "unable to initialize telemetry: %s" (StorageError.to_string e)
    | RecordTelemetry e ->
      Printf.sprintf "unable to record telemetry: %s" (StorageError.to_string e)
  ;;
end

let initialize ~(dir : string) ~(user_id : string option) : (Storage.t, Error.t) Result.t =
  match Storage.(create { root_dir = dir }) with
  | Error e -> Error (InitializeTelemetry e)
  | Ok storage ->
    (match user_id with
     | None -> Ok storage
     | Some id ->
       let profile = ProfileData.{ id } in
       (match Storage.store_profile storage ~profile with
        | Error e ->
          Log.e (Printf.sprintf "Unable to save user ID: %s" (StorageError.to_string e))
        | Ok (Some prev) ->
          Log.d
            (Printf.sprintf "Wrote new ID '%s' (overwrote existing ID '%s')" id prev.id)
        | Ok None -> Log.d (Printf.sprintf "Wrote new ID %s" id));
       Ok storage)
;;

let record (storage : Storage.t) (mk_event_data : unit -> EventData.t)
  : (unit, Error.t) Result.t
  =
  let session = Telemetry.Session.today () in
  match mk_event_data () with
  | exception exn -> Error (Error.CreateTelemetryEvent exn)
  | event_data ->
    let event = Event.create ~session ~event_data in
    Result.map_error (Storage.store_event storage ~event) ~f:(fun err ->
      Error.RecordTelemetry err)
;;
