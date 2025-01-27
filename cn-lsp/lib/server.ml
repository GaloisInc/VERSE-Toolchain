open! Base
module Json = Yojson.Safe

(* Linol *)
module Rpc = Linol_lwt.Jsonrpc2
module IO = Rpc.IO

(* LSP Types *)
module CNotif = Lsp.Client_notification
module ConfigurationItem = Lsp.Types.ConfigurationItem
module ConfigurationParams = Lsp.Types.ConfigurationParams
module Diagnostic = Lsp.Types.Diagnostic
module DidSaveTextDocumentParams = Lsp.Types.DidSaveTextDocumentParams
module DocumentUri = Lsp.Types.DocumentUri
module MessageType = Lsp.Types.MessageType
module PublishDiagnosticsParams = Lsp.Types.PublishDiagnosticsParams
module Registration = Lsp.Types.Registration
module RegistrationParams = Lsp.Types.RegistrationParams
module ShowMessageParams = Lsp.Types.ShowMessageParams
module SReq = Lsp.Server_request
module TextDocumentContentChangeEvent = Lsp.Types.TextDocumentContentChangeEvent
module TextDocumentIdentifier = Lsp.Types.TextDocumentIdentifier
module TextDocumentItem = Lsp.Types.TextDocumentItem
module VersionedTextDocumentIdentifier = Lsp.Types.VersionedTextDocumentIdentifier

(* Telemetry *)
module EventData = ServerTelemetry.EventData
module ProfileData = ServerTelemetry.ProfileData
module Event = Telemetry.Event.M (EventData)
module Storage = Telemetry.Disk.M (EventData) (ProfileData)

let cwindow (level : MessageType.t) (notify : Rpc.notify_back) (msg : string) : unit IO.t =
  let params = ShowMessageParams.create ~message:msg ~type_:level in
  let msg = Lsp.Server_notification.ShowMessage params in
  notify#send_notification msg
;;

let cinfo (notify : Rpc.notify_back) (msg : string) : unit IO.t =
  cwindow MessageType.Info notify msg
;;

let sprintf = Printf.sprintf

class lsp_server (env : LspCn.cerb_env) =
  object (self)
    val env : LspCn.cerb_env = env
    val mutable server_config : ServerConfig.t = ServerConfig.default
    val mutable telemetry_storage : Storage.t option = None
    inherit Rpc.server

    (* Required *)
    method spawn_query_handler f = Linol_lwt.spawn f

    (***************************************************************)
    (***  Notifications  *******************************************)

    (* Required *)
    method on_notif_doc_did_open
      ~notify_back:(_ : Rpc.notify_back)
      (doc : TextDocumentItem.t)
      ~content:(_ : string)
      : unit IO.t =
      let uri = DocumentUri.to_string doc.uri in
      Log.d (sprintf "Opened document %s" uri);
      let event_data =
        EventData.{ event_type = OpenFile { file = uri }; event_result = None }
      in
      self#record_telemetry event_data;
      IO.return ()

    (* Required *)
    method on_notif_doc_did_change
      ~(notify_back : Rpc.notify_back)
      (doc : VersionedTextDocumentIdentifier.t)
      (_changes : TextDocumentContentChangeEvent.t list)
      ~old_content:(_ : string)
      ~new_content:(_ : string)
      : unit IO.t =
      self#clear_diagnostics_for notify_back doc.uri

    (* Required *)
    method on_notif_doc_did_close
      ~notify_back:(_ : Rpc.notify_back)
      (doc : TextDocumentIdentifier.t)
      : unit IO.t =
      let uri = DocumentUri.to_string doc.uri in
      Log.d (sprintf "Closed document %s" uri);
      let event_data =
        EventData.{ event_type = CloseFile { file = uri }; event_result = None }
      in
      self#record_telemetry event_data;
      IO.return ()

    method on_notif_doc_did_save
      ~(notify_back : Rpc.notify_back)
      (params : DidSaveTextDocumentParams.t)
      : unit IO.t =
      let open IO in
      if server_config.run_CN_on_save
      then self#run_cn notify_back params.textDocument.uri
      else return ()

    method on_notif_initialized (notify_back : Rpc.notify_back) : unit IO.t =
      let open IO in
      let* cfg = self#fetch_configuration notify_back in
      server_config <- cfg;
      (match server_config.telemetry_dir with
       | None -> ()
       | Some dir -> self#initialize_telemetry dir);
      let event_data = EventData.{ event_type = ServerStart; event_result = None } in
      self#record_telemetry event_data;
      let* () = self#register_did_change_configuration notify_back in
      return ()

    method on_notification_unhandled
      ~(notify_back : Rpc.notify_back)
      (notif : CNotif.t)
      : unit IO.t =
      let open IO in
      match notif with
      | CNotif.Initialized -> self#on_notif_initialized notify_back
      | CNotif.ChangeConfiguration params ->
        let config_section = params.settings |> Json.Util.member ServerConfig.section in
        (match ServerConfig.of_yojson config_section with
         | Error err -> failwith (sprintf "Failed to decode config: %s" err)
         | Ok cfg ->
           Log.d (sprintf "Replacing config with: %s" (Json.to_string config_section));
           let old_telemetry_dir = server_config.telemetry_dir in
           server_config <- cfg;
           let new_telemetry_dir = server_config.telemetry_dir in
           if not (Option.equal String.equal old_telemetry_dir new_telemetry_dir)
           then
             cinfo
               notify_back
               "Restart server for changes to telemetry configuration to take effect"
           else return ())
      | _ ->
        let s =
          Json.to_string (Jsonrpc.Notification.yojson_of_t (CNotif.to_jsonrpc notif))
        in
        let () = Log.d ("Unhandled notification: " ^ s) in
        return ()

    (***************************************************************)
    (***  Requests  ************************************************)

    method on_unknown_request
      ~(notify_back : Rpc.notify_back)
      ~server_request:(_ : Rpc.server_request_handler_pair -> Jsonrpc.Id.t IO.t)
      ~id:(_ : Jsonrpc.Id.t)
      (method_name : string)
      (params : Jsonrpc.Structured.t option)
      : Json.t IO.t =
      let open IO in
      match method_name with
      | "$/runCN" ->
        let obj = Jsonrpc.Structured.yojson_of_t (Option.value_exn params) in
        let uri =
          Json.Util.(
            obj |> member "textDocument" |> member "uri" |> DocumentUri.t_of_yojson)
        in
        (* The URI isn't set automatically on unknown/custom requests *)
        let () = notify_back#set_uri uri in
        let* () = self#run_cn notify_back uri in
        return `Null
      | _ -> failwith ("Unknown method: " ^ method_name)

    (***************************************************************)
    (***  Other  ***************************************************)

    (** Fetch the client's current configuration *)
    method fetch_configuration (notify_back : Rpc.notify_back) : ServerConfig.t IO.t =
      let open IO in
      let section = ConfigurationItem.create ~section:ServerConfig.section () in
      let params = ConfigurationParams.create ~items:[ section ] in
      let req = SReq.WorkspaceConfiguration params in
      let cfg_promise, cfg_resolver = Lwt.task () in
      let handle (response : (Json.t list, Jsonrpc.Response.Error.t) Result.t) : unit IO.t
        =
        let cfg_res =
          match response with
          | Ok [] -> Error "No CN config section found"
          | Ok [ section ] -> ServerConfig.of_yojson section
          | Ok sections ->
            let ss = String.concat ~sep:"," (List.map sections ~f:Json.to_string) in
            Error (sprintf "Too many config sections: [%s]" ss)
          | Error e ->
            Error
              (sprintf
                 "Client responded with error: %s"
                 (Json.to_string (Jsonrpc.Response.Error.yojson_of_t e)))
        in
        match cfg_res with
        | Ok cfg ->
          Lwt.wakeup_later cfg_resolver cfg;
          return ()
        | Error s -> failwith s
      in
      let _id = notify_back#send_request req handle in
      cfg_promise

    (** "Register" for a given client capability *)
    method register_capability
      ~(notify_back : Rpc.notify_back)
      ~(method_ : string)
      ?(registerOptions : Json.t option)
      ()
      : unit IO.t =
      let open IO in
      let registration = Registration.create ~id:method_ ~method_ ?registerOptions () in
      let params = RegistrationParams.create ~registrations:[ registration ] in
      let req = SReq.ClientRegisterCapability params in
      let handle response =
        match response with
        | Ok _ ->
          Log.d (sprintf "successfully registered method '%s'" method_);
          return ()
        | Error e ->
          Log.e
            (sprintf
               "failed to register method '%s': %s"
               method_
               (Json.to_string (Jsonrpc.Response.Error.yojson_of_t e)));
          return ()
      in
      let _id = notify_back#send_request req handle in
      return ()

    (** Ask the client to send a [workspace/didChangeConfiguration] notification
        when the configuration changes *)
    method register_did_change_configuration (notify_back : Rpc.notify_back) : unit IO.t =
      let method_ = "workspace/didChangeConfiguration" in
      let registerOptions =
        ConfigurationItem.(yojson_of_t (create ~section:ServerConfig.section ()))
      in
      self#register_capability ~notify_back ~method_ ~registerOptions ()

    method run_cn (notify_back : Rpc.notify_back) (uri : DocumentUri.t) : unit IO.t =
      let open IO in
      let begin_event =
        EventData.
          { event_type = BeginVerify { file = Uri.to_path uri }; event_result = None }
      in
      self#record_telemetry begin_event;
      match LspCn.(run (run_cn env uri)) with
      | Ok [] ->
        let end_event =
          EventData.
            { event_type = EndVerify { file = Uri.to_path uri }
            ; event_result = Some Success
            }
        in
        self#record_telemetry end_event;
        cinfo notify_back "No issues found"
      | Ok errs ->
        let end_event =
          EventData.
            { event_type = EndVerify { file = Uri.to_path uri }
            ; event_result = Some Failure
            }
        in
        self#record_telemetry end_event;
        let diagnostics = Hashtbl.to_alist (LspCn.errors_to_diagnostics errs) in
        self#publish_all notify_back diagnostics
      | Error err ->
        (match LspCn.error_to_diagnostic err with
         | None ->
           let end_event =
             EventData.
               { event_type = EndVerify { file = Uri.to_path uri }
               ; event_result = None (* could encode something richer here... *)
               }
           in
           self#record_telemetry end_event;
           Log.e (sprintf "Unable to decode error: %s" (LspCn.error_to_string err));
           return ()
         | Some (diag_uri, diag) ->
           let end_event =
             EventData.
               { event_type = EndVerify { file = Uri.to_path uri }
               ; event_result = Some Failure
               }
           in
           self#record_telemetry end_event;
           self#publish_diagnostics_for notify_back diag_uri [ diag ])

    method initialize_telemetry (dir : string) : unit =
      match Storage.(create { root_dir = dir }) with
      | Error _e -> Log.e "Unable to create telemetry storage"
      | Ok storage ->
        telemetry_storage <- Some storage;
        (match server_config.user_id with
         | None -> ()
         | Some id ->
           let profile = ProfileData.{ id } in
           (match Storage.store_profile storage ~profile with
            | Error _e -> Log.e "Unable to save user ID"
            | Ok (Some prev) ->
              Log.d (sprintf "Wrote new ID %s (overwrite existing ID %s)" id prev.id)
            | Ok None -> Log.d (sprintf "Wrote new ID %s" id)))

    method record_telemetry (event_data : EventData.t) : unit =
      match server_config.telemetry_dir, telemetry_storage with
      (* No telemetry directory has been configured *)
      | None, _ -> ()
      (* A directory has been configured, but for some reason we haven't
         initialized telemetry storage *)
      | Some dir, None -> self#initialize_telemetry dir
      (* A directory has been configured and we've initialized storage. Don't
         check that the directory and initialized storage match, because we only
         promise to initialize storage based on the directory configured at
         startup. *)
      | Some _, Some storage ->
        let session = Telemetry.Session.today () in
        let event = Event.create ~session ~event_data in
        (match Storage.store_event storage ~event with
         | Ok () -> ()
         | Error _e -> Log.e "couldn't store event")

    method clear_diagnostics_for
      (notify_back : Rpc.notify_back)
      (uri : DocumentUri.t)
      : unit IO.t =
      self#publish_diagnostics_for notify_back uri []

    method publish_diagnostics_for
      (notify_back : Rpc.notify_back)
      (uri : DocumentUri.t)
      (ds : Diagnostic.t list)
      : unit IO.t =
      match notify_back#get_uri with
      | Some cur_uri when DocumentUri.equal uri cur_uri -> notify_back#send_diagnostic ds
      | _ ->
        let version =
          match self#find_doc uri with
          | None -> None
          | Some doc_state -> Some doc_state.version
        in
        let params = PublishDiagnosticsParams.create ~uri ?version ~diagnostics:ds () in
        notify_back#send_notification (Lsp.Server_notification.PublishDiagnostics params)

    method publish_all
      (notify_back : Rpc.notify_back)
      (diags : (DocumentUri.t * Diagnostic.t list) list)
      : unit IO.t =
      let open IO in
      match diags with
      | [] -> return ()
      | (uri, ds) :: rest ->
        let* () = self#publish_diagnostics_for notify_back uri ds in
        self#publish_all notify_back rest
  end [@@warning "-7"]
(*
   Warning 7 tells us that a method has been overridden. We disable it because
   we have to override the default `Linol_lwt.Jsonrpc2.server` methods to define
   its behavior.
*)

let run ~(socket_path : string) : unit =
  let open IO in
  let () = Log.d "Starting" in
  let cn_env =
    match LspCn.(run (setup ())) with
    | Ok t -> t
    | Error e ->
      let msg = LspCn.error_to_string e in
      let () = Log.e ("Failed to start: " ^ msg) in
      Stdlib.exit 1
  in
  (* We have separate declarations because we want this function to have access
     to the server's custom methods, but [Rpc.create] expects something
     encapsulated as an [Rpc.server] in particular - and that encapsulation
     hides our methods. *)
  let cn_server = new lsp_server cn_env in
  let rpc_server = (cn_server :> Rpc.server) in
  let sockaddr = Lwt_unix.ADDR_UNIX socket_path in
  let sock = Lwt_unix.(socket PF_UNIX SOCK_STREAM) 0 in
  let task =
    let* () = Lwt_unix.connect sock sockaddr in
    let ic = Lwt_io.of_fd ~mode:Lwt_io.Input sock in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.Output sock in
    let server = Rpc.create ~ic ~oc rpc_server in
    let shutdown () =
      match rpc_server#get_status with
      | `ReceivedExit | `ReceivedShutdown ->
        (* Note: this is written this way to accomodate linol v0.6 - If
           upgrading to 0.7+, this logic can and should move to the
           [on_req_shutdown] method introduced in 0.7 *)
        let event_data = EventData.{ event_type = ServerStop; event_result = None } in
        cn_server#record_telemetry event_data;
        true
      | `Running -> false
    in
    let* () = Rpc.run ~shutdown server in
    let () = Log.d "Shutting down" in
    Lwt_unix.close sock
  in
  match Linol_lwt.run task with
  | () -> ()
  | exception e ->
    let () = Log.e (Exn.to_string e) in
    Stdlib.exit 1
;;
