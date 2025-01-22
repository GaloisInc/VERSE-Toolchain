open! Base
module Json = Yojson.Safe

(* Linol *)
module Rpc = Linol_lwt.Jsonrpc2
module IO = Rpc.IO

(* LSP Types *)
module CNotif = Lsp.Client_notification
module CodeLens = Lsp.Types.CodeLens
module CodeLensOptions = Lsp.Types.CodeLensOptions
module ConfigurationItem = Lsp.Types.ConfigurationItem
module ConfigurationParams = Lsp.Types.ConfigurationParams
module Diagnostic = Lsp.Types.Diagnostic
module DidSaveTextDocumentParams = Lsp.Types.DidSaveTextDocumentParams
module DocumentUri = Lsp.Types.DocumentUri
module MessageType = Lsp.Types.MessageType
module ProgressToken = Lsp.Types.ProgressToken
module PublishDiagnosticsParams = Lsp.Types.PublishDiagnosticsParams
module Registration = Lsp.Types.Registration
module RegistrationParams = Lsp.Types.RegistrationParams
module ShowMessageParams = Lsp.Types.ShowMessageParams
module SReq = Lsp.Server_request
module TextDocumentContentChangeEvent = Lsp.Types.TextDocumentContentChangeEvent
module TextDocumentIdentifier = Lsp.Types.TextDocumentIdentifier
module TextDocumentItem = Lsp.Types.TextDocumentItem
module VersionedTextDocumentIdentifier = Lsp.Types.VersionedTextDocumentIdentifier

let cwindow (level : MessageType.t) (notify : Rpc.notify_back) (msg : string) : unit IO.t =
  let params = ShowMessageParams.create ~message:msg ~type_:level in
  let msg = Lsp.Server_notification.ShowMessage params in
  notify#send_notification msg
;;

let cinfo (notify : Rpc.notify_back) (msg : string) : unit IO.t =
  cwindow MessageType.Info notify msg
;;

module Config = struct
  (** The client controls these options, and sends them at a server's request *)
  type t = { run_CN_on_save : bool }

  (** The name of the configuration "section" the client uses to identify
      CN-specific settings *)
  let section : string = "CN"

  let default : t = { run_CN_on_save = false }

  let t_of_yojson (json : Json.t) : t option =
    let open Json.Util in
    try
      let run_CN_on_save = json |> member "runOnSave" |> to_bool in
      Some { run_CN_on_save }
    with
    | _ -> None
  ;;
end

let sprintf = Printf.sprintf

class lsp_server (env : LspCn.cerb_env) =
  object (self)
    val env : LspCn.cerb_env = env
    val mutable server_config : Config.t = Config.default
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
      let msg = "Opened document: " ^ DocumentUri.to_string doc.uri in
      let () = Log.d msg in
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
      let msg = "Closed document: " ^ DocumentUri.to_string doc.uri in
      let () = Log.d msg in
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
      let* () = self#fetch_configuration notify_back in
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
        let config_section = params.settings |> Json.Util.member Config.section in
        let () = self#set_configuration config_section in
        return ()
      | _ ->
        let s =
          Json.to_string (Jsonrpc.Notification.yojson_of_t (CNotif.to_jsonrpc notif))
        in
        let () = Log.d ("Unhandled notification: " ^ s) in
        return ()

    (***************************************************************)
    (***  Requests  ************************************************)

    method on_req_code_lens
      ~notify_back:(_ : Rpc.notify_back)
      ~id:(_ : Jsonrpc.Id.t)
      ~(uri : DocumentUri.t)
      ~workDoneToken:(_ : ProgressToken.t option)
      ~partialResultToken:(_ : ProgressToken.t option)
      (_ : Rpc.doc_state)
      : CodeLens.t list IO.t =
      IO.return (Lenses.gillian_lenses_for uri)

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

    method config_code_lens_options = Some (CodeLensOptions.create ())

    (** Set the server's configuration to the provided, JSON-encoded
        configuration *)
    method set_configuration (config_section : Json.t) : unit =
      match Config.t_of_yojson config_section with
      | None ->
        Log.e
          (sprintf
             "Unrecognized config section, ignoring: %s"
             (Json.to_string config_section))
      | Some cfg ->
        let () =
          Log.d (sprintf "Replacing config with: %s" (Json.to_string config_section))
        in
        server_config <- cfg

    (** Fetch the client's current configuration and update the server's version
        of it to match *)
    method fetch_configuration (notify_back : Rpc.notify_back) : unit IO.t =
      let open IO in
      let section = ConfigurationItem.create ~section:Config.section () in
      let params = ConfigurationParams.create ~items:[ section ] in
      let req = SReq.WorkspaceConfiguration params in
      let handle (response : (Json.t list, Jsonrpc.Response.Error.t) Result.t) : unit IO.t
        =
        let () =
          match response with
          | Ok [ section ] -> self#set_configuration section
          | Ok [] -> Log.w "No CN config section found"
          | Ok sections ->
            let ss = String.concat ~sep:"," (List.map sections ~f:Json.to_string) in
            Log.e (sprintf "Too many config sections: [%s]" ss)
          | Error e ->
            Log.e
              (sprintf
                 "Client responded with error: %s"
                 (Json.to_string (Jsonrpc.Response.Error.yojson_of_t e)))
        in
        return ()
      in
      let _id = notify_back#send_request req handle in
      return ()

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
        ConfigurationItem.(yojson_of_t (create ~section:Config.section ()))
      in
      self#register_capability ~notify_back ~method_ ~registerOptions ()

    method run_cn (notify_back : Rpc.notify_back) (uri : DocumentUri.t) : unit IO.t =
      let open IO in
      match LspCn.(run (run_cn env uri)) with
      | Ok [] -> cinfo notify_back "No issues found"
      | Ok errs ->
        let diagnostics = Hashtbl.to_alist (LspCn.errors_to_diagnostics errs) in
        self#publish_all notify_back diagnostics
      | Error err ->
        (match LspCn.error_to_diagnostic err with
         | None ->
           let () =
             Log.e (sprintf "Unable to decode error: %s" (LspCn.error_to_string err))
           in
           return ()
         | Some (diag_uri, diag) ->
           self#publish_diagnostics_for notify_back diag_uri [ diag ])

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
  (* We encapsulate the type this way (with `:>`) because our class defines more
     methods than `Rpc.server` specifies *)
  let s = (new lsp_server cn_env :> Rpc.server) in
  let sockaddr = Lwt_unix.ADDR_UNIX socket_path in
  let sock = Lwt_unix.(socket PF_UNIX SOCK_STREAM) 0 in
  let task =
    let* () = Lwt_unix.connect sock sockaddr in
    let ic = Lwt_io.of_fd ~mode:Lwt_io.Input sock in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.Output sock in
    let server = Rpc.create ~ic ~oc s in
    let shutdown () =
      match s#get_status with
      | `ReceivedExit -> true
      | _ -> false
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
