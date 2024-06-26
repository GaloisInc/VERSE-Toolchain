(*
   Disable method override warning: we have to override the default
   `Linol_lwt.Jsonrpc2.server` methods to define its behavior.
*)
[@@@warning "-7"]

module Json = Yojson.Safe

(* Linol *)
module Rpc = Linol_lwt.Jsonrpc2
module IO = Rpc.IO

(* LSP *)
module LspTy = Lsp.Types
module ConfigurationItem = LspTy.ConfigurationItem
module ConfigurationParams = LspTy.ConfigurationParams
module Diagnostic = LspTy.Diagnostic
module DidSaveTextDocumentParams = LspTy.DidSaveTextDocumentParams
module DocumentUri = LspTy.DocumentUri
module MessageType = LspTy.MessageType
module PublishDiagnosticsParams = LspTy.PublishDiagnosticsParams
module ShowMessageParams = LspTy.ShowMessageParams
module TextDocumentContentChangeEvent = LspTy.TextDocumentContentChangeEvent
module TextDocumentIdentifier = LspTy.TextDocumentIdentifier
module TextDocumentItem = LspTy.TextDocumentItem
module VersionedTextDocumentIdentifier = LspTy.VersionedTextDocumentIdentifier
module CNotif = Lsp.Client_notification
module SReq = Lsp.Server_request

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

    (* Overridden *)
    method on_notif_doc_did_save
      ~(notify_back : Rpc.notify_back)
      (params : DidSaveTextDocumentParams.t)
      : unit IO.t =
      let open IO in
      if server_config.run_CN_on_save
      then self#run_cn notify_back params.textDocument.uri
      else return ()

    method on_notif_initialized (notify_back : Rpc.notify_back) : unit IO.t =
      self#update_configuration notify_back

    method on_notification_unhandled
      ~(notify_back : Rpc.notify_back)
      (notif : CNotif.t)
      : unit IO.t =
      let open IO in
      match notif with
      | CNotif.Initialized -> self#on_notif_initialized notify_back
      | _ ->
        let s =
          Json.to_string (Jsonrpc.Notification.yojson_of_t (CNotif.to_jsonrpc notif))
        in
        let () = Log.d ("Unhandled notification: " ^ s) in
        return ()

    (***************************************************************)
    (***  Requests  ************************************************)

    (* Overridden *)
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
        let obj = Jsonrpc.Structured.yojson_of_t (Option.get params) in
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

    method update_configuration (notify_back : Rpc.notify_back) : unit IO.t =
      let open IO in
      let cfg_section = ConfigurationItem.create ~section:"CN" () in
      let params = ConfigurationParams.create ~items:[ cfg_section ] in
      let req = SReq.WorkspaceConfiguration params in
      let handle (response : (Json.t list, Jsonrpc.Response.Error.t) result) : unit IO.t =
        let () =
          match response with
          | Ok [ section ] ->
            (match Config.t_of_yojson section with
             | None ->
               Log.e
                 (sprintf
                    "Unrecognized config section, ignoring: %s"
                    (Json.to_string section))
             | Some cfg ->
               let () =
                 Log.d (sprintf "Replacing config with: %s" (Json.to_string section))
               in
               server_config <- cfg)
          | Ok [] -> Log.w "No CN config section found"
          | Ok sections ->
            let ss = String.concat "," (List.map Json.to_string sections) in
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

    method run_cn (notify_back : Rpc.notify_back) (uri : DocumentUri.t) : unit IO.t =
      let open IO in
      match LspCn.(run (run_cn env uri)) with
      | Ok () -> cinfo notify_back "No issues found"
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
  end

let run (socket_path : string) : unit =
  let open IO in
  let () = Log.d "Starting" in
  let cn_env =
    match LspCn.(run (setup ())) with
    | Ok t -> t
    | Error e ->
      let msg = LspCn.error_to_string e in
      let () = Log.e ("Failed to start: " ^ msg) in
      exit 1
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
    let shutdown () = s#get_status = `ReceivedExit in
    let* () = Rpc.run ~shutdown server in
    let () = Log.d "Shutting down" in
    Lwt_unix.close sock
  in
  match Linol_lwt.run task with
  | () -> ()
  | exception e ->
    let () = Log.e (Printexc.to_string e) in
    exit 1
;;
