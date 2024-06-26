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
      let msg = "Saved document: " ^ DocumentUri.to_string params.textDocument.uri in
      let () = Log.d msg in
      let* () = cinfo notify_back "Saved!" in
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
