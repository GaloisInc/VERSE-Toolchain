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

let cerr (notify : Rpc.notify_back) (msg : string) : unit IO.t =
  cwindow MessageType.Error notify msg
;;

class lsp_server =
  object
    inherit Rpc.server

    (* Required *)
    method spawn_query_handler f = Linol_lwt.spawn f

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
      ~notify_back:(_ : Rpc.notify_back)
      (doc : VersionedTextDocumentIdentifier.t)
      (_changes : TextDocumentContentChangeEvent.t list)
      ~old_content:(_ : string)
      ~new_content:(_ : string)
      : unit IO.t =
      let msg = "Changed document: " ^ DocumentUri.to_string doc.uri in
      let () = Log.d msg in
      IO.return ()

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

    (* Overridden *)
    method on_unknown_request
      ~(notify_back : Rpc.notify_back)
      ~server_request:(_ : Rpc.server_request_handler_pair -> Jsonrpc.Id.t IO.t)
      ~id:(_ : Jsonrpc.Id.t)
      (method_name : string)
      (_params : Jsonrpc.Structured.t option)
      : Json.t IO.t =
      let open IO in
      let* () =
        match method_name with
        | "$/runCN" -> cerr notify_back "TODO"
        | _ -> return ()
      in
      failwith ("Unknown method: " ^ method_name)
  end

let run () : unit =
  let () = Log.d "Starting" in
  let s = new lsp_server in
  let server = Rpc.create_stdio ~env:() s in
  let task =
    let shutdown () = s#get_status = `ReceivedExit in
    Rpc.run ~shutdown server
  in
  match Linol_lwt.run task with
  | () -> ()
  | exception e ->
    let () = Log.e (Printexc.to_string e) in
    exit 1
;;
