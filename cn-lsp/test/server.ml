open! Base
open Lwt.Syntax
module Rpc = Linol_lwt.Jsonrpc2

module TestServer = struct
  type t =
    { read_from : Lwt_io.input_channel
    ; write_to : Lwt_io.output_channel
    }

  (** Send a request to the server *)
  let send_request (server : t) (req : Jsonrpc.Request.t) : unit Lwt.t =
    let str = Yojson.Safe.to_string (Jsonrpc.Request.yojson_of_t req) in
    let msg = Printf.sprintf "Content-Length: %i\r\n\r\n%s" (String.length str) str in
    let* () = Lwt_io.write server.write_to msg in
    Lwt_io.flush server.write_to
  ;;

  (** Read the next available message from the server *)
  let read_packet (server : t) : Jsonrpc.Packet.t Lwt.t =
    let* result = Lwt_io.read_line server.read_from in
    match Stdlib.Scanf.sscanf_opt result "Content-Length: %i" (fun len -> len) with
    | None -> failwith "Failed to read 'Content-Length' header"
    | Some len ->
      let* _ = Lwt_io.read_line server.read_from in
      let* response = Lwt_io.read ~count:len server.read_from in
      if Int.equal (String.length response) len
      then Lwt.return (Jsonrpc.Packet.t_of_yojson (Yojson.Safe.from_string response))
      else
        failwith
          (Printf.sprintf
             "Insufficient server output available: wanted %i bytes, but only %i were \
              available"
             len
             (String.length response))
  ;;

  (** A short, human-readable identifier for a [Jsonrpc.Packet.t]'s variant *)
  let packet_tag (packet : Jsonrpc.Packet.t) : string =
    match packet with
    | Batch_call _ -> "batch call"
    | Batch_response _ -> "batch response"
    | Notification _ -> "notification"
    | Request _ -> "request"
    | Response _ -> "response"
  ;;

  (** Read the next message available from the server and interpret it as a
      [Jsonrpc.Notification.t] *)
  let read_notification (server : t) : Jsonrpc.Notification.t Lwt.t =
    let* packet = read_packet server in
    match packet with
    | Notification n -> Lwt.return n
    | _ ->
      failwith
        (Printf.sprintf
           "Expected notification, but saw %s: %s"
           (packet_tag packet)
           (Yojson.Safe.to_string (Jsonrpc.Packet.yojson_of_t packet)))
  ;;

  (** Read the next message available from the server and interpret it as a
      [Jsonrpc.Request.t] *)
  let read_request (server : t) : Jsonrpc.Request.t Lwt.t =
    let* packet = read_packet server in
    match packet with
    | Request r -> Lwt.return r
    | _ ->
      failwith
        (Printf.sprintf
           "Expected request, but saw %s: %s"
           (packet_tag packet)
           (Yojson.Safe.to_string (Jsonrpc.Packet.yojson_of_t packet)))
  ;;

  (** Read the next message available from the server and interpret it as a
      [Jsonrpc.Response.t] *)
  let read_response (server : t) : Jsonrpc.Response.t Lwt.t =
    let* packet = read_packet server in
    match packet with
    | Response r -> Lwt.return r
    | _ ->
      failwith
        (Printf.sprintf
           "Expected response, but saw %s: %s"
           (packet_tag packet)
           (Yojson.Safe.to_string (Jsonrpc.Packet.yojson_of_t packet)))
  ;;

  let create () : t =
    let cn_server =
      match Cnlsp.Server.create_server () with
      | Ok server -> server
      | Error e ->
        failwith
          (Printf.sprintf "Unable to create server: %s" (Cnlsp.Verify.Error.to_string e))
    in
    let rpc_server = (cn_server :> Rpc.server) in
    let client_socket, server_socket = Lwt_unix.socketpair PF_UNIX SOCK_STREAM 0 in
    let server_in = Lwt_io.of_fd ~mode:Lwt_io.Input server_socket in
    let server_out = Lwt_io.of_fd ~mode:Lwt_io.Output server_socket in
    let server = Rpc.create ~ic:server_in ~oc:server_out rpc_server in
    let shutdown () =
      match rpc_server#get_status with
      | `ReceivedExit | `ReceivedShutdown ->
        (* Note: this is written this way to accomodate linol v0.6 - If
           upgrading to 0.7+, this logic can and should move to the
           [on_req_shutdown] method introduced in 0.7 *)
        true
      | `Running -> false
    in
    let _ = Rpc.run ~shutdown server in
    let read_from = Lwt_io.of_fd ~mode:Lwt_io.Input client_socket in
    let write_to = Lwt_io.of_fd ~mode:Lwt_io.Output client_socket in
    { read_from; write_to }
  ;;
end

let test_send_initialize (server : TestServer.t) () : unit Lwt.t =
  let capabilities = Lsp.Types.ClientCapabilities.create () in
  let params = Lsp.Types.InitializeParams.create ~capabilities () in
  let request =
    Jsonrpc.Request.create
      ~method_:"initialize"
      ~id:(`Int 1)
      ~params:
        (Jsonrpc.Structured.t_of_yojson (Lsp.Types.InitializeParams.yojson_of_t params))
      ()
  in
  let* () = TestServer.send_request server request in
  let* response = TestServer.read_response server in
  match response.result with
  | Error e -> Jsonrpc.Response.Error.raise e
  | Ok r ->
    let result = Lsp.Types.InitializeResult.t_of_yojson r in
    let capabilities = result.capabilities in
    (match capabilities.codeLensProvider with
     | None -> failwith "should provide code lenses"
     | Some _clp -> ());
    Lwt.return ()
;;

let test_verify_success (server : TestServer.t) (c_source : string) () : unit Lwt.t =
  Lwt_io.with_temp_file ~suffix:".c" (fun (filename, oc) ->
    let* () = Lwt_io.write oc c_source in
    let uri = Cnlsp.Uri.of_path filename in
    let params : Cnlsp.Server.VerifyParams.t = { uri; fn = None; fn_range = None } in
    let request =
      Jsonrpc.Request.create
        ~method_:"$/cnVerify"
        ~id:(`Int 1)
        ~params:
          (Jsonrpc.Structured.t_of_yojson (Cnlsp.Server.VerifyParams.to_yojson params))
        ()
    in
    let expect_method ~expected ~actual =
      if String.equal expected actual
      then Lwt.return ()
      else failwith (Printf.sprintf "expected method %s, but saw %s" expected actual)
    in
    let* () = TestServer.send_request server request in
    let* progress_create = TestServer.read_request server in
    let* () =
      expect_method
        ~expected:"window/workDoneProgress/create"
        ~actual:progress_create.method_
    in
    let* progress_begin = TestServer.read_notification server in
    let* () = expect_method ~expected:"$/progress" ~actual:progress_begin.method_ in
    let* progress_end = TestServer.read_notification server in
    let* () = expect_method ~expected:"$/progress" ~actual:progress_end.method_ in
    let* no_issues = TestServer.read_notification server in
    let* () = expect_method ~expected:"window/showMessage" ~actual:no_issues.method_ in
    let show_message_params =
      Lsp.Types.ShowMessageParams.t_of_yojson
        (Jsonrpc.Structured.yojson_of_t (Option.value_exn no_issues.params))
    in
    if String.is_prefix show_message_params.message ~prefix:"No issues found"
    then Lwt.return ()
    else
      failwith
        (Printf.sprintf
           "expected \"No issues found...\" message, but saw \"%s\""
           show_message_params.message))
;;

let c_source : string =
  String.concat
    ~sep:"\n"
    [ "int foo()"
    ; "/*@"
    ; "requires true;"
    ; "ensures return == 3i32;"
    ; "@*/"
    ; "{"
    ; "  return 3;"
    ; "}"
    ]
;;

let tests =
  let formatter = Stdlib.Format.formatter_of_out_channel Out_channel.stdout in
  let reporter = Logs.format_reporter ~app:formatter ~dst:formatter () in
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter reporter;
  let server = TestServer.create () in
  let tc s nm f = Alcotest_lwt.test_case s nm (fun _switch () -> f ()) in
  [ tc "initialize" `Quick (test_send_initialize server)
  ; tc "verify - succeed" `Quick (test_verify_success server c_source)
  ]
;;
