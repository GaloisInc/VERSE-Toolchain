open Base
module SNotif = Lsp.Server_notification
module SReq = Lsp.Server_request

module Token = struct
  type t = Lsp.Types.ProgressToken.t

  (** Create a fresh, nameless token, guaranteed to be different from all
      previously-created tokens *)
  let anonymous : unit -> t =
    let fresh = ref 0 in
    fun () ->
      let i = !fresh in
      fresh := i + 1;
      `Int i
  ;;

  (** Create a named token, which may be the same as another token *)
  let named (s : string) : t = `String s
end

(** A server-provided notification for the client to start displaying progress
    against the given [token]. *)
let notif_begin (token : Token.t) ~(title : string) : SNotif.t =
  let begin_ = Lsp.Types.WorkDoneProgressBegin.create ~title () in
  let progress = Lsp.Progress.Begin begin_ in
  let params = Lsp.Types.ProgressParams.create ~token ~value:progress in
  SNotif.WorkDoneProgress params
;;

(** A server-provided notification for the client to stop displaying progress
    against the given [token]. *)
let notif_end (token : Token.t) : SNotif.t =
  let end_ = Lsp.Types.WorkDoneProgressEnd.create () in
  let progress = Lsp.Progress.End end_ in
  let params = Lsp.Types.ProgressParams.create ~token ~value:progress in
  SNotif.WorkDoneProgress params
;;

(** A server-provided request for the client to recognize the given [token]. *)
let req_create (token : Token.t) : unit SReq.t =
  let params = Lsp.Types.WorkDoneProgressCreateParams.create ~token in
  SReq.WorkDoneProgressCreate params
;;
