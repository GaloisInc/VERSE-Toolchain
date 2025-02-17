open Base
module SNotif = Lsp.Server_notification
module SReq = Lsp.Server_request

module Token = struct
  type t = Lsp.Types.ProgressToken.t

  let fold (x : t) ~(int : int -> 'a) ~(str : string -> 'a) : 'a =
    match x with
    | `Int i -> int i
    | `String s -> str s
  ;;

  let to_string (x : t) : string = fold x ~int:Int.to_string ~str:Fn.id

  let compare (a : t) (b : t) : int =
    let int a = fold b ~int:(Int.compare a) ~str:(fun _ -> -1) in
    let str a = fold b ~int:(fun _ -> 1) ~str:(String.compare a) in
    fold a ~int ~str
  ;;

  let hash (x : t) : int = fold x ~int:Int.hash ~str:String.hash
  let sexp_of_t (x : t) : Sexp.t = fold x ~int:Int.sexp_of_t ~str:String.sexp_of_t

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
  let begin_ = Lsp.Types.WorkDoneProgressBegin.create ~cancellable:true ~title () in
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
