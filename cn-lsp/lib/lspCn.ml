open! Base
module Diagnostic = Lsp.Types.Diagnostic

type error = Cn.TypeErrors.t

let error_to_string (err : error) : string =
  let report = Cn.TypeErrors.pp_message err.msg in
  let short = Cn.Pp.plain report.short in
  let desc = Option.value (Option.map report.descr ~f:Cn.Pp.plain) ~default:"<none>" in
  "CN Error: loc = "
  ^ Cn.Locations.to_string err.loc
  ^ ", short = "
  ^ short
  ^ ", desc = "
  ^ desc
;;

let error_to_diagnostic (err : error) : (Uri.t * Diagnostic.t) option =
  let report = Cn.TypeErrors.pp_message err.msg in
  let short = Cn.Pp.plain report.short in
  let message =
    match report.descr with
    | None -> short
    | Some d -> short ^ "\n" ^ Cn.Pp.plain d
  in
  let source = "CN" in
  match LspCerb.loc_to_source_range err.loc with
  | None -> None
  | Some (path, range) ->
    Some (Uri.of_path path, Diagnostic.create ~message ~range ~source ())
;;

type 'a m = 'a Cn.Resultat.t

let ( let* ) (a : 'a m) (f : 'a -> 'b m) : 'b m = Cn.Resultat.bind a f

(* No reason in principle not to have `return`, it just hasn't been used so
   far in practice *)

let run (x : 'a m) : ('a, error) Result.t = x
