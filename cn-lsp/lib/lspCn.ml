open! Base
module Diagnostic = Lsp.Types.Diagnostic

module Error = struct
  type t = Cn.TypeErrors.t

  let to_string (err : t) : string =
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

  let to_diagnostic (err : t) : (Uri.t * Diagnostic.t) option =
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
end

type 'a m = 'a Cn.Resultat.t

let ( let* ) (a : 'a m) (f : 'a -> 'b m) : 'b m = Cn.Resultat.bind a f

(* No reason in principle not to have `return`, it just hasn't been used so
   far in practice *)
