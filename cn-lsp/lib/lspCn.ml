open! Base
module Diagnostic = Lsp.Types.Diagnostic

module Error = struct
  type t = Cn.TypeErrors.t

  (** Generate an HTML-formatted report for the given error, save it in the
      provided output directory, and return the full path to the report. May
      return [None] if the error doesn't have enough information to generate a
      report. *)
  let to_html_report (err : t) ~(fn_name : string option) ~(output_dir : string)
    : Uri.t option
    =
    let output_dir = Filename_extended.expand output_dir in
    let error_report = Cn.TypeErrors.pp_message err.msg in
    match error_report.state with
    | None -> None
    | Some report_state ->
      let report_path = Cn.TypeErrors.mk_state_file_name ?fn_name output_dir err.loc in
      let err_source_file = Cerb_location.get_filename err.loc in
      let _report_path = Cn.Report.make report_path err_source_file report_state in
      Some (Uri.of_path report_path)
  ;;

  let to_string (err : t) : string =
    let report = Cn.TypeErrors.pp_message err.msg in
    let short = Cn.Pp.plain report.short in
    let loc = Cn.Locations.to_string err.loc in
    match report.descr with
    | None -> Printf.sprintf "%s: %s" loc short
    | Some desc -> Printf.sprintf "%s: %s (%s)" loc short (Cn.Pp.plain desc)
  ;;

  (** Create a [Diagnostic.t] about this error, and report the file in which that
      diagnostic should be published. If [html_report] is provided, link to that
      report in the diagnostic. *)
  let to_diagnostic (err : t) ~(html_report : Uri.t option)
    : (Uri.t * Diagnostic.t) option
    =
    let report = Cn.TypeErrors.pp_message err.msg in
    let short = Cn.Pp.plain report.short in
    let message =
      match report.descr with
      | None -> short
      | Some d -> short ^ "\n" ^ Cn.Pp.plain d
    in
    let code, codeDescription =
      match html_report with
      | None -> None, None
      | Some link ->
        let code = `String "Click for full error report" in
        let codeDescription = Lsp.Types.CodeDescription.create ~href:link in
        Some code, Some codeDescription
    in
    let source = "CN" in
    match LspCerb.loc_to_source_range err.loc with
    | None -> None
    | Some (path, range) ->
      Some
        ( Uri.of_path path
        , Diagnostic.create ~message ~range ~source ?code ?codeDescription () )
  ;;
end

type 'a m = 'a Cn.Or_TypeError.t

let ( let* ) (a : 'a m) (f : 'a -> 'b m) : 'b m = Cn.Or_TypeError.bind a f

(* No reason in principle not to have `return`, it just hasn't been used so
   far in practice *)
