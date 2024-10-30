open Base
module Dap = Debug_protocol

type t =
  { procedure_name : string
  ; report : Cn.Report.report
  ; source_file : string
  }

let report_from_file (file : string) : (Cn.Report.report, string) Result.t =
  let js = Yojson.Safe.from_file file in
  Cn.Report.report_of_yojson js
;;

let from_launch_args (launch_args : Launch.Command.Arguments.t) : (t, string) Result.t =
  let ( let* ) x f = Result.bind x ~f in
  let source_file = launch_args.program in
  (* TODO: this improperly relies on the fact that
     `Cn.TypeErrors.mk_report_file_name` happens to only care about the filename
     in its error location parameter *)
  let source_loc =
    Cerb_location.point { Lexing.dummy_pos with pos_fname = source_file }
  in
  let source_dir = Stdlib.Filename.dirname source_file in
  let* procedure_name =
    Result.of_option launch_args.procedure_name ~error:"no procedure name"
  in
  let report_file =
    Cn.TypeErrors.mk_report_file_name ~fn_name:procedure_name source_dir source_loc
  in
  Log.d (Printf.sprintf "Looking for report file at %s" report_file);
  let* report = report_from_file report_file in
  Result.Ok { procedure_name; report; source_file }
;;
