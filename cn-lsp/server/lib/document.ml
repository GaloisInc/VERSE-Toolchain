open Base
module DocumentUri = Lsp.Types.DocumentUri
module CF = Cerb_frontend
module Cabs = Cerb_frontend.Cabs

type loc = Cerb_location.t
type spec = [ `Spec of string ]

let sprintf = Printf.sprintf

let uri_of_loc (loc : loc) : Uri.t option =
  Option.map (Cerb_location.get_filename loc) ~f:DocumentUri.of_path
;;

type ident = [ `Ident of string ]

(** An identifier and the spec that applies to it, if any *)
type ident_info =
  { ident : ident
  ; spec : spec option
  }

module Scope = struct
  type t = { global : (string, Uri.t * Range.t * spec option) Hashtbl.t }

  let create () : t = { global = Hashtbl.create (module String) }

  let add_global
    (scope : t)
    (`Ident ident : ident)
    (uri : Uri.t)
    (range : Range.t)
    (spec : spec option)
    : t
    =
    let () = Hashtbl.set scope.global ~key:ident ~data:(uri, range, spec) in
    scope
  ;;

  let find_ident (scope : t) (`Ident ident : ident)
    : (Uri.t * Range.t * spec option) option
    =
    Hashtbl.find scope.global ident
  ;;
end
(*
   module Locations = struct
  type t =
    { current_file : uri
    ; current_locations : (Range.t * LocMap.info) list
    ; past_locations : (uri, (Range.t * LocMap.info) list) Hashtbl.t
    }

  let create (uri : uri) : t =
    { current_file = uri; current_locations = []; past_locations = Hashtbl.create 64 }
  ;;

  (** In [past_locations], map [current_file] to all [current_locations] (as
      well as whatever locations [current_file] already mapped to). *)
  let reconcile_locations (locs : t) : unit =
    Hashtbl.add_with
      (fun ~new_ ~old -> List.append new_ old)
      locs.current_file
      locs.current_locations
      locs.past_locations
  ;;

  (** In the document at [uri], remember that [range] contains [ident], along with
      its associated [spec]. *)
  let remember_ident_location
    (locs : t)
    (uri : uri)
    (range : Range.t)
    (ident : string)
    (spec : spec option)
    : t
    =
    let loc_info = LocMap.ident_info ident spec in
    (* Is this equality check expensive? *)
    if DocumentUri.equal uri locs.current_file
    then { locs with current_locations = (range, loc_info) :: locs.current_locations }
    else (
      let () = reconcile_locations locs in
      { locs with current_file = uri; current_locations = [ range, loc_info ] })
  ;;
end *)

type document =
  { scope : Scope.t
  ; current_file : Uri.t
  ; locations : (Uri.t, (Range.t * ident_info) list) Hashtbl.t
  }

let create_document (uri : Uri.t) : document =
  { scope = Scope.create (); current_file = uri; locations = Hashtbl.create (module Uri) }
;;

let add_global (doc : document) (ident : ident) (range : Range.t) (spec : spec option)
  : document
  =
  let scope' = Scope.add_global doc.scope ident doc.current_file range spec in
  { doc with scope = scope' }
;;

let remember_ident_location
  (doc : document)
  (range : Range.t)
  (ident : string)
  (spec : spec option)
  : document
  =
  let ident = `Ident ident in
  let info = { ident; spec } in
  let () = Hashtbl.add_multi ~key:doc.current_file ~data:(range, info) doc.locations in
  doc
;;

let rec attributes_to_spec (attributes : CF.Annot.attributes) : spec option =
  let () = Log.d "attributes_to_spec" in
  match attributes with
  | CF.Annot.Attrs [ attr ] -> attribute_to_spec attr
  | CF.Annot.Attrs _ -> None

and attribute_to_spec (attribute : CF.Annot.attribute) : spec option =
  let () = Log.d "attribute_to_spec" in
  match attribute.attr_id with
  | Identifier (_, "magic") ->
    Some
      (`Spec (String.concat ~sep:"\n" (List.map attribute.attr_args ~f:attr_arg_to_spec)))
  | _ -> None

and attr_arg_to_spec ((_loc, str, _other) : loc * string * (loc * string) list) : string =
  let () = Log.d "attr_arg_to_spec" in
  str
;;

let process_cabs_ident (doc : document) (CF.Symbol.Identifier (loc, ident)) =
  let () =
    Log.d (sprintf "process_cabs_ident\t\t%s" (Cerb_location.location_to_string loc))
  in
  match Scope.find_ident doc.scope (`Ident ident) with
  | None ->
    let () = Log.e (Printf.sprintf "Encountered unbound variable: %s" ident) in
    doc
  | Some (_source_uri, _source_range, spec) ->
    (match Range.of_cerb_loc loc with
     | None ->
       let () = Log.e (Printf.sprintf "Encountered unbound variable: %s" ident) in
       doc
     | Some range -> remember_ident_location doc range ident spec)
;;

let process_optional
  (process : document -> 'a -> document)
  (doc : document)
  (x_opt : 'a option)
  : document
  =
  match x_opt with
  | None -> doc
  | Some x -> process doc x
;;

let rec process_cabs_statement
  (doc : document)
  (Cabs.CabsStatement (_, _, stmt) : Cabs.cabs_statement)
  : document
  =
  let () = Log.d "process_cabs_statement" in
  match stmt with
  | Cabs.CabsSlabel (_, _) -> doc
  | Cabs.CabsScase (e, stmt) ->
    process_cabs_statement (process_cabs_expression doc e) stmt
  | Cabs.CabsSdefault stmt -> process_cabs_statement doc stmt
  | Cabs.CabsSblock stmts -> List.fold_left stmts ~init:doc ~f:process_cabs_statement
  | Cabs.CabsSdecl decl -> process_cabs_declaration doc decl
  | Cabs.CabsSnull -> doc
  | Cabs.CabsSexpr expr -> process_cabs_expression doc expr
  | Cabs.CabsSif (b, t, f) ->
    let doc' = process_cabs_expression doc b in
    let doc'' = process_cabs_statement doc' t in
    process_optional process_cabs_statement doc'' f
  | Cabs.CabsSswitch (e, stmt) ->
    process_cabs_statement (process_cabs_expression doc e) stmt
  | Cabs.CabsSwhile (e, stmt) ->
    process_cabs_statement (process_cabs_expression doc e) stmt
  | Cabs.CabsSdo (e, stmt) -> process_cabs_statement (process_cabs_expression doc e) stmt
  | Cabs.CabsSfor (for_clause_opt, cond_opt, update_opt, stmt) ->
    let doc' = process_optional process_for_clause doc for_clause_opt in
    let doc'' = process_optional process_cabs_expression doc' cond_opt in
    let doc''' = process_optional process_cabs_expression doc'' update_opt in
    process_cabs_statement doc''' stmt
  | Cabs.CabsSgoto _lbl -> doc
  | Cabs.CabsScontinue -> doc
  | Cabs.CabsSbreak -> doc
  | Cabs.CabsSreturn e_opt -> process_optional process_cabs_expression doc e_opt
  | Cabs.CabsSpar ss -> List.fold_left ss ~init:doc ~f:process_cabs_statement
  | Cabs.CabsSasm (_volatile, _inline, _instrs) -> doc
  | Cabs.CabsScaseGNU (lo, hi, stmt) ->
    let doc' = List.fold_left [ lo; hi ] ~init:doc ~f:process_cabs_expression in
    process_cabs_statement doc' stmt
  | Cabs.CabsSmarker stmt -> process_cabs_statement doc stmt

and process_cabs_declaration (doc : document) (decl : Cabs.cabs_declaration) : document =
  (* TODO: scope *)
  let () = Log.d "process_cabs_declaration" in
  match decl with
  | Cabs.Declaration_base (_attrs, _specifiers, declarators) ->
    List.fold_left declarators ~init:doc ~f:process_init_declarator
  | Cabs.Declaration_static_assert _ -> doc

and process_init_declarator
  (doc : document)
  (Cabs.InitDecl (_loc, _decl, init_opt) : Cabs.init_declarator)
  : document
  =
  (* TODO: scope *)
  let () = Log.d "process_init_declarator" in
  match init_opt with
  | None -> doc
  | Some (Cabs.Init_expr expr) -> process_cabs_expression doc expr
  | Some (Cabs.Init_list _) -> doc

and process_for_clause (doc : document) (clause : Cabs.for_clause) : document =
  (* TODO: scope *)
  match clause with
  | Cabs.FC_expr e -> process_cabs_expression doc e
  | Cabs.FC_decl (_loc, decl) -> process_cabs_declaration doc decl

and process_cabs_expression
  (doc : document)
  (Cabs.CabsExpression (loc, expr) : Cabs.cabs_expression)
  : document
  =
  let () =
    Log.d (sprintf "process_cabs_expression\t%s" (Cerb_location.location_to_string loc))
  in
  match expr with
  | Cabs.CabsEident (CF.Symbol.Identifier (_loc, ident)) ->
    (* The location `Cabs` ascribes to `Symbol.Identifier` is the starting point
       of the identifier, but we want the range containing the identifier, which
       we can get from the `CabsExpression` wrapper. *)
    process_cabs_ident doc (CF.Symbol.Identifier (loc, ident))
  | Cabs.CabsEconst _ -> doc
  | Cabs.CabsEstring _ -> doc
  | Cabs.CabsEgeneric (e, gas) ->
    List.fold_left
      gas
      ~init:(process_cabs_expression doc e)
      ~f:process_cabs_generic_association
  | Cabs.CabsEsubscript (arr, sel) ->
    List.fold_left [ arr; sel ] ~init:doc ~f:process_cabs_expression
  | Cabs.CabsEcall (f, args) ->
    List.fold_left (f :: args) ~init:doc ~f:process_cabs_expression
  | Cabs.CabsEmemberof (e, _field) -> process_cabs_expression doc e
  | Cabs.CabsEmemberofptr (e, _field) -> process_cabs_expression doc e
  | Cabs.CabsEpostincr e -> process_cabs_expression doc e
  | Cabs.CabsEpostdecr e -> process_cabs_expression doc e
  | Cabs.CabsEcompound (_ty, is) -> List.fold_left is ~init:doc ~f:process_initializer
  | Cabs.CabsEpreincr e -> process_cabs_expression doc e
  | Cabs.CabsEpredecr e -> process_cabs_expression doc e
  | Cabs.CabsEunary (_, e) -> process_cabs_expression doc e
  | Cabs.CabsEsizeof_expr e -> process_cabs_expression doc e
  | Cabs.CabsEsizeof_type _ty -> doc
  | Cabs.CabsEalignof _ty -> doc
  | Cabs.CabsEcast (_ty, e) -> process_cabs_expression doc e
  | Cabs.CabsEbinary (_op, e1, e2) ->
    List.fold_left [ e1; e2 ] ~init:doc ~f:process_cabs_expression
  | Cabs.CabsEcond (b, t, f) ->
    List.fold_left [ b; t; f ] ~init:doc ~f:process_cabs_expression
  | Cabs.CabsEassign (_op, e1, e2) ->
    List.fold_left
      [ e1; e2 ]
      ~init:doc
      ~f:process_cabs_expression (* TODO: update scope *)
  | Cabs.CabsEcomma (e1, e2) ->
    List.fold_left [ e1; e2 ] ~init:doc ~f:process_cabs_expression
  | Cabs.CabsEassert e -> process_cabs_expression doc e
  | Cabs.CabsEoffsetof (_ty, _ident) -> doc
  | Cabs.CabsEva_start (e, _ident) -> process_cabs_expression doc e
  | Cabs.CabsEva_copy (e1, e2) ->
    List.fold_left [ e1; e2 ] ~init:doc ~f:process_cabs_expression
  | Cabs.CabsEva_arg (e, _ty) -> process_cabs_expression doc e
  | Cabs.CabsEva_end e -> process_cabs_expression doc e
  | Cabs.CabsEprint_type e -> process_cabs_expression doc e
  | Cabs.CabsEbmc_assume e -> process_cabs_expression doc e
  | Cabs.CabsEgcc_statement stmts ->
    List.fold_left stmts ~init:doc ~f:process_cabs_statement (* TODO: scope? *)
  | Cabs.CabsEcondGNU (e1, e2) ->
    List.fold_left [ e1; e2 ] ~init:doc ~f:process_cabs_expression
  | Cabs.CabsEbuiltinGNU builtin -> process_gnu_builtin doc builtin

and process_cabs_generic_association (doc : document) (ga : Cabs.cabs_generic_association)
  : document
  =
  match ga with
  | Cabs.GA_type (_ty, e) -> process_cabs_expression doc e
  | Cabs.GA_default e -> process_cabs_expression doc e

and process_initializer
  (doc : document)
  ((ds_opt, init_) : Cabs.designator list option * Cabs.initializer_)
  : document
  =
  let doc' =
    process_optional
      (fun d ds -> List.fold_left ds ~init:d ~f:process_designator)
      doc
      ds_opt
  in
  process_initializer_ doc' init_

and process_initializer_ (doc : document) (init_ : Cabs.initializer_) : document =
  match init_ with
  | Cabs.Init_expr e -> process_cabs_expression doc e
  | Cabs.Init_list (_loc, is) -> List.fold_left is ~init:doc ~f:process_initializer

and process_designator (doc : document) (d : Cabs.designator) : document =
  match d with
  | Cabs.Desig_array e -> process_cabs_expression doc e
  | Cabs.Desig_member _ -> doc

and process_gnu_builtin (doc : document) (builtin : Cabs.gnu_builtin_function) : document =
  match builtin with
  | Cabs.GNUbuiltin_types_compatible_p (_t1, _t2) -> doc
  | Cabs.GNUbuiltin_choose_expr (e1, e2, e3) ->
    List.fold_left [ e1; e2; e3 ] ~init:doc ~f:process_cabs_expression
;;

let rec declarator_to_located_name (declarator : Cabs.declarator) : loc * string =
  match declarator with
  | Cabs.Declarator (_pdecl, ddecl) -> direct_declarator_to_located_name ddecl

and direct_declarator_to_located_name (declarator : Cabs.direct_declarator) : loc * string
  =
  match declarator with
  | Cabs.DDecl_identifier (_attrs, Identifier (l, s)) -> l, s
  | Cabs.DDecl_declarator decl -> declarator_to_located_name decl
  | Cabs.DDecl_array (ddecl, _adecl) -> direct_declarator_to_located_name ddecl
  | Cabs.DDecl_function (ddecl, _params) -> direct_declarator_to_located_name ddecl
;;

let process_function_definition
  (doc : document)
  (Cabs.FunDef (loc, attrs, _specifiers, declarator, stmt) : Cabs.function_definition)
  : document
  =
  let () = Log.d "process_function_definition" in
  let doc' =
    match uri_of_loc loc with
    | None -> doc
    | Some uri when DocumentUri.equal uri doc.current_file -> doc
    | Some uri -> { doc with current_file = uri }
  in
  (* Map the identifier to the location/spec *)
  (* Map the location to the spec *)
  let loc, fn_name = declarator_to_located_name declarator in
  let spec = attributes_to_spec attrs in
  match Range.of_cerb_loc loc with
  | None -> doc'
  | Some range ->
    let doc'' = add_global doc' (`Ident fn_name) range spec in
    let doc''' = remember_ident_location doc'' range fn_name spec in
    process_cabs_statement doc''' stmt
;;

let process_external_declaration (doc : document) (decl : Cabs.external_declaration)
  : document
  =
  let () = Log.d "process_external_declaration" in
  match decl with
  | Cabs.EDecl_func function_definition ->
    process_function_definition doc function_definition
  | Cabs.EDecl_decl _ -> doc
  | Cabs.EDecl_magic _ -> doc
  | Cabs.EDecl_funcCN _ -> doc
  | Cabs.EDecl_lemmaCN _ -> doc
  | Cabs.EDecl_predCN _ -> doc
  | Cabs.EDecl_datatypeCN _ -> doc
  | Cabs.EDecl_type_synCN _ -> doc
  | Cabs.EDecl_fun_specCN _ -> doc
;;

let process_external_declarations (uri : Uri.t) (decls : Cabs.external_declaration list)
  : document
  =
  let doc = create_document uri in
  List.fold_left decls ~init:doc ~f:process_external_declaration
;;

let c_preprocessor : string = "cc"

let c_preprocessor_arguments : string list =
  [ (* Set standard to C11 *)
    "-std=c11"
  ; (* Run in preprocessor mode *)
    "-E"
  ; (* ??? *)
    "-CC"
  ; (* Disable standard inclusions *)
    "-nostdinc"
  ; (* ??? *)
    "-undef"
  ; (* TODO: should be able to do these only once, or avoid them entirely? *)
    "-I"
  ; Cerb_runtime.in_runtime "libc/include"
  ; "-I"
  ; Cerb_runtime.in_runtime "libcore"
  ; "-include"
  ; Cerb_runtime.in_runtime "libc/include/builtins.h"
  ; "-DDEBUG"
  ; "-DCN_MODE"
  ]
;;

type proc_out =
  { exit_code : int
  ; stdout : string
  ; stderr : string
  }

let read_process (cmd : string) (args : string array) : (proc_out, string) Result.t =
  try
    let out_read, out_write = Unix.pipe () in
    Unix.set_close_on_exec out_read;
    let out_ic = Unix.in_channel_of_descr out_read in
    let err_read, err_write = Unix.pipe () in
    Unix.set_close_on_exec err_read;
    let err_ic = Unix.in_channel_of_descr err_read in
    let pid = Unix.create_process cmd args Unix.stdin out_write err_write in
    Unix.close out_write;
    Unix.close err_write;
    Stdlib.flush_all ();
    let out = In_channel.input_all out_ic in
    Stdlib.close_in out_ic;
    let err = In_channel.input_all err_ic in
    Stdlib.close_in err_ic;
    match Unix.waitpid [] pid with
    | _, WEXITED exit_code | _, WSIGNALED exit_code | _, WSTOPPED exit_code ->
      Ok { exit_code; stdout = out; stderr = err }
  with
  | Unix.Unix_error (err, fn, _param) -> Error (Unix.error_message err ^ ": " ^ fn)
;;

let read_process_with_input (cmd : string) (args : string array) (input : string)
  : (proc_out, string) Result.t
  =
  (* Could probably deduplicate this and `read_process` somewhat *)
  try
    let in_read, in_write = Unix.pipe () in
    let out_read, out_write = Unix.pipe () in
    let err_read, err_write = Unix.pipe () in
    Unix.set_close_on_exec in_write;
    Unix.set_close_on_exec out_read;
    Unix.set_close_on_exec err_read;
    let in_oc = Unix.out_channel_of_descr in_write in
    let out_ic = Unix.in_channel_of_descr out_read in
    let err_ic = Unix.in_channel_of_descr err_read in
    Out_channel.output_string in_oc input;
    Stdlib.flush in_oc;
    let pid = Unix.create_process cmd args in_read out_write err_write in
    Stdlib.close_out in_oc;
    Unix.close in_read;
    Unix.close out_write;
    Unix.close err_write;
    Stdlib.flush_all ();
    let out = In_channel.input_all out_ic in
    let err = In_channel.input_all err_ic in
    Stdlib.close_in out_ic;
    Stdlib.close_in err_ic;
    match Unix.waitpid [] pid with
    | _, WEXITED exit_code | _, WSIGNALED exit_code | _, WSTOPPED exit_code ->
      Ok { exit_code; stdout = out; stderr = err }
  with
  | Unix.Unix_error (err, fn, _param) -> Error (Unix.error_message err ^ ": " ^ fn)
;;

let preprocess_source (source : string) : (proc_out, string) Result.t =
  let args = Array.of_list (c_preprocessor_arguments @ [ "-" ]) in
  read_process_with_input c_preprocessor args source
;;

let preprocess_file (uri : Uri.t) : (proc_out, string) Result.t =
  let path = DocumentUri.to_path uri in
  let args = Array.of_list (c_preprocessor_arguments @ [ path ]) in
  read_process c_preprocessor args
;;

let parse_document_source (uri : Uri.t) (source : string)
  : (Cabs.external_declaration list, string) Result.t
  =
  let () = CF.Switches.set [ "inner_arg_temps"; "at_magic_comments" ] in
  let path = DocumentUri.to_path uri in
  match C_parser_driver.parse_from_string ~filename:path source with
  | CF.Exception.Result (TUnit decls) -> Ok decls
  | CF.Exception.Exception (loc, cause) -> Error (CF.Pp_errors.to_string (loc, cause))
;;

let parse_document_file (uri : Uri.t) : (Cabs.external_declaration list, string) Result.t =
  match preprocess_file uri with
  | Ok out -> parse_document_source uri out.stdout
  | Error s -> Error s
;;
