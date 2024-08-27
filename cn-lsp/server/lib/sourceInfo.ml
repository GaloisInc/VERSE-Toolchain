open Base
module DocumentUri = Lsp.Types.DocumentUri

type ident_info = Document.ident_info
type document_info = ident_info ITree.t

(* This might be a good place to interpose some sort of process for updating the
   tree when the document changes - either modifying its actual contents, or
   including some sort of temporary shim that we apply to locations on lookup *)

let empty : document_info = ITree.empty

let from_list (elements : (Range.t * ident_info) list) : document_info =
  ITree.from_list elements
;;

let to_list (doc_info : document_info) : (Range.t * ident_info) list =
  ITree.to_list doc_info
;;

let insert (range : Range.t) (info : ident_info) (loc_map : document_info) =
  ITree.insert range info loc_map
;;

let info_at (doc_info : document_info) (posn : Position.t) : ident_info option =
  match ITree.intersecting doc_info posn with
  | [] -> None
  | intersections ->
    let ident_infos = List.map intersections ~f:snd in
    (match ident_infos with
     | [] -> None
     | [ i ] -> Some i
     | _ ->
       let () = Log.e "multiple identifiers at one position" in
       None)
;;

type t = (DocumentUri.t, document_info) Hashtbl.t

let document (map : t) (uri : DocumentUri.t) : document_info option = Hashtbl.find map uri

let from_source (uri : DocumentUri.t) (source : string) : (t, string) Result.t =
  match Document.parse_document_source uri source with
  | Error e -> Error e
  | Ok decls ->
    let doc = Document.process_external_declarations uri decls in
    Ok (Document.info_by_uri doc)
;;

let from_file (uri : DocumentUri.t) : (t, string) Result.t =
  match Document.parse_document_file uri with
  | Error e -> Error e
  | Ok decls ->
    let doc = Document.process_external_declarations uri decls in
    Ok (Document.info_by_uri doc)
;;

let merge (a : t) (b : t) : t =
  let f ~key:(_ : DocumentUri.t) join =
    match join with
    | `Left l -> Some l
    | `Right r -> Some r
    | `Both (l, _r) -> Some l
  in
  Hashtbl.merge a b ~f
;;
