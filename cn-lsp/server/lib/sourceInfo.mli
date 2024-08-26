type ident_info = Document.ident_info
type document_info

val from_list : (Range.t * ident_info) list -> document_info
val to_list : document_info -> (Range.t * ident_info) list
val info_at : document_info -> Position.t -> ident_info option

type t

val from_source : Lsp.Types.DocumentUri.t -> string -> (t, string) result
val from_file : Lsp.Types.DocumentUri.t -> (t, string) result
