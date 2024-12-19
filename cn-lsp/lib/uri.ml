open! Base

include Lsp.Types.DocumentUri

let sexp_of_t (uri : t) : Sexp.t = String.sexp_of_t (to_path uri)
