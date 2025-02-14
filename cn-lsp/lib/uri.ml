open! Base
include Lsp.Types.DocumentUri

let sexp_of_t (uri : t) : Sexp.t = String.sexp_of_t (to_path uri)
let to_yojson (uri : t) : Yojson.Safe.t = yojson_of_t uri
let of_yojson (js : Yojson.Safe.t) : (t, string) Result.t = Ok (t_of_yojson js)
