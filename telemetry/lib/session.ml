open Base

type t =
  | Day of
      { year : int
      ; month : int
      ; day : int
      }
  | Custom of { id : Int64.t }
[@@deriving eq, hash, ord, sexp, show]

let today () : t =
  let tm = Unix.(gmtime (time ())) in
  Day { year = tm.tm_year + 1900; month = tm.tm_mon + 1; day = tm.tm_mday }
;;

let custom () : t = Custom { id = Random.int64_incl Int64.zero Int64.max_value }
