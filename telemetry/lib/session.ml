open Base

type t =
  | Day of
      { year : int
      ; month : int
      ; day : int
      }
  | Custom of { id : Int64.t }

let today () : t =
  let tm = Unix.(gmtime (time ())) in
  Day { year = tm.tm_year + 1900; month = tm.tm_mon + 1; day = tm.tm_mday }
;;

let custom () : t = Custom { id = Random.int64_incl Int64.zero Int64.max_value }

let to_filename (session : t) : string =
  let tag =
    match session with
    | Day day -> Printf.sprintf "%i-%i-%i" day.year day.month day.day
    | Custom { id } -> Int64.to_string id
  in
  "session-" ^ tag
;;

let of_filename (path : string) : t option =
  match
    Stdlib.Scanf.sscanf_opt
      (Stdlib.Filename.basename path)
      "session-%i-%i-%i"
      (fun y m d -> y, m, d)
  with
  | Some (year, month, day) -> Some (Day { year; month; day })
  | None ->
    (match
       Stdlib.Scanf.sscanf_opt (Stdlib.Filename.basename path) "session-%Li" Fn.id
     with
     | Some i -> Some (Custom { id = i })
     | None -> None)
;;
