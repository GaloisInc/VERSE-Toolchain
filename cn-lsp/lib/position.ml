open! Base

type t = Lsp.Types.Position.t

let create ~(line : int) ~(character : int) = Lsp.Types.Position.create ~line ~character
let eq (p1 : t) (p2 : t) : bool = p1.line = p2.line && p1.character = p2.character

let lt (p1 : t) (p2 : t) : bool =
  p1.line < p2.line || (p1.line = p2.line && p1.character < p2.character)
;;

let le (p1 : t) (p2 : t) : bool = lt p1 p2 || eq p1 p2

let gt (p1 : t) (p2 : t) : bool =
  p1.line > p2.line || (p1.line = p2.line && p1.character > p2.character)
;;

let ge (p1 : t) (p2 : t) : bool = gt p1 p2 || eq p1 p2
let compare (p1 : t) (p2 : t) : int = if lt p1 p2 then -1 else if gt p1 p2 then 1 else 0
let to_string (p : t) : string = Printf.sprintf "%i:%i" p.line p.character
