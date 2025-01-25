open! Base

type error = Cn.TypeErrors.t
type 'a m = 'a Cn.Resultat.t

let ( let* ) (a : 'a m) (f : 'a -> 'b m) : 'b m = Cn.Resultat.bind a f

(* No reason in principle not to have `return`, it just hasn't been used so
   far in practice *)

let run (x : 'a m) : ('a, error) Result.t = x
