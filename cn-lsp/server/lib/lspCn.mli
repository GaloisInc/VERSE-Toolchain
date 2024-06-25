type error

val error_to_string : error -> string
val error_to_diagnostic : error -> Lsp.Types.Diagnostic.t

type 'a m

val ( let* ) : 'a m -> ('a -> 'b m) -> 'b m
val return : 'a -> 'a m
val run : 'a m -> ('a, error) result

type cerb_env

val setup : unit -> cerb_env m
val run_cn : cerb_env -> Lsp.Types.DocumentUri.t -> unit m
