(** A CN error. *)
type error

(** Convert a CN error to a string. *)
val error_to_string : error -> string

(** Convert a CN error to an LSP diagnostic. *)
val error_to_diagnostic
  :  error
  -> (Lsp.Types.DocumentUri.t * Lsp.Types.Diagnostic.t) option

(** The type of the CN "monad". *)
type 'a m

(** CN's monadic bind. *)
val ( let* ) : 'a m -> ('a -> 'b m) -> 'b m

(** CN's monadic return. *)
val return : 'a -> 'a m

(** Run the CN monad. *)
val run : 'a m -> ('a, error) result

(** A reusable "environment" needed to run CN. *)
type cerb_env

(** Create the environment needed to run CN. *)
val setup : unit -> cerb_env m

(** Run CN on the given document to potentially produce errors. Use [run] to
    interpret the result, and [error_to_string] and [error_to_diagnostic] to
    process any errors. *)
val run_cn : cerb_env -> Lsp.Types.DocumentUri.t -> unit m
