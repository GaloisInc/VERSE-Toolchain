Require Import List.
Import ListNotations.
Require Import Lia.

Definition max_elem_list (l: list nat) : nat := fold_right max 0 l.

Theorem every_elem_le_max : forall (l: list nat) (n: nat), (In n l) -> (n <= (max_elem_list l)).
Proof.
Admitted.
