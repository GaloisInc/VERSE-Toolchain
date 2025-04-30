Require Import List.
Import ListNotations.

Theorem app_nil_r: forall {A: Type} (l: list A), l ++ [] = l.
Proof.
Admitted.
