From stdpp Require Import stringmap mapset.

Definition atom := string.
Definition amap := stringmap.
Definition aset := stringset.

Class Stale {D} A := stale : A -> D.

Definition fv_of_set (s: aset) := fresh_string_of_set "x" s.
Lemma fv_of_set_fresh (s: aset) : (fv_of_set s) ∉ s.
Proof.
  apply fresh_string_of_set_fresh.
Qed.

Definition atom_dec := string_dec.
