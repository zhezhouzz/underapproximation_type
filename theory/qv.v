Require Import Setoid.

Lemma implies_to_or (P Q :Prop) :(P -> Q) <-> (~P \/ Q).
Admitted.

Lemma not_forall_to_exists (P: nat -> Prop):
  ~ (forall u, P u) <-> (exists u, ~ P u).
Admitted.
Lemma implies_forall_exists (P Q: nat -> Prop):
  ((forall u, P u) -> (exists w, Q w)) <-> (exists w u, P u -> Q w).
Proof.
  intros. split; intros.
  - rewrite implies_to_or in H.
    destruct H.
    + rewrite not_forall_to_exists in H. destruct H as (u & H). exists 0, u. intro. exfalso. auto.
    + destruct H as (w & H). exists w, 0. auto.
  - destruct H as (w & u & H).
    exists w. auto.
Qed.
