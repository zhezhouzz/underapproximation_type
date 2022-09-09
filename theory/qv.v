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

Lemma refl (P: Prop): P \/ ~ P.
Admitted.

Lemma implies_exists_exists (P Q: nat -> Prop):
  ((exists u, P u) -> (exists w, Q w)) <-> (forall u, exists w, P u -> Q w).
Proof.
  intros. split; intros.
  - destruct (refl (P u)).
    + assert (exists u, P u). exists u. auto. apply H in H1. destruct H1. exists x. auto.
    + exists 0. intros. exfalso. auto.
  - destruct H0 as (u & H0). destruct (H u). exists x. auto.
Qed.

Lemma not_exists_to_forall (P: nat -> Prop):
  ~ (exists u, P u) <-> (forall u, ~ P u).
Admitted.

Lemma implies_exists_exists' (P Q: nat -> Prop):
  ((exists u, P u) -> (exists w, Q w)) <-> (exists w, forall u, P u -> Q w).
Proof.
  intros. split; intros.
  - rewrite implies_to_or in H.
    destruct H.
    + rewrite not_exists_to_forall in H. exists 0. intros. exfalso. apply (H u). auto.
    + destruct H. exists x. auto.
  - destruct H0. destruct H. exists x0. apply (H x). auto.
Qed.

Lemma exists_implies (P: nat -> Prop) (Q: Prop) :
  ((exists u, P u) -> Q) <-> (forall u, P u -> Q).
Proof.
  intros. split; intros.
  - rewrite implies_to_or in H.
    destruct H; auto.
    + rewrite not_exists_to_forall in H. exfalso. apply (H u). auto.
  - destruct H0. eauto.
Qed.
