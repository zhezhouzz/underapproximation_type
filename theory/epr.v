Require Import Setoid.
Lemma implies_ex_raw (P: nat -> nat -> Prop) (Q: Prop) :
  forall v, ((exists a, P v a) -> Q) <->
    (forall a, P v a -> Q).
Proof.
  intros.
  split.
  + intros. eapply H. exists a. eauto.
  + intros. destruct H0. eapply H. eauto.
Qed.

Lemma implies_to_or (P Q :Prop) :(P -> Q) <-> (~P \/ Q).
Admitted.

Lemma not_exists_to_forall (P: nat -> Prop):
  ~ (exists u, P u) <-> (forall u, ~ P u).
Admitted.

Lemma not_forall_to_exists (P: nat -> Prop):
  ~ (forall u, P u) <-> (exists u, ~ P u).
Admitted.

Lemma implies_fa_raw (P: nat -> nat -> Prop) (Q: Prop) :
  forall v, ((forall a, P v a) -> Q) <->
    (exists a, P v a -> Q).
Proof.
  intros.
  split.
  + intros. rewrite implies_to_or in H.
    destruct H. rewrite not_forall_to_exists in H.
    destruct H. exists x. intros. exfalso. auto.
    exists 0. auto.
  + intros. destruct H. apply H. auto.
Qed.

Lemma implies_epr (P Q: nat -> nat -> nat -> Prop) :
  forall v, ((exists a, forall u, P v a u) -> (exists a, forall u, Q v a u)) <->
    (forall a, exists u, P v a u -> (exists a', forall u, Q v a' u)).
Proof.
  intros.
  split.
  + intros.
    setoid_rewrite (implies_ex_raw (fun v a => forall u : nat, P v a u)) in H.
    remember (H a) as H'. clear HeqH'.
    setoid_rewrite (implies_fa_raw (fun v u => P v a u)) in H'.
    destruct H' as (u & H').
    exists u. auto.
  + intros. destruct H0 as (a & H0). destruct (H a) as (u & H'').
    apply H'' in H0. destruct H0. exists x. auto.
Qed.

Lemma implies_forall (P Q: nat -> nat -> Prop) (R: nat -> nat -> nat -> Prop) :
  (forall l1, (forall u, P l1 u) -> forall l2, (forall w, Q l2 w) -> forall v, R l1 l2 v) <->
    (forall l1 l2 v, (forall u, P l1 u) -> (forall w, Q l2 w) -> R l1 l2 v).
Proof.
  split; intros.
  + apply H; auto.
  + apply H; auto.
Qed.

Lemma and_forall (P Q: nat -> nat -> Prop) (R: nat -> nat -> nat -> Prop) :
  (exists l1, (forall u, P l1 u) /\ exists l2, (forall w, Q l2 w) /\ exists v, R l1 l2 v) <->
    (exists l1 l2 v, (forall u, P l1 u) /\ (forall w, Q l2 w) /\ R l1 l2 v).
Proof.
  split; intros.
  + destruct H as (l1 & H1 & H).
    destruct H as (l2 & H2 & H).
    destruct H as (v & H).
    exists l1, l2, v. repeat split; auto.
  + destruct H as (l1 & l2 & v & H1 & H2 & H).
    exists l1. split; auto.
    exists l2. split; auto.
    exists v. auto.
Qed.

Lemma or_forall (P Q: nat -> nat -> Prop) (R: nat -> nat -> nat -> Prop) :
  (exists l1, (forall u, P l1 u) \/ exists l2, (forall w, Q l2 w) \/ exists v, R l1 l2 v) <->
    (exists l1 l2 v, (forall u, P l1 u) \/ (forall w, Q l2 w) \/ R l1 l2 v).
Proof.
  split; intros.
  + destruct H as (l1 & H).
    destruct H. exists l1, 0, 0. left. auto.
    destruct H as (l2 & H). destruct H. exists l1, l2, 0. right. left. auto.
    destruct H as (v & H).
    exists l1, l2, v. right. right. auto.
  + destruct H as (l1 & l2 & v & H). destruct H; exists l1.
    left. auto.
    right. destruct H; exists l2.
    left. auto. right. exists v. auto.
Qed.
