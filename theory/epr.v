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

Lemma implies_forall (P Q: nat -> nat -> Prop) (R1 R2: nat -> nat -> nat -> Prop) :
  (forall l1, (forall u, P l1 u) -> forall v, (forall w, R2 l1 v w) -> (exists z, R1 l1 v z)) <->
    (forall l1 v, (forall u, P l1 u) -> (forall w, R2 l1 v w) -> (exists z, R1 l1 v z))
Proof.
  split; intros.
  + apply H; auto.
  + apply H; auto.
Qed.
