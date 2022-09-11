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
