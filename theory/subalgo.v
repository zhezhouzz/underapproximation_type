Lemma distruct_prop: forall (P: Prop), P \/ ~ P. Admitted.

Lemma case_in_out: forall (q1: nat -> nat -> Prop) (q2:nat -> Prop) (p:nat -> Prop),
    (forall v, exists x, q2 v -> p x /\ q1 x v) <->
      (forall v, q2 v -> (exists x, p x /\ q1 x v)).
Proof.
  intros.
  split; intros.
  + destruct (H v).
    exists x. apply H1 in H0. auto.
  + destruct (distruct_prop (q2 v)).
    - apply H in H0. destruct H0. exists x. auto.
    - exists 0. intros. exfalso. auto.
Qed.

Lemma case_out_in: forall (q1: nat -> Prop) (q2:nat -> nat -> Prop) (p:nat -> Prop),
    (forall v, exists x, (p x -> q2 x v) -> q1 v) <->
      (forall v, (forall x, p x -> q2 x v) -> q1 v).
Proof.
  intros.
  split; intros.
  + destruct (H v). auto.
  + destruct (distruct_prop ((forall x : nat, p x -> q2 x v))).
    - exists 0. auto.
    - exfalso. apply H0. apply H.
