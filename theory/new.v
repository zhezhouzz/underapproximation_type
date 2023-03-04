Require Import Setoid.

Lemma implies_to_or (P Q :Prop) :(P -> Q) <-> (~P \/ Q).
Admitted.
Lemma not_forall_to_exists (P: nat -> Prop):
  ~ (forall u, P u) <-> (exists u, ~ P u).
Admitted.
Lemma not_exists_to_forall (P: nat -> Prop):
  ~ (exists u, P u) <-> (forall u, ~ P u).
Admitted.
Lemma refl (P: Prop): P \/ ~ P.
Admitted.

Definition has_over_arrow_type (P: nat -> Prop) (Q: nat -> nat -> Prop) (f: nat -> nat -> Prop) :=
  forall x v, P x /\ Q x v -> f x v.

Definition has_under_arrow_type (R: nat -> Prop) (P: nat -> Prop) (Q: nat -> nat -> Prop) (g: nat -> nat -> nat -> Prop) :=
  forall (f: nat -> nat -> Prop), has_over_arrow_type P Q f -> exists u, R u /\ g u = f.

Definition has_under_arrow_type' (R: nat -> Prop) (P: nat -> Prop) (Q: nat -> nat -> Prop) (g: nat -> nat -> nat -> Prop) :=
  forall x v, P x /\ Q x v -> exists u, R u /\ g u x v.

Lemma l1: forall R P Q g, has_under_arrow_type R P Q g <-> has_under_arrow_type' R P Q g.
Proof.
  intros. unfold has_under_arrow_type, has_under_arrow_type'.
  split; intro.
  - intros. destruct H0.
    unfold has_over_arrow_type in H.
    assert (exists u : nat, R u /\ g u = fun x' v' => P x' /\ Q x' v').
    apply H. auto.
    destruct H2 as (u & HR & Heq).
    exists u. split; auto. rewrite Heq. split. auto. auto.
  - intros. unfold has_over_arrow_type in H0.
    assert (exists u, forall x v : nat, P x /\ Q x v -> R u /\ g u x v). admit.
    destruct H1 as (u & H1).
    exists u.
    assert ((exists u, R u /\ g u x v) \/ (forall u, ~ (R u /\ g u x v))). admit.
    destruct H2.

    + auto.
    + exfalso.
      

      set (fun x' v' => x = x' /\ v = v').
      set (H P0).
      apply H in P0.
     
      
