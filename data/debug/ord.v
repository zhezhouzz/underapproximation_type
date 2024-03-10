From Coq Require Import Classes.DecidableClass.
From Coq Require Import Lia.
From Coq Require Import ZArith.

(* Import PeanoNat.Nat. *)
(* From Coq Require Import ZArith. *)

(* Ltac dec_tf := try (subst; rewrite decide_True; auto); try (rewrite decide_False; auto). *)

Variable IL: Type.
Variable lenF: IL -> Z.
Variable consF: Z -> IL -> IL.
Variable list_mem: IL -> Z -> Prop.
Variable ord: IL -> Z -> Z -> Prop.
Variable sorted: IL -> Prop.
Variable list_min: IL -> Z -> Prop.

Definition prop1 (v: IL) (size: Z) := ((size = 0/\ (lenF v) = 0) \/ (~size = 0/\ (exists y, (exists size_1, ((0 <= size_1/\ size_1 = (size - 1))/\ (exists l, (((size_1 < size/\ size_1 >= 0)/\ (lenF l) = size_1/\ (forall u, (forall w, (ord l u w -> u <= w))))/\ v = (consF y l))))))))%Z.

Definition prop2 (v: IL) (size: Z) := ((lenF v) = size/\ (forall u, (forall w, (ord v u w -> u <= w))))%Z.

Lemma list_nonempty_de_consF (l: IL) : (lenF l > 0)%Z -> exists x l', l = consF x l'.
Admitted.

Lemma list_lenF_nonneg (l: IL) : (lenF l >= 0)%Z.
Admitted.

Lemma list_lenF_nonneg' (l: IL) : (0 <= lenF l)%Z.
Admitted.

#[export] Hint Resolve list_lenF_nonneg : core.
#[export] Hint Resolve list_lenF_nonneg' : core.

Lemma list_consF_len (x: Z) (l: IL) : (lenF (consF x l) = lenF l + 1)%Z.
Admitted.

#[export] Hint Rewrite list_consF_len : core.
#[export] Hint Resolve list_consF_len : core.

Lemma list_consF_ord (x: Z) (l: IL) (u: Z) (w: Z) : ord (consF x l) u w <-> ((x = u /\ list_mem l w) \/ ord l u w).
Admitted.

Lemma list_empty_exists: exists l, (lenF l = 0)%Z.
Admitted.

Lemma list_empty_sorted (l: IL) : (lenF l = 0 -> sorted l)%Z.
Admitted.

#[export] Hint Resolve list_empty_sorted : core.

Lemma list_empty_no_mem (l: IL) (u: Z) : (lenF l = 0 -> ~ list_mem l u)%Z.
Admitted.

Lemma list_empty_any_min (l: IL) (u: Z) : (lenF l = 0 -> list_min l u)%Z.
Admitted.

#[export] Hint Resolve list_empty_any_min : core.

Lemma list_min_exists (l: IL) : (exists x, forall u, list_mem l u -> x <= u) %Z.
Admitted.

Lemma list_bounded_list_exists (x: Z) (n: Z) : (n >= 0 -> exists l, list_min l x /\ sorted l /\ lenF l = n)%Z.
Admitted.

Lemma check_ex : (exists size, (0 <= size/\ (exists x, (~size = 0/\ (exists y, (x <= y/\ (exists size_1, ((0 <= size_1/\ size_1 = (size - 1))/\ (exists l, (((size_1 < size/\ size_1 >= 0)/\ (lenF l) = size_1/\ (sorted l/\ list_min l y))/\ (exists v, v = x)))))))))))%Z.
Proof.
  exists 1%Z. intuition.
  exists 2%Z. intuition.
  exists 3%Z. intuition.
  exists 0%Z. intuition.
  destruct (list_bounded_list_exists 3 0)%Z as (l & Hl); intuition.
  exists l. intuition.
  exists 2%Z. intuition.
Qed.

Lemma check : (forall size, (0 <= size -> (forall x, (forall v, (((lenF v) = size/\ (list_min v x/\ sorted v)) -> ((size = 0/\ (lenF v) = 0) \/ (~size = 0/\ (exists y, (x <= y/\ (exists size_1, ((0 <= size_1/\ size_1 = (size - 1))/\ (exists l, (((size_1 < size/\ size_1 >= 0)/\ (lenF l) = size_1/\ (list_min l y/\ sorted l))/\ v = (consF x l))))))))))))))%Z.
Proof.
  intros. destruct (Z.eqb_spec size 0).
  - left. intuition.
  - right. intuition. subst.
    assert ((lenF v > 0)%Z) as Htmp; intuition.
    apply list_nonempty_de_consF in Htmp. destruct Htmp as (y & v' & Hyv'). subst. clear n. clear H.
    exists x. intuition.
    exists (lenF v'). intuition. rewrite list_consF_len. intuition.
    exists v'. intuition. rewrite list_consF_len. intuition.
    admit. admit.

Lemma subtyping : forall (v: IL) (size: Z), (size >= 0)%Z -> prop2 v size -> prop1 v size.
Proof.
  unfold prop1, prop2. intros.
  destruct (Z.eqb_spec size 0).
  - left. intuition.
  - right. intuition. subst.
    assert ((lenF v > 0)%Z) as Htmp; intuition.
    apply list_nonempty_de_consF in Htmp. destruct Htmp as (x & v' & Hxv'). subst. clear n. clear H.
    exists x, (lenF v'). intuition. rewrite list_consF_len. intuition.
    exists v'. intuition. rewrite list_consF_len. intuition.
    apply H2. rewrite list_consF_ord. right. auto.
Qed.
