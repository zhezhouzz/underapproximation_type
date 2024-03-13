From Coq Require Import Classes.DecidableClass.
From Coq Require Import Lia.
From Coq Require Import ZArith.

(* Import PeanoNat.Nat. *)
(* From Coq Require Import ZArith. *)

(* Ltac dec_tf := try (subst; rewrite decide_True; auto); try (rewrite decide_False; auto). *)

Variable IL: Type.
Variable len: IL -> Z -> Prop.
Variable emp: IL -> Prop.
Variable hd: IL -> Z -> Prop.
Variable tl: IL -> IL -> Prop.
Variable list_mem: IL -> Z -> Prop.
Variable sorted: IL -> Prop.

Lemma list_len_0_emp (l : IL) :(len l 0) -> (emp l) .
Admitted.
#[export] Hint Resolve list_len_0_emp : core.

Lemma list_emp_len_0 (l : IL) (n: Z) :(len l n) -> (emp l) -> (n = 0)%Z .
Admitted.
#[export] Hint Resolve list_emp_len_0 : core.

Lemma list_len_not_0_not_emp (l : IL) (n: Z) : not (n = 0)%Z -> (len l n) -> not (emp l).
Admitted.
#[export] Hint Resolve list_len_not_0_not_emp : core.


Lemma decide_emp: forall v, emp v \/ not (emp v).
Admitted.

Lemma list_not_emp_ex_hd (l : IL): not (emp l) -> exists x, hd l x.
Admitted.
#[export] Hint Resolve list_not_emp_ex_hd : core.

Lemma list_not_emp_ex_tl (l : IL): not (emp l) -> exists l', tl l l'.
Admitted.
#[export] Hint Resolve list_not_emp_ex_tl : core.

Lemma list_sorted_tl_sorted (l : IL) (l' : IL): tl l l' -> sorted l -> sorted l'.
Admitted.
#[export] Hint Resolve list_sorted_tl_sorted : core.

Lemma list_len_tl_len_minus_1 (l : IL) (l' : IL) (n: Z): tl l l' -> (len l n) -> (len l' (n - 1)%Z).
Admitted.
#[export] Hint Resolve  list_len_tl_len_minus_1 : core.

Lemma list_len_tl_len_minus_1' (l : IL) (l' : IL) (n: Z): tl l l' -> (len l' (n - 1)%Z) -> (len l n).
Admitted.
#[export] Hint Resolve  list_len_tl_len_minus_1' : core.

(* Lemma list_nonempty_de_consF (l: IL) : (lenF l > 0)%Z -> exists x l', l = consF x l'. *)
(* Admitted. *)

(* Lemma list_nonempty_de_consF (l: IL) : (lenF l > 0)%Z -> exists x l', l = consF x l'. *)
(* Admitted. *)

(* Lemma list_lenF_nonneg (l: IL) : (lenF l >= 0)%Z. *)
(* Admitted. *)

(* Lemma list_lenF_nonneg' (l: IL) : (0 <= lenF l)%Z. *)
(* Admitted. *)

(* #[export] Hint Resolve list_lenF_nonneg : core. *)
(* #[export] Hint Resolve list_lenF_nonneg' : core. *)

(* Lemma list_consF_len (x: Z) (l: IL) : (lenF (consF x l) = lenF l + 1)%Z. *)
(* Admitted. *)

(* #[export] Hint Rewrite list_consF_len : core. *)
(* #[export] Hint Resolve list_consF_len : core. *)

(* Lemma list_consF_ord (x: Z) (l: IL) (u: Z) (w: Z) : ord (consF x l) u w <-> ((x = u /\ list_mem l w) \/ ord l u w). *)
(* Admitted. *)

(* Lemma list_empty_exists: exists l, (lenF l = 0)%Z. *)
(* Admitted. *)

(* Lemma list_empty_sorted (l: IL) : (lenF l = 0 -> sorted l)%Z. *)
(* Admitted. *)

(* #[export] Hint Resolve list_empty_sorted : core. *)

(* Lemma list_empty_no_mem (l: IL) (u: Z) : (lenF l = 0 -> ~ list_mem l u)%Z. *)
(* Admitted. *)

(* Lemma list_empty_any_min (l: IL) (u: Z) : (lenF l = 0 -> list_min l u)%Z. *)
(* Admitted. *)

(* #[export] Hint Resolve list_empty_any_min : core. *)

(* Lemma list_min_exists (l: IL) : (exists x, forall u, list_mem l u -> x <= u) %Z. *)
(* Admitted. *)

(* Lemma list_bounded_list_exists (x: Z) (n: Z) : (n >= 0 -> exists l, list_min l x /\ sorted l /\ lenF l = n)%Z. *)
(* Admitted. *)

Lemma sized_list_check: (forall s, (0 <= s -> (forall v, ((forall n, (len v n -> n <= s)) -> ((s = 0/\ emp v) \/ (~s = 0/\ (exists x_1, ((x_1/\ emp v) \/ (~x_1/\ (exists s_1, ((0 <= s_1/\ s_1 = (s - 1))/\ (exists x_3, (((s_1 < s/\ s_1 >= 0)/\ (forall n, (len x_3 n -> n <= s_1)))/\ (exists x_4, (hd v x_4/\ tl v x_3)))))))))))))))%Z.
Proof.
  intros. destruct (Z.eqb_spec s 0).
  - left. subst. intuition.
  - right. intuition.

  intros. destruct (decide_emp v).
  - left. intuition.



Lemma check : (forall size, (0 <= size -> (forall v, ((len v size/\ sorted v) -> ((size = 0/\ emp v) \/ (~size = 0/\ (exists size_1, ((0 <= size_1/\ size_1 = (size - 1))/\ (exists l, (((size_1 < size/\ size_1 >= 0)/\ len l size_1/\ sorted l)/\ (exists y, (hd v y/\ tl v l))))))))))))%Z.
Proof.
  intros. destruct (Z.eqb_spec size 0).
  - left. subst. intuition.
  - right. intuition.
    destruct (decide_emp v).
    + apply list_len_not_0_not_emp in H1; auto. intuition.
    + exists (size - 1)%Z. intuition.
      destruct (list_not_emp_ex_hd _ H0) as (x & Hx).
      destruct (list_not_emp_ex_tl _ H0) as (l & Hl).
      exists l. intuition; eauto.
Qed.

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
