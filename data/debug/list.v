From Coq Require Import Classes.DecidableClass.
From Coq Require Import Lia.
From Coq Require Import ZArith.
From Coq Require Import Setoid.
From Coq Require Import Logic.Classical.

Definition int := Z.
Variable IL: Type.
(* Variable len: IL -> int -> Prop. *)
Variable emp: IL -> Prop.
Variable hd: IL -> int -> Prop.
Variable len: IL -> int -> Prop.
Variable tl: IL -> IL -> Prop.
Variable list_mem: IL -> int -> Prop.
Variable uniq: IL -> Prop.
Variable sorted: IL -> Prop.
Variable concat: IL -> IL -> IL -> Prop.

(** uniq *)

Ltac z_simpl :=
  match goal with
  | H: context [(?a - ?b + ?b)%Z] |- _  => rewrite Z.sub_add in H
  | |- context [(?a - ?b + ?b)%Z]  =>  rewrite Z.sub_add
  end.

Lemma list_uniq_any_len (i: int): (i >= 0 -> exists (l: IL), len l i /\ uniq l)%Z.
Admitted.

(* Lemma list_uniq_any_len_with_hd (i: int) (h: int): (i > 0 -> exists (l: IL) (l': IL), len l i /\ hd l h /\ tl l l' /\ uniq l' /\ uniq l)%Z. *)
(* Admitted. *)

(* Lemma list_uniq_mem_in (l: IL) (h: int): *)
(*   (uniq l /\ list_mem l h -> exists (l1: IL) (l2: IL) (l3: IL), *)
(*       (not (list_mem l3 h)) /\ (not (list_mem l1 h)) /\ hd l2 h /\ tl l2 l3 /\ concat l1 l2 l)%Z. *)
(* Admitted. *)

Lemma list_uniq_not_mem (l: IL): uniq l -> exists (x : int), (not (list_mem l x)).
Admitted.

(* Lemma list_cons (h: int) (t: IL): (exists (l: IL), hd l h /\ tl l t)%Z. *)
(* Admitted. *)

Lemma list_uniq_tl_uniq (l: IL) (l': IL): uniq l /\ tl l l' -> uniq l'.
Admitted.

Lemma list_uniq_hd_not_in_tl (l: IL) (h: int) (l': IL): uniq l /\ tl l l' /\ hd l h -> not (list_mem l' h).
Admitted.

Lemma list_uniq_fst_second_not_eq (l: IL) (h: int) (t: IL) (h': int): (uniq l /\ hd l h /\ tl l t /\ hd t h' -> not (h = h'))%Z.
Admitted.

Lemma list_mem_hd_or_tl (l: IL) (h: int) (t: IL) (u: int): (list_mem l u /\ hd l h /\ tl l t -> (list_mem t u \/ u = h))%Z.
Admitted.

Lemma list_mem_tl_also_l (l: IL) (t: IL) (u: int): (list_mem t u /\ tl l t -> list_mem l u)%Z.
Admitted.

Lemma list_hd_is_mem (l: IL) (u: int): (hd l u -> list_mem l u)%Z.
Admitted.

Lemma list_len_not_zero_not_emp (l: IL) (i: int): (len l i /\ ~ (i = 0) -> ~ emp l)%Z.
Admitted.

Lemma list_destruct_non_emp (l: IL): (~ emp l -> exists (h: int), exists (t: IL), hd l h /\ tl l t)%Z.
Admitted.

Lemma list_len_tl_len (l: IL) (l': IL) (i: int): (len l (i + 1) /\ tl l l' -> len l' i)%Z.
Admitted.

Lemma list_len_zero_is_emp (l: IL): (len l 0 -> emp l)%Z.
Admitted.

Lemma uniq_query2: (forall i, (0 <= i -> (forall v, (exists s, (uniq s/\ len s i/\ (exists x, (~list_mem s x/\ ((uniq v/\ len v (i + 1)) -> ((emp s/\ (exists x_0, (emp x_0/\ hd v x/\ tl v x_0))) \/ (exists h, (exists t, (hd s h/\ tl s t/\ ~x = h/\ (exists i_1, (uniq t/\ len t i_1/\ (exists x_3, (uniq x_3/\ len x_3 (i_1 + 1)/\ 0 <= i_1/\ i_1 < i/\ ~list_mem t h/\ hd v x/\ tl v x_3))))))))))))))))%Z.
Proof.
  intros.
  destruct (classic (uniq v /\ len v (i + 1)%Z)).
  2: { destruct (list_uniq_any_len i) as (s & Hs). lia. exists s. intuition.
       destruct (list_uniq_not_mem s) as (x & Hx); auto. exists x. intuition. }
  assert (~ emp v). { eapply list_len_not_zero_not_emp. intuition; eauto. lia. }
  destruct (list_destruct_non_emp v) as (h & t & Hh & Ht); auto.
  assert (uniq t). { eapply list_uniq_tl_uniq; intuition; eauto. }
  assert (not (list_mem t h)). { eapply list_uniq_hd_not_in_tl; intuition; eauto. }
  assert (len t i). { eapply list_len_tl_len; intuition; eauto. }
  destruct (classic (i = 0%Z)); subst.
  - exists t. intuition. exists h. intuition.
    assert (emp t). { apply list_len_zero_is_emp. eapply list_len_tl_len; intuition; eauto. }
    left. intuition. exists t. intuition.
  - assert (~ emp t). { eapply list_len_not_zero_not_emp. intuition; eauto. }
    destruct (list_destruct_non_emp t) as (h' & t' & Hh' & Ht'); auto.
    exists t. intuition.
    exists h. intuition.
    assert (not (h = h')). { eapply list_uniq_fst_second_not_eq; intuition; eauto. }
    right. exists h', t'. intuition.
    exists (i - 1)%Z. intuition.
    apply list_uniq_tl_uniq with (l := t); intuition; eauto.
    eapply list_len_tl_len; intuition; try z_simpl; eauto.
    exists t. intuition. eapply list_len_tl_len; intuition; try z_simpl; eauto.
    eapply list_uniq_hd_not_in_tl in H11; eauto.
Qed.

Lemma uniq_query: (forall i, (0 <= i -> (forall v, (exists s, (uniq s/\ len s i/\ (exists x, (~list_mem s x/\ ((uniq v/\ len v (i + 1)/\ (forall u, (list_mem v u <-> (list_mem s u \/ u = x)))) -> ((emp s/\ (exists x_0, (emp x_0/\ hd v x/\ tl v x_0))) \/ (exists h, (exists t, (hd s h/\ tl s t/\ ~x = h/\ (exists i_1, (uniq t/\ len t i_1/\ (exists x_3, (uniq x_3/\ len x_3 (i_1 + 1)/\ (forall u, (list_mem x_3 u <-> (list_mem t u \/ u = h)))/\ 0 <= i_1/\ i_1 < i/\ ~list_mem t h/\ hd v x/\ tl v x_3))))))))))))))))%Z.
Proof.
  intros.
  destruct (classic (uniq v /\ len v (i + 1)%Z)).
  2: { destruct (list_uniq_any_len i) as (s & Hs). lia. exists s. intuition.
       destruct (list_uniq_not_mem s) as (x & Hx); auto. exists x. intuition. }
  assert (~ emp v). { eapply list_len_not_zero_not_emp. intuition; eauto. lia. }
  destruct (list_destruct_non_emp v) as (h & t & Hh & Ht); auto.
  assert (uniq t). { eapply list_uniq_tl_uniq; intuition; eauto. }
  assert (not (list_mem t h)). { eapply list_uniq_hd_not_in_tl; intuition; eauto. }
  assert (len t i). { eapply list_len_tl_len; intuition; eauto. }
  destruct (classic (i = 0%Z)); subst.
  - exists t. intuition. exists h. intuition.
    assert (emp t). { apply list_len_zero_is_emp. eapply list_len_tl_len; intuition; eauto. }
    left. intuition. exists t. intuition.
  - assert (~ emp t). { eapply list_len_not_zero_not_emp. intuition; eauto. }
    destruct (list_destruct_non_emp t) as (h' & t' & Hh' & Ht'); auto.
    exists t. intuition.
    exists h. intuition.
    assert (not (h = h')). { eapply list_uniq_fst_second_not_eq; intuition; eauto. }
    right. exists h', t'. intuition.
    exists (i - 1)%Z. intuition.
    apply list_uniq_tl_uniq with (l := t); intuition; eauto.
    eapply list_len_tl_len; intuition; try z_simpl; eauto.
    exists t. intuition. eapply list_len_tl_len; intuition; try z_simpl; eauto.
    eapply list_mem_hd_or_tl; eauto.
    eapply list_mem_tl_also_l; eauto.
    subst. eapply list_hd_is_mem; eauto.
    eapply list_uniq_hd_not_in_tl in H12; eauto.
Qed.


Lemma list_sorted_any_len (i: int): (i >= 0 ->  exists (l: IL), len l i /\ sorted l)%Z.
Admitted.

Lemma list_destruct (l: IL): (emp l \/ exists (h: int), exists (t: IL), hd l h /\ tl l t)%Z.
Admitted.

Lemma list_sorted_tl_sorted (l: IL) (l': IL): (sorted l /\ tl l l' -> sorted l')%Z.
Admitted.

Lemma list_sorted_fst_second_lt (l: IL) (h: int) (t: IL) (h': int): (sorted l /\ hd l h /\ tl l t /\ hd t h' -> h < h')%Z.
Admitted.

Lemma sorted_query: (forall i, (0 <= i -> (forall v, (exists x, (exists s, (sorted s/\ len s i/\ ((sorted v/\ len v (i + 1)) -> ((emp s/\ (exists x_0, (emp x_0/\ hd v x/\ tl v x_0))) \/ (exists h, (exists t, (hd s h/\ tl s t/\ ~x = h/\ x < h/\ (exists x_3, (hd x_3 h/\ tl x_3 t/\ hd v x/\ tl v x_3)))))))))))))%Z.
Proof.
  intros.
  destruct (classic (sorted v)).
  2: { destruct (list_sorted_any_len i) as (s & Hs). lia. exists 0%Z, s. intuition. }
  destruct (classic (len v (i + 1)%Z)).
  2: { destruct (list_sorted_any_len i) as (s & Hs). lia. exists 0%Z, s. intuition. }
  assert (~ emp v). { eapply list_len_not_zero_not_emp. intuition; eauto. lia. }
  destruct (list_destruct_non_emp v) as (h & t & Hh & Ht); auto.
  assert (sorted t). { eapply list_sorted_tl_sorted; intuition; eauto. }
  assert (len t i). { eapply list_len_tl_len; intuition; eauto. }
  exists h, t. intuition.
  destruct (classic (i = 0%Z)); subst.
  - assert (emp t). { apply list_len_zero_is_emp. eapply list_len_tl_len; intuition; eauto. }
    left. intuition. exists t. intuition.
  - right.
    assert (~ emp t). { eapply list_len_not_zero_not_emp. intuition; eauto. }
    destruct (list_destruct_non_emp t) as (h' & t' & Hh' & Ht'); auto.
    assert (h < h')%Z. { eapply list_sorted_fst_second_lt; intuition; eauto. }
    exists h', t'. intuition. exists t. intuition.
Qed.
