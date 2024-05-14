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
Variable lenlt: IL -> int -> Prop.
Variable lenlte: IL -> int -> Prop.
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

Lemma list_emp_no_hd (l: IL) (h: int): (emp l) -> (not (hd l h)).
Admitted.

Lemma list_uniq_not_mem (l: IL): uniq l -> exists (x : int), (not (list_mem l x)).
Admitted.

(* Lemma list_cons (h: int) (t: IL): (exists (l: IL), hd l h /\ tl l t)%Z. *)
(* Admitted. *)

Lemma list_len_leq_zero (l: IL) (n: int): (len l n -> 0 <= n)%Z.
Admitted.

Lemma list_nonemp_len_lt_zero (l: IL) (n: int): ((not (emp l)) /\ len l n -> 0 < n)%Z.
Admitted.

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

(* Lemma list_sorted_any_len (i: int): (i >= 0 ->  exists (l: IL), len l i /\ sorted l)%Z. *)
(* Admitted. *)

(* Lemma list_destruct (l: IL): (emp l \/ exists (h: int), exists (t: IL), hd l h /\ tl l t)%Z. *)
(* Admitted. *)

Lemma list_sorted_tl_sorted (l: IL) (l': IL): (sorted l /\ tl l l' -> sorted l')%Z.
Admitted.

Lemma list_sorted_fst_second_lt (l: IL) (h: int) (t: IL) (h': int): (sorted l /\ hd l h /\ tl l t /\ hd t h' -> h < h')%Z.
Admitted.

Lemma list_singleton_list_sorted (l: IL): (len l 1 -> sorted l)%Z.
Admitted.

Lemma list_singleton_list_uniq (l: IL): (len l 1 -> uniq l)%Z.
Admitted.

Lemma list_singleton_list_ex (h: int): (exists (l: IL) (l': IL), len l 1 /\ hd l h /\ tl l l' /\ len l' 0)%Z.
Admitted.

Lemma list_sorted_list_ex (n: int) (h: int): ( n <= 1 -> exists (l: IL), len l n /\ hd l h /\ sorted l)%Z.
Admitted.

Lemma list_len_lenlte (l: IL) (n: int) (m: int): (len l n /\ n <= m -> lenlte l m)%Z.
Admitted.

Lemma unique_union_inter: (forall v, (forall i, (0 <= i -> (forall n, ((0 <= n/\ 2 <= n) -> ((sorted v/\ len v n) -> (exists s1, (sorted s1/\ lenlte s1 (n - 1)/\ (exists s2, (sorted s2/\ lenlte s2 (n - 1)/\ ((emp s1/\ v = s2) \/ (exists h1, (exists t1, (hd s1 h1/\ tl s1 t1/\ (exists h2, (exists t2, (hd s2 h2/\ tl s2 t2/\ ~h1 = h2/\ h1 < h2/\ (exists i_3, (i_3 < i/\ 0 <= i_3/\ emp t1/\ (exists x_3, (sorted x_3/\ len x_3 i_3/\ sorted s2/\ lenlte s2 i_3/\ hd v h1/\ tl v x_3)))))))))))))))))))))%Z.
Proof.
  intros.
  assert (~ emp v). { eapply list_len_not_zero_not_emp. intuition; eauto. lia. }
  destruct (list_destruct_non_emp v) as (h & t & Hh & Ht); auto.
  assert (sorted t). { eapply list_sorted_tl_sorted; intuition; eauto. }
  assert (len t (n - 1)%Z). { eapply list_len_tl_len; intuition; try z_simpl; eauto. }
  destruct (list_singleton_list_ex h) as (s1 & t1 & Hs1).
  assert (sorted s1). { apply list_singleton_list_sorted; intuition. }
  exists s1. intuition. apply list_len_lenlte with 1%Z; intuition; eauto.
  exists t. intuition. apply list_len_lenlte with (n - 1)%Z; intuition; eauto.
  right.
  exists h, t1. intuition.
  assert (~ emp t). { eapply list_len_not_zero_not_emp. intuition; eauto. lia. }
  destruct (list_destruct_non_emp t) as (h2 & t2 & Hh2 & Ht2); auto.
  assert (h < h2)%Z. { eapply list_sorted_fst_second_lt; intuition; eauto. }
  exists h2, t2. intuition.
  exists (i - 1)%Z. intuition. apply list_sorted_tl_sorted with s1; intuition; eauto.
  apply list_len_lenlte with 0%Z; intuition; eauto.
  exists t. intuition.
  eapply list_len_lenlte; intuition; eauto.
Qed.


Lemma unique_insert_sub: (forall v, (forall i, ((0 <= i/\ 0 < i) -> (forall s, ((uniq s/\ len s (i - 1)) -> (exists x, (~list_mem s x/\ (exists h, (exists t, ((hd s h/\ tl s t) -> (~x = h -> (exists i_1, (i_1 < i/\ 0 <= i_1/\ 0 < i_1/\ ((uniq v/\ len v (i_1 - 1)) -> v = t))))))))))))))%Z.
Proof.
  intros. intuition.
  destruct (list_uniq_not_mem s); auto.
  exists x. intuition.
  destruct (classic (emp s)).
  exists x, v. intros. exfalso. apply (list_emp_no_hd s x); intuition.
  destruct (list_destruct_non_emp s) as (h & t & Hh & Ht); auto.
  exists h, v. intros.
  exists (i - 1)%Z. intuition. apply list_nonemp_len_lt_zero with s; eauto.
Qed.

Lemma unique_insert: (forall v, (forall i, ((0 <= i/\ 0 < i) -> (exists s, (uniq s/\ len s (i - 1)/\ (exists x, (~list_mem s x/\ ((uniq v/\ len v i) -> ((emp s/\ (exists x_0, (emp x_0/\ hd v x/\ tl v x_0))) \/ (exists h, (exists t, (hd s h/\ tl s t/\ ~x = h/\ (exists i_1, (i_1 < i/\ 0 <= i_1/\ 0 < i_1/\ uniq t/\ len t (i_1 - 1)/\ (exists x_3, (uniq x_3/\ len x_3 i_1/\ ~list_mem t h/\ hd v x/\ tl v x_3))))))))))))))))%Z.
Proof.
  intros.
  destruct (list_uniq_any_len (i - 1)%Z) as (s & Hs). intuition.
  exists s. intuition.
  assert (~ emp v). { eapply list_len_not_zero_not_emp. intuition; eauto. lia. }
  destruct (list_destruct_non_emp v) as (h & t & Hh & Ht); auto.

  destruct (list_uniq_not_mem s) as (x & Hs); auto.
  exists x. intuition.
  destruct (classic (emp s)).
  - left. intuition.
    assert (~ emp v). { eapply list_len_not_zero_not_emp. intuition; eauto. lia. }
    destruct (list_destruct_non_emp v) as (h' & t' & Hh' & Ht'); auto.
  (* destruct (classic (uniq v /\ len v i)). *)
  (* 2: { destruct (list_uniq_any_len (i - 1)%Z) as (s & Hs). lia. exists s. intuition. *)
  (*      destruct (list_uniq_not_mem s) as (x & Hx); auto. exists x. intuition. } *)
  assert (~ emp v). { eapply list_len_not_zero_not_emp. intuition; eauto. lia. }
  destruct (list_destruct_non_emp v) as (h & t & Hh & Ht); auto.
  assert (uniq t). { eapply list_uniq_tl_uniq; intuition; eauto. }
  assert (not (list_mem t h)). { eapply list_uniq_hd_not_in_tl; intuition; eauto. }
  assert (len t (i - 1)%Z). { eapply list_len_tl_len; intuition; try z_simpl; eauto. }
  destruct (classic (i = 1%Z)); subst.
  - exists t. intuition. exists h. intuition.
    assert (emp t). { apply list_len_zero_is_emp. eapply list_len_tl_len; intuition; eauto. }
    left. intuition. exists t. intuition.
  - assert (~ emp t). { eapply list_len_not_zero_not_emp. intuition; eauto. lia. }
    destruct (list_destruct_non_emp t) as (h' & t' & Hh' & Ht'); auto.
    exists t. intuition.
    exists h. intuition.
    assert (not (h = h')). { eapply list_uniq_fst_second_not_eq; intuition; eauto. }
    right. exists h', t'. intuition.
    exists (i - 1)%Z. intuition.
    apply list_uniq_tl_uniq with (l := t); intuition; eauto.
    eapply list_len_tl_len; intuition; try z_simpl; eauto.
    exists t. intuition.
    (* eapply list_len_tl_len; intuition; try z_simpl; eauto. *)
    eapply list_uniq_hd_not_in_tl in H9; eauto.


Lemma sorted_union_2: (forall n, (0 <= n -> (forall v, ((sorted v/\ len v n) -> (exists s1, (sorted s1/\ lenlte s1 n/\ (exists s2, (sorted s2/\ lenlte s2 n/\ (exists h1, (exists t1, (hd s1 h1/\ tl s1 t1/\ (exists h2, (exists t2, (hd s2 h2/\ tl s2 t2/\ ~h1 = h2/\ h1 < h2/\ (exists n_1, (n_1 < n/\ 0 <= n_1/\ sorted t1/\ lenlte t1 n_1/\ (exists x_3, (sorted x_3/\ len x_3 n_1/\ sorted s2/\ lenlte s2 n_1/\ hd v h1/\ tl v x_3))))))))))))))))))%Z.
Proof.
  intros.
  assert (~ emp v). { eapply list_len_not_zero_not_emp. intuition; eauto. lia. }
  destruct (list_destruct_non_emp v) as (h & t & Hh & Ht); auto.
  assert (sorted t). { eapply list_sorted_tl_sorted; intuition; eauto. }
  assert (len t (n - 1)%Z). { eapply list_len_tl_len; intuition; try z_simpl; eauto. }
  destruct (list_singleton_list_ex h) as (s1 & t1 & Hs1).
  assert (sorted s1). { apply list_singleton_list_sorted; intuition. }
  exists s1. intuition. apply list_len_lenlte with 1%Z; intuition; eauto.
  exists t. intuition. apply list_len_lenlte with (n - 1)%Z; intuition; eauto.
  exists h, t1. intuition.
  assert (~ emp t). { eapply list_len_not_zero_not_emp. intuition; eauto. lia. }
  destruct (list_destruct_non_emp t) as (h2 & t2 & Hh2 & Ht2); auto.
  assert (h < h2)%Z. { eapply list_sorted_fst_second_lt; intuition; eauto. }
  exists h2, t2. intuition.
  exists (n - 1)%Z. intuition. apply list_sorted_tl_sorted with s1; intuition; eauto.
  apply list_len_lenlte with 0%Z; intuition; eauto.
  exists t. intuition.
  eapply list_len_lenlte; intuition; eauto.
Qed.

(* Lemma sorted_union: (forall n, (2 <= n -> (forall i, ((0 <= i/\ i <= 1) -> (forall v, ((sorted v/\ len v n) -> (exists s1, (sorted s1/\ len s1 i/\ (exists s2, (sorted s2/\ len s2 (n - i)/\ (exists h1, (exists t1, (hd s1 h1/\ tl s1 t1/\ (exists h2, (exists t2, (hd s2 h2/\ tl s2 t2/\ ~h1 = h2/\ h1 < h2/\ (exists n_1, (n_1 < n/\ 2 <= n_1/\ (exists i_1, (0 <= i_1/\ i_1 <= 1/\ sorted t1/\ len t1 i_1/\ (exists x_3, (sorted x_3/\ len x_3 n_1/\ sorted s2/\ len s2 (n_1 - i_1)/\ hd v h1/\ tl v x_3))))))))))))))))))))))%Z. *)
(* Proof. *)
(*   intros. *)
(*   assert (~ emp v). { eapply list_len_not_zero_not_emp. intuition; eauto. lia. } *)
(*   destruct (list_destruct_non_emp v) as (h & t & Hh & Ht); auto. *)
(*   assert (sorted t). { eapply list_sorted_tl_sorted; intuition; eauto. } *)
(*   assert (len t (n - 1)%Z). { eapply list_len_tl_len; intuition; try z_simpl; eauto. } *)
(*   assert (i = 0 \/ i = 1)%Z. { lia. } destruct H5; subst. *)
(*   - admit. *)
(*   - destruct (list_singleton_list_ex h) as (s1 & t1 & Hs1). *)
(*     assert (sorted s1). { apply list_singleton_list_sorted; intuition. } *)
(*     exists s1. intuition. *)
(*     exists t. intuition. *)
(*     exists h, t1. intuition. *)
(*     assert (~ emp t). { eapply list_len_not_zero_not_emp. intuition; eauto. lia. } *)
(*     destruct (list_destruct_non_emp t) as (h2 & t2 & Hh2 & Ht2); auto. *)
(*     assert (h < h2)%Z. { eapply list_sorted_fst_second_lt; intuition; eauto. } *)
(*     exists h2, t2. intuition. *)
(*     exists (n - 1)%Z. intuition. apply list_sorted_tl_sorted with s1; intuition; eauto. *)
(*   apply list_len_lenlte with 0%Z; intuition; eauto. *)
(*   exists t. intuition. *)
(*   eapply list_len_lenlte; intuition; eauto. *)
(* Qed. *)


Lemma inlined_sorted_union: (forall i, (forall v, (forall l_0, (forall l'_0, (forall h_0, (forall t_0, ((exists l_1, (exists l'_1, (len l_1 1/\ hd l_1 h_0/\ tl l_1 l'_1/\ len l'_1 0))) -> (((exists h_1, (exists t_1, (~emp t_0 -> (hd t_0 h_1/\ tl t_0 t_1))))/\ (exists h_2, (exists t_2, (~emp l'_0 -> (hd l'_0 h_2/\ tl l'_0 t_2))))/\ (exists h_3, (exists t_3, (~emp l_0 -> (hd l_0 h_3/\ tl l_0 t_3))))) -> ((len l_0 1/\ hd l_0 i/\ tl l_0 l'_0/\ len l'_0 0) -> ((~emp v -> (hd v h_0/\ tl v t_0)) -> (2 <= i -> ((sorted v/\ len v i) -> (exists s1, (sorted s1/\ lenlte s1 i/\ (exists s2, (sorted s2/\ lenlte s2 i/\ (exists h1, (exists t1, (hd s1 h1/\ tl s1 t1/\ (exists h2, (exists t2, (hd s2 h2/\ tl s2 t2/\ ~h1 = h2/\ h1 < h2/\ (exists i_1, (i_1 < i/\ 1 <= i_1/\ sorted t1/\ lenlte t1 i_1/\ (exists x_3, (sorted x_3/\ len x_3 i_1/\ sorted s2/\ lenlte s2 i_1/\ hd v h1/\ tl v x_3))))))))))))))))))))))))))%Z.
Proof.
  intros.
  assert (~ emp v). { eapply list_len_not_zero_not_emp. intuition; eauto. lia. }
  intuition.
  assert (sorted t_0). { eapply list_sorted_tl_sorted; intuition; eauto. }
  assert (len t_0 (i - 1)%Z). { eapply list_len_tl_len; intuition; try z_simpl; eauto. }
  destruct H as (s1 & t1 & Hs1).
  assert (sorted s1). { apply list_singleton_list_sorted; intuition. }
  exists s1. intuition. apply list_len_lenlte with 1%Z; intuition; eauto.
  exists t_0. intuition. apply list_len_lenlte with (i - 1)%Z; intuition; eauto.
  exists h_0, t1. intuition.
  assert (~ emp t_0). { eapply list_len_not_zero_not_emp. intuition; eauto. lia. }
  destruct H6 as (h2 & t2 & Hh2 & Ht2); auto.
  assert (h_0 < h2)%Z. { apply list_sorted_fst_second_lt with (l := v) (t := t_0); intuition; eauto. }
  exists h2, t2. intuition.
  exists (i - 1)%Z. intuition. apply list_sorted_tl_sorted with s1; intuition; eauto.
  apply list_len_lenlte with 0%Z; intuition; eauto.
  exists t_0. intuition.
  eapply list_len_lenlte; intuition; eauto.
Qed.

Lemma sorted_union: (forall i, (2 <= i -> (forall v, ((sorted v/\ len v i) -> (exists s1, (sorted s1/\ lenlte s1 i/\ (exists s2, (sorted s2/\ lenlte s2 i/\ (exists h1, (exists t1, (hd s1 h1/\ tl s1 t1/\ (exists h2, (exists t2, (hd s2 h2/\ tl s2 t2/\ ~h1 = h2/\ h1 < h2/\ (exists i_1, (i_1 < i/\ 1 <= i_1/\ sorted t1/\ lenlte t1 i_1/\ (exists x_3, (sorted x_3/\ len x_3 i_1/\ sorted s2/\ lenlte s2 i_1/\ hd v h1/\ tl v x_3))))))))))))))))))%Z.
Proof.
  intros.
  assert (~ emp v). { eapply list_len_not_zero_not_emp. intuition; eauto. lia. }
  destruct (list_destruct_non_emp v) as (h & t & Hh & Ht); auto.
  assert (sorted t). { eapply list_sorted_tl_sorted; intuition; eauto. }
  assert (len t (i - 1)%Z). { eapply list_len_tl_len; intuition; try z_simpl; eauto. }
  destruct (list_singleton_list_ex h) as (s1 & t1 & Hs1).
  assert (sorted s1). { apply list_singleton_list_sorted; intuition. }
  exists s1. intuition. apply list_len_lenlte with 1%Z; intuition; eauto.
  exists t. intuition. apply list_len_lenlte with (i - 1)%Z; intuition; eauto.
  exists h, t1. intuition.
  assert (~ emp t). { eapply list_len_not_zero_not_emp. intuition; eauto. lia. }
  destruct (list_destruct_non_emp t) as (h2 & t2 & Hh2 & Ht2); auto.
  assert (h < h2)%Z. { eapply list_sorted_fst_second_lt; intuition; eauto. }
  exists h2, t2. intuition.
  exists (i - 1)%Z. intuition. apply list_sorted_tl_sorted with s1; intuition; eauto.
  apply list_len_lenlte with 0%Z; intuition; eauto.
  exists t. intuition.
  eapply list_len_lenlte; intuition; eauto.
Qed.



Lemma uniq_query3: (forall i, (0 < i -> (forall v, ((uniq v/\ len v i) -> (exists s, (uniq s/\ len s (i - 1)/\ (exists x, (~list_mem s x/\ ((emp s/\ (exists x_0, (emp x_0/\ hd v x/\ tl v x_0))) \/ (exists h, (exists t, (hd s h/\ tl s t/\ ~x = h/\ (exists i_1, (i_1 < i/\ 0 < i_1/\ uniq t/\ len t (i_1 - 1)/\ (exists x_3, (uniq x_3/\ len x_3 i_1/\ ~list_mem t h/\ hd v x/\ tl v x_3))))))))))))))))%Z.
  Proof.
  intros.
  (* destruct (classic (uniq v /\ len v i)). *)
  (* 2: { destruct (list_uniq_any_len (i - 1)%Z) as (s & Hs). lia. exists s. intuition. *)
  (*      destruct (list_uniq_not_mem s) as (x & Hx); auto. exists x. intuition. } *)
  assert (~ emp v). { eapply list_len_not_zero_not_emp. intuition; eauto. lia. }
  destruct (list_destruct_non_emp v) as (h & t & Hh & Ht); auto.
  assert (uniq t). { eapply list_uniq_tl_uniq; intuition; eauto. }
  assert (not (list_mem t h)). { eapply list_uniq_hd_not_in_tl; intuition; eauto. }
  assert (len t (i - 1)%Z). { eapply list_len_tl_len; intuition; try z_simpl; eauto. }
  destruct (classic (i = 1%Z)); subst.
  - exists t. intuition. exists h. intuition.
    assert (emp t). { apply list_len_zero_is_emp. eapply list_len_tl_len; intuition; eauto. }
    left. intuition. exists t. intuition.
  - assert (~ emp t). { eapply list_len_not_zero_not_emp. intuition; eauto. lia. }
    destruct (list_destruct_non_emp t) as (h' & t' & Hh' & Ht'); auto.
    exists t. intuition.
    exists h. intuition.
    assert (not (h = h')). { eapply list_uniq_fst_second_not_eq; intuition; eauto. }
    right. exists h', t'. intuition.
    exists (i - 1)%Z. intuition.
    apply list_uniq_tl_uniq with (l := t); intuition; eauto.
    eapply list_len_tl_len; intuition; try z_simpl; eauto.
    exists t. intuition.
    (* eapply list_len_tl_len; intuition; try z_simpl; eauto. *)
    eapply list_uniq_hd_not_in_tl in H9; eauto.
Qed.

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
