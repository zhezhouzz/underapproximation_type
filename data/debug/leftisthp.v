From Coq Require Import Classes.DecidableClass.
From Coq Require Import Lia.
From Coq Require Import ZArith.

Variable IT: Type.
Variable leftisthp_depth: IT -> Z -> Prop.
Variable leftisthp_leaf: IT -> Prop.
Variable leftisthp_root: IT -> Z -> Prop.
Variable leftisthp_rank: IT -> Z -> Prop.
Variable leftisthp_lch: IT -> IT -> Prop.
Variable leftisthp_rch: IT -> IT -> Prop.

Definition ch (tr: IT) (tr': IT) := leftisthp_lch tr tr' \/ leftisthp_rch tr tr'.

Lemma leftisthp_leftisthp_depth_0_leftisthp_leaf (l : IT) :(leftisthp_depth l 0) -> (leftisthp_leaf l) .
Admitted.
#[export] Hint Resolve leftisthp_leftisthp_depth_0_leftisthp_leaf : core.

(* Lemma leftisthp_leftisthp_leaf_leftisthp_depth_0 (l : IT) (n: Z) :(leftisthp_depth l n) -> (leftisthp_leaf l) -> (n = 0)%Z . *)
(* Admitted. *)
(* #[export] Hint Resolve leftisthp_leftisthp_leaf_leftisthp_depth_0 : core. *)

Lemma leftisthp_leftisthp_depth_not_0_not_leftisthp_leaf (l : IT) (n: Z) : not (n = 0)%Z -> (leftisthp_depth l n) -> not (leftisthp_leaf l).
Admitted.
#[export] Hint Resolve leftisthp_leftisthp_depth_not_0_not_leftisthp_leaf : core.

(* Lemma decide_leftisthp_leaf: forall v, leftisthp_leaf v \/ not (leftisthp_leaf v). *)
(* Admitted. *)

Lemma leftisthp_not_leftisthp_leaf_ex (l : IT): not (leftisthp_leaf l) -> exists r x l1 l2, leftisthp_root l x /\ leftisthp_lch l l1 /\ leftisthp_rch l l2 /\ leftisthp_rank l r /\ leftisthp_depth l2 (r - 1)%Z /\ (r > 0)%Z.
Admitted.

Lemma leftisthp_leftisthp_depth_ch_leftisthp_depth_minus_1 (tr tr1: IT) (n: Z):
  leftisthp_lch tr tr1 -> ((leftisthp_depth tr (n + 1)%Z) <-> (leftisthp_depth tr1 n)%Z).
Admitted.
#[export] Hint Resolve leftisthp_leftisthp_depth_ch_leftisthp_depth_minus_1 : core.

(* Lemma leftisthp_leftisthp_depth_lt_ex (l: IT) (l1: IT) (d: Z): leftisthp_lch l l1 -> leftisthp_depth l d -> exists d1, leftisthp_depth l1 d1. *)
(* Admitted. *)

(* Lemma leftisthp_leftisthp_depth_rt_ex (l: IT) (l1: IT) (d: Z): leftisthp_rch l l1 -> leftisthp_depth l d -> exists d1, leftisthp_depth l1 d1. *)
(* Admitted. *)

(* Lemma leftisthp_leftisthp_depth_lt_minus_1 (l: IT) (l1: IT) (d: Z) (d1: Z): leftisthp_lch l l1 -> leftisthp_depth l d -> leftisthp_depth l1 d1 -> (d1 < d)%Z. *)
(* Admitted. *)
(* #[export] Hint Resolve leftisthp_leftisthp_depth_lt_minus_1 : core. *)

(* Lemma leftisthp_leftisthp_depth_rt_minus_1 (l: IT) (l1: IT) (d: Z) (d1: Z): leftisthp_rch l l1 -> leftisthp_depth l d -> leftisthp_depth l1 d1 -> (d1 < d)%Z. *)
(* Admitted. *)
(* #[export] Hint Resolve leftisthp_leftisthp_depth_rt_minus_1 : core. *)

Lemma leftisthp_right_depth_leq_depth (l : IT) (n : Z) (r: Z): (leftisthp_depth l n) -> (leftisthp_rank l r) -> (r <= n)%Z.
Admitted.
#[export] Hint Resolve leftisthp_right_depth_leq_depth : core.

Lemma leftisthp_gen: (forall s, (0 <= s -> (forall v, (leftisthp_depth v s -> ((s = 0 /\ leftisthp_leaf v) \/ (~s = 0 /\ (exists s_1, (0 <= s_1 /\ s_1 < s /\ s_1 = (s - 1) /\ (exists lt, (leftisthp_depth lt s_1 /\ (exists b_2, (0 <= b_2 /\ b_2 = (s - 1) /\ (exists s2, (0 <= s2 /\ s2 <= b_2 /\ (exists s_2, (0 <= s_2 /\ s_2 < s /\ s_2 = s2 /\ (exists rt, (leftisthp_depth rt s_2 /\ (exists x_2, (leftisthp_rank v (s2 + 1) /\ (leftisthp_root v x_2 /\ (leftisthp_lch v lt /\ leftisthp_rch v rt))))))))))))))))))))))%Z.
Proof.
  intros. destruct (Z.eqb_spec s 0)%Z.
  - left. subst. intuition.
  - right. intuition.
    destruct (leftisthp_not_leftisthp_leaf_ex v) as (r & x & lt & rt & H1 & H2 & H3 & H4 & H5); eauto.
    exists (s - 1)%Z. intuition.
    exists lt. intuition. eapply leftisthp_leftisthp_depth_ch_leftisthp_depth_minus_1 in H2; eauto.
    erewrite <- H2. assert (s - 1 + 1 = s)%Z. lia. rewrite H5. auto.
    exists (s - 1)%Z. intuition.
    exists (r - 1)%Z. intuition; eauto.
    eapply leftisthp_right_depth_leq_depth in H4; eauto. lia.
    exists (r - 1)%Z. intuition; eauto.
    eapply leftisthp_right_depth_leq_depth in H4; eauto. lia.
    exists rt. intuition.
    exists x. intuition.
    assert (r - 1 + 1 = r)%Z. lia. rewrite H5. auto.
Qed.

      assert (dd <= d)%Z as Hddd; eauto.
      destruct (leftisthp_not_leftisthp_leaf_ex v) as (x & lt & rt & Hx & Hlt & Hrt); auto.
      assert (lo < x /\ x < hi)%Z. { eauto; lia. }
      intuition.
      exists hi. intuition.
      exists x; eauto. intuition.
      exists (d - 1)%Z. intuition.
      exists x; eauto. intuition.
      exists lt. intuition; eauto. apply H2. eauto.
      eapply leftisthp_leftisthp_depth_lt_minus_1 in H5; eauto. lia.
      exists (d - 1)%Z. intuition.
      exists hi; eauto. intuition.
      exists rt. intuition; eauto. apply H2; eauto.
      eapply leftisthp_leftisthp_depth_rt_minus_1 in H5; eauto. lia.
Qed.

Lemma complete_leftisthp_gen: (forall s, (0 <= s -> (forall v, ((leftisthp_depth v s /\ complete v) -> ((s = 0 /\ leftisthp_leaf v) \/ (~s = 0 /\ (exists s_1, (0 <= s_1 /\ s_1 < s /\ s_1 = (s - 1) /\ (exists lt, (leftisthp_depth lt s_1 /\ complete lt /\ (exists s_2, (0 <= s_2 /\ s_2 < s /\ s_2 = (s - 1) /\ (exists rt, (leftisthp_depth rt s_2 /\ complete rt /\ (exists n, (leftisthp_root v n /\ (leftisthp_lch v lt /\ leftisthp_rch v rt)))))))))))))))))%Z.
Proof.
  intros. destruct (Z.eqb_spec s 0)%Z.
  - left. subst. intuition.
  - right. intuition.
    assert (~ leftisthp_leaf v); eauto.
    destruct (leftisthp_not_leftisthp_leaf_ex v) as (x & lt & rt & Hx & Hlt & Hrt); auto.
    exists (s - 1)%Z. intuition.
    exists lt. intuition; eauto.
    exists (s - 1)%Z. intuition.
    exists rt. intuition; eauto.
Qed.

Lemma leftisthp_depth_heap_gen:(
                        forall d, (0 <= d -> (forall mx, (forall v, ((heap v /\ ((exists u, (leftisthp_depth v u /\ u <= d)) /\ (forall u, (leftisthp_root v u -> u < mx)))) -> ((d = 0 /\ leftisthp_leaf v) \/ (~d = 0 /\ (exists x_1, ((x_1 /\ leftisthp_leaf v) \/ (~x_1 /\ (exists n, ((n < mx /\ (exists d_1, ((0 <= d_1 /\ d_1 = (d - 1)) /\ (exists lt, (((d_1 < d /\ d_1 >= 0) /\ heap lt /\ ((exists u, (leftisthp_depth lt u /\ u <= d_1)) /\ (forall u, (leftisthp_root lt u -> u < n)))) /\ (exists d_2, ((0 <= d_2 /\ d_2 = (d - 1)) /\ (exists rt, (((d_2 < d /\ d_2 >= 0) /\ heap rt /\ ((exists u, (leftisthp_depth rt u /\ u <= d_2)) /\ (forall u, (leftisthp_root rt u -> u < n)))) /\ leftisthp_root v n /\ (leftisthp_lch v lt /\ leftisthp_rch v rt)))))))))) \/ (~n < mx /\ leftisthp_leaf v)))))))))))))%Z.
Proof.
  intros. destruct (Z.eqb_spec d 0)%Z.
  - left. subst. intuition. destruct H0 as (s & Hs & Hs'). apply leftisthp_leftisthp_depth_0_leftisthp_leaf. admit.
  - right. intuition. destruct H0 as (s & Hs & Hs').
    destruct (decide_leftisthp_leaf v).
    + exists True. intuition.
    + exists False. right. intuition.
      destruct (leftisthp_not_leftisthp_leaf_ex v) as (x & lt & rt & Hx & Hlt & Hrt); auto.
      exists x; eauto. left. intuition.
      exists (d - 1)%Z. intuition.
      exists lt. intuition; eauto. eapply leftisthp_leftisthp_depth_lt_ex in Hlt; eauto. destruct Hlt as (d1 & Hd1 & Hd1s). exists d1.
      intuition.
      exists (d - 1)%Z. intuition.
      exists rt. intuition; eauto. eapply leftisthp_leftisthp_depth_rt_ex in Hrt; eauto. destruct Hrt as (d1 & Hd1 & Hd1s). exists d1.
      intuition.
Qed.


Lemma ranged_set_gen:
  (forall diff, (0 <= diff -> (forall lo, (forall v, (((forall u, (leftisthp_mem v u -> (lo < u /\ u < (lo + diff)))) /\ bst v) -> (((lo + diff) <= (1 + lo) /\ leftisthp_leaf v) \/ (~(lo + diff) <= (1 + lo) /\ (exists x_2, ((x_2 /\ leftisthp_leaf v) \/ (~x_2 /\ (exists b_2, (((1 + lo) < b_2 /\ b_2 = (lo + diff)) /\ (exists x, ((lo < x /\ x < b_2) /\ (exists diff_1, ((0 <= diff_1 /\ diff_1 = (x - lo)) /\ (exists hi_0, ((hi_0 = (lo + diff_1) /\ hi_0 = x) /\ (exists lt, (((diff_1 < diff /\ diff_1 >= 0) /\ (forall u, (leftisthp_mem lt u -> (lo < u /\ u < hi_0))) /\ bst lt) /\ (exists diff_2, ((0 <= diff_2 /\ diff_2 = ((lo + diff) - x)) /\ (exists hi_1, ((hi_1 = (x + diff_2) /\ hi_1 = (lo + diff)) /\ (exists rt, (((diff_2 < diff /\ diff_2 >= 0) /\ (forall u, (leftisthp_mem rt u -> (x < u /\ u < hi_1))) /\ bst rt) /\ leftisthp_root v x /\ (leftisthp_lch v lt /\ leftisthp_rch v rt)))))))))))))))))))))))))))%Z.
Proof.
  intros. destruct (Z.leb_spec diff 1)%Z.
  - left. intuition.
    destruct (decide_leftisthp_leaf v); intuition.
    destruct (leftisthp_not_leftisthp_leaf_ex v) as (x & lt & rt & Hx & Hlt & Hrt); auto.
    apply leftisthp_leftisthp_root_mem in Hx. apply H2 in Hx. lia.
  - right. intuition.
    destruct (decide_leftisthp_leaf v).
    + exists True. intuition.
    + exists False. right. intuition.
      destruct (leftisthp_not_leftisthp_leaf_ex v) as (x & lt & rt & Hx & Hlt & Hrt); auto.
      exists (lo + diff)%Z. intuition.
      exists x. intuition.
      { apply leftisthp_leftisthp_root_mem in Hx. apply H2 in Hx. lia. }
      { apply leftisthp_leftisthp_root_mem in Hx. apply H2 in Hx. lia. }
      exists (x - lo)%Z. intuition.
      { apply leftisthp_leftisthp_root_mem in Hx. apply H2 in Hx. lia. }
      exists x. intuition.
      exists lt. intuition; eauto.
      { apply leftisthp_leftisthp_root_mem in Hx. apply H2 in Hx. lia. }
      { apply leftisthp_leftisthp_root_mem in Hx. apply H2 in Hx. lia. }
      { eapply leftisthp_mem_leftisthp_lch_mem in H4; eauto. apply H2 in H4. lia. }
      exists (lo + diff - x)%Z. intuition.
      { apply leftisthp_leftisthp_root_mem in Hx. apply H2 in Hx. lia. }
      exists (x + (lo + diff - x))%Z. intuition.
      exists rt. intuition; eauto.
      { apply leftisthp_leftisthp_root_mem in Hx. apply H2 in Hx. lia. }
      { apply leftisthp_leftisthp_root_mem in Hx. apply H2 in Hx. lia. }
      { eapply leftisthp_mem_leftisthp_rch_mem in H4; eauto. apply H2 in H4. lia. }
Qed.


Lemma leftisthp_depth_leftisthp_gen:
(forall s, (0 <= s -> (forall v, ((exists u, (leftisthp_depth v u/\ u <= s)) -> ((s = 0/\ leftisthp_leaf v) \/ (~s = 0/\ (exists x_1, ((x_1/\ leftisthp_leaf v) \/ (~x_1/\ (exists s_1, ((0 <= s_1/\ s_1 = (s - 1))/\ (exists lt, (((s_1 < s/\ s_1 >= 0)/\ (exists u, (leftisthp_depth lt u/\ u <= s_1)))/\ (exists s_2, ((0 <= s_2/\ s_2 = (s - 1))/\ (exists rt, (((s_2 < s/\ s_2 >= 0)/\ (exists u, (leftisthp_depth rt u/\ u <= s_2)))/\ (exists n, (leftisthp_root v n/\ (leftisthp_lch v lt/\ leftisthp_rch v rt))))))))))))))))))))%Z.
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
