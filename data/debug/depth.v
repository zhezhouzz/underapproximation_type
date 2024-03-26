From Coq Require Import Classes.DecidableClass.
From Coq Require Import Lia.
From Coq Require Import ZArith.

Variable IT: Type.
Variable depth: IT -> Z -> Prop.
Variable leaf: IT -> Prop.
Variable root: IT -> Z -> Prop.
Variable lch: IT -> IT -> Prop.
Variable rch: IT -> IT -> Prop.
Variable tree_mem: IT -> Z -> Prop.
Variable bst: IT -> Prop.
Variable heap: IT -> Prop.
Variable complete: IT -> Prop.

Definition ch (tr: IT) (tr': IT) := lch tr tr' \/ rch tr tr'.

Lemma tree_depth_0_leaf (l : IT) :(depth l 0) -> (leaf l) .
Admitted.
#[export] Hint Resolve tree_depth_0_leaf : core.

Lemma tree_leaf_depth_0 (l : IT) (n: Z) :(depth l n) -> (leaf l) -> (n = 0)%Z .
Admitted.
#[export] Hint Resolve tree_leaf_depth_0 : core.

Lemma tree_depth_not_0_not_leaf (l : IT) (n: Z) : not (n = 0)%Z -> (depth l n) -> not (leaf l).
Admitted.
#[export] Hint Resolve tree_depth_not_0_not_leaf : core.

Lemma decide_leaf: forall v, leaf v \/ not (leaf v).
Admitted.

Lemma tree_not_leaf_ex (l : IT): not (leaf l) -> exists x l1 l2, root l x /\ lch l l1 /\ rch l l2.
Admitted.

Lemma tree_root_mem (l : IT) (x: Z) : root l x -> tree_mem l x.
Admitted.
#[export] Hint Resolve tree_root_mem : core.

Lemma tree_leaf_bst (l : IT) : leaf l -> bst l.
Admitted.
#[export] Hint Resolve tree_leaf_bst : core.

Lemma tree_bst_lch_bst (l : IT) (l1 : IT): lch l l1 -> bst l -> bst l1.
Admitted.
#[export] Hint Resolve tree_bst_lch_bst : core.

Lemma tree_bst_rch_bst (l : IT) (l1 : IT): rch l l1 -> bst l -> bst l1.
Admitted.
#[export] Hint Resolve tree_bst_rch_bst : core.

Lemma tree_mem_lch_mem (l : IT) (l1 : IT) (x: Z): lch l l1 -> tree_mem l1 x -> tree_mem l x.
Admitted.
#[export] Hint Resolve tree_mem_lch_mem : core.

Lemma tree_mem_rch_mem (l : IT) (l1 : IT) (x: Z): rch l l1 -> tree_mem l1 x -> tree_mem l x.
Admitted.
#[export] Hint Resolve tree_mem_rch_mem : core.

Lemma tree_bst_lch_mem_lt_root (l : IT) (l1 : IT) (x: Z) (y: Z): bst l -> lch l l1 -> root l x -> tree_mem l1 y -> (y < x)%Z.
Admitted.
#[export] Hint Resolve tree_bst_lch_mem_lt_root : core.

Lemma tree_bst_rch_mem_gt_root (l : IT) (l1 : IT) (x: Z) (y: Z): bst l -> rch l l1 -> root l x -> tree_mem l1 y -> (x < y)%Z.
Admitted.
#[export] Hint Resolve tree_bst_rch_mem_gt_root : core.


Lemma tree_depth_ch_depth_minus_1 (tr tr1 tr2: IT) (n n1 n2: Z):
  lch tr tr1 -> rch tr tr2 -> depth tr n -> depth tr1 n1 -> depth tr2 n2 ->
  ((n1 > n2 /\ n = n1 + 1) \/ (n1 <= n2 /\ n = n2 + 1))%Z.
Admitted.
#[export] Hint Resolve tree_depth_ch_depth_minus_1 : core.

Lemma tree_depth_lt_ex (l: IT) (l1: IT) (d: Z): lch l l1 -> depth l d -> exists d1, depth l1 d1.
Admitted.

Lemma tree_depth_rt_ex (l: IT) (l1: IT) (d: Z): rch l l1 -> depth l d -> exists d1, depth l1 d1.
Admitted.

Lemma tree_depth_lt_minus_1 (l: IT) (l1: IT) (d: Z) (d1: Z): lch l l1 -> depth l d -> depth l1 d1 -> (d1 < d)%Z.
Admitted.
#[export] Hint Resolve tree_depth_lt_minus_1 : core.

Lemma tree_depth_rt_minus_1 (l: IT) (l1: IT) (d: Z) (d1: Z): rch l l1 -> depth l d -> depth l1 d1 -> (d1 < d)%Z.
Admitted.
#[export] Hint Resolve tree_depth_rt_minus_1 : core.

(* heap *)

Lemma tree_heap_lch_heap (l : IT) (l1 : IT): lch l l1 -> heap l -> heap l1.
Admitted.
#[export] Hint Resolve tree_heap_lch_heap : core.

Lemma tree_heap_rch_heap (l : IT) (l1 : IT): rch l l1 -> heap l -> heap l1.
Admitted.
#[export] Hint Resolve tree_heap_rch_heap : core.

Lemma tree_heap_root_lt_lch_root (l : IT) (l1 : IT) (x: Z) (y: Z): heap l -> lch l l1 -> root l x -> root l1 y -> (y < x)%Z.
Admitted.
#[export] Hint Resolve tree_heap_root_lt_lch_root: core.

Lemma tree_heap_root_rt_rch_root (l : IT) (l1 : IT) (x: Z) (y: Z): heap l -> rch l l1 -> root l x -> root l1 y -> (y < x)%Z.
Admitted.
#[export] Hint Resolve tree_heap_root_rt_rch_root: core.

(* complete *)

Lemma tree_complete_lch_complete (l : IT) (l1 : IT): lch l l1 -> complete l -> complete l1.
Admitted.
#[export] Hint Resolve tree_complete_lch_complete : core.

Lemma tree_complete_rch_complete (l : IT) (l1 : IT): rch l l1 -> complete l -> complete l1.
Admitted.
#[export] Hint Resolve tree_complete_rch_complete : core.

Lemma tree_complete_lch_depth_minus_1 (l : IT) (l1 : IT) (n: Z): lch l l1 -> complete l -> depth l n -> depth l1 (n - 1)%Z.
Admitted.
#[export] Hint Resolve tree_complete_lch_depth_minus_1 : core.

Lemma tree_complete_rch_depth_minus_1 (l : IT) (l1 : IT) (n: Z): rch l l1 -> complete l -> depth l n -> depth l1 (n - 1)%Z.
Admitted.
#[export] Hint Resolve tree_complete_rch_depth_minus_1 : core.

(** bst *)

Lemma tree_bst_depth_ex (l: IT): bst l -> exists n, depth l n.
Admitted.

(* abduction *)

Lemma depth_heap_abduction: (forall d, (0 <= d -> (forall mx, (forall v, ((heap v /\ ((exists u, (depth v u /\ u <= d)) /\ (forall u, (root v u -> u < mx)))) -> ((~leaf v /\ ~mx = 0 /\ d = 0) \/ (~leaf v /\ ~mx = 0 /\ ~d = 0) \/ (leaf v /\ ~mx = 0 /\ ~d = 0) \/ (leaf v /\ mx = 0 /\ ~d = 0) \/ (d = 0 /\ leaf v) \/ (~d = 0 /\ (exists x_1, (~x_1 /\ (exists n, (n < mx /\ (exists d_1, (0 <= d_1 /\ d_1 < d /\ d_1 = (d - 1) /\ (exists lt, (heap lt /\ (exists u, (depth lt u /\ u <= d_1)) /\ (forall u, (root lt u -> u < n)) /\ (exists d_2, (0 <= d_2 /\ d_2 < d /\ d_2 = (d - 1) /\ (exists rt, (heap rt /\ (exists u, (depth rt u /\ u <= d_2)) /\ (forall u, (root rt u -> u < n)) /\ root v n /\ lch v lt /\ rch v rt)))))))))))))))))))%Z.
Proof.
  intros. destruct (Z.eqb_spec d 0)%Z.
  - do 4 right. left. subst. intuition. destruct H0 as (s & Hs & Hs'). eauto. admit.
  - destruct H0 as (Hv & (s & Hs & Hs') & HH).
    destruct (decide_leaf v).
    + do 2 right.
      destruct (Z.eqb mx 0)%Z eqn: Hz; intuition.
    + right.
      destruct (Z.eqb mx 0)%Z eqn: Hz; auto.
      2:{ intuition. }
      do 4 right. intuition.
      exists False. intuition.
      destruct (tree_not_leaf_ex v) as (x & lt & rt & Hx & Hlt & Hrt); auto.
      exists x; eauto. intuition.
      exists (d - 1)%Z. intuition.
      exists lt. intuition; eauto. edestruct (tree_depth_lt_ex) as (d1 & Hd1); eauto. exists d1. intuition; eauto.
      eapply tree_depth_lt_minus_1 in Hlt; eauto. lia.
      exists (d - 1)%Z. intuition.
      exists rt. intuition; eauto. edestruct (tree_depth_rt_ex) as (d1 & Hd1); eauto. exists d1. intuition; eauto.
      eapply tree_depth_rt_minus_1 in Hrt; eauto. lia.
Admitted.



Lemma sized_bst_gen: (
                       forall d, (0 <= d -> (forall lo, (forall hi, (lo < hi -> (forall v, (((forall u, (tree_mem v u -> (lo < u /\ u < hi))) /\ (bst v /\ (forall n, (depth v n -> n <= d)))) -> ((d = 0 /\ leaf v) \/ (~d = 0 /\ (exists x_1, ((x_1 /\ leaf v) \/ (~x_1 /\ (lo + 1) < hi /\ (exists b_3, ((1 + lo) < b_3 /\ b_3 = hi /\ (exists x, (lo < x /\ x < b_3 /\ (exists d_1, (0 <= d_1 /\ d_1 < d /\ d_1 = (d - 1) /\ (exists hi_0, (lo < hi_0 /\ hi_0 = x /\ (exists lt, ((forall u, (tree_mem lt u -> (lo < u /\ u < hi_0))) /\ bst lt /\ (forall n, (depth lt n -> n <= d_1)) /\ (exists d_2, (0 <= d_2 /\ d_2 < d /\ d_2 = (d - 1) /\ (exists hi_1, (x < hi_1 /\ hi_1 = hi /\ (exists rt, ((forall u, (tree_mem rt u -> (x < u /\ u < hi_1))) /\ bst rt /\ (forall n, (depth rt n -> n <= d_2)) /\ root v x /\ lch v lt /\ rch v rt))))))))))))))))))))))))))))%Z.
Proof.
  intros. destruct (Z.eqb_spec d 0)%Z.
  - left. subst. intuition. destruct (tree_bst_depth_ex v) as (d & Hd); eauto. admit.
  - right. intuition.
    destruct (decide_leaf v).
    + exists True. intuition.
    + exists False. right.
      destruct (tree_bst_depth_ex v) as (dd & Hdd); eauto.
      assert (dd <= d)%Z as Hddd; eauto.
      destruct (tree_not_leaf_ex v) as (x & lt & rt & Hx & Hlt & Hrt); auto.
      assert (lo < x /\ x < hi)%Z. { eauto; lia. }
      intuition.
      exists hi. intuition.
      exists x; eauto. intuition.
      exists (d - 1)%Z. intuition.
      exists x; eauto. intuition.
      exists lt. intuition; eauto. apply H2. eauto.
      eapply tree_depth_lt_minus_1 in H5; eauto. lia.
      exists (d - 1)%Z. intuition.
      exists hi; eauto. intuition.
      exists rt. intuition; eauto. apply H2; eauto.
      eapply tree_depth_rt_minus_1 in H5; eauto. lia.
Qed.

Lemma complete_tree_gen: (forall s, (0 <= s -> (forall v, ((depth v s /\ complete v) -> ((s = 0 /\ leaf v) \/ (~s = 0 /\ (exists s_1, (0 <= s_1 /\ s_1 < s /\ s_1 = (s - 1) /\ (exists lt, (depth lt s_1 /\ complete lt /\ (exists s_2, (0 <= s_2 /\ s_2 < s /\ s_2 = (s - 1) /\ (exists rt, (depth rt s_2 /\ complete rt /\ (exists n, (root v n /\ (lch v lt /\ rch v rt)))))))))))))))))%Z.
Proof.
  intros. destruct (Z.eqb_spec s 0)%Z.
  - left. subst. intuition.
  - right. intuition.
    assert (~ leaf v); eauto.
    destruct (tree_not_leaf_ex v) as (x & lt & rt & Hx & Hlt & Hrt); auto.
    exists (s - 1)%Z. intuition.
    exists lt. intuition; eauto.
    exists (s - 1)%Z. intuition.
    exists rt. intuition; eauto.
Qed.

Lemma depth_heap_gen:(
                        forall d, (0 <= d -> (forall mx, (forall v, ((heap v /\ ((exists u, (depth v u /\ u <= d)) /\ (forall u, (root v u -> u < mx)))) -> ((d = 0 /\ leaf v) \/ (~d = 0 /\ (exists x_1, ((x_1 /\ leaf v) \/ (~x_1 /\ (exists n, ((n < mx /\ (exists d_1, ((0 <= d_1 /\ d_1 = (d - 1)) /\ (exists lt, (((d_1 < d /\ d_1 >= 0) /\ heap lt /\ ((exists u, (depth lt u /\ u <= d_1)) /\ (forall u, (root lt u -> u < n)))) /\ (exists d_2, ((0 <= d_2 /\ d_2 = (d - 1)) /\ (exists rt, (((d_2 < d /\ d_2 >= 0) /\ heap rt /\ ((exists u, (depth rt u /\ u <= d_2)) /\ (forall u, (root rt u -> u < n)))) /\ root v n /\ (lch v lt /\ rch v rt)))))))))) \/ (~n < mx /\ leaf v)))))))))))))%Z.
Proof.
  intros. destruct (Z.eqb_spec d 0)%Z.
  - left. subst. intuition. destruct H0 as (s & Hs & Hs'). apply tree_depth_0_leaf. admit.
  - right. intuition. destruct H0 as (s & Hs & Hs').
    destruct (decide_leaf v).
    + exists True. intuition.
    + exists False. right. intuition.
      destruct (tree_not_leaf_ex v) as (x & lt & rt & Hx & Hlt & Hrt); auto.
      exists x; eauto. left. intuition.
      exists (d - 1)%Z. intuition.
      exists lt. intuition; eauto. eapply tree_depth_lt_ex in Hlt; eauto. destruct Hlt as (d1 & Hd1 & Hd1s). exists d1.
      intuition.
      exists (d - 1)%Z. intuition.
      exists rt. intuition; eauto. eapply tree_depth_rt_ex in Hrt; eauto. destruct Hrt as (d1 & Hd1 & Hd1s). exists d1.
      intuition.
Qed.


Lemma ranged_set_gen:
  (forall diff, (0 <= diff -> (forall lo, (forall v, (((forall u, (tree_mem v u -> (lo < u /\ u < (lo + diff)))) /\ bst v) -> (((lo + diff) <= (1 + lo) /\ leaf v) \/ (~(lo + diff) <= (1 + lo) /\ (exists x_2, ((x_2 /\ leaf v) \/ (~x_2 /\ (exists b_2, (((1 + lo) < b_2 /\ b_2 = (lo + diff)) /\ (exists x, ((lo < x /\ x < b_2) /\ (exists diff_1, ((0 <= diff_1 /\ diff_1 = (x - lo)) /\ (exists hi_0, ((hi_0 = (lo + diff_1) /\ hi_0 = x) /\ (exists lt, (((diff_1 < diff /\ diff_1 >= 0) /\ (forall u, (tree_mem lt u -> (lo < u /\ u < hi_0))) /\ bst lt) /\ (exists diff_2, ((0 <= diff_2 /\ diff_2 = ((lo + diff) - x)) /\ (exists hi_1, ((hi_1 = (x + diff_2) /\ hi_1 = (lo + diff)) /\ (exists rt, (((diff_2 < diff /\ diff_2 >= 0) /\ (forall u, (tree_mem rt u -> (x < u /\ u < hi_1))) /\ bst rt) /\ root v x /\ (lch v lt /\ rch v rt)))))))))))))))))))))))))))%Z.
Proof.
  intros. destruct (Z.leb_spec diff 1)%Z.
  - left. intuition.
    destruct (decide_leaf v); intuition.
    destruct (tree_not_leaf_ex v) as (x & lt & rt & Hx & Hlt & Hrt); auto.
    apply tree_root_mem in Hx. apply H2 in Hx. lia.
  - right. intuition.
    destruct (decide_leaf v).
    + exists True. intuition.
    + exists False. right. intuition.
      destruct (tree_not_leaf_ex v) as (x & lt & rt & Hx & Hlt & Hrt); auto.
      exists (lo + diff)%Z. intuition.
      exists x. intuition.
      { apply tree_root_mem in Hx. apply H2 in Hx. lia. }
      { apply tree_root_mem in Hx. apply H2 in Hx. lia. }
      exists (x - lo)%Z. intuition.
      { apply tree_root_mem in Hx. apply H2 in Hx. lia. }
      exists x. intuition.
      exists lt. intuition; eauto.
      { apply tree_root_mem in Hx. apply H2 in Hx. lia. }
      { apply tree_root_mem in Hx. apply H2 in Hx. lia. }
      { eapply tree_mem_lch_mem in H4; eauto. apply H2 in H4. lia. }
      exists (lo + diff - x)%Z. intuition.
      { apply tree_root_mem in Hx. apply H2 in Hx. lia. }
      exists (x + (lo + diff - x))%Z. intuition.
      exists rt. intuition; eauto.
      { apply tree_root_mem in Hx. apply H2 in Hx. lia. }
      { apply tree_root_mem in Hx. apply H2 in Hx. lia. }
      { eapply tree_mem_rch_mem in H4; eauto. apply H2 in H4. lia. }
Qed.


Lemma depth_tree_gen:
(forall s, (0 <= s -> (forall v, ((exists u, (depth v u/\ u <= s)) -> ((s = 0/\ leaf v) \/ (~s = 0/\ (exists x_1, ((x_1/\ leaf v) \/ (~x_1/\ (exists s_1, ((0 <= s_1/\ s_1 = (s - 1))/\ (exists lt, (((s_1 < s/\ s_1 >= 0)/\ (exists u, (depth lt u/\ u <= s_1)))/\ (exists s_2, ((0 <= s_2/\ s_2 = (s - 1))/\ (exists rt, (((s_2 < s/\ s_2 >= 0)/\ (exists u, (depth rt u/\ u <= s_2)))/\ (exists n, (root v n/\ (lch v lt/\ rch v rt))))))))))))))))))))%Z.
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
