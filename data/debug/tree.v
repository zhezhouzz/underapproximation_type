From Coq Require Import Classes.DecidableClass.
From Coq Require Import Lia.
From Coq Require Import ZArith.
From Coq Require Import Setoid.
From Coq Require Import Logic.Classical.

Definition int := Z.
Variable IT: Type.
Variable depth: IT -> Z -> Prop.
Variable num_node: IT -> Z -> Prop.
Variable leaf: IT -> Prop.
Variable root: IT -> Z -> Prop.
Variable lch: IT -> IT -> Prop.
Variable rch: IT -> IT -> Prop.
Variable tree_mem: IT -> Z -> Prop.
Variable bst: IT -> Prop.
Variable heap: IT -> Prop.
Variable complete: IT -> Prop.
Variable botright: IT -> IT -> int -> Prop.

Ltac z_simpl :=
  match goal with
  | H: context [(?a - ?b + ?b)%Z] |- _  => rewrite Z.sub_add in H
  | |- context [(?a - ?b + ?b)%Z]  =>  rewrite Z.sub_add
  end.

(* Lemma tree_num_node_exists (tr: IT): exists (n: int), num_node tr n. *)
(* Admitted. *)

Lemma tree_destruct_non_leaf (tr: IT): (~ leaf tr -> exists (y: int), exists (l: IT), exists (r: IT), exists (nl: int), exists (nr: int), root tr y /\ lch tr l /\ rch tr r /\ num_node l nl /\ num_node r nr)%Z.
Admitted.

Lemma tree_num_node_gte_zero (tr: IT) (n: int): ((num_node tr n) -> (0 <= n))%Z.
Admitted.

Lemma tree_num_node_gt_zero_is_not_leaf (tr: IT) (n: int): ((num_node tr n /\ n > 0) -> (not (leaf tr)))%Z.
Admitted.

Lemma tree_num_node_1_ch_leaf (tr: IT) (tr': IT): ((num_node tr 1 /\ (lch tr tr' \/ rch tr tr')) -> (leaf tr'))%Z.
Admitted.

Lemma tree_leaf_num_node_zero (tr: IT): ((leaf tr) -> (num_node tr 0))%Z.
Admitted.

Lemma tree_num_node_zero_leaf (tr: IT): ((num_node tr 0) -> (leaf tr))%Z.
Admitted.

Lemma tree_num_node_ch_sum_plus_1 (tr: IT) (l: IT) (r: IT) (n: int) (nl: int) (nr: int): (( lch tr l /\ rch tr r /\ num_node tr n /\ num_node l nl /\ num_node r nr) -> (n = 1 + nl + nr))%Z.
Admitted.

Lemma tree_leaf_bst (tr: IT): ((leaf tr) -> (bst tr))%Z.
Admitted.

Lemma tree_bst_lch_bst (tr: IT) (tr': IT): ((bst tr /\ lch tr tr') -> (bst tr'))%Z.
Admitted.

Lemma tree_bst_rch_bst (tr: IT) (tr': IT): ((bst tr /\ rch tr tr') -> (bst tr'))%Z.
Admitted.

Lemma tree_bst_destruct_botright_non_leaf (tr: IT): ((not (leaf tr)) /\ bst tr -> exists (tr': IT), exists (x: int), exists (y: int), exists (l: IT), exists (r: IT), exists (nl: int), exists (nr: int), botright tr tr' x /\ ((not (leaf tr')) -> (root tr' y /\ lch tr' l /\ rch tr' r /\ num_node l nl /\ num_node r nr)))%Z.
Admitted.

Lemma tree_bst_botright_rest_bst (tr: IT) (tr': IT) (y: int): (bst tr /\ botright tr tr' y -> bst tr')%Z.
Admitted.

Lemma tree_bst_botright_rest_bst_num_node_minus_1 (tr: IT) (tr': IT) (y: int) (n: int): (num_node tr n /\ botright tr tr' y -> num_node tr' (n - 1))%Z.
Admitted.

Lemma tree_bst_botright_num_node_1 (tr: IT) (tr': IT) (y: int) (n: int): (num_node tr 1 /\ botright tr tr' y -> root tr y)%Z.
Admitted.

Lemma tree_leaf_num_node_eq_zero (tr: IT) (n: int): (leaf tr /\ num_node tr n) -> (n = 0)%Z.
Admitted.

Lemma tree_num_node_one_any_leaf_child (tr: IT) (tr': IT): (num_node tr 1 /\ leaf tr') -> (lch tr tr' /\ rch tr tr')%Z.
Admitted.

Lemma tree_bst_botright_not_root (tr: IT) (tr': IT) (y: int): (botright tr tr' y) -> (not (root tr' y)).
Admitted.

Lemma tree_bst_botright_when_lt_root_of_rest (tr: IT) (tr': IT) (y: int) (x: int) (l: IT) (r': IT) (l': IT) (nl': int): (botright tr tr' y /\ root tr' x /\ lch tr' l' /\ rch tr' r' /\ y < x /\ lch tr l /\ num_node l' nl')%Z -> (root tr x /\ rch tr r' /\ num_node l (nl' + 1))%Z.
Admitted.

Lemma tree_bst_botright_when_gt_root_of_rest (tr: IT) (tr': IT) (y: int) (x: int) (r: IT) (r': IT) (l': IT) (nr': int): (botright tr tr' y /\ root tr' x /\ lch tr' l' /\ rch tr' r' /\ x < y /\ rch tr r /\ num_node r' nr')%Z -> (root tr x /\ lch tr l' /\ num_node r (nr' + 1))%Z.
Admitted.

(* Lemma bst_insert_inlined: (forall v, (forall i, (forall tr'_0, (forall y_1, (forall y_0, (forall l_0, (forall r_0, (forall nl_0, (forall nr_0, (((exists tr'_1, (exists y_5, ((~leaf r_0/\ bst r_0) -> botright r_0 tr'_1 y_5)))/\ (exists tr'_2, (exists y_6, ((~leaf l_0/\ bst l_0) -> botright l_0 tr'_2 y_6)))/\ (exists tr'_3, (exists y_7, ((~leaf tr'_0/\ bst tr'_0) -> botright tr'_0 tr'_3 y_7)))) -> (((exists y_2, (exists l_1, (exists r_1, (exists nl_1, (exists nr_1, (~leaf r_0 -> (root r_0 y_2/\ lch r_0 l_1/\ rch r_0 r_1/\ num_node l_1 nl_1/\ num_node r_1 nr_1)))))))/\ (exists y_3, (exists l_2, (exists r_2, (exists nl_2, (exists nr_2, (~leaf l_0 -> (root l_0 y_3/\ lch l_0 l_2/\ rch l_0 r_2/\ num_node l_2 nl_2/\ num_node r_2 nr_2)))))))/\ (exists y_4, (exists l_3, (exists r_3, (exists nl_3, (exists nr_3, (~leaf tr'_0 -> (root tr'_0 y_4/\ lch tr'_0 l_3/\ rch tr'_0 r_3/\ num_node l_3 nl_3/\ num_node r_3 nr_3)))))))) -> (((~leaf v/\ bst v) -> botright v tr'_0 y_1) -> ((~leaf v -> (root v y_0/\ lch v l_0/\ rch v r_0/\ num_node l_0 nl_0/\ num_node r_0 nr_0)) -> ((0 <= i/\ 0 < i) -> ((bst v/\ num_node v i) -> (exists s, (bst s/\ num_node s (i - 1)/\ (exists x, ((leaf s/\ (exists x_0, (leaf x_0/\ (exists x_1, (leaf x_1/\ root v x/\ lch v x_1/\ rch v x_0))))) \/ (exists y, (exists l, (exists r, (root s y/\ lch s l/\ rch s r/\ ((x = y/\ root v y/\ lch v l/\ rch v r) \/ (~x = y/\ ((x < y/\ (exists i_1, (i_1 < i/\ 0 <= i_1/\ 0 < i_1/\ bst l/\ num_node l (i_1 - 1)/\ (exists x_5, (bst x_5/\ num_node x_5 i_1/\ root v y/\ lch v x_5/\ rch v r))))) \/ (~x < y/\ (exists i_3, (i_3 < i/\ 0 <= i_3/\ 0 < i_3/\ bst r/\ num_node r (i_3 - 1)/\ (exists x_7, (bst x_7/\ num_node x_7 i_3/\ root v y/\ lch v l/\ rch v x_7)))))))))))))))))))))))))))))))%Z. *)
(* Proof. *)
(*   intros. *)
(*   assert (not (leaf v)). apply tree_num_node_gt_zero_is_not_leaf with i; intuition; eauto. *)
(*   rename tr'_0 into s. rename y_1 into x. rename y_0 into vy. rename l_0 into vl. rename r_0 into vr. *)
(*   rename nl_0 into vnl. rename nr_0 into vrl. *)
(*   clear H. intuition. *)
(*   clear H. clear H4. *)
(*   assert (bst s). eapply tree_bst_botright_rest_bst; intuition; eauto. *)
(*   assert (num_node s (i - 1))%Z. eapply tree_bst_botright_rest_bst_num_node_minus_1; intuition; eauto. *)
(*   exists s. intuition. *)
(*   exists x. *)
(*   destruct (classic (leaf s)). *)
(*   + left. *)
(*     assert (i - 1 = 0)%Z. eapply tree_leaf_num_node_eq_zero; intuition; eauto. *)
(*     assert (i = 1)%Z. lia. subst. *)
(*     intuition. exists s. intuition. exists s. *)
(*     assert (num_node v 1 /\ leaf s) as Htmp. intuition. *)
(*     apply tree_num_node_one_any_leaf_child in Htmp. intuition. *)
(*     intuition. *)
(*     eapply tree_bst_botright_num_node_1; intuition; eauto. *)
(*   + right. *)
(*     destruct H9 as (y & l & r & nl & nr & Hylr); auto. intuition. *)
(*     assert (bst l). eapply tree_bst_lch_bst; intuition; eauto. *)
(*     assert (bst r). apply tree_bst_rch_bst with s; intuition; eauto. *)
(*     assert (bst vl). apply tree_bst_lch_bst with v; intuition; eauto. *)
(*     assert (bst vr). apply tree_bst_rch_bst with v; intuition; eauto. *)
(*     assert (i - 1 = 1 + nl + nr)%Z. eapply tree_num_node_ch_sum_plus_1; intuition; eauto. *)
(*     assert (0 <= nr)%Z. eapply tree_num_node_gte_zero; eauto. *)
(*     assert (0 <= nl)%Z. eapply tree_num_node_gte_zero; eauto. *)
(*     exists y, l, r. intuition. *)
(*     destruct (classic (x = y)). *)
(*     - left. subst. apply tree_bst_botright_not_root in H12. intuition. *)
(*     - right. intuition. *)
(*       destruct (classic (x < y)%Z). *)
(*       * left. intuition. *)
(*         exists (nl + 1)%Z. intuition. assert (nl + 1 - 1 = nl)%Z as Htmp by lia. rewrite Htmp. auto. *)
(*         exists vl. *)
(*         assert (botright v s x /\ root s y /\ lch s l /\ rch s r /\ x < y /\ lch v vl /\ num_node l nl)%Z as Htmp. intuition. *)
(*         apply tree_bst_botright_when_lt_root_of_rest in Htmp. *)
(*         intuition. *)
(*       * right. intuition. *)
(*         exists (nr + 1)%Z. intuition. assert (nr + 1 - 1 = nr)%Z as Htmp by lia. rewrite Htmp. auto. *)
(*         exists vr. *)
(*         assert (botright v s x /\ root s y /\ lch s l /\ rch s r /\ y < x /\ rch v vr /\ num_node r nr)%Z as Htmp. intuition. *)
(*         apply tree_bst_botright_when_gt_root_of_rest  in Htmp. *)
(*         intuition. *)
(* Qed. *)

Lemma bst_insert: (forall v, (forall i, ((0 <= i/\ 0 < i) -> ((bst v/\ num_node v i) -> (exists s, (bst s/\ num_node s (i - 1)/\ (exists x, ((leaf s/\ (exists x_0, (leaf x_0/\ (exists x_1, (leaf x_1/\ root v x/\ lch v x_1/\ rch v x_0))))) \/ (exists y, (exists l, (exists r, (root s y/\ lch s l/\ rch s r/\ ((x = y/\ root v y/\ lch v l/\ rch v r) \/ (~x = y/\ ((x < y/\ (exists i_1, (i_1 < i/\ 0 <= i_1/\ 0 < i_1/\ bst l/\ num_node l (i_1 - 1)/\ (exists x_5, (bst x_5/\ num_node x_5 i_1/\ root v y/\ lch v x_5/\ rch v r))))) \/ (~x < y/\ (exists i_3, (i_3 < i/\ 0 <= i_3/\ 0 < i_3/\ bst r/\ num_node r (i_3 - 1)/\ (exists x_7, (bst x_7/\ num_node x_7 i_3/\ root v y/\ lch v l/\ rch v x_7))))))))))))))))))))%Z.
Proof.
  intros.
  assert (not (leaf v)). apply tree_num_node_gt_zero_is_not_leaf with i; intuition; eauto.
  destruct (tree_bst_destruct_botright_non_leaf v) as (s & x & y & l & r & nl & nr & Hsx); intuition; auto.
  destruct (tree_destruct_non_leaf v) as (vy & vl & vr & nvl & nvr & Hvylr); auto.
  (* assert (i = 1 + nvl + nvr)%Z. eapply tree_num_node_ch_sum_plus_1; intuition; eauto. *)
  assert (bst s). eapply tree_bst_botright_rest_bst; intuition; eauto.
  assert (num_node s (i - 1))%Z. eapply tree_bst_botright_rest_bst_num_node_minus_1; intuition; eauto.
  exists s. intuition.
  exists x.
  destruct (classic (leaf s)).
  + left.
    assert (i - 1 = 0)%Z. eapply tree_leaf_num_node_eq_zero; intuition; eauto.
    assert (i = 1)%Z. lia. subst.
    intuition. exists s. intuition. exists s.
    assert (num_node v 1 /\ leaf s) as Htmp. intuition.
    apply tree_num_node_one_any_leaf_child in Htmp. intuition.
    intuition.
    apply tree_bst_botright_num_node_1 with s; intuition; eauto.
  + right. intuition.
    assert (bst l). eapply tree_bst_lch_bst; intuition; eauto.
    assert (bst r). apply tree_bst_rch_bst with s; intuition; eauto.
    assert (bst vl). apply tree_bst_lch_bst with v; intuition; eauto.
    assert (bst vr). apply tree_bst_rch_bst with v; intuition; eauto.
    assert (i - 1 = 1 + nl + nr)%Z. eapply tree_num_node_ch_sum_plus_1; intuition; eauto.
    intuition.
    assert (0 <= nr)%Z. eapply tree_num_node_gte_zero; eauto.
    assert (0 <= nl)%Z. eapply tree_num_node_gte_zero; eauto.
    exists y, l, r. intuition.
    destruct (classic (x = y)).
    - left. subst. apply tree_bst_botright_not_root in H0. intuition.
    - right. intuition.
      destruct (classic (x < y)%Z).
      * left. intuition.
        exists (nl + 1)%Z. intuition. assert (nl + 1 - 1 = nl)%Z as Htmp by lia. rewrite Htmp. auto.
        exists vl.
        assert (botright v s x /\ root s y /\ lch s l /\ rch s r /\ x < y /\ lch v vl /\ num_node l nl)%Z as Htmp. intuition.
        apply tree_bst_botright_when_lt_root_of_rest in Htmp.
        intuition.
      * right. intuition.
        exists (nr + 1)%Z. intuition. assert (nr + 1 - 1 = nr)%Z as Htmp by lia. rewrite Htmp. auto.
        exists vr.
        assert (botright v s x /\ root s y /\ lch s l /\ rch s r /\ y < x /\ rch v vr /\ num_node r nr)%Z as Htmp. intuition.
        apply tree_bst_botright_when_gt_root_of_rest  in Htmp.
        intuition.
Qed.

Lemma bst_insert_v_eq_one: (forall v, (forall i, ((0 <= i/\ 1 = i) -> ((bst v/\ num_node v i) -> (exists s, (bst s/\ num_node s (i - 1)/\ (exists x, (leaf s/\ (exists x_0, (leaf x_0/\ (exists x_1, (leaf x_1/\ root v x/\ lch v x_1/\ rch v x_0))))))))))))%Z.
Proof.
  intros. intuition. subst. simpl.
  assert (not (leaf v)). eapply tree_num_node_gt_zero_is_not_leaf; intuition; eauto.
  destruct (tree_destruct_non_leaf v) as (x & l & r & nl & nr & Hylr); auto.
  assert (leaf l). eapply tree_num_node_1_ch_leaf; intuition; eauto.
  assert (leaf r). eapply tree_num_node_1_ch_leaf; intuition; eauto.
  exists l. intuition.
  apply tree_leaf_bst; auto. apply tree_leaf_num_node_zero; auto.
  exists x. intuition. exists r. intuition. exists l. intuition.
Qed.
