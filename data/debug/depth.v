From Coq Require Import Classes.DecidableClass.
From Coq Require Import Lia.
From Coq Require Import ZArith.

Variable IT: Type.
Variable depth: IT -> Z -> Prop.
Variable leaf: IT -> Prop.
Variable root: IT -> Z -> Prop.
Variable lch: IT -> IT -> Prop.
Variable rch: IT -> IT -> Prop.

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

Lemma tree_not_leaf_ex_root (l : IT): not (leaf l) -> exists x, root l x.
Admitted.
#[export] Hint Resolve tree_not_leaf_ex_root : core.

Lemma tree_not_leaf_ex_ch (l : IT): not (leaf l) -> exists l1 l2, lch l l1 /\ rch l l2.
Admitted.
#[export] Hint Resolve tree_not_leaf_ex_ch : core.

(* Lemma tree_sorted_ch_sorted (l : IT) (l' : IT): ch l l' -> sorted l -> sorted l'. *)
(* Admitted. *)
(* #[export] Hint Resolve tree_sorted_ch_sorted : core. *)

Lemma tree_depth_ch_depth_minus_1 (tr tr1 tr2: IT) (n n1 n2: Z):
  lch tr tr1 -> rch tr tr2 -> depth tr n -> depth tr1 n1 -> depth tr2 n2 ->
  ((n1 > n2 /\ n = n1 + 1) \/ (n1 <= n2 /\ n = n2 + 1))%Z.
Admitted.
#[export] Hint Resolve tree_depth_ch_depth_minus_1 : core.

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
