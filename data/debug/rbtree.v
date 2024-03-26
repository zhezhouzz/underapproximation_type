From Coq Require Import Classes.DecidableClass.
From Coq Require Import Lia.
From Coq Require Import ZArith.

Variable IT: Type.
Variable num_black: IT -> Z -> Prop.
Variable rb_leaf: IT -> Prop.
Variable rb_root: IT -> Z -> Prop.
Variable rb_root_color: IT -> bool -> Prop.
Variable rb_lch: IT -> IT -> Prop.
Variable rb_rch: IT -> IT -> Prop.
Variable no_red_red: IT -> Prop.

Definition ch (tr: IT) (tr': IT) := rb_lch tr tr' \/ rb_rch tr tr'.

Lemma tree_num_black_0_rb_leaf (l : IT) :(num_black l 0) -> ~ (rb_root_color l true) -> (rb_leaf l) .
Admitted.
#[export] Hint Resolve tree_num_black_0_rb_leaf : core.

Lemma tree_rb_leaf_num_black_0 (l : IT) (n: Z) :(num_black l n) -> (rb_leaf l) -> (n = 0)%Z .
Admitted.
#[export] Hint Resolve tree_rb_leaf_num_black_0 : core.

Lemma tree_num_black_not_0_not_rb_leaf (l : IT) (n: Z) : not (n = 0)%Z -> (num_black l n) -> not (rb_leaf l).
Admitted.
#[export] Hint Resolve tree_num_black_not_0_not_rb_leaf : core.

Lemma decide_rb_leaf: forall v, rb_leaf v \/ not (rb_leaf v).
Admitted.

Lemma tree_not_rb_leaf_ex_ch (l : IT): not (rb_leaf l) -> exists x c l1 l2, rb_lch l l1 /\ rb_rch l l2 /\ rb_root l x /\ rb_root_color l c.
Admitted.

Lemma num_black_root_black_lt_minus_1 (v lt: IT) (h: Z): rb_root_color v false -> num_black v h -> rb_lch v lt -> num_black lt (h - 1)%Z.
Admitted.
#[export] Hint Resolve num_black_root_black_lt_minus_1 : core.

Lemma num_black_root_black_rt_minus_1 (v rt: IT) (h: Z): rb_root_color v false -> num_black v h -> rb_rch v rt -> num_black rt (h - 1)%Z.
Admitted.
#[export] Hint Resolve num_black_root_black_rt_minus_1 : core.

Lemma num_black_root_red_lt_same (v lt: IT) (h: Z): rb_root_color v true -> num_black v h -> rb_lch v lt -> num_black lt h.
Admitted.
#[export] Hint Resolve num_black_root_red_lt_same : core.

Lemma num_black_root_red_rt_same (v rt: IT) (h: Z): rb_root_color v true -> num_black v h -> rb_rch v rt -> num_black rt h.
Admitted.
#[export] Hint Resolve num_black_root_red_rt_same : core.

Lemma num_black_root_black_0_lt_leaf (v lt: IT) (h: Z): num_black v 0 -> rb_lch v lt -> rb_leaf lt.
Admitted.
#[export] Hint Resolve num_black_root_black_0_lt_leaf : core.

Lemma num_black_root_black_0_rt_leaf (v rt: IT) (h: Z): num_black v 0 -> rb_rch v rt -> rb_leaf rt.
Admitted.
#[export] Hint Resolve num_black_root_black_0_rt_leaf : core.

Lemma num_black_root_black_0_rt_red (v rt: IT) (h: Z): num_black v 0 -> rb_rch v rt -> rb_root_color v true.
Admitted.
#[export] Hint Resolve num_black_root_black_0_rt_red : core.

Lemma no_red_red_lt (v lt: IT): no_red_red v -> rb_lch v lt -> no_red_red lt.
Admitted.
#[export] Hint Resolve no_red_red_lt : core.

Lemma no_red_red_rt (v rt: IT): no_red_red v -> rb_rch v rt -> no_red_red rt.
Admitted.
#[export] Hint Resolve no_red_red_rt : core.

Lemma no_red_red_root_red_lt_not_red (v lt: IT): no_red_red v -> rb_lch v lt -> rb_root_color v true -> ~ rb_root_color lt true.
Admitted.
#[export] Hint Resolve no_red_red_root_red_lt_not_red : core.

Lemma no_red_red_root_red_rt_not_red (v rt: IT): no_red_red v -> rb_rch v rt -> rb_root_color v true -> ~ rb_root_color rt true.
Admitted.
#[export] Hint Resolve no_red_red_root_red_rt_not_red : core.

Lemma black_lt_black_num_black_gt_1 (v lt: IT) (h: Z):
  num_black v h -> rb_lch v lt -> rb_root_color v false -> rb_root_color lt false -> (h > 1)%Z.
Admitted.
#[export] Hint Resolve black_lt_black_num_black_gt_1 : core.

Lemma black_rt_black_num_black_gt_1 (v rt: IT) (h: Z):
  num_black v h -> rb_rch v rt -> rb_root_color v false -> rb_root_color rt false -> (h > 1)%Z.
Admitted.
#[export] Hint Resolve black_rt_black_num_black_gt_1 : core.

(* Lemma abduction_debug: (forall inv, (inv >= 0 -> (forall color, (forall h, ((h >= 0 /\ (((color = true) -> (h + h) = inv) /\ (~(color = true) -> ((h + h) + 1) = inv))) -> (forall v, ((num_black v h /\ (no_red_red v /\ (h = 0 /\ (((color = true) -> ~rb_root_color v true) /\ (~(color = true) -> (h = 0 -> ~rb_root_color v false)))))) -> ((h = 0 /\ (((color = true) /\ rb_leaf v) \/ (~(color = true) /\ (exists x_1, (x_1 /\ rb_leaf v))))) \/ (~h = 0 /\ (color = true) /\ (exists x_2, (rb_leaf x_2 /\ (exists x_3, (rb_leaf x_3 /\ rb_root_color v false /\ rb_root v h /\ rb_lch v x_3 /\ rb_rch v x_2)))))))))))))%Z. *)
(* Proof. *)
(*   intros.  destruct (Z.eqb_spec h 0). *)
(*   - left. subst. intuition. *)
(*     destruct color. *)
(*     + left. intuition. *)
(*     + right. intuition. destruct (decide_rb_leaf v). *)
(*       * exists True. intuition. *)
(*       * exists True. intuition. admit. *)
(*   - right. intuition. *)



Lemma num_black_tree_gen:
(forall inv, (inv >= 0 -> (forall color, (forall h, ((h >= 0/\ (((color = true) -> (h + h) = inv)/\ (~(color = true) -> ((h + h) + 1) = inv))) -> (forall v, ((num_black v h/\ (no_red_red v/\ (((color = true) -> ~rb_root_color v true)/\ (~(color = true) -> (h = 0 -> ~rb_root_color v false))))) -> ((h = 0/\ (((color = true)/\ rb_leaf v) \/ (~(color = true)/\ (exists x_1, ((x_1/\ rb_leaf v) \/ (~x_1/\ (exists x_2, (rb_leaf x_2/\ (exists x_3, (exists x_4, (rb_leaf x_4/\ rb_root_color v true/\ (rb_root v x_3/\ (rb_lch v x_4/\ rb_rch v x_2))))))))))))) \/ (~h = 0/\ (exists rt, (((color = true)/\ (exists inv_1, ((inv_1 >= 0/\ inv_1 = (inv - 1))/\ (exists h_0, (((h_0 >= 0/\ ((False -> (h_0 + h_0) = inv_1)/\ (~False -> ((h_0 + h_0) + 1) = inv_1)))/\ h_0 = (h - 1))/\ (exists lt2, (((inv_1 < inv/\ inv_1 >= 0)/\ num_black lt2 h_0/\ (no_red_red lt2/\ ((False -> ~rb_root_color lt2 true)/\ (~False -> (h_0 = 0 -> ~rb_root_color lt2 false)))))/\ (exists inv_2, ((inv_2 >= 0/\ inv_2 = (inv - 1))/\ (exists h_1, (((h_1 >= 0/\ ((False -> (h_1 + h_1) = inv_2)/\ (~False -> ((h_1 + h_1) + 1) = inv_2)))/\ h_1 = (h - 1))/\ (exists rt2, (((inv_2 < inv/\ inv_2 >= 0)/\ num_black rt2 h_1/\ (no_red_red rt2/\ ((False -> ~rb_root_color rt2 true)/\ (~False -> (h_1 = 0 -> ~rb_root_color rt2 false)))))/\ rb_root_color v false/\ (rb_root v rt/\ (rb_lch v lt2/\ rb_rch v rt2))))))))))))))) \/ (~(color = true)/\ (exists c, ((c/\ (exists inv_3, ((inv_3 >= 0/\ inv_3 = (inv - 1))/\ (exists h_2, (((h_2 >= 0/\ ((True -> (h_2 + h_2) = inv_3)/\ (~True -> ((h_2 + h_2) + 1) = inv_3)))/\ h_2 = h)/\ (exists lt3, (((inv_3 < inv/\ inv_3 >= 0)/\ num_black lt3 h_2/\ (no_red_red lt3/\ ((True -> ~rb_root_color lt3 true)/\ (~True -> (h_2 = 0 -> ~rb_root_color lt3 false)))))/\ (exists inv_4, ((inv_4 >= 0/\ inv_4 = (inv - 1))/\ (exists h_3, (((h_3 >= 0/\ ((True -> (h_3 + h_3) = inv_4)/\ (~True -> ((h_3 + h_3) + 1) = inv_4)))/\ h_3 = h)/\ (exists rt3, (((inv_4 < inv/\ inv_4 >= 0)/\ num_black rt3 h_3/\ (no_red_red rt3/\ ((True -> ~rb_root_color rt3 true)/\ (~True -> (h_3 = 0 -> ~rb_root_color rt3 false)))))/\ rb_root_color v true/\ (rb_root v rt/\ (rb_lch v lt3/\ rb_rch v rt3))))))))))))))) \/ (~c/\ (exists inv_5, ((inv_5 >= 0/\ inv_5 = (inv - 2))/\ (exists h_4, (((h_4 >= 0/\ ((False -> (h_4 + h_4) = inv_5)/\ (~False -> ((h_4 + h_4) + 1) = inv_5)))/\ h_4 = (h - 1))/\ (exists lt4, (((inv_5 < inv/\ inv_5 >= 0)/\ num_black lt4 h_4/\ (no_red_red lt4/\ ((False -> ~rb_root_color lt4 true)/\ (~False -> (h_4 = 0 -> ~rb_root_color lt4 false)))))/\ (exists inv_6, ((inv_6 >= 0/\ inv_6 = (inv - 2))/\ (exists h_5, (((h_5 >= 0/\ ((False -> (h_5 + h_5) = inv_6)/\ (~False -> ((h_5 + h_5) + 1) = inv_6)))/\ h_5 = (h - 1))/\ (exists rt4, (((inv_6 < inv/\ inv_6 >= 0)/\ num_black rt4 h_5/\ (no_red_red rt4/\ ((False -> ~rb_root_color rt4 true)/\ (~False -> (h_5 = 0 -> ~rb_root_color rt4 false)))))/\ rb_root_color v false/\ (rb_root v rt/\ (rb_lch v lt4/\ rb_rch v rt4)))))))))))))))))))))))))))))%Z.
Proof.
  intros. destruct (Z.eqb_spec h 0).
  - left. subst. intuition.
    destruct color.
    + left. intuition.
    + right. intuition. destruct (decide_rb_leaf v).
      * exists True. left. intuition.
      * exists False. right. intuition.
        destruct (tree_not_rb_leaf_ex_ch v) as (x & c & lt & rt & Hlt & Hrt & Hx & Hc); auto.
        exists rt. intuition; eauto.
        exists x, lt. intuition; eauto.
  - right. intuition.
    destruct (tree_not_rb_leaf_ex_ch v) as (x & c & lt & rt & Hlt & Hrt & Hx & Hc); eauto.
    exists x.
    destruct color.
    + left. intuition. destruct c. intuition.
      exists (inv - 1)%Z. intuition. exists (h - 1)%Z. intuition.
      exists lt. intuition; eauto.
      eapply black_lt_black_num_black_gt_1 in Hlt; eauto. lia.
      exists (inv - 1)%Z. intuition. exists (h - 1)%Z. intuition.
      exists rt. intuition; eauto.
      eapply black_rt_black_num_black_gt_1 in Hrt; eauto. lia.
    + right. intuition.
      destruct c.
      * exists True. left. intuition.
        exists (inv - 1)%Z. intuition. exists h. intuition.
        exists lt. intuition; eauto.
        eapply no_red_red_root_red_lt_not_red in Hlt; eauto.
        exists (inv - 1)%Z. intuition. exists h. intuition.
        exists rt. intuition; eauto.
        eapply no_red_red_root_red_rt_not_red in Hrt; eauto.
      * exists False. right. intuition.
        exists (inv - 2)%Z. intuition. exists (h - 1)%Z. intuition.
        exists lt. intuition; eauto.
        eapply black_lt_black_num_black_gt_1 in Hlt; eauto. lia.
        exists (inv - 2)%Z. intuition. exists (h - 1)%Z. intuition.
        exists rt. intuition; eauto.
        eapply black_rt_black_num_black_gt_1 in Hrt; eauto. lia.
Qed.
