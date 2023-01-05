From stdpp Require Import mapset.
From stdpp Require Import natmap.
From CT Require Import CoreLangProp.
From CT Require Import OperationalSemanticsProp.
From CT Require Import BasicTypingProp.
From CT Require Import SyntaxSugar.
From CT Require Import Refinement.
From CT Require Import RefinementTac.

Import Atom.
Import CoreLang.
Import Tactics.
Import NamelessTactics.
Import ListCtx.
Import OperationalSemantics.
Import OperationalSemanticsProp.
Import BasicTyping.
Import SyntaxSugar.
Import Refinement.
Import RefinementTac.

Lemma closed_rty_mk_eq_constant: forall c, closed_rty 0 ∅ (mk_eq_constant c).
Proof.
  intros. repeat constructor; auto.
Qed.

Lemma closed_rty_mk_under_bot: forall B, closed_rty 0 ∅ (mk_under_bot B).
Proof.
  intros. repeat constructor; auto.
Qed.

Lemma closed_rty_mk_under_top: forall B, closed_rty 0 ∅ (mk_under_top B).
Proof.
  intros. repeat constructor; auto.
Qed.

Lemma closed_rty_mk_over_top: forall B, closed_rty 0 ∅ (mk_over_top B).
Proof.
  intros. repeat constructor; auto.
Qed.

Lemma closed_rty_mk_eq_var: forall B x, closed_rty 0 {[x]} (mk_eq_var B x).
Proof.
  intros. repeat constructor; intros; auto; try my_simplify_map_eq.
Qed.

Lemma closed_rty_mk_op_ret: forall op, closed_rty 2 ∅ (mk_op_ret op).
Proof.
  intros. repeat constructor; intros; auto; mydestr.
  - repeat eexists; repeat split; my_simplify_map_eq.
    repeat (rewrite <- H; auto).
  - repeat eexists; repeat split; my_simplify_map_eq.
    repeat (rewrite H; auto).
Qed.

Lemma closed_rty_mk_op: forall op, closed_rty 0 ∅ (mk_op op).
Proof.
  intros.
  destruct (closed_rty_mk_op_ret op); mydestr.
  repeat (constructor; intros; auto).
Qed.
