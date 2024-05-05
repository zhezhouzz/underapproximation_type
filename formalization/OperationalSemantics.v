From stdpp Require Import mapset.
From CT Require Import CoreLang.
From CT Require Import CoreLangClass.

Import Atom.
Import CoreLang.
Import Tactics.
Import NamelessTactics.

(** This file defines the operational semantics of the core language. *)

Inductive eval_op: op -> constant -> constant -> Prop :=
| EvalOpPlusOne: forall (n: nat), eval_op op_plus_one n (S n)
| EvalOpMinusOne: forall (n: nat), eval_op op_minus_one (S n) n
| EvalOpEqZero1: eval_op op_eq_zero 0 true
| EvalOpEqZero2: forall (n: nat), eval_op op_eq_zero (S n) false
| EvalOpRannat: forall (n: nat) (m: nat), eval_op op_rannat n m
| EvalOpRanbool: forall (n: nat) (m: bool), eval_op op_ranbool n m.

Global Hint Constructors eval_op : core.

Notation "'op{' op '~' c1 '}⇓{' c2 '}'" := (eval_op op c1 c2) (at level 60, c1 constr, c2 constr).

Reserved Notation "t1 '↪' t2" (at level 60, t1 constr, t2 constr).
Inductive step : tm -> tm -> Prop :=
| STOp: forall op (c1 c: constant) (e: tm),
    body e ->
    op{ op ~ c1 }⇓{ c } ->
    (tletop op c1 e) ↪ (e ^^ c)
| STLetE1: forall e1 e1' e,
    body e ->
    e1 ↪ e1' ->
    (tlete e1 e) ↪ (tlete e1' e)
| STLetE2: forall (v1: value) (e: tm),
    lc v1 -> body e ->
    (tlete (treturn v1) e) ↪ (e ^^ v1)
| STLetAppLam: forall T (v_x: value) (e1: tm) e,
    body e1 -> body e -> lc v_x ->
    (tletapp (vlam T e1) v_x e) ↪ tlete (e1 ^^ v_x) e
| STLetAppFix: forall T_f (v_x: value) (e1: tm) e,
    body (vlam T_f e1) -> lc v_x -> body e ->
    tletapp (vfix T_f (vlam T_f e1)) v_x e ↪
            tletapp ((vlam T_f e1) ^^ v_x) (vfix T_f (vlam T_f e1)) e
| STMatchbTrue: forall e1 e2,
    lc e1 -> lc e2 ->
    (tmatchb true e1 e2) ↪ e1
| STMatchbFalse: forall e1 e2,
    lc e1 -> lc e2 ->
    (tmatchb false e1 e2) ↪ e2
where "t1 '↪' t2" := (step t1 t2).

Global Hint Constructors step : core.

Inductive multistep : tm -> tm -> Prop :=
| multistep_refl : forall (e : tm), lc e -> multistep e e
| multistep_step : forall (x y z: tm), x ↪ y -> multistep y z -> multistep x z.

Notation "t1 '↪*' t2" := (multistep t1 t2) (at level 60, t1 constr, t2 constr).

Global Hint Constructors multistep : core.

(** * Properties of operational semantics *)
Import CoreLangClass.

Lemma step_regular: forall e1 e2, e1 ↪ e2 -> lc e1 /\ lc e2.
Proof.
  induction 1; split; auto; lc_solver_ast.
Qed.

Lemma step_regular1: forall e1 e2, e1 ↪ e2 -> lc e1.
Proof.
  intros. apply step_regular in H. destruct H; auto.
Qed.

Lemma step_regular2: forall e1 e2, e1 ↪ e2 -> lc e2.
Proof.
  intros. apply step_regular in H. destruct H; auto.
Qed.

Global Hint Resolve step_regular1: core.
Global Hint Resolve step_regular2: core.

Theorem multistep_trans : forall (x y z : tm), x ↪* y -> y ↪* z -> x ↪* z.
Proof.
  intros. generalize dependent z.
  induction H; eauto.
Qed.

Theorem multistep_R : forall (x y : tm), x ↪ y -> x ↪* y.
Proof. intros. eauto.
Qed.

Lemma multi_step_regular: forall e1 e2, e1 ↪* e2 -> lc e1 /\ lc e2.
Proof.
  induction 1; intuition eauto.
Qed.

Lemma multi_step_regular1: forall e1 e2, e1 ↪* e2 -> lc e1.
Proof.
  intros. apply multi_step_regular in H. destruct H; auto.
Qed.

Lemma multi_step_regular2: forall e1 e2, e1 ↪* e2 ->  lc e2.
Proof.
  intros. apply multi_step_regular in H. destruct H; auto.
Qed.

Lemma value_reduction_refl: forall (v1: value) v2, v1 ↪* v2 -> v2 = v1.
Proof.
  intros * H. sinvert H; easy.
Qed.

Ltac step_regular_simp :=
  repeat match goal with
    (* | H: (treturn _) ↪* _  |- _ => *)
    (*     apply value_reduction_refl in H; simp_hyps; simplify_eq *)
    | [H: _ ↪* _ |- lc _ ] => apply multi_step_regular in H; destruct H; auto
    | [H: _ ↪ _ |- lc _ ] => apply step_regular in H; destruct H; auto
    | [H: _ ↪* _ |- body _] => apply multi_step_regular in H; destruct H; auto
    | [H: _ ↪ _ |- body _] => apply step_regular in H; destruct H; auto
    end.

Lemma reduction_tlete:  forall e_x (e: tm) (v : value),
    tlete e_x e ↪* v -> (exists (v_x: value), e_x ↪* v_x /\ (e ^^ v_x) ↪* v).
Proof.
  intros.
  remember (tlete e_x e). remember (treturn v).
  generalize dependent e_x.
  induction H; intros; subst. easy.
  sinvert H.
  - ospecialize* IHmultistep; eauto.
    simp_hyps.
    exists v_x. split; eauto.
  - exists v1. repeat esplit; eauto.
Qed.

Lemma reduction_tlete':  forall e_x (e: tm) (v_x v : value),
    body e -> e_x ↪* v_x -> (e ^^ v_x) ↪* v -> tlete e_x e ↪* v.
Proof.
  intros * Hb H. remember (treturn v_x).
  induction H; intros; subst.
  - econstructor; eauto using STLetE2.
  - simp_hyps; eauto.
Qed.

Lemma reduction_mk_app_e_v (f v_x v : value) :
  lc v_x ->
  mk_app_e_v f v_x ↪* v ->
  tletapp f v_x (vbvar 0) ↪* v.
Proof.
  intros Hlc H.
  sinvert H. sinvert H0. easy.
  nameless_eval.
Qed.

Lemma reduction_mk_app_e_v' (f v_x v : value) :
  tletapp f v_x (vbvar 0) ↪* v ->
  mk_app_e_v f v_x ↪* v.
Proof.
  intros H.
  assert (lc v_x). {
    apply multi_step_regular1 in H. sinvert H. eauto.
  }
  eapply_eq multistep_step.
  eapply STLetE2.
  all: step_regular_simp; try (nameless_eval; lc_solver_ast).
  - auto_exists_L. intros. nameless_eval. lc_solver_ast.
Qed.

Lemma reduction_letapplam Tb (e: tm) (v_x : value) (v : value) :
  lc v_x ->
  mk_app_e_v (vlam Tb e) v_x ↪* v ->
  e ^^ v_x ↪* v.
Proof.
  intros Hlc H.
  sinvert H.
  sinvert H0; try easy.
  nameless_eval.
  sinvert H1. sinvert H.
  apply reduction_tlete in H0.
  simp_hyps. simpl in *.
  sinvert H1; try easy.
Qed.

Lemma eta_reduction_lete_0: forall (e: tm) (v: value), e ↪* v -> tlete e (vbvar 0) ↪* v.
Proof.
  intros. eapply reduction_tlete'; eauto. lc_solver_ast. nameless_eval.
  econstructor. step_regular_simp.
Qed.

Lemma eta_reduction_letapp: forall (e e' e'': tm) Tb (v_x: value), lc v_x -> body e -> tlete (e ^^ v_x) e' ↪* e'' -> tletapp (vlam Tb e) v_x e' ↪* e''.
Proof.
  econstructor. apply STLetAppLam.
  all: step_regular_simp; lc_solver_ast.
Qed.

Lemma reduction_letapplam' Tb (e: tm) (v_x : value) (v : value) :
  lc v_x ->
  body e ->
  e ^^ v_x ↪* v ->
  mk_app_e_v (vlam Tb e) v_x ↪* v.
Proof.
  intros Hlc Hb H.
  eapply_eq multistep_step.
  eapply STLetE2.
  qauto using lc_abs_iff_body.
  - auto_exists_L. intros. nameless_eval. lc_solver_ast.
  - nameless_eval. apply eta_reduction_letapp; lc_solver_ast.
    apply eta_reduction_lete_0; auto.
Qed.

Lemma reduction_tletapp:  forall v1 v2 (e: tm) (v : value),
    tletapp v1 v2 e ↪* v ->
    (exists (v_x: value), mk_app_e_v v1 v2 ↪* v_x /\ (e ^^ v_x) ↪* v).
Proof.
  intros.
  remember (tletapp v1 v2 e). remember (treturn v).
  generalize dependent v2.
  generalize dependent v1.
  induction H; intros; subst. easy.
  simp_hyps. sinvert H.
  - eapply reduction_tlete in H0. simp_hyps.
    exists v_x. split; eauto using reduction_letapplam'.
  - ospecialize* H1; eauto. simp_hyps.
    exists v_x.
    repeat split; cycle 1; auto.
    (* Maybe this should be a lemma about [vfix]. *)
    apply reduction_mk_app_e_v in H1; auto.
    apply reduction_mk_app_e_v'.
    econstructor. econstructor; eauto. lc_solver_ast.
    apply multi_step_regular1 in H0.
    sinvert H0. eauto. rewrite lc_fix_iff_body; auto.
Qed.

Lemma reduction_tletop:  forall op v2 (e: tm) (v : value),
    (tletop op v2 e) ↪* v ->
    exists (c2 c_x: constant), v2 = c2 /\ op{op ~ c2}⇓{ c_x } /\ (e ^^ c_x) ↪* v .
Proof.
  intros.
  sinvert H. sinvert H0.
  eauto 10.
Qed.

Lemma reduction_matchb_true:  forall e1 e2 (v : value),
    tmatchb true e1 e2 ↪* v -> e1 ↪* v.
Proof.
  intros.
  sinvert H.
  sinvert H0. auto.
Qed.

Lemma reduction_matchb_false:  forall e1 e2 (v : value),
    tmatchb false e1 e2 ↪* v -> e2 ↪* v.
Proof.
  intros.
  sinvert H.
  sinvert H0. auto.
Qed.
