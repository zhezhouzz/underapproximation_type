From stdpp Require Import mapset.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Classical.
From CT Require Import CoreLang.
From CT Require Import CoreLangProp.
From CT Require Import BasicTypingProp.

Import Atom.
Import CoreLang.
Import Tactics.
Import NamelessTactics.
Import ListCtx.
Import OperationalSemantics.
Import BasicTyping.
Import BasicTypingProp.

Reserved Notation "t1 '⥇' t2" (at level 60).

Inductive err_step : tm -> tm -> Prop :=
| ErrST_Lete1: forall e1 e1' e,
    body e ->
    e1 ⥇ e1' ->
    (tlete e1 e) ⥇ (tlete e1' e)
| ErrST_Lete2: forall e,
    body e -> (tlete terr e) ⥇ terr
where "t1 '⥇' t2" := (err_step t1 t2).

Definition relation (X : Type) := X -> X -> Prop.

Lemma err_step_regular: forall e1 e2, e1 ⥇ e2 -> lc e1 /\ lc e2.
Proof.
  intros.
  induction H; split; repeat destruct_hyp_conj; auto; destruct H; econstructor; eauto.
Qed.

Lemma err_step_regular1: forall e1 e2, e1 ⥇ e2 -> lc e1.
Proof.
  intros. apply err_step_regular in H. destruct H; auto.
Qed.

Lemma err_step_regular2: forall e1 e2, e1 ⥇ e2 -> lc e2.
Proof.
  intros. apply err_step_regular in H. destruct H; auto.
Qed.

Global Hint Resolve err_step_regular1: core.
Global Hint Resolve err_step_regular2: core.

Inductive multierrstep : relation tm :=
| multierr_step_refl : forall (x : tm),
    lc x ->
    multierrstep x x
| multierr_step_step : forall (x y z : tm),
    err_step x y ->
    multierrstep y z ->
    multierrstep x z.

Global Hint Constructors multierrstep : core.

Theorem multierr_step_R : forall (x y : tm),
    err_step x y -> multierrstep x y.
Proof.
  intros x y H.
  apply multierr_step_step with y. apply H. apply multierr_step_refl; eauto.
Qed.

Theorem multierr_step_trans :
  forall (x y z : tm),
      multierrstep x y  ->
      multierrstep y z ->
      multierrstep x z.
Proof.
  intros x y z G H.
  induction G.
    - (* multierr_step_refl *) assumption.
    - (* multierr_step_step *)
      apply multierr_step_step with y. assumption.
      apply IHG. assumption.
Qed.

Definition normal_form (t : tm) : Prop :=
  ~ exists t', t ⥇ t'.

Definition deterministic {tm : Type} (R : relation tm) :=
  forall x y1 y2 : tm, R x y1 -> R x y2 -> y1 = y2.

Notation "t1 '⥇*' t2" := (multierrstep t1 t2) (at level 40).

Lemma multi_err_step_regular: forall e1 e2, e1 ⥇* e2 -> lc e1 /\ lc e2.
Proof.
  intros.
  induction H; auto. destruct IHmultierrstep. split; auto. eapply err_step_regular1; eauto.
Qed.

Lemma multi_err_step_regular1: forall e1 e2, e1 ⥇* e2 -> lc e1.
Proof.
  intros. apply multi_err_step_regular in H. destruct H; auto.
Qed.

Lemma multi_err_step_regular2: forall e1 e2, e1 ⥇* e2 -> lc e2.
Proof.
  intros. apply multi_err_step_regular in H. destruct H; auto.
Qed.

Ltac err_step_regular_simp :=
  repeat match goal with
    | [H: _ ⥇ _ |- lc _] => apply err_step_regular in H; destruct H; auto
    | [H: _ ⥇ _ |- body _] => apply err_step_regular in H; destruct H; auto
    | [H: _ ⥇* _ |- lc _] => apply multi_err_step_regular in H; destruct H; auto
    | [H: _ ⥇* _ |- body _] => apply multi_err_step_regular in H; destruct H; auto
    end.
