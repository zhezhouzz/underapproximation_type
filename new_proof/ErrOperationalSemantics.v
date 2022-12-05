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

Lemma err_preservation: forall Γ T (e e': tm), e ⥇ e' -> Γ ⊢t e ⋮t T -> Γ ⊢t e' ⋮t T.
Proof.
  intros. generalize dependent e'.
  induction H0; intros; auto;
    match goal with
    | [H: _ ⥇ _ |- _ ] => inversion H; subst
    end; auto.
  - econstructor; auto; instantiate_atom_listctx; auto_apply; listctx_set_solver.
  - constructor. basic_typing_solver2.
Qed.

Lemma multi_err_preservation: forall Γ T (e e': tm), e ⥇* e' -> Γ ⊢t e ⋮t T -> Γ ⊢t e' ⋮t T.
Proof.
  intros.
  induction H; auto.
  - apply IHmultierrstep. eapply err_preservation; eauto.
Qed.

Ltac destruct_hyp_disj :=
  repeat match goal with
    | [H: _ \/ _ |- _ ] => destruct H
    end.

Ltac progress_solver a :=
  (* repeat match goal with *)
  (*   (* | [H: forall x, _ -> (_ \/ _) \/ (_ \/ _) |- _ ] => auto_pose_fv a; repeat specialize_with a *) *)
  (*   | [H: _ \/ _ |- _ ] => destruct H *)
  (* end; *)
  match goal with
  | [H: ∃ e', _ ⥇ e' |- _ ] => right; left; destruct H
  | [H: exists e', _ ↪ e' |- _] => left; left; destruct H
  | [H: exists v, _ = tvalue v |- _] => left; left; destruct H; subst
  | [H: _ = terr |- _ ] => subst; right; left; eexists; eapply ErrST_Lete2
  | [ |- body _ ] => auto_exists_L; intro a; intros; repeat specialize_with a; basic_typing_solver2
  | [|- lc _] => basic_typing_solver2
  end.

Lemma empty_basic_typing_base_const_exists: forall (v: value) (B: base_ty), [] ⊢t v ⋮v B -> (exists (c: constant), v = c).
Proof.
  intros. inversion H; subst.
  - exists c; auto.
  - inversion H1.
Qed.

Lemma empty_basic_typing_bool_value_exists: forall (v: value), [] ⊢t v ⋮v TBool -> v = true \/ v = false.
Proof.
  intros. inversion H; subst.
  - destruct c; inversion H0. destruct b; inversion H0. left; auto. right; auto.
  - inversion H1.
Qed.

Lemma empty_basic_typing_int_value_exists: forall (v: value), [] ⊢t v ⋮v TNat -> (exists (i: nat), v = i).
Proof.
  intros. inversion H; subst.
  - destruct c; inversion H0. exists n; auto.
  - inversion H1.
Qed.

Lemma empty_basic_typing_eval_op_result_exists: forall (op:biop) (v1 v2: value) (T1 T2 Tx: base_ty),
    [] ⊢t op ⋮v T1 ⤍ T2 ⤍ Tx -> [] ⊢t v2 ⋮v T2 -> [] ⊢t v1 ⋮v T1 ->
    exists (c1 c2 c3: constant), v1 = c1 /\ v2 = c2 /\ eval_op op c1 c2 c3 /\ [] ⊢t c3 ⋮v Tx.
Proof.
  intros.
  inversion H; subst.
  assert (exists (c1: constant), v1 = c1) as Htmp. eapply empty_basic_typing_base_const_exists; eauto. destruct Htmp; subst.
  assert (exists (c2: constant), v2 = c2) as Htmp. eapply empty_basic_typing_base_const_exists; eauto. destruct Htmp; subst.
  destruct op;
    simpl in H0; apply empty_basic_typing_int_value_exists in H0; destruct H0; subst;
    simpl in H1; apply empty_basic_typing_int_value_exists in H1; destruct H1; subst.
  - exists x2, x1, (x2 + x1). repeat split; auto. econstructor; auto.
  - exists x2, x1, (Nat.eqb x2 x1). repeat split; auto. econstructor; auto.
  - exists x2, x1, (Nat.ltb x2 x1). repeat split; auto. econstructor; auto.
  - exists x2, x1, 0. repeat split; auto. econstructor; auto.
Qed.

Lemma empty_basic_typing_arrow_value_lam_exists: forall (v: value) T1 T2, [] ⊢t v ⋮v T1 ⤍ T2 ->
                                                                     (exists (op:biop), v = op) \/
                                                                     (exists e, v = vlam T1 e) \/ (exists e, v = vfix (T1 ⤍ T2) (vlam T1 e)).
Proof.
  intros. inversion H; subst.
  - left. exists op; auto.
  - inversion H1.
  - right; left. exists e; auto.
  - right; right; exists e; auto.
Qed.

Lemma progress: forall e T, [] ⊢t e ⋮t T -> ((exists e', e ↪ e') \/ (exists v, e = tvalue v)) \/ ((exists e', e ⥇ e') \/ (e = terr)).
Proof.
  intros e T H. remember [] as Γ; induction H; intros; subst;
    repeat match goal with
      | [H: [] = [] -> _ |- _ ] =>
          assert (([]: listctx ty) = []) as Htmp by auto; specialize (H Htmp); try clear Htmp
      end.
  - right; right; auto.
  - left; right; exists v; auto.
  - destruct_hyp_disj.
    + repeat progress_solver a. eexists; econstructor; eauto; progress_solver a.
    + progress_solver a; exists (e2 ^t^ x); constructor; auto; progress_solver a.
    + progress_solver a; eexists; econstructor; eauto; progress_solver a.
    + repeat progress_solver a.
  - left; left. eapply empty_basic_typing_eval_op_result_exists in H1; eauto.
    destruct H1 as (c1 & c2 & c3 & Hc). repeat destruct_hyp_conj; subst.
    exists (e ^t^ c3). econstructor; eauto; progress_solver a.
  - left; left. eapply empty_basic_typing_arrow_value_lam_exists in H.
    destruct_hyp_disj; destruct 
    + 
    destruct H.
  - apply empty_basic_typing_bool_value_exists in H.
    destruct H; subst; try clear H;
    left; left; eexists; econstructor; eauto; progress_solver a.
  
    + clear IHtm_has_type2. left; left; eexists; econstructor; eauto; progress_solver a.
    + 

      left; left; eexists; econstructor; eauto; progress_solver a.
      left; left; eexists; econstructor; eauto; progress_solver a.
      subst.




    match goal with
    | [H: [] = [] -> _ |- _ ] =>
        assert (([]: listctx ty) = []) as Htmp by auto; specialize (H Htmp); try clear Htmp
    end. left; left. admit.
  - left; left. admit.
  - admit.
  - admit.
  - admit.
  - admit.
  - admit.
  

    progress_solver a.
    eexists; econstructor; eauto. progress_solver a.
  - progress_solver a.
    exists (e2 ^t^ x); constructor; auto; progress_solver a.
  - progress_solver a.
    eexists; econstructor; eauto; progress_solver a.
  -  match goal with
     
     end. 
  - auto_pose_fv a; repeat specialize_with a.
    repeat match goal with
    | [H: _ \/ _ |- _ ] => destruct H
    end.
    progress_solver a.

    eexists; econstructor; eauto. auto_exists_L. intros. repeat specialize_with x0. basic_typing_solver2.
