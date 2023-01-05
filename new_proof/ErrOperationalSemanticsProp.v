From stdpp Require Import mapset.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Classical.
From CT Require Import CoreLangProp.
From CT Require Import BasicTypingProp.
From CT Require Import ErrOperationalSemantics.

Import Atom.
Import CoreLang.
Import Tactics.
Import NamelessTactics.
Import ListCtx.
Import OperationalSemantics.
Import BasicTyping.
Import BasicTypingProp.
Import ErrOperationalSemantics.

(* err perservation *)
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

Ltac progress_solver a :=
  match goal with
  | [H: ∃ e', _ ⥇ e' |- _ ] => right; left; destruct H
  | [H: exists e', _ ↪ e' |- _] => left; left; destruct H
  | [H: exists v, _ = tvalue v |- _] => left; left; destruct H; subst
  | [H: _ = terr |- _ ] => subst; right; left; eexists; eapply ErrST_Lete2
  | [ |- body _ ] => auto_exists_L; intro a; intros; repeat specialize_with a; basic_typing_solver2
  | [|- lc _] => basic_typing_solver2
  end.

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
  - left; left.
    assert (lc v1) by basic_typing_solver2.
    eapply empty_basic_typing_arrow_value_lam_exists in H.
    destruct_hyp_disj; destruct H; subst.
    + exists (tlete (x ^t^ v2) e). inversion H3; subst. econstructor; eauto; progress_solver a.
    + exists (tletapp ((vlam T1 x) ^v^ (vfix (T1 ⤍ Tx) (vlam T1 x))) v2 e ).
      inversion H3; subst. econstructor; eauto; progress_solver a.
  - apply empty_basic_typing_bool_value_exists in H.
    destruct H; subst; try clear H;
    left; left; eexists; econstructor; eauto; progress_solver a.
Qed.

Lemma err_tlete1: forall e, body e -> tlete terr e ⥇* terr.
Proof.
  intros.
  eapply multierr_step_step. instantiate (1:= terr). constructor; auto.
  apply multierr_step_refl. constructor.
Qed.

Lemma err_tlete2: forall e1 e, body e -> e1 ⥇* terr -> tlete e1 e ⥇* tlete terr e.
Proof.
  intros. generalize dependent e.
  induction H0; intros.
  - apply multierr_step_refl. destruct H0. econstructor; eauto.
  - eapply multierr_step_step. econstructor; eauto.
    apply IHmultierrstep; auto.
Qed.

Lemma err_reduction_halt: forall e e', e ⥇ e' -> e ⥇* terr /\ e' ⥇* terr.
Proof.
  intros.
  induction H; try destruct_hyp_conj; split; auto.
  - eapply multierr_step_trans. instantiate (1:= tlete terr e).
    apply err_tlete2; auto.
    apply err_tlete1; auto.
  - eapply multierr_step_trans. instantiate (1:= tlete terr e).
    apply err_tlete2; auto.
    apply err_tlete1; auto.
  - apply err_tlete1; auto.
Qed.

Lemma progress': forall e T, [] ⊢t e ⋮t T -> ((exists e', e ↪ e') \/ (exists v, e = tvalue v)) \/ (e ⥇* terr).
Proof.
  intros. apply progress in H. destruct H.
  - left; auto.
  - right. destruct H.
    + destruct H. apply err_reduction_halt in H. destruct H; auto.
    + subst. constructor; auto.
Qed.
