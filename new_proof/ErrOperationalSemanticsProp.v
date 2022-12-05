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

Lemma progress: forall Γ e T, Γ ⊢t e ⋮t T -> ((exists e', e ↪ e') \/ (exists v', e = tvalue v)) \/ ((exists e', e ⥇ e') \/ (e = terr)).
