From stdpp Require Import mapset.
From stdpp Require Import natmap.
From Coq.Program Require Import Wf.
From CT Require Import CoreLangClass.
From CT Require Import OperationalSemantics.
From CT Require Import BasicTypingClass.
From CT Require Import RefinementTypeClass.
From CT Require Import Instantiation.
From CT Require Import Denotation.

Import Atom.
Import CoreLang.
Import Tactics.
Import NamelessTactics.
Import ListCtx.
Import OperationalSemantics.
Import BasicTyping.
Import BasicTypingProp.
Import RefinementType.
Import Qualifier.
Import Instantiation.
Import Denotation.

Fixpoint ctxRst (Γ: listctx rty) (p: env -> Prop) :=
  match Γ with
  | [] => p ∅
  | (x, [: b | ϕ]) :: Γ => exists (v: value), ⟦ {: b | ϕ} ⟧ v /\ ctxRst Γ (fun Γv => p (<[x:=v]> Γv))
  | (x, {: b | ϕ}) :: Γ => forall (v: value), ⟦ {: b | ϕ} ⟧ v -> ctxRst Γ (fun Γv => p (<[x:=v]> Γv))
  | (x, ρx ⇨ τ) :: Γ => forall (v: value), ⟦ ρx ⇨ τ ⟧ v -> ctxRst Γ (fun Γv => p (<[x:=v]> Γv))
  end.

Notation "'⟪' Γ '⟫' p " := (ctxRst Γ p) (at level 20, format "⟪ Γ ⟫ p", Γ constr).

Lemma ctxRst_rewrite (Γ: listctx rty): forall (p1 p2: env -> Prop),
    (forall Γv, p1 Γv -> p2 Γv) -> ctxRst Γ p1 -> ctxRst Γ p2.
Admitted.

Inductive ctxRstRev: listctx rty -> (env -> Prop) -> Prop :=
| ctxRstRev_base: forall (p: env -> Prop), p ∅ -> ctxRstRev [] p
| ctxRstRev_under: forall Γ (p: env -> Prop) (x: atom) b ϕ,
  ctxRstRev Γ (fun Γv => exists (v: value), ⟦ [: b | m{Γv} ϕ] ⟧ v /\ p (<[x:=v]> Γv)) ->
  ctxRstRev (Γ ++ [(x, [: b | ϕ])]) p
| ctxRstRev_over: forall Γ (p: env -> Prop) (x: atom) b ϕ,
  ctxRstRev Γ (fun Γv => forall (v: value), ⟦ {: b | m{Γv} ϕ} ⟧ v -> p (<[x:=v]> Γv)) ->
  ctxRstRev (Γ ++ [(x, {: b | ϕ})]) p
| ctxRstRev_arr: forall Γ (p: env -> Prop) (x: atom) τ1 τ2,
    ctxRstRev Γ (fun Γv => forall (v: value), ⟦ m{Γv} (τ1 ⇨ τ2) ⟧ v -> p (<[x:=v]> Γv)) ->
    ctxRstRev (Γ ++ [(x, τ1 ⇨ τ2)]) p.

Global Hint Constructors ctxRstRev: core.

Lemma ctxRstRev_rewrite (Γ: listctx rty): forall (p1 p2: env -> Prop),
    (forall Γv, p1 Γv -> p2 Γv) -> ctxRstRev Γ p1 -> ctxRstRev Γ p2.
Admitted.

Lemma ctxRstRev_head_over (Γ: listctx rty): forall (p: env -> Prop) (x: atom) b ϕ,
    (forall (v: value), ⟦ {: b | ϕ} ⟧ v -> (ctxRstRev (map (fun t => (t.1, substitute x v t.2)) Γ) (λ Γv : env, p (<[x:= v]> Γv)))) ->
  ctxRstRev ((x, {:b|ϕ}) :: Γ) (λ Γv : env, p Γv).
Proof.
  induction Γ; intros.
  - assert ([(x, {:b|ϕ})] = [] ++ [(x, {:b|ϕ})]) as Htmp. list_simplifier. auto.
    rewrite Htmp. clear Htmp.
    constructor. constructor. intros. apply H in H0. simpl in *. admit.
  - 

    simpl in H.
  assert (forall Γv : env, ((fun Γv => forall v: value, (⟦{:b|ϕ}⟧) v → p (<[x := v]> Γv)) Γv) -> p Γv).
  { intros. }
  induction Γ; intros.
  - apply ctxRstRev_over.

Lemma CtxREq (Γ1 Γ2: listctx rty) (p: env -> Prop) :
  ctxRst (Γ1 ++ Γ2) p -> ctxRst Γ1 (fun Γv1 => ctxRstRev Γ2 (fun Γv2 => p (Γv1 ∪ Γv2))).
Proof.
  generalize dependent Γ1.
  induction Γ2; simpl; intros.
  - listctx_set_simpl. eapply ctxRst_rewrite; eauto. intros.
    constructor. rewrite map_union_empty; auto.
  - apply IHΓ2.
    assert (Γv ∪ ∅ = Γv). rewrite map_union_empty. auto. rewrite H1.  simplify_set_eq. naive_solver.

    generalize dependent p.
    assert (∀ Γv1 p, ctxRstRev [] (λ Γv2 : env, p (Γv1 ∪ Γv2)) = p Γv1). admit.
    induction Γ1; simpl in *. intuition.
    intros.
    auto_destruct_pair. rename a into x. destruct r.
    intros. apply H0 in H1. listctx_set_simpl.
    + intros. rewrite H0.


Lemma twoCtxREq (Γ: listctx rty) (p: env -> Prop) : ctxRst Γ p <-> ctxRstRev Γ p.
Proof.
  split.
  - admit.
  - induction Γ; simpl; intros. admit.
    auto_destruct_pair. rename a into x. destruct r.
    + intros.
  - induction Γ; simpl; intros.

