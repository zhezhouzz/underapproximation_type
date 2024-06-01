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
