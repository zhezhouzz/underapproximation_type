(** * MoreStlc: More on the Simply Typed Lambda-Calculus *)

Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From PLF Require Import CoreLangSimp.
From PLF Require Import NormalTypeSystemSimp.
From PLF Require Import LinearContext.
From PLF Require Import RfTypeDef.
From PLF Require Import TypeClosedSimp.
From PLF Require Import TermOrdering.
From PLF Require Import DenotationSimp.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Lists.List.
From Coq Require Import FunInd.
From Coq Require Import Recdef.

Import CoreLangSimp.
Import NormalTypeSystemSimp.
Import LinearContext.
Import RfTypeDef.
Import TypeClosedSimp.
Import NoDup.
Import DenotationSimp.
Import ListNotations.

(* Work only when there is no dup in Gamma! *)
Fixpoint erase_ctx (Gamma: lcontxt) :=
  match Gamma with
  | nil => empty
  | (x, tau)::Gamma => update (erase_ctx Gamma) x (ou\_ tau _/)
  end.

Lemma erase_included: forall Gamma, includedin empty (erase_ctx Gamma).
Admitted.

Global Hint Resolve erase_included: core.

Lemma no_dup_implies_ctx_lift: forall Gamma x tau_x e tau,
    type_ctx_no_dup empty (Gamma <l> x :l: tau_x) ->
    (erase_ctx (Gamma <l> x :l: tau_x)) \N- e \Tin tau <-> (erase_ctx ((x, tau_x) :: Gamma)) \N- e \Tin tau.
Admitted.

Global Hint Resolve no_dup_implies_ctx_lift: core.

Lemma over_eq_var_in_Gamma_has_type: forall Gamma x T phi,
    l_find_right_most Gamma x = Some (Oty ({{v: T | phi }})) -> (erase_ctx Gamma) \N- x \Vin T.
Admitted.

Global Hint Resolve over_eq_var_in_Gamma_has_type: core.

Lemma under_eq_var_in_Gamma_has_type: forall Gamma x T phi,
    l_find_right_most Gamma x = Some (Uty ([[v: T | phi ]])) -> (erase_ctx Gamma) \N- x \Vin T.
Admitted.

Global Hint Resolve under_eq_var_in_Gamma_has_type: core.

Lemma var_in_Gamma_has_type: forall Gamma x tau,
    l_find_right_most Gamma x = Some tau -> (erase_ctx Gamma) \N- x \Vin (ou\_ tau _/).
Admitted.

Global Hint Resolve var_in_Gamma_has_type: core.

Lemma tmR_in_ctx_aux_implies_has_type: forall Gamma st tau e,
    tmR_in_ctx_aux st Gamma tau e -> (erase_ctx Gamma) \N- e \Tin ou\_ tau _/.
Admitted.

Global Hint Resolve tmR_in_ctx_aux_implies_has_type: core.
