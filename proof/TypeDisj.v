Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From PLF Require Import CoreLangSimp.
From PLF Require Import NormalTypeSystemSimp.
From PLF Require Import LinearContext.
From PLF Require Import RfTypeDef.
From PLF Require Import WellFormedSimp.
From PLF Require Import DenotationSimp.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Lists.List.

Import CoreLangSimp.
Import LinearContext.
Import WellFormedSimp.
Import DenotationSimp.
Import ListNotations.

(* disjunction judgement *)

Definition disjunct (Gamma: context) (tau1 tau2 tau3: underty) :=
  (forall e, tmR_in_ctx Gamma tau1 e /\ tmR_in_ctx Gamma tau2 e <-> tmR_in_ctx Gamma tau3 e).

Notation "Gamma '\C-' tau1 '\tyor' tau2 '\tyeq' tau3 " := (disjunct Gamma tau1 tau2 tau3) (at level 40).
