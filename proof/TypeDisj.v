Set Warnings "-notation-overridden,-parsing".
From CT Require Import Maps.
From CT Require Import CoreLang.
From CT Require Import NormalTypeSystem.
From CT Require Import LinearContext.
From CT Require Import RfTypeDef.
From CT Require Import TypeClosed.
From CT Require Import Denotation.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Lists.List.

Import CoreLang.
Import LinearContext.
Import TypeClosed.
Import Denotation.
Import ListNotations.

(* disjunction judgement *)

Definition disjunct (Gamma: context) (tau1 tau2 tau3: underty) :=
  (forall e, tmR_in_ctx_all_st Gamma tau1 e /\ tmR_in_ctx_all_st Gamma tau2 e <-> tmR_in_ctx_all_st Gamma tau3 e).

Notation "Gamma '\C-' tau1 '\tyor' tau2 '\tyeq' tau3 " := (disjunct Gamma tau1 tau2 tau3) (at level 40).

(* Lemma disj_same_ty: forall Gamma tau1 tau2 tau3, *)
(*   Gamma \C- tau1 \tyor tau2 \tyeq tau3 -> u\_ tau1 _/ =  u\_ tau3 _/ /\ u\_ tau2 _/ =  u\_ tau3 _/. *)
(* Proof with eauto. *)
(*   intros. *)
(*   split. edestruct H. *)
