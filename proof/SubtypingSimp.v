Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From PLF Require Import CoreLangSimp.
From PLF Require Import NormalTypeSystemSimp.
From PLF Require Import LinearContext.
From PLF Require Import RfTypeDef.
From PLF Require Import TypeClosedSimp.
From PLF Require Import DenotationSimp.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Lists.List.

Import CoreLangSimp.
Import LinearContext.
Import TypeClosedSimp.
Import DenotationSimp.
Import ListNotations.

(* subtyping judgement *)
Inductive is_subtype : context -> underty -> underty -> Prop :=
| Sub_UBase: forall Gamma T phi1 phi2,
    (forall e, tmR_in_ctx_all_st Gamma ([[v: T | phi1 ]]) e -> tmR_in_ctx_all_st Gamma ([[v: T | phi2 ]]) e) ->
    is_subtype Gamma ([[v: T | phi1 ]]) ([[v: T | phi2 ]])
| Sub_IndependArrow_IndependArrow: forall Gamma (tau11 tau21: underty) (tau12 tau22: underty),
    is_subtype Gamma tau21 tau11 ->
    is_subtype Gamma tau12 tau22 ->
    is_subtype Gamma (tau11 u--> tau12) (tau21 u--> tau22)
| Sub_DependArrow_DependArrow: forall Gamma T x phi11 phi21 (tau12 tau22: underty),
    (* We flip the overty to underty here *)
    is_subtype Gamma ([[v: T | phi11 ]]) ([[v: T | phi21 ]]) ->
    is_subtype (Gamma <l> x :l: ({{v: T | phi21 }})) tau12 tau22 ->
    is_subtype Gamma (x o: {{v: T | phi11 }} o--> tau12) (x o: {{v: T | phi21 }} o--> tau22).

Notation "Gamma '\C-' t1 '\<:' t2" := (is_subtype Gamma t1 t2) (at level 40).

Lemma is_subtype_spec: forall Gamma t1 t2,
    is_subtype Gamma t1 t2 ->
    (forall e, tmR_in_ctx_all_st Gamma t1 e -> tmR_in_ctx_all_st Gamma t2 e).
Admitted.

(* (* closing judgement *) *)
(* Definition closing_with_drop (Gamma1 Gamma2: context) (tau: underty) := *)
(*   well_formed Gamma1 tau /\ (forall e, tmR_in_ctx Gamma1 tau e <-> tmR_in_ctx (Gamma1 ++ Gamma2) tau e). *)

(* Notation "Gamma1 '\<' Gamma2 '\>' '\C-' tau " := (closing_with_drop Gamma1 Gamma2 tau) (at level 40). *)
