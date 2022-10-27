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

Reserved Notation "Gamma '\C-' t1 '\<:' t2"  (at level 40).

(* subtyping judgement *)
Inductive is_subtype : context -> underty -> underty -> Prop :=
| Sub_UBase: forall Gamma T phi1 phi2,
    (forall e, under_tmR ([[v: T | phi1 ]]) e -> under_tmR ([[v: T | phi2 ]]) e) ->
    Gamma \C- [[v: T | phi1 ]] \<: [[v: T | phi2 ]]
| Sub_IndependArrow_IndependArrow: forall Gamma (tau11 tau21: underarrowty) (tau12 tau22: underty),
    Gamma \C- tau21 \<: tau11 ->
    Gamma \C- tau12 \<: tau22 ->
    Gamma \C- tau11 u--> tau12 \<: tau21 u--> tau22
| Sub_DependArrow_DependArrow: forall Gamma T x1 phi11 x2 phi21 (tau12 tau22: underty),
    (* We flip the overty to underty here *)
    Gamma \C- [[v: T | phi11 ]] \<: [[v: T | phi21 ]] ->
    (Gamma <l> x2 :l: ({{v: T | phi21 }})) \C- (under_subst_id x1 x2 tau12) \<: tau22 ->
    Gamma \C- x1 o: {{v: T | phi11 }} o--> tau12 \<: x2 o: {{v: T | phi21 }} o--> tau22

where "Gamma '\C-' t1 '\<:' t2" := (is_subtype Gamma t1 t2).

Lemma is_subtype_spec: forall Gamma t1 t2,
    is_subtype Gamma t1 t2 ->
    (forall e, tmR_in_ctx Gamma t1 e -> tmR_in_ctx Gamma t2 e).
Admitted.

(* closing judgement *)
Definition closing_with_drop (Gamma1 Gamma2: context) (tau: underty) :=
  well_formed Gamma1 tau /\ (forall e, tmR_in_ctx Gamma1 tau e <-> tmR_in_ctx (Gamma1 ++ Gamma2) tau e).

Notation "Gamma1 '\<' Gamma2 '\>' '\C-' tau " := (closing_with_drop Gamma1 Gamma2 tau) (at level 40).

(* disjunction judgement *)

Definition disjunct (Gamma: context) (tau1 tau2 tau3: underty) :=
  (forall e, tmR_in_ctx Gamma tau1 e /\ tmR_in_ctx Gamma tau2 e <-> tmR_in_ctx Gamma tau3 e).

Notation "Gamma '\C-' tau1 '\tyor' tau2 '\tyeq' tau3 " := (disjunct Gamma tau1 tau2 tau3) (at level 40).
