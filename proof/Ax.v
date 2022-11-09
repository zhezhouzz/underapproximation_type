Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From PLF Require Import Types.
(* From PLF Require Import Smallstep. *)
From PLF Require Import CoreLangSimp.
From PLF Require Import NormalTypeSystemSimp.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.

Import NormalTypeSystemSimp.

Definition state_permute {A:Type}: forall (st: string -> option A) x y (a a': A),
    (x |-> a; y |-> a'; st) = (y |-> a'; x |-> a; st).
Admitted.

Definition exists_not_free_var_in_tm: forall e, exists x, ~ x \FVtm e.
Admitted.

Lemma closed_term_has_no_free_var: forall e x T, empty \N- e \Tin T -> ~ x \FVtm e.
Admitted.

(* for vfix Axiom *)
Definition const_order: constant -> constant -> Prop.
Admitted.

Lemma const_order_is_well_founded: well_founded const_order.
Admitted.
