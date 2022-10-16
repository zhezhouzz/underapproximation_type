Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From PLF Require Import Smallstep.
From PLF Require Import CoreLang.
From PLF Require Import LinearContext.
From PLF Require Import NormalTypeSystem.
From PLF Require Import RefinementType.
From Coq Require Import Lists.List.
Import ListNotations.

(* Import RefinementType. *)

Lemma has_refinement_type_has_basic_type: forall (e: tm) (tau: uty),
    empty \C- e \T--> tau -> (Maps.empty |- e \in (erase tau)).
Admitted.

Global Hint Resolve has_refinement_type_has_basic_type: core.

Theorem soundness: forall (e: tm) (T: basic_ty) (phi: constant -> Prop),
    empty \C- e \T--> [[v: T | closed_r phi ]] ->
    forall c, T = constant_basic_type_infer c -> phi c -> e -->* (tvalue (vconst c)).
Proof with eauto.
  intros e T phi H c HcT. subst.
  assert (Maps.empty |- e \in (constant_basic_type_infer c )) as Hnt. apply has_refinement_type_has_basic_type in H...
  induction H; intros.
  - inversion Hnt; subst. destruct c; inversion H1. apply multi_R...
  - admit.
  - admit.
  - admit.
  - admit.
  - admit.
  - admit.
  - admit.
  - admit.
Admitted.
