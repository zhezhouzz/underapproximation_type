Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From PLF Require Import CoreLangSimp.
From PLF Require Import NormalTypeSystemSimp.
From PLF Require Import LinearContext.
From PLF Require Import RfTypeDef.
From PLF Require Import WellFormedSimp.
From PLF Require Import DenotationSimp.
From PLF Require Import CtxInvariantSimp.
From PLF Require Import TypeDisj.
From PLF Require Import SubtypingSimp.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Lists.List.

Import CoreLangSimp.
Import LinearContext.
Import WellFormedSimp.
Import DenotationSimp.
Import CtxInvariantSimp.
Import TypeDisj.
Import SubtypingSimp.
Import ListNotations.

(* Definition apply_op_over_refinements op (phi1 phi2: refinement): refinement := *)
(*   (fun st v => *)
(*      forall n1 n2, phi1 st (cnat n1) -> phi2 st (cnat n2) -> *)
(*               v = (apply_op op n1 n2)). *)

(* Definition mk_op_retty op (phi1 phi2: refinement) := *)
(*   (BaseUnder (op_ret_ty op) (apply_op_over_refinements op phi1 phi2)). *)

(* Definition apply_op_from_cids (op: biop) (cid1 cid2: cid): tm := *)
(* (apply_op op n1 n2). *)

Definition const_order: constant -> constant -> Prop.
Admitted.

Lemma const_order_is_well_founded: well_founded const_order.
Admitted.

Reserved Notation "Gamma '\C-' t '\Tin' T" (at level 40).
Reserved Notation "Gamma '\C-' t '\Vin' T" (at level 40).

Inductive term_under_type_chek : context -> tm -> underty -> Prop :=
| UT_Value: forall Gamma v uty, Gamma \C- v \Vin uty -> Gamma \C- (tvalue v) \Tin uty
| UT_Exn: forall Gamma T, ctx_inv Gamma -> Gamma \C- texn \Tin (mk_bot T)
| UT_Sub: forall Gamma e tau1 tau2,
    l_empty \C- e \Tin tau1 -> l_empty  \C- tau1 \<: tau2 ->
    well_formed l_empty tau2 -> ctx_inv Gamma ->
    Gamma \C- e \Tin tau2
| UT_Eq: forall Gamma e tau1 tau2,
    Gamma \C- e \Tin tau1 ->
    Gamma \C- tau1 \<: tau2 -> Gamma \C- tau2 \<: tau1 ->
    well_formed Gamma tau2 -> ctx_inv Gamma ->
    Gamma \C- e \Tin tau2
| UT_Merge: forall Gamma e (tau1 tau2 tau3: underty),
    Gamma \C- e \Tin tau1 -> Gamma \C- e \Tin tau2 -> (Gamma \C- tau1 \tyor tau2 \tyeq tau3) ->
    well_formed l_empty tau3 ->
    Gamma \C- e \Tin tau3
| UT_Lete: forall (Gamma: context) x e_x e tau, forall tau_x,
    Gamma \C- e_x \Tin tau_x -> (Gamma <l> x :l: tau_x) \C- e \Tin tau ->
    well_formed Gamma tau ->
    Gamma \C- (tlete x e_x e) \Tin tau
| UT_LetOp: forall (Gamma: context) x op (v1 v2: cid) e tau, forall phi1 phi2,
    Gamma \C- v1 \Vin [[v: TNat | phi1 ]] ->
    Gamma \C- v2 \Vin [[v: TNat | phi2 ]] ->
    (Gamma <l> x :l: (mk_op_retty_from_cids op v1 v2)) \C- e \Tin tau ->
    well_formed Gamma tau ->
    Gamma \C- (tletbiop x op v1 v2 e) \Tin tau
| UT_LetAppIndepend: forall Gamma x v1 v2 e tau, forall tauarg tau_x,
    Gamma \C- v1 \Vin (tauarg u--> tau_x) -> Gamma \C- v2 \Vin tauarg ->
    (Gamma <l> x :l: tau_x) \C- e \Tin tau ->
    well_formed Gamma tau ->
    Gamma \C- (tletapp x v1 v2 e) \Tin tau
(* the value can only be constant or variables *)
| UT_LetAppDepend1: forall Gamma x v1 (c2: cid) e tau, forall T phi a tau_x,
    Gamma \C- v1 \Vin (a o: {{v: T | phi}} o--> tau_x) -> Gamma \C- c2 \Vin [[v: T | phi]] ->
   (Gamma <l> x :l:(under_subst_cid a c2 tau_x)) \C- e \Tin tau ->
    well_formed Gamma tau ->
    Gamma \C- (tletapp x v1 c2 e) \Tin tau
with value_under_type_check : context -> value -> underty -> Prop :=
| UT_Contant: forall Gamma c, ctx_inv Gamma -> Gamma \C- (vconst c) \Vin (mk_eq_constant c)
| UT_VarUnder: forall Gamma x (tau: underty),
    ctx_inv Gamma -> l_find_right_most Gamma x = Some (Uty tau) ->  Gamma \C- (vvar x) \Vin tau
| UT_VarOver: forall Gamma T x,
    ctx_inv Gamma -> (exists phi, l_find_right_most Gamma x = Some (Oty ({{v: T | phi}}))) ->
    Gamma \C- (vvar x) \Vin (mk_eq_var T x)
| UT_LamDep: forall Gamma x tau_x e tau,
    well_formed Gamma (x o: tau_x o--> tau) ->
    (Gamma <l> x :l: Oty tau_x) \C- e \Tin tau ->
                                          Gamma \C- (vlam x (o\_ tau_x _/) e) \Vin (x o: tau_x o--> tau)
| UT_LamIndep: forall Gamma x tau_x e tau,
    well_formed Gamma (tau_x u--> tau) ->
    (Gamma <l> x :l: Uty tau_x) \C- e \Tin tau ->
                                          Gamma \C- (vlam x (u\_ tau_x _/) e) \Vin (tau_x u--> tau)
where
"Gamma '\C-' t '\Tin' T" := (term_under_type_chek Gamma t T) and "Gamma '\C-' t '\Vin' T" := (value_under_type_check Gamma t T).

Scheme value_under_type_check_rec := Induction for value_under_type_check Sort Prop
    with term_under_type_chek_rec := Induction for term_under_type_chek Sort Prop.

(* Definition mk_op_retty_spec: forall Gamma op (cid1 cid2: cid) (phi1 phi2: refinement), *)
(*     ctx_inv Gamma -> *)
(*     Gamma \C- cid1 \Vin [[v:TNat | phi1]] -> *)
(*     Gamma \C- cid2 \Vin [[v:TNat | phi2]] -> *)
(*     tmR_in_ctx Gamma (mk_op_retty_from_cids op cid1 cid2) (apply_op op n1 n2). *)
(* Admitted. *)

Lemma type_judgement_implies_inv: forall Gamma e tau,
    Gamma \C- e \Tin tau -> ctx_inv Gamma /\ well_formed Gamma tau.
Admitted.

Lemma type_judgement_implies_basic_type_judgement: forall Gamma e tau,
    Gamma \C- e \Tin tau -> (lctx_to_basic_ctx Gamma) |- e \in u\_ tau _/.
Admitted.