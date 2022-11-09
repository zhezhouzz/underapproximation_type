Set Warnings "-notation-overridden,-parsing".
From CT Require Import Maps.
From CT Require Import CoreLang.
From CT Require Import NormalTypeSystem.
From CT Require Import LinearContext.
From CT Require Import RfTypeDef.
From CT Require Import TypeClosed.
From CT Require Import Denotation.
From CT Require Import CtxErase.
From CT Require Import WellFormed.
From CT Require Import TypeDisj.
From CT Require Import Subtyping.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Lists.List.

Import CoreLang.
Import LinearContext.
Import NoDup.
Import Ax.
Import TypeClosed.
Import Denotation.
Import CtxErase.
Import WellFormed.
Import TypeDisj.
Import Subtyping.
Import ListNotations.

(* Definition apply_op_over_refinements op (phi1 phi2: refinement): refinement := *)
(*   (fun st v => *)
(*      forall n1 n2, phi1 st (cnat n1) -> phi2 st (cnat n2) -> *)
(*               v = (apply_op op n1 n2)). *)

(* Definition mk_op_retty op (phi1 phi2: refinement) := *)
(*   (BaseUnder (op_ret_ty op) (apply_op_over_refinements op phi1 phi2)). *)

(* Definition apply_op_from_cids (op: biop) (cid1 cid2: cid): tm := *)
(* (apply_op op n1 n2). *)

Reserved Notation "Gamma '\C-' t '\Tin' T" (at level 40).
Reserved Notation "Gamma '\C-' t '\Vin' T" (at level 40).

Inductive term_under_type_chek : context -> tm -> underty -> Prop :=
| UT_Value: forall Gamma v uty, Gamma \C- v \Vin uty -> Gamma \C- (tvalue v) \Tin uty
| UT_Exn: forall Gamma T, well_formed Gamma (mk_bot T) -> Gamma \C- texn \Tin (mk_bot T)
| UT_Sub: forall Gamma e (tau1 tau2:underty),
    well_formed Gamma tau2 ->
    (* empty \N- e \Tin ou\_ tau1 _/ -> empty \N- e \Tin ou\_ tau2 _/ -> *)
    l_empty \C- e \Tin tau1 -> l_empty \C- tau1 \<: tau2 ->
    Gamma \C- e \Tin tau2
| UT_Eq: forall Gamma e tau1 tau2,
    well_formed Gamma tau2 ->
    Gamma \C- e \Tin tau1 ->
    Gamma \C- tau1 \<: tau2 -> Gamma \C- tau2 \<: tau1 ->
    Gamma \C- e \Tin tau2
| UT_Merge: forall Gamma e (tau1 tau2 tau3: underty),
    well_formed Gamma tau3 ->
    u\_ tau1 _/ =  u\_ tau3 _/ -> u\_ tau2 _/ =  u\_ tau3 _/ ->
    Gamma \C- e \Tin tau1 -> Gamma \C- e \Tin tau2 -> (Gamma \C- tau1 \tyor tau2 \tyeq tau3) ->
    Gamma \C- e \Tin tau3
| UT_Lete: forall (Gamma: context) x e_x e tau, forall tau_x,
    well_formed Gamma tau ->
    Gamma \C- e_x \Tin tau_x -> (Gamma <l> x :l: tau_x) \C- e \Tin tau ->
    Gamma \C- (tlete x e_x e) \Tin tau
(* operators only take nat type *)
| UT_LetOp: forall (Gamma: context) x op (v1 v2: cid) e tau, forall phi1 phi2,
    well_formed Gamma tau ->
    Gamma \C- v1 \Vin ([[v: fst_ty_of_op op | phi1 ]]) ->
    Gamma \C- v2 \Vin ([[v: snd_ty_of_op op | phi2 ]]) ->
    (Gamma <l> x :l: (mk_op_retty_from_cids op v1 v2)) \C- e \Tin tau ->
    Gamma \C- (tletbiop x op v1 v2 e) \Tin tau
| UT_LetAppIndepend: forall Gamma x v1 v2 e tau, forall tauarg tau_x,
    well_formed Gamma tau ->
    Gamma \C- v1 \Vin (tauarg u--> tau_x) -> Gamma \C- v2 \Vin tauarg ->
    (Gamma <l> x :l: tau_x) \C- e \Tin tau ->
    Gamma \C- (tletapp x v1 v2 e) \Tin tau
(* the value can only be constant or variables *)
| UT_LetAppDepend: forall Gamma x v1 (c2: cid) e tau, forall T phi a tau_x,
    well_formed Gamma tau ->
    Gamma \C- v1 \Vin (a o: {{v: T | phi}} o--> tau_x) -> Gamma \C- c2 \Vin ([[v: T | phi]]) ->
    (Gamma <l> x :l:(under_subst_cid a c2 tau_x)) \C- e \Tin tau ->
    Gamma \C- (tletapp x v1 c2 e) \Tin tau
| UT_Matchb_true: forall Gamma v e1 e2 tau,
    well_formed Gamma tau ->
    Gamma \C- v \Vin (mk_eq_constant true) ->
    Gamma \C- e1 \Tin tau -> (erase_ctx Gamma) \N- e2 \Tin ou\_ tau _/ ->
    Gamma \C- (tmatchb v e1 e2) \Tin tau
| UT_Matchb_false: forall Gamma v e1 e2 tau,
    well_formed Gamma tau ->
    Gamma \C- v \Vin (mk_eq_constant false) ->
    Gamma \C- e2 \Tin tau -> (erase_ctx Gamma) \N- e1 \Tin ou\_ tau _/ ->
    Gamma \C- (tmatchb v e1 e2) \Tin tau
with value_under_type_check : context -> value -> underty -> Prop :=
| UT_Contant: forall Gamma (c: constant),
    well_formed Gamma (mk_eq_constant c) -> Gamma \C- (vconst c) \Vin (mk_eq_constant c)
| UT_Op: forall Gamma (op: biop) a b,
    well_formed Gamma (mk_op op a b) -> a <> b -> Gamma \C- (vbiop op) \Vin (mk_op op a b)
| UT_VarOver: forall Gamma T x,
    well_formed Gamma (mk_eq_var T x) -> (exists phi, l_find_right_most Gamma x = Some (Oty ({{v: T | phi}}))) ->
    Gamma \C- (vvar x) \Vin (mk_eq_var T x)
| UT_VarUnderBase: forall Gamma T x,
    well_formed Gamma (mk_eq_var T x) -> (exists phi, l_find_right_most Gamma x = Some (Uty ([[v: T | phi]]))) ->
    Gamma \C- (vvar x) \Vin (mk_eq_var T x)
| UT_VarUnderArrArr: forall Gamma x (t1 t2: underty),
    well_formed Gamma (t1 u--> t2) -> l_find_right_most Gamma x = Some (Uty (t1 u--> t2)) -> Gamma \C- (vvar x) \Vin (t1 u--> t2)
| UT_VarUnderOArr: forall Gamma x a tau_a (tau_b: underty),
    well_formed Gamma (a o: tau_a o--> tau_b) -> l_find_right_most Gamma x = Some (Uty (a o: tau_a o--> tau_b)) ->
    Gamma \C- (vvar x) \Vin (a o: tau_a o--> tau_b)
| UT_LamDep: forall Gamma x tau_x e tau,
    well_formed Gamma (x o: tau_x o--> tau) ->
    (Gamma <l> x :l: Oty tau_x) \C- e \Tin tau ->
    Gamma \C- (vlam x (o\_ tau_x _/) e) \Vin (x o: tau_x o--> tau)
| UT_LamIndep: forall Gamma x t1 t2 e tau,
    well_formed Gamma ((t1 u--> t2) u--> tau) ->
    (Gamma <l> x :l: Uty (t1 u--> t2)) \C- e \Tin tau ->
    Gamma \C- (vlam x (u\_ (t1 u--> t2) _/) e) \Vin ((t1 u--> t2) u--> tau)
| UT_LamFix: forall Gamma x T phi f e tau,
    x <> f ->
    well_formed Gamma (x o: {{v:T | phi}} o--> tau) ->
    Gamma \C- (vlam x T (vlam f (T t--> u\_ tau _/) e)) \Vin (x o: {{v:T | phi}} o--> ((x o: {{v:T | well_founded_constraint x phi}} o--> tau) u--> tau)) ->
    Gamma \C- (vfix f (T t--> u\_ tau _/) x T e) \Vin (x o: {{v:T | phi}} o--> tau)
where
"Gamma '\C-' t '\Tin' T" := (term_under_type_chek Gamma t T) and "Gamma '\C-' t '\Vin' T" := (value_under_type_check Gamma t T).

Scheme value_under_type_check_rec := Induction for value_under_type_check Sort Prop
    with term_under_type_chek_rec := Induction for term_under_type_chek Sort Prop.

Global Hint Constructors term_under_type_chek: core.
Global Hint Constructors value_under_type_check: core.

Lemma type_judgement_implies_inv: forall Gamma e tau,
    Gamma \C- e \Tin tau -> well_formed Gamma tau.
Proof.
  intros.
  induction H; auto.
  - inversion H; auto.
Qed.

Global Hint Resolve type_judgement_implies_inv: core.

Lemma type_judgement_implies_no_dup: forall Gamma e tau,
    Gamma \C- e \Tin tau -> type_ctx_no_dup empty Gamma.
Proof.
  intros. apply type_judgement_implies_inv in H... destruct H... apply ctx_inv_implies_no_dup in H...
  rewrite nstate_to_tystate_empty in H... apply H.
Qed.

Global Hint Resolve type_judgement_implies_no_dup: core.

(* Lemma lam_has_type_tm_to_value: forall Gamma y T' e tau0, *)
(*     Gamma \C- vlam y T' e \Tin tau0 -> Gamma \C- vlam y T' e \Vin tau0. *)
(* Proof with eauto. *)
(*   intros. *)
(*   assert (well_formed_type tau0). apply type_judgement_implies_inv in H... destruct H... *)
(*   induction tau0; try inversion H4; subst. *)
(*   - inversion H. *)
(*     inversion H; subst. *)
(*     + inversion H3... *)
(*     + inversion H3; subst. inversion H8... *)
(*     + *)
(*     + apply subtyping_same_ty in H3. simpl in H3. subst. *)

(*   inversion H; subst... *)
(*   - destruct H0. assert (well_formed_type tau0)... *)
(*     induction tau0; try inversion H4; subst. *)
(*     + inversion H1; subst. *)
(*     induction H4... *)

(*     destruct H3. *)

(*     eapply is_subtype_spec... in H2... *)
Lemma type_judgement_implies_lam_var_not_in_Gamma: forall Gamma x T e tau,
    Gamma \C- vlam x T e \Vin tau -> l_find_right_most Gamma x = None.
Proof with eauto.
  intros...
  inversion H; subst.
  + apply type_judgement_implies_inv in H6. destruct H6...
  + apply type_judgement_implies_inv in H6. destruct H6...
Qed.

Lemma type_judgement_implies_basic_type_judgement: forall Gamma e tau,
    Gamma \C- e \Tin tau -> (erase_ctx Gamma) \N- e \Tin u\_ tau _/.
Proof with eauto.
  apply (term_under_type_chek_rec (fun Gamma v tau HvT => (erase_ctx Gamma) \N- v \Vin u\_ tau _/));
    (try eapply mk_op_has_type; eauto); intros...
  - simpl...
  - destruct e... apply over_eq_var_in_Gamma_has_type in H...
  - destruct e... apply under_eq_var_in_Gamma_has_type in H...
  - apply var_in_Gamma_has_type in e...
  - eapply var_in_Gamma_has_type in e...
  - assert (type_ctx_no_dup empty (Gamma <l> x :l: tau_x)). apply type_judgement_implies_no_dup in t...
    erewrite no_dup_implies_ctx_lift in H... destruct tau_x... simpl...
  - destruct w. constructor...
    assert (type_ctx_no_dup empty (Gamma <l> x :l: (t1 u--> t2))). apply type_judgement_implies_no_dup in t...
    erewrite no_dup_implies_ctx_lift in H...
  - destruct w. constructor... simpl in H. inversion H; subst. inversion H4; subst. inversion H5; subst.
    rewrite state_permute...
  - apply subtyping_same_ty in i. simpl in i. rewrite <- i. eapply weakening...
  - apply subtyping_same_ty in i. simpl in i. rewrite <- i...
  - rewrite <- e1...
  - assert (type_ctx_no_dup empty (Gamma <l> x :l: tau_x)). apply type_judgement_implies_no_dup in t0...
    erewrite no_dup_implies_ctx_lift in H0...
  - assert (type_ctx_no_dup empty (Gamma <l> x :l: mk_op_retty_from_cids op v1 v2)). apply type_judgement_implies_no_dup in t...
    erewrite no_dup_implies_ctx_lift in H1...
  - assert (type_ctx_no_dup empty (Gamma <l> x :l: tau_x)). apply type_judgement_implies_no_dup in t...
    erewrite no_dup_implies_ctx_lift in H1...
  - assert (type_ctx_no_dup empty (Gamma <l> x :l: under_subst_cid a c2 tau_x)). apply type_judgement_implies_no_dup in t...
    erewrite no_dup_implies_ctx_lift in H1... simpl in H1... rewrite under_subst_cid_preserve_ty in H1...
Qed.
