Set Warnings "-notation-overridden,-parsing".
From CT Require Import Maps.
From CT Require Import CoreLang.
From Coq Require Import Lists.List.
From Coq Require Import Strings.String.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Logic.Classical.
From CT Require Import NormalTypeSystem.
From CT Require Import RfTypeDef.
From CT Require Import LinearContext.
From CT Require Import NoDup.
Import NoDup.
Import Ax.
Import ListNotations.

(* closed_refinement *)


Definition state_order (st st': state) := (forall x c_x, st x = Some c_x -> st' x = Some c_x).
Notation " st '<st<' st' " := (state_order st st') (at level 80).

Lemma state_order_trans: forall st1 st2 st3, st1 <st< st2 -> st2 <st< st3 -> st1 <st< st3.
Proof.
  unfold state_order. intros. auto.
Qed.

Global Hint Resolve state_order_trans: core.


Definition closed_refinement_under_state (st: state) (phi: refinement): Prop :=
  forall x, st x = None -> ~ appear_free_in_refinement x phi.
  (* forall st', st <st< st' -> phi st = phi st'. *)

Global Hint Unfold closed_refinement_under_state: core.

Definition empstate : state := empty.

Lemma empstate_min: forall st, empstate <st< st.
Proof.
  intros. intros x c_x H. inversion H.
Qed.

Global Hint Resolve empstate_min: core.

(* Lemma state_order_implies_closed: forall st st' phi, *)
(*     st <st< st' -> closed_refinement_under_state st phi -> closed_refinement_under_state st' phi. *)
(* Proof with eauto. *)
(*   unfold closed_refinement_under_state. intros. rewrite <- H0... *)
(* Qed. *)

(* Global Hint Resolve state_order_implies_closed: core. *)

Lemma ZNNPP : forall p:Prop, ~ ~ p <-> p.
Proof with eauto.
  split. intros. apply NNPP...
  intros. intros HH. apply HH...
Qed.

Lemma not_appear_free_in_refinement_alt (name: string) (phi: refinement):
  ~ appear_free_in_refinement name phi <-> (forall st c1, phi st = phi (update st name c1)).
Proof with eauto.
  unfold appear_free_in_refinement. rewrite ZNNPP... reflexivity...
Qed.

Lemma constant_refinement_is_closed: forall (c: constant) (phi: refinement),
    (forall st : state, phi st = (fun c' => c = c')) -> (closed_refinement_under_state empstate phi).
Proof.
  intros.
  intros st HH. rewrite not_appear_free_in_refinement_alt. intros. rewrite H. rewrite H. auto.
Qed.

Lemma refinement_subst_c_closed_rf: forall x1 c2 phi,
    closed_refinement_under_state empstate phi -> phi = refinement_subst_c x1 c2 phi.
Proof with eauto.
  intros.
  assert (forall x, ~ appear_free_in_refinement x phi)... setoid_rewrite not_appear_free_in_refinement_alt in H0.
  unfold refinement_subst_c.
  apply functional_extensionality. intros st. rewrite <- H0...
Qed.

(* closed_type *)

(* Definition phi_sat_tyst (phi: refinement) (tyst: tystate) (c: constant) := *)
(*   (forall st, st \TYSTin tyst -> closed_refinement_under_state st phi /\ phi st c). *)

Definition phi_sat_tyst (phi: refinement) (tyst: tystate) :=
  (forall x, tyst x = None -> ~ appear_free_in_refinement x phi).
  (* (forall st, st \TYSTin tyst -> closed_refinement_under_state st phi /\ phi st c). *)

(* Global Hint Unfold nst_tmR_aux: core. *)

Definition lcontxt := linear_context overunderty.

Inductive st_type_closed : tystate -> overunderty -> Prop :=
| st_type_closed_base1: forall tyst (T: base_ty) phi,
    phi_sat_tyst phi tyst -> st_type_closed tyst ({{v: T | phi}})
| st_type_closed_base2: forall tyst (T: base_ty) phi,
    phi_sat_tyst phi tyst -> st_type_closed tyst ([[v: T | phi]])
| st_type_closed_oarr: forall st x T phi (tau: underty),
    well_formed_type (x o: ({{v: T | phi}}) o--> tau) ->
    (* st x = None -> *)
    st_type_closed st ({{v: T | phi}}) ->
    st_type_closed (x |-> T; st) tau ->
    st_type_closed st (x o: ({{v: T | phi}}) o--> tau)
| st_type_closed_arrarr: forall st (tau_x: underty) (tau: underty),
    well_formed_type (tau_x u--> tau) ->
    st_type_closed st tau_x ->
    st_type_closed st tau ->
    st_type_closed st (tau_x u--> tau).

Global Hint Constructors st_type_closed: core.

Lemma st_type_closed_implies_well_formed_type:
  forall tyst tau, st_type_closed tyst tau -> well_formed_type tau.
Proof with eauto.
  intros.
  induction H...
Qed.

Global Hint Resolve st_type_closed_implies_well_formed_type: core.

Fixpoint st_type_closed_in_ctx_aux (tyst: tystate) (Gamma: lcontxt) (tau: overunderty) : Prop :=
  match Gamma with
   | nil => st_type_closed tyst tau
   | (x, Oty ({{v:T | _}})) :: Gamma | (x, Uty ([[v:T | _]])) :: Gamma =>
                                        st_type_closed_in_ctx_aux (update tyst x T) Gamma tau
   | (_, _) :: Gamma =>
       st_type_closed_in_ctx_aux tyst Gamma tau
  end.

Definition st_type_closed_in_ctx (tyst: tystate) (Gamma: lcontxt) (tau: overunderty) : Prop :=
  st_type_closed_in_ctx_aux tyst Gamma tau /\
    well_formed_type tau /\ type_ctx_no_dup tyst Gamma.

Lemma st_type_closed_in_ctx_bot: forall T, st_type_closed_in_ctx empty l_empty (mk_bot T).
Proof with eauto.
  intros. constructor...
  - simpl. constructor... intro x. intros.
    rewrite not_appear_free_in_refinement_alt. intros...
  - split... constructor...
Qed.

Lemma st_type_closed_in_ctx_destruct_underbase_front : forall Gamma tyst x T phi tau,
    st_type_closed_in_ctx tyst ((x, Uty ([[v:T | phi]])) :: Gamma) tau ->
    st_type_closed_in_ctx (x |-> T; tyst) Gamma tau.
Proof with eauto.
  intros Gamma tyst x T phi tau Hclosed.
  destruct Hclosed as (Hstclosed & Hwf & Hnodup_)... simpl in Hstclosed...
  repeat split... apply nodup_update in Hnodup_...
Qed.

Lemma st_type_closed_in_ctx_destruct_overbase_front : forall Gamma tyst x T phi tau,
    st_type_closed_in_ctx tyst ((x, Oty ({{v:T | phi}})) :: Gamma) tau ->
    st_type_closed_in_ctx (x |-> T; tyst) Gamma tau.
Proof with eauto.
  intros Gamma tyst x T phi tau Hclosed.
  destruct Hclosed as (Hstclosed & Hwf & Hnodup_)... simpl in Hstclosed...
  repeat split... apply nodup_update_over in Hnodup_...
Qed.

Global Hint Resolve st_type_closed_in_ctx_destruct_overbase_front: core.

Lemma st_type_closed_in_ctx_destruct_arrar_front : forall Gamma tyst x t1 t2 tau,
    st_type_closed_in_ctx tyst ((x, Uty (t1 u--> t2)) :: Gamma) tau ->
    st_type_closed_in_ctx tyst Gamma tau.
Proof with eauto.
  intros Gamma tyst x T phi tau Hclosed.
  destruct Hclosed as (Hstclosed & Hwf & Hnodup_)... simpl in Hstclosed...
  repeat split...
Qed.


Lemma st_type_closed_in_ctx_destruct_oarr_front : forall Gamma tyst x a Ta phia t2 tau,
    st_type_closed_in_ctx tyst ((x, Uty (a o: {{v:Ta | phia}} o--> t2)) :: Gamma) tau ->
    st_type_closed_in_ctx tyst Gamma tau.
Proof with eauto.
  intros Gamma tyst x a Ta phia t2 tau Hclosed.
  destruct Hclosed as (Hstclosed & Hwf & Hnodup_)... simpl in Hstclosed...
  repeat split...
Qed.

Lemma st_type_closed_in_ctx_one_underbase: forall tyst x T phi phi0,
    st_type_closed_in_ctx tyst ((x, Uty ([[v:T | phi]]))::nil) phi0 ->
    st_type_closed_in_ctx (x |-> T; tyst) [] phi0.
Proof with eauto.
  intros tyst x T phi tau Hclosed. destruct Hclosed as (Hstclosed & Hwf & Hnodup_)... simpl in Hstclosed...
  repeat split...
Qed.

Global Hint Resolve st_type_closed_in_ctx_one_underbase: core.

Lemma st_type_closed_in_ctx_one_overbase: forall tyst x T phi phi0,
    st_type_closed_in_ctx tyst ((x, Oty ({{v:T | phi}}))::nil) phi0 ->
    st_type_closed_in_ctx (x |-> T; tyst) [] phi0.
Proof with eauto.
  intros tyst x T phi tau Hclosed.  destruct Hclosed as (Hstclosed & Hwf & Hnodup_)... simpl in Hstclosed...
  repeat split...
Qed.

Global Hint Resolve st_type_closed_in_ctx_one_overbase: core.

Lemma st_type_closed_add: forall s T1 tau st,
    st_type_closed st tau ->
    st_type_closed (s |-> T1; st) tau.
Proof with eauto.
  intros s T1 tau st H.
  induction H.
  - constructor... intros x'. intros. apply H. destruct (eqb_spec s x'); subst...
    rewrite update_eq in H0. inversion H0.
    rewrite update_neq in H0...
  - constructor... intros x'. intros. apply H. destruct (eqb_spec s x'); subst...
    rewrite update_eq in H0. inversion H0.
    rewrite update_neq in H0...
  - constructor... destruct (eqb_spec s x); subst...
    + rewrite update_shadow...
    + rewrite update_permute...
  - constructor...
Qed.




(* Module ExCloseRefinement. *)

(*   Example true_refinement: closed_refinement_under_state empsta (fun _ c => c = 3). *)
(*   Proof. *)
(*     unfold closed_refinement_under_state. reflexivity. *)
(*   Qed. *)

(*   (* When the type dismatch, it is equal to bottom refinement *) *)
(*   Example true_refinement2: closed_refinement_under_state TNat (fun _ c => c = true). *)
(*   Proof. *)
(*     unfold closed_refinement_under_state. reflexivity. *)
(*   Qed. *)

(*   Example refinement_under_nctx: st_type_closed_in_ctx [("x"%string, TBase TNat)] TNat (fun st c => c = st "x"%string). *)
(*   Proof. *)
(*     constructor. intros. *)
(*     apply nat_value_n_exists in H. destruct H; subst. *)
(*     constructor. constructor; subst; auto. *)
(*   Qed. *)

(* End ExCloseRefinement. *)

(* Inductive st_type_closed_in_ctx : tystate -> lcontxt -> overunderty -> Prop := *)
(* | st_type_closed_base1: forall st Gamma (T: base_ty) phi, *)
(*     st_type_closed_in_ctx st Gamma phi -> st_type_closed_in_ctx st Gamma ({{v: T | phi}}) *)
(* | st_type_closed_base2: forall st Gamma (T: base_ty) phi, *)
(*     st_type_closed_in_ctx st Gamma phi -> st_type_closed_in_ctx st Gamma ([[v: T | phi]]) *)
(* | st_type_closed_oarr: forall st Gamma x T phi (tau: underty), *)
(*     well_formed_type (x o: ({{v: T | phi}}) o--> tau) -> *)
(*     st_type_closed_in_ctx st Gamma ({{v: T | phi}}) -> *)
(*     st_type_closed_in_ctx st ((x, Oty ({{v: T | phi}})) :: Gamma) tau -> *)
(*     st_type_closed_in_ctx st Gamma (x o: ({{v: T | phi}}) o--> tau) *)
(* | st_type_closed_arrarr: forall st Gamma (tau_x: underty) (tau: underty), *)
(*     well_formed_type (tau_x u--> tau) -> *)
(*     st_type_closed_in_ctx st Gamma tau_x -> *)
(*     st_type_closed_in_ctx st Gamma tau -> *)
(*     st_type_closed_in_ctx st Gamma (tau_x u--> tau). *)

(* Global Hint Constructors st_type_closed_in_ctx: core. *)

Lemma st_type_closed_in_ctx_implies_well_formed_type:
  forall tyst Gamma tau, st_type_closed_in_ctx tyst Gamma tau -> well_formed_type tau.
Proof with eauto.
  intros.
  destruct Gamma. simpl in H.
  destruct H. destruct H0...
  destruct H. destruct H0...
Qed.

Global Hint Resolve st_type_closed_in_ctx_implies_well_formed_type: core.

Lemma st_type_closed_in_ctx_implies_type_ctx_no_dup: forall st Gamma tau,
    st_type_closed_in_ctx st Gamma tau -> type_ctx_no_dup st Gamma.
Proof with eauto.
  intros.
  destruct Gamma. simpl in H.
  destruct H. destruct H0...
  destruct H. destruct H0...
Qed.

Global Hint Resolve st_type_closed_in_ctx_implies_type_ctx_no_dup: core.

Lemma st_type_closed_implies_over_not_free: forall x (tau:overbasety),
    st_type_closed empty tau ->
    ~ appear_free_in_overbasety x tau.
Proof with eauto.
  intros. inversion H; subst. simpl...
Qed.

Lemma st_type_closed_implies_not_free: forall x (tau:underty) st,
    st x = None ->
    st_type_closed st tau ->
    ~ appear_free_in_underty x tau.
Proof with eauto.
  intro x.
  induction tau; intros st Hst H...
  - inversion H; subst...
  - inversion H; subst... simpl. intro HF. destruct HF...
    + inversion H5; subst. apply H3 in H0...
    + destruct H0. eapply IHtau in H6... rewrite update_neq...
  - inversion H; subst... simpl. intro HF. destruct HF...
    + eapply  IHtau1  in H4...
    + eapply  IHtau2  in H5...
Qed.

Global Hint Resolve st_type_closed_implies_not_free: core.

Lemma st_type_closed_in_ctx_emp_implies_all_not_free: forall x Gamma (tau: underty) st,
    st x = None ->
    l_find_right_most Gamma x = None ->
    st_type_closed_in_ctx st Gamma tau ->
    ~ appear_free_in_underty x tau.
Proof with eauto.
  intro x.
  induction Gamma; intros tau st Hst Hfind HH...
  - inversion HH; subst...
  - destruct a.
    destruct o. destruct u.
    + apply st_type_closed_in_ctx_destruct_underbase_front in HH.
      eapply IHGamma in HH... rewrite update_neq...
    + destruct o. apply st_type_closed_in_ctx_destruct_oarr_front in HH.
      eapply IHGamma in HH; auto. eapply l_find_right_most_none_neq_tl in Hfind...
    + apply st_type_closed_in_ctx_destruct_arrar_front in HH.
      eapply IHGamma in HH; auto. eapply l_find_right_most_none_neq_tl in Hfind...
    + destruct o.
      apply st_type_closed_in_ctx_destruct_overbase_front in HH.
      eapply IHGamma in HH... rewrite update_neq...
Qed.

(* Global Hint Resolve st_type_closed_in_ctx_emp_implies_all_not_free: core. *)

(* Lemma st_type_closed_in_ctx_emp_implies_all: forall st Gamma tau, *)
(*     st_type_closed_in_ctx empty Gamma tau -> st_type_closed_in_ctx st Gamma tau. *)

(* Global Hint Resolve st_type_closed_in_ctx_emp_implies_all: core. *)

Lemma st_type_closed_in_ctx_implies_head_unique: forall Gamma st s tau_s tau,
    st_type_closed_in_ctx st ((s, tau_s) :: Gamma) tau ->
    l_find_right_most Gamma s = None.
Proof with eauto.
  intros...
Qed.

Lemma st_type_closed_in_ctx_perm_aux: forall tyst a tau_a b tau_b Gamma,
    a <> b ->
    (forall tau, st_type_closed_in_ctx_aux tyst ((a, tau_a)::(b, tau_b)::Gamma) tau ->
            st_type_closed_in_ctx_aux tyst ((b, tau_b)::(a, tau_a)::Gamma) tau).
Proof with eauto.
  intros tyst a tau_a b tau_b Gamma Hab. intros.
  destruct (destruct_tau tau_a); destruct (destruct_tau tau_b)...
  - destruct H0 as [(T1 & phi1 & Hsubst1)| (T1 & phi1 & Hsubst1)];
      destruct H1 as [(T2 & phi2 & Hsubst2)| (T2 & phi2 & Hsubst2)]; subst; simpl; simpl in H; try rewrite update_permute...
  - destruct H0 as [(T1 & phi1 & Hsubst1)| (T1 & phi1 & Hsubst1)];
      destruct H1 as [(n & Tn & phin & taun' & Hsubst2)| (t1 & t2 & Hsubst2)]; subst; simpl; simpl in H; try rewrite update_permute...
  - destruct H0 as [(n & Tn & phin & taun' & Hsubst2)| (t1 & t2 & Hsubst2)];
      destruct H1 as [(T1 & phi1 & Hsubst1)| (T1 & phi1 & Hsubst1)]; subst; simpl; simpl in H; try rewrite update_permute...
  - destruct H0 as [(n1 & Tn1 & phin1 & taun'1 & Hsubst1)| (t11 & t21 & Hsubst1)];
      destruct H1 as [(n2 & Tn2 & phin2 & taun'2 & Hsubst2)| (t12 & t22 & Hsubst2)]; subst; simpl; simpl in H...
Qed.

Lemma st_type_closed_in_ctx_perm: forall tyst a tau_a b tau_b Gamma,
    a <> b ->
    (forall tau, st_type_closed_in_ctx tyst ((a, tau_a)::(b, tau_b)::Gamma) tau ->
            st_type_closed_in_ctx tyst ((b, tau_b)::(a, tau_a)::Gamma) tau).
Proof with eauto.
  intros tyst a tau_a b tau_b Gamma Hab. intros.
  destruct H as (H1 & H2 & H3). apply st_type_closed_in_ctx_perm_aux in H1...
  split... split... apply nodup_permute...
Qed.

Global Hint Resolve st_type_closed_in_ctx_one_underbase: core.

Global Hint Resolve st_type_closed_in_ctx_one_overbase: core.

(* Lemma mk_op_is_type_closed: forall st op a b, st_type_closed_in_ctx st l_empty (mk_op op a b). *)

(* Global Hint Resolve mk_op_is_type_closed: core. *)

Lemma mk_eq_over_base_is_type_closed: forall x T phi, st_type_closed_in_ctx empty ((x, Oty ({{v:T | phi}}))::nil) (mk_eq_var T x).
Proof with eauto.
  intros x T phi. repeat constructor...
  unfold phi_sat_tyst. intros. intro HH.
  destruct (eqb_spec x x0); subst... rewrite update_eq in H. inversion H.
  unfold appear_free_in_refinement in HH. apply HH. intros. rewrite update_neq...
Qed.

Global Hint Resolve mk_eq_over_base_is_type_closed: core.

Lemma mk_eq_under_base_is_type_closed: forall x T phi, st_type_closed_in_ctx empty ((x, Uty ([[v:T | phi]]))::nil) (mk_eq_var T x).
Proof with eauto.
  intros x T phi. repeat constructor...
  unfold phi_sat_tyst. intros. intro HH.
  destruct (eqb_spec x x0); subst... rewrite update_eq in H. inversion H.
  unfold appear_free_in_refinement in HH. apply HH. intros. rewrite update_neq...
Qed.

Global Hint Resolve mk_eq_under_base_is_type_closed: core.

Lemma mk_eq_constant_spec (c: constant) : empty \N- vconst c \Tin u\_ mk_eq_constant c _/.
Proof with eauto.
  induction c; simpl; repeat constructor...
Qed.

Lemma subst_constant_eq: forall x c_x c, (<u[ x |c-> c_x ]> mk_eq_constant c) = mk_eq_constant c.
Proof.
  intros. simpl.
  assert (<r[ x |c-> c_x ]> (fun (_ : state) (v : constant) => v = c) = (fun (_ : state) (v : constant) => v = c)).
  unfold refinement_subst_c.
  apply functional_extensionality. intros. apply functional_extensionality. auto.
  setoid_rewrite H. auto.
Qed.

Lemma constant_is_wf: forall c, well_formed_type (mk_eq_constant c).
Proof with eauto.
  intros.
  destruct c; constructor...
Qed.

Global Hint Resolve constant_is_wf: core.

(* Global Hint Resolve nodup_dropfst: core. *)

Lemma constant_eq_is_close: forall Gamma st c,
    type_ctx_no_dup st Gamma ->
    st_type_closed_in_ctx st Gamma (mk_eq_constant c).
Proof with eauto.
  induction Gamma; intros...
  - repeat constructor...
    intros x' Hx'. rewrite not_appear_free_in_refinement_alt. intros...
  - destruct a.
    destruct o... destruct u...
    + constructor... apply IHGamma... eapply nodup_update...
    + constructor. apply IHGamma. apply nodup_dropfst in H... split...
    + constructor. apply IHGamma. apply nodup_dropfst in H... split...
    + destruct o... constructor. apply IHGamma. eapply nodup_update_over... split...
Qed.

Global Hint Resolve mk_eq_constant_spec: core.
Global Hint Resolve subst_constant_eq: core.
Global Hint Resolve constant_eq_is_close: core.

(* type closed lemmas *)

Lemma mk_eq_is_well_formed_type: forall T x, well_formed_type (mk_eq_var T x).
Proof with eauto.
  intros.
  constructor...
Qed.

Global Hint Resolve mk_eq_is_well_formed_type: core.

Lemma st_type_closed_in_ctx_add_binding: forall s T (tau: overunderty) (Gamma2: lcontxt) st,
    st_type_closed_in_ctx_aux st Gamma2 tau ->
    st_type_closed_in_ctx_aux (s |-> T; st) Gamma2 tau.
Proof with eauto.
  intros s T tau.
  induction Gamma2; intros st H... simpl in H. simpl. apply st_type_closed_add...
  destruct a.
  destruct (destruct_tau o).
  - destruct H0 as [(T1 & phi1 & Hsubst1)| (T1 & phi1 & Hsubst1)]; subst; simpl...
    + simpl in H.
      destruct (eqb_spec s0 s); subst...
      rewrite update_shadow... rewrite update_permute...
    + simpl in H.
      destruct (eqb_spec s0 s); subst...
      rewrite update_shadow... rewrite update_permute...
  - destruct H0 as [(n & Tn & phin & taun' & Hsubst2)| (t1 & t2 & Hsubst2)]; subst; simpl.
    + simpl in H. apply IHGamma2 in H; auto.
    + simpl in H. apply IHGamma2 in H; auto.
Qed.


(* Lemma closed_ctx_update_useless: forall Gamma s T1 st tau, *)
(*     l_find_right_most Gamma s = None -> *)
(*     st_type_closed_in_ctx_aux st Gamma tau -> *)
(*     st_type_closed_in_ctx_aux (s |-> T1; st) Gamma tau. *)


Lemma type_closed_add_post_aux: forall (Gamma2: lcontxt) st (tau: overunderty),
    type_ctx_no_dup st Gamma2 -> st_type_closed st tau -> st_type_closed_in_ctx_aux st Gamma2 tau.
Proof with eauto.
  induction Gamma2; intros st tau Hnodup HH...
  destruct a.
  destruct (destruct_tau o).
  - destruct H as [(T1 & phi1 & Hsubst1)| (T1 & phi1 & Hsubst1)]; subst; simpl...
    + apply IHGamma2 in HH. apply st_type_closed_in_ctx_add_binding... eapply type_ctx_no_dup_implies_tail in Hnodup...
    + apply IHGamma2 in HH. apply st_type_closed_in_ctx_add_binding... eapply type_ctx_no_dup_implies_tail in Hnodup...
  - destruct H as [(n & Tn & phin & taun' & Hsubst2)| (t1 & t2 & Hsubst2)]; subst; simpl.
    + apply IHGamma2 in HH; auto. eapply type_ctx_no_dup_implies_tail in Hnodup...
    + apply IHGamma2 in HH; auto. eapply type_ctx_no_dup_implies_tail in Hnodup...
Qed.

Lemma st_type_closed_in_ctx_aux_add_post: forall (Gamma1 Gamma2: lcontxt) st (tau: overunderty),
    type_ctx_no_dup st (Gamma1 ++ Gamma2) ->
    st_type_closed_in_ctx_aux st Gamma1 tau -> st_type_closed_in_ctx_aux st (Gamma1 ++ Gamma2) tau.
Proof with eauto.
  induction Gamma1; intros Gamma2 st tau Hnodup HH... apply type_closed_add_post_aux...
  destruct a.
  destruct (destruct_tau o).
  - destruct H as [(T1 & phi1 & Hsubst1)| (T1 & phi1 & Hsubst1)]; subst; simpl.
    + simpl in HH. apply IHGamma1 with (Gamma2:=Gamma2)in HH; auto.
      rewrite <- app_comm_cons in Hnodup. apply nodup_update_over in Hnodup...
    + simpl in HH. apply IHGamma1 with (Gamma2:=Gamma2)in HH; auto.
      rewrite <- app_comm_cons in Hnodup. apply nodup_update in Hnodup...
  - destruct H as [(n & Tn & phin & taun' & Hsubst2)| (t1 & t2 & Hsubst2)]; subst; simpl.
     + simpl in HH. apply IHGamma1 with (Gamma2:=Gamma2)in HH; auto.
      rewrite <- app_comm_cons in Hnodup. rewrite <- app_one_is_cons in Hnodup. apply type_ctx_no_dup_ctx_post in Hnodup...
     + simpl in HH. apply IHGamma1 with (Gamma2:=Gamma2)in HH; auto.
      rewrite <- app_comm_cons in Hnodup. rewrite <- app_one_is_cons in Hnodup. apply type_ctx_no_dup_ctx_post in Hnodup...
Qed.

Lemma type_closed_add_post: forall (Gamma1 Gamma2: lcontxt) st (tau: overunderty),
    type_ctx_no_dup st (Gamma1 ++ Gamma2) ->
    st_type_closed_in_ctx st Gamma1 tau -> st_type_closed_in_ctx st (Gamma1 ++ Gamma2) tau.
Proof with eauto.
  intros.
  destruct H0 as (HH & H1 & H2). split...
  apply st_type_closed_in_ctx_aux_add_post...
Qed.

Lemma st_type_closed_in_ctx_aux_add_pre: forall (Gamma1 Gamma2: lcontxt) st (tau: overunderty),
    type_ctx_no_dup st (Gamma1 ++ Gamma2) ->
    st_type_closed_in_ctx_aux st Gamma2 tau -> st_type_closed_in_ctx_aux st (Gamma1 ++ Gamma2) tau.
Proof with eauto.
  induction Gamma1; intros Gamma2 st tau Hnodup HH...
  destruct a.
  destruct (destruct_tau o).
  - destruct H as [(T1 & phi1 & Hsubst1)| (T1 & phi1 & Hsubst1)]; subst; simpl.
    + apply IHGamma1 with (Gamma2:=Gamma2)in HH; auto. eapply st_type_closed_in_ctx_add_binding in HH...
      rewrite <- app_comm_cons in Hnodup. rewrite <- app_one_is_cons in Hnodup. apply type_ctx_no_dup_ctx_post in Hnodup...
    + apply IHGamma1 with (Gamma2:=Gamma2)in HH; auto. eapply st_type_closed_in_ctx_add_binding in HH...
      rewrite <- app_comm_cons in Hnodup. rewrite <- app_one_is_cons in Hnodup. apply type_ctx_no_dup_ctx_post in Hnodup...
  - destruct H as [(n & Tn & phin & taun' & Hsubst2)| (t1 & t2 & Hsubst2)]; subst; simpl.
    + simpl in HH. apply IHGamma1 with (Gamma2:=Gamma2)in HH; auto.
      rewrite <- app_comm_cons in Hnodup. rewrite <- app_one_is_cons in Hnodup. apply type_ctx_no_dup_ctx_post in Hnodup...
    + simpl in HH. apply IHGamma1 with (Gamma2:=Gamma2)in HH; auto.
      rewrite <- app_comm_cons in Hnodup. rewrite <- app_one_is_cons in Hnodup. apply type_ctx_no_dup_ctx_post in Hnodup...
Qed.

Lemma type_closed_add_pre: forall (Gamma1 Gamma2: lcontxt) st (tau: overunderty),
    type_ctx_no_dup st (Gamma1 ++ Gamma2) ->
    st_type_closed_in_ctx st Gamma2 tau -> st_type_closed_in_ctx st (Gamma1 ++ Gamma2) tau.
Proof with eauto.
  intros.
  destruct H0 as (HH & H1 & H2). split...
  apply st_type_closed_in_ctx_aux_add_pre...
Qed.

Lemma st_type_closed_in_ctx_last_over_var: forall st Gamma x T phi,
    type_ctx_no_dup st (Gamma ++ ((x, Oty ({{v:T | phi}}))::nil)) ->
    st_type_closed_in_ctx st (Gamma ++ ((x, Oty ({{v:T | phi}}))::nil)) (mk_eq_var T x).
Proof with eauto.
  intros. apply type_closed_add_pre...
  constructor. constructor...
  unfold phi_sat_tyst. intros. intro HH.
  destruct (eqb_spec x x0); subst... rewrite update_eq in H0. inversion H0.
  unfold appear_free_in_refinement in HH. apply HH. intros. rewrite update_neq...
  split; auto. eapply type_ctx_no_dup_ctx_post in H...
Qed.

Lemma st_type_closed_in_ctx_last_under_var: forall st Gamma x T phi,
    type_ctx_no_dup st (Gamma ++ ((x, Uty ([[v:T | phi]]))::nil)) ->
    st_type_closed_in_ctx st (Gamma ++ ((x, Uty ([[v:T | phi]]))::nil)) (mk_eq_var T x).
Proof with eauto.
  intros. apply type_closed_add_pre...
  constructor. constructor...
  unfold phi_sat_tyst. intros. intro HH.
  destruct (eqb_spec x x0); subst... rewrite update_eq in H0. inversion H0.
  unfold appear_free_in_refinement in HH. apply HH. intros. rewrite update_neq...
  split; auto. eapply type_ctx_no_dup_ctx_post in H...
Qed.

(* Global Hint Resolve type_closed_add_pre: core. *)
(* Global Hint Resolve type_closed_add_post: core. *)

(* Fixpoint erase_arr_bindings (Gamma: lcontxt): lcontxt := *)
(*   match Gamma with *)
(*   | nil => nil *)
(*   | (x, ty)::Gamma => *)
(*       match ty with *)
(*       | Oty _ | Uty ([[v: _ | _ ]]) => (x, ty)::(erase_arr_bindings Gamma) *)
(*       | _ => (erase_arr_bindings Gamma) *)
(*       end *)
(*   end. *)

(* Lemma erase_arr_bindings_spec: forall (Gamma: lcontxt), *)
(*     lcontxt_to_baseconctx Gamma = lcontxt_to_baseconctx (erase_arr_bindings Gamma). *)
(* Proof. *)
(*   intro Gamma. induction Gamma; auto. *)
(*   destruct a as (x, xty). *)
(*   destruct xty. destruct u; simpl; auto. *)
(*   - rewrite IHGamma. auto. *)
(*   - destruct o. simpl. rewrite IHGamma. auto. *)
(* Qed. *)

(* Lemma type_closed_even_without_arr_bindings: forall Gamma tau, *)
(*     type_closed_in_ctx Gamma tau <-> type_closed_in_ctx (erase_arr_bindings Gamma) tau. *)
(* Proof. *)
(*   unfold type_closed. setoid_rewrite <- erase_arr_bindings_spec. reflexivity. *)
(* Qed. *)

(* Definition lcontxt_to_basic_ctx (Gamma: lcontxt) := ncontxt_to_basic_ctx (erase_basetypectx (lcontxt_to_baseconctx Gamma)). *)

(* Global Hint Unfold lcontxt_to_basic_ctx: core. *)

(* st_type_closed_ctx: *)
(*   1. should also have no duplicate names *)
(*   2. types are well_formed *)
(*   3. types are closed *)

Inductive st_type_closed_ctx: tystate -> lcontxt -> Prop :=
| st_type_closed_ctx_nil: forall st, st_type_closed_ctx st nil
| st_type_closed_ctx_cons: forall st x tau_x Gamma,
    type_ctx_no_dup st (Gamma <l> x :l: tau_x) ->
    st_type_closed_in_ctx st Gamma tau_x ->
    st_type_closed_ctx st Gamma ->
    st_type_closed_ctx st (Gamma <l> x :l: tau_x).
    (* st x = None -> *)
    (* l_find_right_most Gamma x = None -> *)
    (* st_type_closed_ctx st Gamma -> *)
    (* st_type_closed_in_ctx st Gamma tau_x -> *)
    (* well_formed_type tau_x -> *)
    (* st_type_closed_ctx st (Gamma <l> x :l: tau_x). *)

Global Hint Constructors st_type_closed_ctx: core.

Lemma destructst_type_closed_ctx: forall st x tau_x Gamma,
    st_type_closed_ctx st (Gamma <l> x :l: tau_x) ->
    st x = None /\
      l_find_right_most Gamma x = None /\
      st_type_closed_ctx st Gamma /\
      st_type_closed_in_ctx st Gamma tau_x /\
      well_formed_type tau_x.
Proof with eauto.
  intros.
  inversion H; subst. apply app_one_eq_nil in H2. inversion H2.
  apply app_inj_tail in H0. inversion H0; subst. inversion H5; subst. clear H0. clear H5.
  split. eapply type_ctx_no_dup_cannot_find_last_in_nst in H1...
  split...
Qed.

Global Hint Resolve destructst_type_closed_ctx: core.

Lemma st_type_closed_ctx_implies_head_closed: forall Gamma tyst x tau_x,
    st_type_closed_ctx tyst ((x, tau_x)::Gamma) ->
    (tyst x = None /\
       st_type_closed_in_ctx tyst nil tau_x /\
       well_formed_type tau_x).
Proof with eauto.
  apply (rev_ind (fun Gamma => forall tyst x tau_x,
                      st_type_closed_ctx tyst ((x, tau_x)::Gamma) -> (tyst x = None /\
                                                                      st_type_closed_in_ctx tyst nil tau_x /\
                                                                      well_formed_type tau_x))).
  - intros tyst x tau_x H. rewrite <- app_nil_l in H.
    apply destructst_type_closed_ctx in H. destruct H as (Ha & Hb & Hc & Hd & He)...
  - intros (a & tau_a) Gamma Hind tyst x tau_x H.
    rewrite app_comm_cons in H. apply destructst_type_closed_ctx in H. destruct H as (Ha & Hb & Hc & Hd & He)...
Qed.

Lemma st_type_closed_ctx_implies_head_well_formed_type: forall tyst x tau_x Gamma,
    st_type_closed_ctx tyst ((x, tau_x)::Gamma) -> well_formed_type tau_x.
Proof with eauto.
  intros.
  apply st_type_closed_ctx_implies_head_closed in H... destruct H as (H1 & H2 & H3)...
Qed.

Global Hint Resolve st_type_closed_ctx_implies_head_well_formed_type: core.

Lemma st_type_closed_ctx_destruct_front : forall Gamma tyst x T phi,
    st_type_closed_ctx tyst ((x, Uty ([[v:T | phi]])) :: Gamma) ->
    st_type_closed_ctx (x |-> T; tyst) Gamma.
Proof with eauto.
  apply (rev_ind (fun Gamma => forall (tyst: tystate) x T phi,
                       st_type_closed_ctx tyst ((x, Uty ([[v:T | phi]])) :: Gamma) ->
                       st_type_closed_ctx (x |-> T; tyst) Gamma)).
  - intros nst x T phi Hclosed.
    constructor...
  - intros (a & tau_a) Gamma IHGamma nst x T phi Hclosed.
    rewrite app_comm_cons in Hclosed.
    apply destructst_type_closed_ctx in Hclosed. destruct Hclosed as (Hnst & Hfind & Hctx & Htau & Hwf).
    assert (x <> a) as Hxa...
    constructor; auto.
    (* + assert (x <> a)... simpl. rewrite update_neq... *)
    + apply l_find_right_most_none_neq_tl in Hfind...
      assert (type_ctx_no_dup nst ((x, Uty ([[v:T | phi]])) :: Gamma))...
      apply nodup_update in H... apply nodup_append...
      rewrite update_neq...
    (* + eauto. *)
    + eapply st_type_closed_in_ctx_destruct_underbase_front...
    + eapply st_type_closed_in_ctx_destruct_underbase_front in Htau...
Qed.

(* Global Hint Resolve st_type_closed_ctx_emp_implies_all: core. *)

Lemma st_type_closed_ctx_implies_type_ctx_no_dup: forall st Gamma,
    st_type_closed_ctx st Gamma -> type_ctx_no_dup st Gamma.
Proof with eauto.
  intros.
  induction H...
Qed.


Global Hint Resolve st_type_closed_ctx_implies_type_ctx_no_dup: core.

Lemma st_type_closed_in_ctx_drop_last: forall Gamma st x tau_x tau,
    ~ appear_free_in_underty x tau ->
    st_type_closed_in_ctx st (Gamma ++ ((x, tau_x)::nil)) tau ->
    st_type_closed_in_ctx st Gamma tau.
Proof with eauto.
  apply (rev_ind (fun Gamma => forall st x tau_x tau,
                      ~ appear_free_in_underty x tau ->
                      st_type_closed_in_ctx st (Gamma ++ ((x, tau_x)::nil)) tau ->
                      st_type_closed_in_ctx st Gamma tau)).
  - intros nst x tau_x tau Hfree H.
    constructor... rewrite app_nil_l in H. inversion H; subst. destruct H1. simpl in H0. destruct tau_x. destruct u...
    apply type_ctx_no_dup_implies_head_free in H2. apply l_find_right_most_none_neq_hd in H2. exfalso...
    destruct o.
    apply type_ctx_no_dup_implies_head_free in H2. apply l_find_right_most_none_neq_hd in H2. exfalso...
  - intros (a & tau_a) Gamma IHGamma nst x T phi Hclosed H.
    assert (type_ctx_no_dup empty ((a, tau_a)::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H0. apply l_find_right_most_none_neq_hd in H0. exfalso...
Qed.

Lemma st_type_close_oarr_subst_ctx: forall Gamma st a0 T phi c2 (tau: underty),
    st_type_closed_in_ctx st Gamma (a0 o: {{v:T | phi}} o--> tau) ->
    st_type_closed_in_ctx st Gamma (<u[ a0 |c-> c2 ]> tau).
Proof with eauto.
  apply (rev_ind (fun Gamma => forall st a0 T phi c2 (tau: underty),
                      st_type_closed_in_ctx st Gamma (a0 o: {{v:T | phi}} o--> tau) ->
    st_type_closed_in_ctx st Gamma (<u[ a0 |c-> c2 ]> tau))).
  - intros nst x tau_x tau Hfree tau' HH. inversion HH; subst...
     assert (type_ctx_no_dup empty ((x, Uty tau')::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H1. apply l_find_right_most_none_neq_hd in H1. exfalso...
  - intros (a & tau_a) Gamma IHGamma nst x T phi Hclosed tau HH.
    assert (type_ctx_no_dup empty ((a, tau_a)::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H. apply l_find_right_most_none_neq_hd in H. exfalso...
Qed.

Lemma st_type_closed_in_ctx_construct_oarr: forall st Gamma (a: string) Ta phia (t2: underty),
    st_type_closed_in_ctx st Gamma ({{v: Ta | phia}}) ->
    st_type_closed_in_ctx st (Gamma ++ ((a, Oty ({{v: Ta | phia}}))::nil)) t2 ->
    st_type_closed_in_ctx st Gamma (a o: {{v: Ta | phia}} o--> t2).
Proof with eauto.
  intros. induction Gamma.
  - assert (type_ctx_no_dup empty ((a, Oty ({{v:Ta | phia}}))::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H1. apply l_find_right_most_none_neq_hd in H1. exfalso...
  - assert (type_ctx_no_dup empty ((a, Oty ({{v:Ta | phia}}))::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H1. apply l_find_right_most_none_neq_hd in H1. exfalso...
Qed.

Lemma st_type_close_arrarr_app_ctx_aux: forall Gamma st tau_x (tau: underty),
    st_type_closed_in_ctx st Gamma (tau_x u--> tau) <->
    st_type_closed_in_ctx st Gamma tau_x /\ st_type_closed_in_ctx st Gamma tau.
Proof with eauto.
  apply (rev_ind (fun Gamma => forall st tau_x (tau: underty),
                      st_type_closed_in_ctx st Gamma (tau_x u--> tau) <->
    st_type_closed_in_ctx st Gamma tau_x /\ st_type_closed_in_ctx st Gamma tau)).
  - intros st tau_x tau; split; intro H.
    inversion H; subst.
    destruct (exists_not_free_var_in_tm texn).
    assert (type_ctx_no_dup empty ((x, Uty tau_x)::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H3. apply l_find_right_most_none_neq_hd in H3. exfalso...
    inversion H; subst.
    destruct (exists_not_free_var_in_tm texn).
    assert (type_ctx_no_dup empty ((x, Uty tau_x)::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H3. apply l_find_right_most_none_neq_hd in H3. exfalso...
  - intros (a & tau_a) Gamma IHGamma nst tau_x tau; split; intro HH.
    assert (type_ctx_no_dup empty ((a, Uty tau_x)::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H. apply l_find_right_most_none_neq_hd in H. exfalso...
    assert (type_ctx_no_dup empty ((a, Uty tau_x)::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H. apply l_find_right_most_none_neq_hd in H. exfalso...
Qed.

Lemma st_type_close_arrarr_app_ctx: forall Gamma (x: string) st tau_x (tau: underty),
    st_type_closed_in_ctx st Gamma (tau_x u--> tau) ->
    st_type_closed_in_ctx st Gamma tau_x /\ st_type_closed_in_ctx st Gamma tau.
Proof with eauto.
  intros. rewrite st_type_close_arrarr_app_ctx_aux in H...
Qed.

Lemma closed_ctx_implies_op_closed: forall Gamma st op (c1 c2: cid),
    st_type_closed_ctx st Gamma ->
    st_type_closed_in_ctx st Gamma (mk_op_retty_from_cids op c1 c2).
Proof with eauto.
  induction Gamma; intros.
  -
    destruct (exists_not_free_var_in_tm c1).
     assert (type_ctx_no_dup empty ((x, Uty (mk_op_retty_from_cids op c1 c2))::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H1. apply l_find_right_most_none_neq_hd in H1. exfalso...
  - intros...
    destruct (exists_not_free_var_in_tm c1).
     assert (type_ctx_no_dup empty ((x, Uty (mk_op_retty_from_cids op c1 c2))::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H1. apply l_find_right_most_none_neq_hd in H1. exfalso...
Qed.

Lemma st_type_closed_ctx_fst_last_diff_name: forall st a tau_a Gamma b tau_b,
    st_type_closed_ctx st ((a, tau_a)::Gamma ++ ((b, tau_b)::nil)) -> a <> b.
Proof with eauto.
  intros. eapply type_ctx_no_dup_fst_last_diff_name...
Qed.

Lemma st_type_closed_ctx_pre: forall Gamma2 st Gamma1,
    st_type_closed_ctx st (Gamma1 ++ Gamma2) -> st_type_closed_ctx st Gamma1.
Proof with eauto.
  apply (rev_ind (fun Gamma2 => forall st Gamma1,
                      st_type_closed_ctx st (Gamma1 ++ Gamma2) -> st_type_closed_ctx st Gamma1)).
  - intros st Gamma1 H... rewrite app_nil_r in H...
  - intros (a & tau_a) Gamma2 IHGamma st Gamma1 H.
    rewrite app_assoc in H.
    apply destructst_type_closed_ctx in H. destruct H as (Hnst & Hfind & Hctx & Htau & Hwf)...
Qed.

Lemma st_type_closed_in_ctx_last_samety: forall Gamma st x T phi1 phi2 tau,
    st_type_closed_in_ctx st (Gamma ++ ((x, Oty ({{v:T | phi1}}))::nil)) tau ->
    st_type_closed_in_ctx st (Gamma ++ ((x, Oty ({{v:T | phi2}}))::nil)) tau.
Proof with eauto.
  apply (rev_ind (fun Gamma => forall st x T phi1 phi2 tau,
                      st_type_closed_in_ctx st (Gamma ++ ((x, Oty ({{v:T | phi1}}))::nil)) tau ->
    st_type_closed_in_ctx st (Gamma ++ ((x, Oty ({{v:T | phi2}}))::nil)) tau)).
  - intros. rewrite app_nil_l in H... rewrite app_nil_l.
    inversion H; subst. inversion H1; subst. constructor...
  - intros.
    assert (type_ctx_no_dup empty ((x0, Oty ({{v:T | phi1}}))::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H1. apply l_find_right_most_none_neq_hd in H1. exfalso...
Qed.


Global Hint Resolve st_type_closed_ctx_pre: core.

(* Lemma st_type_closed_ctx_even_without_arr_bindings: forall st Gamma, *)
(*     st_type_closed_ctx st Gamma -> st_type_closed_ctx (erase_arr_bindings Gamma). *)

Definition type_closed_in_ctx (Gamma: lcontxt) (tau: overunderty):= st_type_closed_in_ctx empty Gamma tau.

Global Hint Unfold type_closed_in_ctx: core.

