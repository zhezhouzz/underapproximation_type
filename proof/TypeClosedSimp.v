Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From PLF Require Import CoreLangSimp.
From Coq Require Import Lists.List.
From Coq Require Import Strings.String.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Logic.Classical.
From PLF Require Import NormalTypeSystemSimp.
From PLF Require Import RfTypeDef.
From PLF Require Import LinearContext.
From PLF Require Import NoDup.
Import NoDup.
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
    st x = None ->
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

Fixpoint st_type_closed_in_ctx (tyst: tystate) (Gamma: lcontxt) (tau: overunderty) : Prop :=
  (match Gamma with
   | nil => st_type_closed tyst tau
   | (x, Oty ({{v:T | _}})) :: Gamma | (x, Uty ([[v:T | _]])) :: Gamma =>
       (* tyst x = None /\ st_type_closed_in_ctx (update tyst x T) Gamma tau *)
                                        st_type_closed_in_ctx (update tyst x T) Gamma tau
   | (x, _) :: Gamma => st_type_closed_in_ctx tyst Gamma tau
   end)
  /\ well_formed_type tau /\ type_ctx_no_dup tyst Gamma.

(* Inductive st_type_closed_in_ctx : tystate -> lcontxt -> overunderty -> Prop := *)
(* | st_type_closed_in_ctx_nil: forall tyst tau, st_type_closed tyst tau -> st_type_closed_in_ctx tyst [] tau *)
(* | st_type_closed_in_ctx_overcons: forall tyst x T phix Gamma tau, *)
(*     tyst x = None -> *)
(*     st_type_closed_in_ctx (update tyst x T) Gamma tau -> *)
(*     type_ctx_no_dup tyst ((x, Oty ({{v:T | phix}})) :: Gamma) -> *)
(*     st_type_closed_in_ctx tyst ((x, Oty ({{v:T | phix}})) :: Gamma) tau *)
(* | st_type_closed_in_ctx_undercons: forall tyst x T phix Gamma tau, *)
(*     tyst x = None -> *)
(*     st_type_closed_in_ctx (update tyst x T) Gamma tau -> *)
(*     type_ctx_no_dup tyst (((x, Uty ([[v:T | phix]])) :: Gamma)) -> *)
(*     st_type_closed_in_ctx tyst ((x, Uty ([[v:T | phix]])) :: Gamma) tau *)
(* | st_type_closed_in_ctx_oarrcons: forall tyst x a Ta phia taub Gamma tau, *)
(*     tyst x = None -> *)
(*     well_formed_type (a o: ({{v:Ta | phia}}) o--> taub) -> *)
(*     st_type_closed_in_ctx tyst Gamma tau -> *)
(*     type_ctx_no_dup tyst ((x, Uty (a o: ({{v:Ta | phia}}) o--> taub)) :: Gamma) -> *)
(*     st_type_closed_in_ctx tyst ((x, Uty (a o: ({{v:Ta | phia}}) o--> taub)) :: Gamma) tau *)
(* | st_type_closed_in_ctx_arrarrcons: forall tyst x t1 t2 Gamma tau, *)
(*     tyst x = None -> *)
(*     well_formed_type (t1 u--> t2) -> *)
(*     st_type_closed_in_ctx tyst Gamma tau -> *)
(*     type_ctx_no_dup tyst ((x, Uty (t1 u--> t2)) :: Gamma) -> *)
(*     st_type_closed_in_ctx tyst ((x, Uty (t1 u--> t2)) :: Gamma) tau. *)

(* Global Hint Constructors st_type_closed_in_ctx: core. *)


(* Inductive st_type_closed_in_ctx : tystate -> lcontxt -> refinement -> Prop := *)
(* | st_type_closed_in_ctx_nil: *)
(*   forall tyst phi, phi_sat_tyst phi tyst -> st_type_closed_in_ctx tyst [] phi *)
(*   (* forall tyst phi, (forall st, st \TYSTin tyst -> closed_refinement_under_state st phi) -> *) *)
(*   (*             st_type_closed_in_ctx tyst [] phi *) *)
(* | st_type_closed_in_ctx_overcons: forall tyst x T phix Gamma phi, *)
(*     tyst x = None -> *)
(*     st_type_closed_in_ctx (update tyst x T) Gamma phi -> *)
(*     st_type_closed_in_ctx tyst ((x, Oty ({{v:T | phix}})) :: Gamma) phi *)
(* | st_type_closed_in_ctx_undercons: forall tyst x T phix Gamma phi, *)
(*     tyst x = None -> *)
(*     st_type_closed_in_ctx (update tyst x T) Gamma phi -> *)
(*     st_type_closed_in_ctx tyst ((x, Uty ([[v:T | phix]])) :: Gamma) phi *)
(* | st_type_closed_in_ctx_oarrcons: forall tyst x a Ta phia taub Gamma phi, *)
(*     tyst x = None -> *)
(*     well_formed_type (a o: ({{v:Ta | phia}}) o--> taub) -> *)
(*     st_type_closed_in_ctx tyst Gamma phi -> *)
(*     st_type_closed_in_ctx tyst ((x, Uty (a o: ({{v:Ta | phia}}) o--> taub)) :: Gamma) phi *)
(* | st_type_closed_in_ctx_arrarrcons: forall tyst x t1 t2 Gamma phi, *)
(*     tyst x = None -> *)
(*     well_formed_type (t1 u--> t2) -> *)
(*     st_type_closed_in_ctx tyst Gamma phi -> *)
(*     st_type_closed_in_ctx tyst ((x, Uty (t1 u--> t2)) :: Gamma) phi. *)

(* Global Hint Constructors st_type_closed_in_ctx: core. *)

(* Lemma appear_free_in_refinement_alt (name: string) (phi: refinement): *)
(*   appear_free_in_refinement name phi <-> (exists st c2, phi st <> phi (update st name c2)). *)
(* Proof with eauto. *)
(*   unfold appear_free_in_refinement. *)
(* Admitted. *)

(* Lemma st_type_closed_in_ctx_spec: forall tyst phi, *)
(*     (forall st, st \TYSTin tyst -> closed_refinement_under_state st phi) -> *)
(*     (forall x, (exists T, tyst x = Some T) <-> (appear_free_in_refinement x phi)). *)
(* Proof with eauto. *)
(*   unfold closed_refinement_under_state. unfold state_in_tystate. *)
(*   intros. split. *)
(*   - intros He. rewrite -> appear_free_in_refinement_alt. *)
(*  Admitted. *)

(* Lemma st_type_closed_in_ctx_weakening: forall Gamma1 st Gamma2 phi, *)
(*     st_type_closed_in_ctx st Gamma2 phi -> *)
(*     st_type_closed_in_ctx st (Gamma1 ++ Gamma2) phi. *)
(* Proof with eauto. *)
(*   induction Gamma1; intros st Gamma2 phi H... *)
(*   rewrite <- app_comm_cons. *)
(*   destruct a. destruct o. destruct u. *)
(*   - constructor... apply IHGamma1... *)
(*   destruct u... constructor... *)

(* Lemma st_type_closed_in_ctx_weakening: forall Gamma1 st Gamma2 Gamma3 phi, *)
(*     (Gamma1 ++ Gamma2) = Gamma3 -> *)
(*     st_type_closed_in_ctx st Gamma2 phi -> *)
(*     st_type_closed_in_ctx st Gamma3 phi. *)
(* Proof with eauto. *)
(*   induction Gamma1; intros st Gamma2 Gamma3 phi Hcc H... *)
(*   - simpl in Hcc; subst... *)
(*   - destruct Gamma3. apply app_eq_nil in Hcc. destruct Hcc... inversion H0... *)
(*     rewrite <- app_comm_cons in Hcc. inversion Hcc. *)
(*     eapply IHGamma1 in H2... *)
(*     destruct a. destruct p. inversion H1; subst... *)
(*     destruct o0... *)
(*     destruct u... constructor... *)

Lemma st_type_closed_in_ctx_destruct_front : forall Gamma tyst x T phi tau,
    st_type_closed_in_ctx tyst ((x, Uty ([[v:T | phi]])) :: Gamma) tau ->
    st_type_closed_in_ctx (x |-> T; tyst) Gamma tau.
Proof with eauto.
  intros Gamma tyst x T phi tau Hclosed. simpl in Hclosed...
  destruct Hclosed...
Qed.

Lemma st_type_closed_in_ctx_one_underbase: forall tyst x T phi phi0,
    st_type_closed_in_ctx tyst ((x, Uty ([[v:T | phi]]))::nil) phi0 ->
    st_type_closed_in_ctx (x |-> T; tyst) [] phi0.
Proof with eauto.
  intros tyst x T phi tau Hclosed. simpl in Hclosed...
  destruct Hclosed; subst...
Qed.

Global Hint Resolve st_type_closed_in_ctx_one_underbase: core.

Lemma st_type_closed_in_ctx_one_overbase: forall tyst x T phi phi0,
    st_type_closed_in_ctx tyst ((x, Oty ({{v:T | phi}}))::nil) phi0 ->
    st_type_closed_in_ctx (x |-> T; tyst) [] phi0.
Proof with eauto.
  intros tyst x T phi tau Hclosed.  simpl in Hclosed...
  destruct Hclosed; subst...
Qed.

Global Hint Resolve st_type_closed_in_ctx_one_overbase: core.

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
    + inversion H6; subst. apply H4 in H0...
    + destruct H0. eapply IHtau in H7... rewrite update_neq...
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
  - destruct a. inversion HH; subst...
    destruct o. destruct u.
    + destruct H0. simpl in Hfind.
      assert (l_find_right_most Gamma x = None). apply l_find_right_most_none_neq_tl in Hfind...
      eapply IHGamma in H... rewrite update_neq...
    + assert (l_find_right_most Gamma x = None). apply l_find_right_most_none_neq_tl in Hfind...
      eapply IHGamma in H1...
    + assert (l_find_right_most Gamma x = None). apply l_find_right_most_none_neq_tl in Hfind...
      eapply IHGamma in H1...
    + destruct o.
      assert (l_find_right_most Gamma x = None). apply l_find_right_most_none_neq_tl in Hfind...
      eapply IHGamma in H... rewrite update_neq...
Qed.


(* Global Hint Resolve st_type_closed_in_ctx_emp_implies_all_not_free: core. *)

(* Lemma st_type_closed_in_ctx_emp_implies_all: forall st Gamma tau, *)
(*     st_type_closed_in_ctx empty Gamma tau -> st_type_closed_in_ctx st Gamma tau. *)
(* Admitted. *)

(* Global Hint Resolve st_type_closed_in_ctx_emp_implies_all: core. *)

Lemma st_type_closed_in_ctx_implies_head_unique: forall Gamma st s tau_s tau,
    st_type_closed_in_ctx st ((s, tau_s) :: Gamma) tau ->
    l_find_right_most Gamma s = None.
Proof with eauto.
  intros...
Qed.

Lemma st_type_closed_in_ctx_perm: forall tyst a tau_a b tau_b Gamma,
    a <> b ->
    (forall tau, st_type_closed_in_ctx tyst ((a, tau_a)::(b, tau_b)::Gamma) tau ->
            st_type_closed_in_ctx tyst ((b, tau_b)::(a, tau_a)::Gamma) tau).
Proof with eauto.
  intros tyst a tau_a b tau_b Gamma Hab. intros.
  destruct tau_a.
  destruct u. simpl in H... destruct H... split... destruct H. clear H. clear H0.
  - destruct tau_b. destruct u.
Admitted.

(* Lemma st_type_closed_in_ctx_one_underbase: forall tyst x T phi tau, *)
(*     st_type_closed_in_ctx tyst ((x, Uty ([[v:T | phi]]))::nil) tau -> *)
(*     st_type_closed_in_ctx (x |-> T; tyst) [] tau. *)
(* Admitted. *)

Global Hint Resolve st_type_closed_in_ctx_one_underbase: core.

(* Lemma st_type_closed_in_ctx_one_overbase: forall tyst x T phi tau, *)
(*     st_type_closed_in_ctx tyst ((x, Oty ({{v:T | phi}}))::nil) tau -> *)
(*     st_type_closed_in_ctx (x |-> T; tyst) [] tau. *)
(* Admitted. *)

Global Hint Resolve st_type_closed_in_ctx_one_overbase: core.

Lemma st_type_closed_in_ctx_destruct_underbase_front : forall Gamma tyst x T phi tau,
    st_type_closed_in_ctx tyst ((x, Uty ([[v:T | phi]])) :: Gamma) tau ->
    st_type_closed_in_ctx (x |-> T; tyst) Gamma tau.
Proof with eauto.
  intros Gamma tyst x T phi tau Hclosed.
  destruct Hclosed...
Qed.

Lemma st_type_closed_in_ctx_destruct_overbase_front : forall Gamma tyst x T phi tau,
    st_type_closed_in_ctx tyst ((x, Oty ({{v:T | phi}})) :: Gamma) tau ->
    st_type_closed_in_ctx (x |-> T; tyst) Gamma tau.
Proof with eauto.
  intros Gamma tyst x T phi tau Hclosed.
  destruct Hclosed...
Qed.

Global Hint Resolve st_type_closed_in_ctx_destruct_overbase_front: core.

Lemma st_type_closed_in_ctx_destruct_arrar_front : forall Gamma tyst x t1 t2 tau,
    st_type_closed_in_ctx tyst ((x, Uty (t1 u--> t2)) :: Gamma) tau ->
    st_type_closed_in_ctx tyst Gamma tau.
Proof with eauto.
  intros Gamma tyst x T phi tau Hclosed.
  destruct Hclosed...
Qed.

Lemma st_type_closed_in_ctx_destruct_oarr_front : forall Gamma tyst x a Ta phia t2 tau,
    st_type_closed_in_ctx tyst ((x, Uty (a o: {{v:Ta | phia}} o--> t2)) :: Gamma) tau ->
    st_type_closed_in_ctx tyst Gamma tau.
Proof with eauto.
  intros Gamma tyst x a Ta phia t2 tau Hclosed.
  destruct Hclosed...
Qed.

(* Inductive overunderbasety : Type := *)
(* | Obase: base_ty -> refinement -> overunderbasety *)
(* | Ubase: base_ty -> refinement -> overunderbasety. *)

(* Definition basecontxt := linear_context overunderbasety. *)

(* Fixpoint erase_basetypectx (Gamma: basecontxt): ncontxt := *)
(*   match Gamma with *)
(*   | nil => nil *)
(*   | (x, Obase T _)::Gamma => (x, TBase T)::(erase_basetypectx Gamma) *)
(*   | (x, Ubase T _)::Gamma => (x, TBase T)::(erase_basetypectx Gamma) *)
(*   end. *)

(* Fixpoint erase_lctx (Gamma: lcontxt): ncontxt := *)
(*   match Gamma with *)
(*   | nil => nil *)
(*   | (x, tau)::Gamma => (x, ou\_ tau _/)::(erase_lctx Gamma) *)
(*   end. *)

(* Definition lctx_to_basic_ctx (Gamma: lcontxt) := ncontxt_to_basic_ctx_aux (erase_lctx Gamma) empty. *)

(* Global Hint Unfold lctx_to_basic_ctx: core. *)

(* Inductive type_closed_in_ctx : basecontxt -> overunderty -> Prop := *)
(* | type_closed_base1: forall Gamma (T: base_ty) phi, *)
(*     st_type_closed_in_ctx (erase_basetypectx Gamma) T phi -> *)
(*     type_closed_in_ctx Gamma ({{v: T | phi}}) *)
(* | type_closed_base2: forall Gamma (T: base_ty) phi, *)
(*     st_type_closed_in_ctx (erase_basetypectx Gamma) T phi -> *)
(*     type_closed_in_ctx Gamma ([[v: T | phi]]) *)
(* | type_closed_oarr: forall Gamma x T phi (tau: underty), *)
(*     type_closed_in_ctx Gamma ({{v: T | phi}}) -> *)
(*     type_closed_in_ctx ((x, Obase T phi) :: Gamma) tau -> *)
(*     type_closed_in_ctx Gamma (x o: ({{v: T | phi}}) o--> tau) *)
(* | type_closed_arrarr: forall Gamma (tau_x: underty) (tau: underty), *)
(*     type_closed_in_ctx Gamma tau_x -> *)
(*     type_closed_in_ctx Gamma tau -> *)
(*     type_closed_in_ctx Gamma (tau_x u--> tau). *)

(* before erase refinement from the context, we will erase all function type bindings *)
(* Fixpoint lcontxt_to_baseconctx (Gamma: lcontxt): basecontxt := *)
(*   match Gamma with *)
(*   | nil => nil *)
(*   | (x, ty)::Gamma => *)
(*       match ty with *)
(*       | Oty ({{v: T | phi }}) => (x, Obase T phi)::(lcontxt_to_baseconctx Gamma) *)
(*       | Uty ([[v: T | phi ]]) => (x, Ubase T phi)::(lcontxt_to_baseconctx Gamma) *)
(*       | _ => (lcontxt_to_baseconctx Gamma) *)
(*       end *)
(*   end. *)

(* type closed \S{Ty} *)

Lemma mk_op_is_type_closed: forall st op a b, st_type_closed_in_ctx st l_empty (mk_op op a b).
Admitted.

Global Hint Resolve mk_op_is_type_closed: core.

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

Lemma nodup_update: forall st s b r Gamma,
    type_ctx_no_dup st ((s, Uty ([[v:b | r]])) :: Gamma) ->
    type_ctx_no_dup (s |-> b; st) Gamma.
Admitted.

Lemma nodup_update_over: forall st s b r Gamma,
    type_ctx_no_dup st ((s, Oty ({{v:b | r}})) :: Gamma) ->
    type_ctx_no_dup (s |-> b; st) Gamma.
Admitted.

Lemma nodup_dropfst: forall st s tau_s Gamma,
    type_ctx_no_dup st ((s, tau_s) :: Gamma) ->
    type_ctx_no_dup st Gamma.
Proof with eauto.
  intros. rewrite <- app_one_is_cons in H...
Qed.

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
Admitted.

Global Hint Resolve mk_eq_is_well_formed_type: core.

Lemma type_closed_add_post: forall (Gamma1 Gamma2: lcontxt) st (tau: overunderty),
    type_ctx_no_dup st (Gamma1 ++ Gamma2) ->
    st_type_closed_in_ctx st Gamma1 tau -> st_type_closed_in_ctx st (Gamma1 ++ Gamma2) tau.
Admitted.

Lemma type_closed_add_pre: forall (Gamma1 Gamma2: lcontxt) st (tau: overunderty),
    type_ctx_no_dup st (Gamma1 ++ Gamma2) ->
    st_type_closed_in_ctx st Gamma2 tau -> st_type_closed_in_ctx st (Gamma1 ++ Gamma2) tau.
Admitted.

Lemma st_type_closed_in_ctx_last_over_var: forall st Gamma x T phi,
    type_ctx_no_dup st (Gamma ++ ((x, Oty ({{v:T | phi}}))::nil)) ->
    st_type_closed_in_ctx st (Gamma ++ ((x, Oty ({{v:T | phi}}))::nil)) (mk_eq_var T x).
Proof with eauto.
  intros. apply type_closed_add_pre...
  constructor. constructor... constructor...
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
  constructor. constructor... constructor...
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
(* Admitted. *)
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
    st x = None ->
    l_find_right_most Gamma x = None ->
    st_type_closed_ctx st Gamma ->
    st_type_closed_in_ctx st Gamma tau_x ->
    well_formed_type tau_x ->
    st_type_closed_ctx st (Gamma <l> x :l: tau_x).

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
  apply app_inj_tail in H0. inversion H0; subst. inversion H7; subst. split...
Qed.

Global Hint Resolve destructst_type_closed_ctx: core.

Lemma st_type_closed_ctx_implies_head_well_formed_type: forall tyst x tau_x Gamma,
    st_type_closed_ctx tyst ((x, tau_x)::Gamma) -> well_formed_type tau_x.
Admitted.

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
    constructor; auto.
    + assert (x <> a)... rewrite update_neq...
    + apply l_find_right_most_none_neq_tl in Hfind...
    + eauto.
    + eapply st_type_closed_in_ctx_destruct_underbase_front...
Qed.

Lemma st_type_closed_ctx_emp_implies_all: forall st Gamma,
    st_type_closed_ctx empty Gamma -> st_type_closed_ctx st Gamma.
Admitted.

Global Hint Resolve st_type_closed_ctx_emp_implies_all: core.

Lemma st_type_closed_ctx_implies_type_ctx_no_dup: forall st Gamma,
    st_type_closed_ctx st Gamma -> type_ctx_no_dup st Gamma.
Admitted.

Global Hint Resolve st_type_closed_ctx_implies_type_ctx_no_dup: core.

Lemma st_type_closed_ctx_fst_last_diff_name: forall st a tau_a Gamma b tau_b,
    st_type_closed_ctx st ((a, tau_a)::Gamma ++ ((b, tau_b)::nil)) -> a <> b.
Proof with eauto.
  intros. eapply type_ctx_no_dup_fst_last_diff_name...
Qed.

Lemma st_type_closed_ctx_pre: forall st Gamma1 Gamm2,
    st_type_closed_ctx st (Gamma1 ++ Gamm2) -> st_type_closed_ctx st Gamma1.
Admitted.

Global Hint Resolve st_type_closed_ctx_pre: core.

(* Lemma st_type_closed_ctx_even_without_arr_bindings: forall st Gamma, *)
(*     st_type_closed_ctx st Gamma -> st_type_closed_ctx (erase_arr_bindings Gamma). *)
(* Admitted. *)

Definition type_closed_in_ctx (Gamma: lcontxt) (tau: overunderty):= st_type_closed_in_ctx empty Gamma tau.

Global Hint Unfold type_closed_in_ctx: core.
