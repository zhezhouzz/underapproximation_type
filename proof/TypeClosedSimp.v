Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From PLF Require Import CoreLangSimp.
From Coq Require Import Lists.List.
From Coq Require Import Strings.String.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.
From PLF Require Import NormalTypeSystemSimp.
From PLF Require Import RfTypeDef.
From PLF Require Import LinearContext.
From PLF Require Import Nstate.
From PLF Require Import NoDup.
Import Nstate.
Import NoDup.
Import ListNotations.

(* closed_refinement *)

Definition closed_refinement_under_state (st: state) (phi: refinement): Prop := forall st', st <st< st' -> phi st = phi st'.

Global Hint Unfold closed_refinement_under_state: core.

Definition empstate : state := empty.

Lemma empstate_min: forall st, empstate <st< st.
Proof.
  intros. intros x c_x H. inversion H.
Qed.

Global Hint Resolve empstate_min: core.

Lemma state_order_implies_closed: forall st st' phi,
    st <st< st' -> closed_refinement_under_state st phi -> closed_refinement_under_state st' phi.
Proof with eauto.
  unfold closed_refinement_under_state. intros. rewrite <- H0...
Qed.

Global Hint Resolve state_order_implies_closed: core.

Lemma constant_refinement_is_closed: forall (c: constant) (phi: refinement),
    (forall st : state, phi st = (fun c' => c = c')) -> (closed_refinement_under_state empstate phi).
Proof.
  intros.
  intros st HH. rewrite H. rewrite H. auto.
Qed.

Lemma refinement_subst_c_closed_rf: forall x1 c2 phi,
    closed_refinement_under_state empstate phi -> refinement_subst_c x1 c2 phi = phi.
Proof with eauto.
  intros.
  unfold closed_refinement_under_state in H. unfold refinement_subst_c.
  apply functional_extensionality. intros st.
  rewrite <- H...
Qed.

(* closed_type *)

Definition phi_sat_tyst (phi: refinement) (tyst: tystate) (c: constant) :=
  (forall st, st \TYSTin tyst -> closed_refinement_under_state st phi /\ phi st c).

(* Global Hint Unfold nst_tmR_aux: core. *)

Definition lcontxt := linear_context overunderty.

Inductive closed_refinement_in_ctx : tystate -> lcontxt -> refinement -> Prop :=
| closed_refinement_in_ctx_nil:
  forall tyst phi, (forall st, st \TYSTin tyst -> closed_refinement_under_state st phi) ->
              closed_refinement_in_ctx tyst [] phi
| closed_refinement_in_ctx_overcons: forall tyst x T phix Gamma phi,
    closed_refinement_in_ctx (update tyst x T) Gamma phi ->
    closed_refinement_in_ctx tyst ((x, Oty ({{v:T | phix}})) :: Gamma) phi
| closed_refinement_in_ctx_undercons: forall tyst x T phix Gamma phi,
    closed_refinement_in_ctx (update tyst x T) Gamma phi ->
    closed_refinement_in_ctx tyst ((x, Uty ([[v:T | phix]])) :: Gamma) phi
| closed_refinement_in_ctx_oarrcons: forall tyst x a Ta phia taub Gamma phi,
    well_formed_type (a o: ({{v:Ta | phia}}) o--> taub) ->
    closed_refinement_in_ctx tyst Gamma phi ->
    closed_refinement_in_ctx tyst ((x, Uty (a o: ({{v:Ta | phia}}) o--> taub)) :: Gamma) phi
| closed_refinement_in_ctx_arrarrcons: forall tyst x t1 t2 Gamma phi,
    well_formed_type (t1 u--> t2) ->
    closed_refinement_in_ctx tyst Gamma phi ->
    closed_refinement_in_ctx tyst ((x, Uty (t1 u--> t2)) :: Gamma) phi.

Global Hint Constructors closed_refinement_in_ctx: core.

Lemma closed_refinement_in_ctx_weakening: forall st Gamma1 Gamma2 Gamma3 phi,
    (Gamma1 ++ Gamma2) = Gamma3 ->
    closed_refinement_in_ctx st Gamma2 phi ->
    closed_refinement_in_ctx st Gamma3 phi.
Admitted.

Definition closed_refinement_in_ctx_drop: forall st nst x T phi phi0 e_x,
    closed_refinement_in_ctx (st\_ nst _/) [(x, Uty ([[v:T | phi]]))] phi0 ->
    empty \N- e_x \Tin T ->
    st \NSTin (x |-> (e_x, T); nst) ->
    closed_refinement_under_state st phi0.
Proof with eauto.
  intros st nst x T phi phi0 e_x Hctx HT Hsub.
  inversion Hctx; subst... inversion H5;subst...
Qed.

Lemma closed_refinement_in_ctx_one_underbase: forall tyst x T phi phi0,
    closed_refinement_in_ctx tyst ((x, Uty ([[v:T | phi]]))::nil) phi0 ->
    closed_refinement_in_ctx (x |-> T; tyst) [] phi0.
Admitted.

Global Hint Resolve closed_refinement_in_ctx_one_underbase: core.

Lemma closed_refinement_in_ctx_one_overbase: forall tyst x T phi phi0,
    closed_refinement_in_ctx tyst ((x, Oty ({{v:T | phi}}))::nil) phi0 ->
    closed_refinement_in_ctx (x |-> T; tyst) [] phi0.
Admitted.

Global Hint Resolve closed_refinement_in_ctx_one_overbase: core.

Lemma closed_refinement_in_ctx_destruct_front : forall Gamma tyst x T phi tau,
    closed_refinement_in_ctx tyst ((x, Uty ([[v:T | phi]])) :: Gamma) tau ->
    closed_refinement_in_ctx (x |-> T; tyst) Gamma tau.
Proof with eauto.
  intros Gamma tyst x T phi tau Hclosed.
  inversion Hclosed; subst...
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

(*   Example refinement_under_nctx: closed_refinement_in_ctx [("x"%string, TBase TNat)] TNat (fun st c => c = st "x"%string). *)
(*   Proof. *)
(*     constructor. intros. *)
(*     apply nat_value_n_exists in H. destruct H; subst. *)
(*     constructor. constructor; subst; auto. *)
(*   Qed. *)

(* End ExCloseRefinement. *)

Inductive st_type_closed_in_ctx : tystate -> lcontxt -> overunderty -> Prop :=
| st_type_closed_base1: forall st Gamma (T: base_ty) phi,
    closed_refinement_in_ctx st Gamma phi -> st_type_closed_in_ctx st Gamma ({{v: T | phi}})
| st_type_closed_base2: forall st Gamma (T: base_ty) phi,
    closed_refinement_in_ctx st Gamma phi -> st_type_closed_in_ctx st Gamma ([[v: T | phi]])
| st_type_closed_oarr: forall st Gamma x T phi (tau: underty),
    well_formed_type (x o: ({{v: T | phi}}) o--> tau) ->
    st_type_closed_in_ctx st Gamma ({{v: T | phi}}) ->
    st_type_closed_in_ctx st ((x, Oty ({{v: T | phi}})) :: Gamma) tau ->
    st_type_closed_in_ctx st Gamma (x o: ({{v: T | phi}}) o--> tau)
| st_type_closed_arrarr: forall st Gamma (tau_x: underty) (tau: underty),
    well_formed_type (tau_x u--> tau) ->
    st_type_closed_in_ctx st Gamma tau_x ->
    st_type_closed_in_ctx st Gamma tau ->
    st_type_closed_in_ctx st Gamma (tau_x u--> tau).

Global Hint Constructors st_type_closed_in_ctx: core.

Lemma st_type_closed_in_ctx_implies_well_formed_type:
  forall tyst Gamma tau, st_type_closed_in_ctx tyst Gamma tau -> well_formed_type tau.
Admitted.

Global Hint Resolve st_type_closed_in_ctx_implies_well_formed_type: core.

Lemma st_type_closed_in_ctx_emp_implies_all_not_free: forall Gamma (tau: underty) x,
    l_find_right_most Gamma x = None ->
    st_type_closed_in_ctx empty Gamma tau ->
    ~ appear_free_in_underty x tau.
Admitted.

Global Hint Resolve st_type_closed_in_ctx_emp_implies_all_not_free: core.

Lemma st_type_closed_in_ctx_emp_implies_all: forall st Gamma tau,
    st_type_closed_in_ctx empty Gamma tau -> st_type_closed_in_ctx st Gamma tau.
Admitted.

Global Hint Resolve st_type_closed_in_ctx_emp_implies_all: core.

Lemma st_type_closed_in_ctx_perm: forall tyst a tau_a b tau_b Gamma,
    st_type_closed_in_ctx tyst [] tau_b ->
    (forall tau, st_type_closed_in_ctx tyst ((a, tau_a)::(b, tau_b)::Gamma) tau ->
            st_type_closed_in_ctx tyst ((b, tau_b)::(a, tau_a)::Gamma) tau).
Admitted.

Lemma st_type_closed_in_ctx_one_underbase: forall tyst x T phi tau,
    st_type_closed_in_ctx tyst ((x, Uty ([[v:T | phi]]))::nil) tau ->
    st_type_closed_in_ctx (x |-> T; tyst) [] tau.
Admitted.

Global Hint Resolve st_type_closed_in_ctx_one_underbase: core.

Lemma st_type_closed_in_ctx_one_overbase: forall tyst x T phi tau,
    st_type_closed_in_ctx tyst ((x, Oty ({{v:T | phi}}))::nil) tau ->
    st_type_closed_in_ctx (x |-> T; tyst) [] tau.
Admitted.

Global Hint Resolve st_type_closed_in_ctx_one_overbase: core.

Lemma st_type_closed_in_ctx_destruct_underbase_front : forall Gamma tyst x T phi tau,
    st_type_closed_in_ctx tyst ((x, Uty ([[v:T | phi]])) :: Gamma) tau ->
    st_type_closed_in_ctx (x |-> T; tyst) Gamma tau.
Proof with eauto.
  intros Gamma tyst x T phi tau Hclosed.
  induction Gamma; inversion Hclosed; subst.
  + inversion H...
  + inversion H...
  + inversion H...
  + inversion H; subst...
Admitted.

Lemma st_type_closed_in_ctx_destruct_overbase_front : forall Gamma tyst x T phi tau,
    st_type_closed_in_ctx tyst ((x, Oty ({{v:T | phi}})) :: Gamma) tau ->
    st_type_closed_in_ctx (x |-> T; tyst) Gamma tau.
Proof with eauto.
Admitted.

Global Hint Resolve st_type_closed_in_ctx_destruct_overbase_front: core.

Lemma st_type_closed_in_ctx_destruct_arrar_front : forall Gamma tyst x t1 t2 tau,
    st_type_closed_in_ctx tyst ((x, Uty (t1 u--> t2)) :: Gamma) tau ->
    st_type_closed_in_ctx tyst Gamma tau.
Proof with eauto.
Admitted.

Lemma st_type_closed_in_ctx_destruct_oarr_front : forall Gamma tyst x a Ta phia t2 tau,
    st_type_closed_in_ctx tyst ((x, Uty (a o: {{v:Ta | phia}} o--> t2)) :: Gamma) tau ->
    st_type_closed_in_ctx tyst Gamma tau.
Proof with eauto.
Admitted.

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
(*     closed_refinement_in_ctx (erase_basetypectx Gamma) T phi -> *)
(*     type_closed_in_ctx Gamma ({{v: T | phi}}) *)
(* | type_closed_base2: forall Gamma (T: base_ty) phi, *)
(*     closed_refinement_in_ctx (erase_basetypectx Gamma) T phi -> *)
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

Definition type_closed_in_ctx (Gamma: lcontxt) (tau: overunderty):= st_type_closed_in_ctx empty Gamma tau.

Global Hint Unfold type_closed_in_ctx: core.

(* type closed \S{Ty} *)

Lemma mk_op_is_type_closed: forall op a b, type_closed_in_ctx l_empty (mk_op op a b).
Admitted.

Global Hint Resolve mk_op_is_type_closed: core.

Lemma mk_eq_over_base_is_type_closed: forall x T phi, type_closed_in_ctx ((x, Oty ({{v:T | phi}}))::nil) (mk_eq_var T x).
Proof with eauto.
  intros x T phi. repeat constructor...
  unfold closed_refinement_under_state... simpl. intros st Hst st' Hsub.
  remember (Hst x) as Hst'. clear HeqHst'. rewrite update_eq in Hst'. edestruct Hst'.
  reflexivity.
  rewrite H. apply Hsub in H. rewrite H. reflexivity.
Qed.

Global Hint Resolve mk_eq_over_base_is_type_closed: core.

Lemma mk_eq_under_base_is_type_closed: forall x T phi, type_closed_in_ctx ((x, Uty ([[v:T | phi]]))::nil) (mk_eq_var T x).
Proof with eauto.
  intros x T phi. repeat constructor...
  unfold closed_refinement_under_state... intros st Hst st' Hsub.
  remember (Hst x) as Hst'. clear HeqHst'. rewrite update_eq in Hst'. edestruct Hst'.
  reflexivity.
  rewrite H. apply Hsub in H. rewrite H. reflexivity.
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

Lemma constant_eq_is_close: forall Gamma c, type_closed_in_ctx Gamma (mk_eq_constant c).
Proof.
  intros. constructor.
Admitted.
(*   assert (closed_refinement_in_ctx nil (ty_of_cotyst c) (fun (_ : state) (v : constant) => v = c)). *)
(*   constructor. *)
(*   unfold closed_refinement_under_state; destruct c; simpl; reflexivity. *)
(*   apply closed_refinement_in_ctx_weakening with (Gamma1 := (erase_basetypectx (lcontxt_to_baseconctx Gamma))) (Gamma2 := nil); auto. *)
(*   rewrite app_nil_r. auto. *)
(* Qed. *)

Global Hint Resolve mk_eq_constant_spec: core.
Global Hint Resolve subst_constant_eq: core.
Global Hint Resolve constant_eq_is_close: core.

(* type closed lemmas *)

Lemma type_closed_add_post: forall (Gamma1 Gamma2: lcontxt) (tau: overunderty),
    type_closed_in_ctx Gamma1 tau -> type_closed_in_ctx (Gamma1 ++ Gamma2) tau.
Admitted.

Lemma type_closed_add_pre: forall (Gamma1 Gamma2: lcontxt) (tau: overunderty),
    type_closed_in_ctx Gamma2 tau -> type_closed_in_ctx (Gamma1 ++ Gamma2) tau.
Admitted.

Lemma st_type_closed_in_ctx_last_over_var: forall st Gamma x T phi, st (Gamma ++ ((x, Oty ({{v:T | phi}}))::nil)) (mk_eq_var T x).
Admitted.

Lemma st_type_closed_in_ctx_last_under_var: forall st Gamma x T phi, st (Gamma ++ ((x, Uty ([[v:T | phi]]))::nil)) (mk_eq_var T x).
Admitted.

Lemma type_closed_drop_post: forall (Gamma1 Gamma2: lcontxt) (tau: overunderty),
	  (forall x tau_x, l_find_right_most Gamma2 x = Some tau_x -> ~ appear_free_in_overunderty x tau) ->
	  type_closed_in_ctx (Gamma1 ++ Gamma2) tau -> type_closed_in_ctx Gamma1 tau.
Admitted.

Global Hint Resolve type_closed_add_pre: core.
Global Hint Resolve type_closed_add_post: core.

Fixpoint erase_arr_bindings (Gamma: lcontxt): lcontxt :=
  match Gamma with
  | nil => nil
  | (x, ty)::Gamma =>
      match ty with
      | Oty _ | Uty ([[v: _ | _ ]]) => (x, ty)::(erase_arr_bindings Gamma)
      | _ => (erase_arr_bindings Gamma)
      end
  end.

(* Lemma erase_arr_bindings_spec: forall (Gamma: lcontxt), *)
(*     lcontxt_to_baseconctx Gamma = lcontxt_to_baseconctx (erase_arr_bindings Gamma). *)
(* Proof. *)
(*   intro Gamma. induction Gamma; auto. *)
(*   destruct a as (x, xty). *)
(*   destruct xty. destruct u; simpl; auto. *)
(*   - rewrite IHGamma. auto. *)
(*   - destruct o. simpl. rewrite IHGamma. auto. *)
(* Qed. *)

Lemma type_closed_even_without_arr_bindings: forall Gamma tau,
    type_closed_in_ctx Gamma tau <-> type_closed_in_ctx (erase_arr_bindings Gamma) tau.
Proof.
Admitted.
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
