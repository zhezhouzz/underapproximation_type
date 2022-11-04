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
Import ListNotations.

(* closed_refinement *)

Definition state_order (st st': state) := (forall x c_x, st x = Some c_x -> st' x = Some c_x).
Notation " st '<st<' st' " := (state_order st st') (at level 80).

Lemma state_order_trans: forall st1 st2 st3, st1 <st< st2 -> st2 <st< st3 -> st1 <st< st3.
Proof.
  unfold state_order. intros. auto.
Qed.

Global Hint Resolve state_order_trans: core.

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

(* First we define the non-deter state *)

Definition nstate := string -> option tm.

Definition state_in_non_deter_state (st: state) (nst: nstate): Prop :=
  forall x, (match nst x with
        | None => True
        | Some e =>
            match st x with
            | None => False
            | Some c => e -->* c
            end
        end).

Notation " st '\NSTin' nst " := (state_in_non_deter_state st nst) (at level 40).

Definition phi_sat_nst (phi: refinement) (nst: nstate) (c: constant) :=
  (forall st, st \NSTin nst -> closed_refinement_under_state st phi /\ phi st c).

(* Global Hint Unfold nst_tmR_aux: core. *)

Definition lcontxt := linear_context overunderty.

Inductive closed_refinement_in_ctx : nstate -> lcontxt -> refinement -> Prop :=
| closed_refinement_in_ctx_nil:
  forall nst phi, (forall st, st \NSTin nst -> closed_refinement_under_state st phi) -> closed_refinement_in_ctx nst [] phi
| closed_refinement_in_ctx_cons: forall nst x tau Gamma phi,
    (forall (c_x: constant), empty |- c_x \Vin (ou\_ tau _/) ->
                                closed_refinement_in_ctx (update nst x (tvalue c_x)) Gamma phi) ->
    closed_refinement_in_ctx nst ((x, tau) :: Gamma) phi.

Global Hint Constructors closed_refinement_in_ctx: core.

Lemma closed_refinement_in_ctx_weakening: forall st Gamma1 Gamma2 Gamma3 phi,
    (Gamma1 ++ Gamma2) = Gamma3 ->
    closed_refinement_in_ctx st Gamma2 phi ->
    closed_refinement_in_ctx st Gamma3 phi.
Admitted.

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

Inductive st_type_closed_in_ctx : nstate -> lcontxt -> overunderty -> Prop :=
| st_type_closed_base1: forall st Gamma (T: base_ty) phi,
    closed_refinement_in_ctx st Gamma phi -> st_type_closed_in_ctx st Gamma ({{v: T | phi}})
| st_type_closed_base2: forall st Gamma (T: base_ty) phi,
    closed_refinement_in_ctx st Gamma phi -> st_type_closed_in_ctx st Gamma ([[v: T | phi]])
| st_type_closed_oarr: forall st Gamma x T phi (tau: underty),
    st_type_closed_in_ctx st Gamma ({{v: T | phi}}) ->
    st_type_closed_in_ctx st ((x, Oty ({{v: T | phi}})) :: Gamma) tau ->
    st_type_closed_in_ctx st Gamma (x o: ({{v: T | phi}}) o--> tau)
| st_type_closed_arrarr: forall st Gamma (tau_x: underty) (tau: underty),
    st_type_closed_in_ctx st Gamma tau_x ->
    st_type_closed_in_ctx st Gamma tau ->
    st_type_closed_in_ctx st Gamma (tau_x u--> tau).

Global Hint Constructors st_type_closed_in_ctx: core.

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
  unfold closed_refinement_under_state... intros st Hst st' Hsub.
  remember (Hst x) as Hst'. clear HeqHst'. rewrite update_eq in Hst'.
  destruct (st x) eqn: HH... rewrite Hsub with (c_x := c)... inversion Hst'.
Qed.

Global Hint Resolve mk_eq_over_base_is_type_closed: core.

Lemma mk_eq_under_base_is_type_closed: forall x T phi, type_closed_in_ctx ((x, Uty ([[v:T | phi]]))::nil) (mk_eq_var T x).
Proof with eauto.
  intros x T phi. repeat constructor...
  unfold closed_refinement_under_state... intros st Hst st' Hsub.
  remember (Hst x) as Hst'. clear HeqHst'. rewrite update_eq in Hst'.
  destruct (st x) eqn: HH... rewrite Hsub with (c_x := c)... inversion Hst'.
Qed.

Global Hint Resolve mk_eq_under_base_is_type_closed: core.

Lemma mk_eq_constant_spec (c: constant) : empty |- vconst c \Tin u\_ mk_eq_constant c _/.
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
(*   assert (closed_refinement_in_ctx nil (ty_of_const c) (fun (_ : state) (v : constant) => v = c)). *)
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

Inductive type_ctx_no_dup: lcontxt -> Prop :=
| type_ctx_no_dup_nil: type_ctx_no_dup nil
| type_ctx_no_dup_cons: forall x tau_x Gamma,
    l_find_right_most Gamma x = None ->
    type_ctx_no_dup ((x, tau_x) :: Gamma).

Global Hint Constructors type_ctx_no_dup: core.

Lemma type_ctx_no_dup_cannot_find_last: forall Gamma b tau_b,
    type_ctx_no_dup (Gamma ++ ((b, tau_b)::nil)) -> l_find_right_most Gamma b = None.
Admitted.

Global Hint Resolve type_ctx_no_dup_cannot_find_last: core.

Lemma type_ctx_no_dup_fst_last_diff_name: forall a tau_a Gamma b tau_b,
    type_ctx_no_dup ((a, tau_a)::Gamma ++ ((b, tau_b)::nil)) -> a <> b.
Admitted.

Global Hint Resolve type_ctx_no_dup_fst_last_diff_name: core.

Lemma type_ctx_no_dup_ctx_sub: forall Gamma1 Gamma2,
    type_ctx_no_dup (Gamma1 ++ Gamma2) -> type_ctx_no_dup Gamma1 /\ type_ctx_no_dup Gamma2.
Admitted.

Global Hint Resolve type_ctx_no_dup_ctx_sub: core.

(* st_type_closed_ctx: *)
(*   1. should also have no duplicate names *)
(*   2. types are well_formed *)
(*   3. types are closed *)

Inductive st_type_closed_ctx: nstate -> lcontxt -> Prop :=
| st_type_closed_ctx_nil: forall st, st_type_closed_ctx st nil
| st_type_closed_ctx_cons: forall st x tau_x Gamma,
    l_find_right_most Gamma x = None ->
    st_type_closed_ctx st Gamma ->
    st_type_closed_in_ctx st Gamma tau_x ->
    well_formed_type tau_x ->
    st_type_closed_ctx st (Gamma <l> x :l: tau_x).

Global Hint Constructors st_type_closed_ctx: core.

Lemma st_type_closed_ctx_emp_implies_all: forall st Gamma,
    st_type_closed_ctx empty Gamma -> st_type_closed_ctx st Gamma.
Admitted.

Global Hint Resolve st_type_closed_ctx_emp_implies_all: core.

Lemma st_type_closed_ctx_implies_type_ctx_no_dup: forall st Gamma, st_type_closed_ctx st Gamma -> type_ctx_no_dup Gamma.
Admitted.

Global Hint Resolve st_type_closed_ctx_implies_type_ctx_no_dup: core.

Lemma st_type_closed_ctx_fst_last_diff_name: forall st a tau_a Gamma b tau_b,
    st_type_closed_ctx st ((a, tau_a)::Gamma ++ ((b, tau_b)::nil)) -> a <> b.
Proof with eauto.
  intros. eapply type_ctx_no_dup_fst_last_diff_name...
Qed.

Lemma st_type_closed_ctx_prefix: forall st Gamma1 Gamm2,
    st_type_closed_ctx st (Gamma1 ++ Gamm2) -> st_type_closed_ctx st Gamma1.
Admitted.

(* Lemma st_type_closed_ctx_even_without_arr_bindings: forall st Gamma, *)
(*     st_type_closed_ctx st Gamma -> st_type_closed_ctx (erase_arr_bindings Gamma). *)
(* Admitted. *)
