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

Definition ncontxt := linear_context ty.
Definition lcontxt := linear_context overunderty.

Fixpoint ncontxt_to_basic_ctx_aux (Gamma: ncontxt) (ctx: partial_map ty) :=
  match Gamma with
  | nil => ctx
  | (x, xty) :: Gamma => ncontxt_to_basic_ctx_aux Gamma (update ctx x xty)
  end.

Definition ncontxt_to_basic_ctx (Gamma: ncontxt) := ncontxt_to_basic_ctx_aux Gamma empty.

Global Hint Unfold ncontxt_to_basic_ctx: core.

Inductive type_closed_refinement : ncontxt -> base_ty -> refinement -> Prop :=
| type_closed_refinement_nil: forall T phi, is_closed_refinement T phi -> type_closed_refinement [] T phi
| type_closed_refinement_cons: forall x T Gamma Tphi phi,
    (forall (c_x: constant), empty |- c_x \Vin T ->
                                type_closed_refinement Gamma Tphi (fun st c => phi (t_update st x c_x) c)) ->
    type_closed_refinement ((x, T) :: Gamma) Tphi phi.

Global Hint Constructors type_closed_refinement: core.

Lemma type_closed_refinement_weakening: forall Gamma1 Gamma2 Gamma3 T phi,
    (Gamma1 ++ Gamma2) = Gamma3 ->
    type_closed_refinement Gamma2 T phi ->
    type_closed_refinement Gamma3 T phi.
Admitted.

Module ExCloseRefinement.

  Example true_refinement: is_closed_refinement TNat (fun _ c => c = 3).
  Proof.
    unfold is_closed_refinement. reflexivity.
  Qed.

  (* When the type dismatch, it is equal to bottom refinement *)
  Example true_refinement2: is_closed_refinement TNat (fun _ c => c = true).
  Proof.
    unfold is_closed_refinement. reflexivity.
  Qed.

  Example refinement_under_nctx: type_closed_refinement [("x"%string, TBase TNat)] TNat (fun st c => c = st "x"%string).
  Proof.
    constructor. intros.
    apply nat_value_n_exists in H. destruct H; subst.
    constructor. constructor; subst; auto.
  Qed.

End ExCloseRefinement.

Inductive overunderbasety : Type :=
| Obase: base_ty -> refinement -> overunderbasety
| Ubase: base_ty -> refinement -> overunderbasety.

Definition basecontxt := linear_context overunderbasety.

Fixpoint erase_basetypectx (Gamma: basecontxt): ncontxt :=
  match Gamma with
  | nil => nil
  | (x, Obase T _)::Gamma => (x, TBase T)::(erase_basetypectx Gamma)
  | (x, Ubase T _)::Gamma => (x, TBase T)::(erase_basetypectx Gamma)
  end.

Fixpoint erase_lctx (Gamma: lcontxt): ncontxt :=
  match Gamma with
  | nil => nil
  | (x, tau)::Gamma => (x, ou\_ tau _/)::(erase_lctx Gamma)
  end.

Definition lctx_to_basic_ctx (Gamma: lcontxt) := ncontxt_to_basic_ctx_aux (erase_lctx Gamma) empty.

Global Hint Unfold lctx_to_basic_ctx: core.

Inductive type_closed_in_basectx : basecontxt -> overunderty -> Prop :=
| type_closed_base1: forall Gamma (T: base_ty) phi,
    type_closed_refinement (erase_basetypectx Gamma) T phi ->
    type_closed_in_basectx Gamma ({{v: T | phi}})
| type_closed_base2: forall Gamma (T: base_ty) phi,
    type_closed_refinement (erase_basetypectx Gamma) T phi ->
    type_closed_in_basectx Gamma ([[v: T | phi]])
| type_closed_oarr: forall Gamma x T phi (tau: underty),
    type_closed_in_basectx Gamma ({{v: T | phi}}) ->
    type_closed_in_basectx ((x, Obase T phi) :: Gamma) tau ->
    type_closed_in_basectx Gamma (x o: ({{v: T | phi}}) o--> tau)
| type_closed_arrarr: forall Gamma (tau_x: underty) (tau: underty),
    type_closed_in_basectx Gamma tau_x ->
    type_closed_in_basectx Gamma tau ->
    type_closed_in_basectx Gamma (tau_x u--> tau).

Global Hint Constructors type_closed_in_basectx: core.

(* before erase refinement from the context, we will erase all function type bindings *)
Fixpoint lcontxt_to_baseconctx (Gamma: lcontxt): basecontxt :=
  match Gamma with
  | nil => nil
  | (x, ty)::Gamma =>
      match ty with
      | Oty ({{v: T | phi }}) => (x, Obase T phi)::(lcontxt_to_baseconctx Gamma)
      | Uty ([[v: T | phi ]]) => (x, Ubase T phi)::(lcontxt_to_baseconctx Gamma)
      | _ => (lcontxt_to_baseconctx Gamma)
      end
  end.

Definition type_closed (Gamma: lcontxt) (tau: overunderty):=
  type_closed_in_basectx (lcontxt_to_baseconctx Gamma) tau.

Global Hint Unfold type_closed: core.

(* type closed \S{Ty} *)

Lemma mk_op_is_type_closed: forall op a b, type_closed l_empty (mk_op op a b).
Admitted.

Global Hint Resolve mk_op_is_type_closed: core.

Lemma mk_eq_over_base_is_type_closed: forall x T phi, type_closed ((x, Oty ({{v:T | phi}}))::nil) (mk_eq_var T x).
Proof with eauto.
  intros x T phi. repeat constructor...
  unfold is_closed_refinement... intros st st'. rewrite t_update_eq. rewrite t_update_eq. reflexivity.
Qed.

Global Hint Resolve mk_eq_over_base_is_type_closed: core.

Lemma mk_eq_under_base_is_type_closed: forall x T phi, type_closed ((x, Uty ([[v:T | phi]]))::nil) (mk_eq_var T x).
Proof with eauto.
  intros x T phi. repeat constructor...
  unfold is_closed_refinement... intros st st'. rewrite t_update_eq. rewrite t_update_eq. reflexivity.
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

Lemma constant_eq_is_close: forall Gamma c, type_closed Gamma (mk_eq_constant c).
Proof.
  intros. constructor.
  assert (type_closed_refinement nil (ty_of_const c) (fun (_ : state) (v : constant) => v = c)).
  constructor.
  unfold is_closed_refinement; destruct c; simpl; reflexivity.
  apply type_closed_refinement_weakening with (Gamma1 := (erase_basetypectx (lcontxt_to_baseconctx Gamma))) (Gamma2 := nil); auto.
  rewrite app_nil_r. auto.
Qed.

Global Hint Resolve mk_eq_constant_spec: core.
Global Hint Resolve subst_constant_eq: core.
Global Hint Resolve constant_eq_is_close: core.

(* type closed lemmas *)

Lemma type_closed_add_post: forall (Gamma1 Gamma2: lcontxt) (tau: overunderty),
    type_closed Gamma1 tau -> type_closed (Gamma1 ++ Gamma2) tau.
Admitted.

Lemma type_closed_add_pre: forall (Gamma1 Gamma2: lcontxt) (tau: overunderty),
    type_closed Gamma2 tau -> type_closed (Gamma1 ++ Gamma2) tau.
Admitted.

Lemma type_closed_drop_post: forall (Gamma1 Gamma2: lcontxt) (tau: overunderty),
	(forall x tau_x, l_find_right_most Gamma2 x = Some tau_x -> ~ appear_free_in_overunderty x tau) ->
	type_closed (Gamma1 ++ Gamma2) tau -> type_closed Gamma1 tau.
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

Lemma erase_arr_bindings_spec: forall (Gamma: lcontxt),
    lcontxt_to_baseconctx Gamma = lcontxt_to_baseconctx (erase_arr_bindings Gamma).
Proof.
  intro Gamma. induction Gamma; auto.
  destruct a as (x, xty).
  destruct xty. destruct u; simpl; auto.
  - rewrite IHGamma. auto.
  - destruct o. simpl. rewrite IHGamma. auto.
Qed.

Lemma type_closed_even_without_arr_bindings: forall Gamma tau,
    type_closed Gamma tau <-> type_closed (erase_arr_bindings Gamma) tau.
Proof.
  unfold type_closed. setoid_rewrite <- erase_arr_bindings_spec. reflexivity.
Qed.

Definition lcontxt_to_basic_ctx (Gamma: lcontxt) := ncontxt_to_basic_ctx (erase_basetypectx (lcontxt_to_baseconctx Gamma)).

Global Hint Unfold lcontxt_to_basic_ctx: core.

Inductive type_ctx_no_dup: lcontxt -> Prop :=
| type_ctx_no_dup_nil: type_ctx_no_dup nil
| type_ctx_no_dup_cons: forall x tau_x Gamma,
    l_find_right_most Gamma x = None ->
    type_ctx_no_dup (Gamma <l> x :l: tau_x).

Global Hint Constructors type_ctx_no_dup: core.

Lemma type_ctx_no_dup_fst_last_diff_name: forall a tau_a Gamma b tau_b,
    type_ctx_no_dup ((a, tau_a)::Gamma ++ ((b, tau_b)::nil)) -> a <> b.
Admitted.

Global Hint Resolve type_ctx_no_dup_fst_last_diff_name: core.

Lemma type_ctx_no_dup_ctx_sub: forall Gamma1 Gamma2,
    type_ctx_no_dup (Gamma1 ++ Gamma2) -> type_ctx_no_dup Gamma1 /\ type_ctx_no_dup Gamma2.
Admitted.

Global Hint Resolve type_ctx_no_dup_ctx_sub: core.

(* type_closed_ctx: *)
(*   1. should also have no duplicate names *)
(*   2. types are well_formed *)
(*   3. types are closed *)

Inductive type_closed_ctx: lcontxt -> Prop :=
| type_closed_ctx_nil: type_closed_ctx nil
| type_closed_ctx_cons: forall x tau_x Gamma,
    l_find_right_most Gamma x = None ->
    type_closed_ctx Gamma ->
    type_closed Gamma tau_x ->
    well_formed_type tau_x ->
    type_closed_ctx (Gamma <l> x :l: tau_x).

Global Hint Constructors type_closed_ctx: core.

Lemma type_closed_ctx_implies_type_ctx_no_dup: forall Gamma, type_closed_ctx Gamma -> type_ctx_no_dup Gamma.
Admitted.

Global Hint Resolve type_closed_ctx_implies_type_ctx_no_dup: core.

Lemma type_closed_ctx_fst_last_diff_name: forall a tau_a Gamma b tau_b,
    type_closed_ctx ((a, tau_a)::Gamma ++ ((b, tau_b)::nil)) -> a <> b.
Admitted.

Lemma type_closed_ctx_prefix: forall Gamma1 Gamm2,
    type_closed_ctx (Gamma1 ++ Gamm2) -> type_closed_ctx Gamma1.
Admitted.

Lemma type_closed_ctx_even_without_arr_bindings: forall Gamma,
    type_closed_ctx Gamma -> type_closed_ctx (erase_arr_bindings Gamma).
Admitted.
