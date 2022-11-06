Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From PLF Require Import CoreLangSimp.
From PLF Require Import NormalTypeSystemSimp.
From PLF Require Import LinearContext.
From PLF Require Import RfTypeDef.
From PLF Require Import TypeClosedSimp.
From PLF Require Import DenotationSimp.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Lists.List.

Import CoreLangSimp.
Import LinearContext.
Import Nstate.
Import NoDup.
Import TypeClosedSimp.
Import DenotationSimp.
Import TermOrdering.
Import ListNotations.

(* define the non-empty nst *)

Definition well_formed_nst (nst: nstate) := exists st, st \NSTin nst.

Global Hint Unfold well_formed_nst: core.

Lemma constant_has_denotation (c: constant): forall nst, well_formed_nst nst -> tmR_aux nst (mk_eq_constant c) (vconst c).
Proof with eauto.
  intros.
  destruct c. unfold mk_eq_constant.
  - constructor... constructor... constructor... simpl... constructor... constructor...
    intros; subst... destruct H as (st & Hst). apply H1 in Hst... destruct Hst; subst...
  - constructor... constructor... simpl. split. constructor. constructor.
    intros; subst... destruct H as (st & Hst). apply H1 in Hst... destruct Hst; subst...
Qed.

Global Hint Resolve constant_has_denotation: core.

(* It only makes sense when there is no duplicate bindings. *)
Inductive ctx_inv: nstate -> context -> Prop:=
| ctx_inv_nil: forall nst, well_formed_nst nst -> ctx_inv nst nil
| ctx_inv_cons_under: forall nst Gamma x (tau: overunderty),
    ctx_inv nst Gamma ->
    st_type_closed_ctx (st\_ nst _/) (Gamma ++ ((x, tau)::nil)) ->
    (forall e, tmR_in_ctx_aux nst Gamma tau e -> (exists (c: constant), e -->* c)) ->
    ctx_inv nst (Gamma ++ ((x, tau)::nil)).

Global Hint Constructors ctx_inv: core.

Lemma destruct_ctx_inv: forall nst x tau Gamma,
    ctx_inv nst (Gamma ++ ((x, tau)::nil)) ->
    ctx_inv nst Gamma /\
      st_type_closed_ctx (st\_ nst _/) (Gamma ++ ((x, tau)::nil)) /\
      (forall e, tmR_in_ctx_aux nst Gamma tau e -> (exists (c: constant), e -->* c)).
Proof with eauto.
  intros.
  inversion H; subst. apply app_one_eq_nil in H0. inversion H0.
  apply app_inj_tail in H0. inversion H0; subst. inversion H5; subst. split...
Qed.

Lemma destruct_ctx_inv_one: forall nst x tau,
    ctx_inv nst (((x, tau)::nil)) ->
    (well_formed_nst nst) /\
      st_type_closed_ctx (st\_ nst _/) (((x, tau)::nil)) /\
      (forall e, tmR_in_ctx_aux nst [] tau e -> (exists (c: constant), e -->* c)).
Proof with eauto.
  intros. inversion H; subst. apply app_list_unit_eq_unit in H0. inversion H0; subst. inversion H3; subst.
  split...
  inversion H1; subst... symmetry in H5. apply app_one_eq_nil in H5. inversion H5.
Qed.

Lemma ctx_inv_implies_prefix_ctx_inv: forall Gamma2 st Gamma1, ctx_inv st (Gamma1 ++ Gamma2) -> ctx_inv st Gamma1.
Proof with eauto.
  intro Gamma2.
  induction Gamma2; intros st Gamma1 H.
  - rewrite app_nil_r in H...
  - setoid_rewrite <- app_one_is_cons in H... rewrite app_assoc in H. apply IHGamma2 in H. inversion H; subst...
    + destruct a. apply app_one_eq_nil in H0. inversion H0.
    + apply app_inj_tail in H0. destruct H0; subst...
Qed.

Global Hint Resolve ctx_inv_implies_prefix_ctx_inv: core.

Lemma ctx_inv_implies_type_closed_ctx: forall Gamma st,
    ctx_inv st Gamma -> st_type_closed_ctx (st\_ st _/) Gamma.
Proof with eauto.
  apply (rev_ind (fun Gamma => forall st, ctx_inv st Gamma -> st_type_closed_ctx (st\_ st _/) Gamma))...
  intros (x, xty) Gamma H st Hconcat. inversion Hconcat; subst.
  + apply app_one_eq_nil in H0. inversion H0.
  + apply app_inj_tail in H0. destruct H0; subst...
Qed.

Global Hint Resolve ctx_inv_implies_type_closed_ctx: core.

Lemma ctx_inv_implies_no_dup: forall st Gamma,
    ctx_inv st Gamma -> type_ctx_no_dup (st\_ st _/) Gamma.
Admitted.

Global Hint Resolve ctx_inv_implies_no_dup: core.

Lemma ctx_inv_destruct_underbase: forall Gamma nst x T phi e_x,
    ctx_inv nst ((x, Uty ([[v:T | phi]])) :: Gamma) ->
    tmR_aux nst ([[v:T | phi]]) e_x ->
    ctx_inv (x |-> (e_x, T); nst) Gamma.
Proof with eauto.
  apply (rev_ind (fun Gamma => forall nst x T phi e_x,
                      ctx_inv nst ((x, Uty ([[v:T | phi]])) :: Gamma) ->
                      tmR_aux nst ([[v:T | phi]]) e_x ->
                      ctx_inv (x |-> (e_x, T); nst) Gamma)).
  - intros nst x T phi e_x Hinv He_xD.
    apply destruct_ctx_inv_one in Hinv. destruct Hinv as (Hwf & Hclosed & Hnotbot).
    assert (empty \N- e_x \Tin T) as He_xT... eapply tmR_has_type in He_xD...
    constructor... destruct Hwf as (st & Hst).
    assert (exists c : constant, e_x -->* c)... destruct H as (c_x & Hc_xE).
    exists (x |-> c_x; st). intro a.
    destruct (eqb_spec x a); subst...
    + rewrite update_eq. rewrite update_eq. split...
    + rewrite update_neq... rewrite update_neq... apply Hst...
  - intros (a & tau_a) Gamma Hind nst x T phi e_x Hinv He_xD.
    rewrite app_comm_cons in Hinv... apply destruct_ctx_inv in Hinv... destruct Hinv as (H1 & H2 & H3).
    assert (ctx_inv nst ((x, Uty ([[v:T | phi]]))::nil)) as Hfst...
    { rewrite <- app_one_is_cons in H1... }
    apply destruct_ctx_inv_one in Hfst. destruct Hfst as (Hwf & Hclosed & Hnotbot).
    (* assert (type_ctx_no_dup nst (((x, Uty ([[v:T | phi]])) :: Gamma) ++ ((a, tau_a)::nil))) as Hnodup... *)
    assert (type_ctx_no_dup (st\_ nst _/) (((x, Uty ([[v:T | phi]])) :: Gamma))) as Hnodup...
    constructor...
    + simpl. rewrite nstate_to_tystate_hd. eapply st_type_closed_ctx_destruct_front...
    + intros e HeD. apply H3. constructor... rewrite nstate_tystate_same_none...
      exists e_x. split... intros.
      assert (exists c0 : constant, e_x0 -->* c0)...
      eapply step_preserve_ctx_denotation... apply eta_drop_lete_not_bot...
Qed.

(* Global Hint Resolve ctx_inv_destruct_front: core. *)

Global Hint Resolve l_find_right_most_none_neq_tl: core.


Lemma inv_ctx_preserve_denotation: forall nst Gamma tau e,
    ctx_inv nst Gamma ->
    tmR_aux nst tau e ->
    tmR_in_ctx_aux nst Gamma tau e.
Admitted.

(* Lemma inv_ctx_implies_construct_arrarr: forall nst x t1 t2 Gamma tau e, *)
(*     ctx_inv nst ((x, Uty (t1 u--> t2)) :: Gamma) -> *)
(*     tmR_in_ctx_aux nst Gamma tau e -> *)
(*     tmR_in_ctx_aux nst ((x, Uty (t1 u--> t2)) :: Gamma) tau e. *)
(* Admitted. *)

Lemma inv_ctx_implies_head: forall nst x tau_x Gamma,
    ctx_inv nst ((x, tau_x) :: Gamma) -> ctx_inv nst [(x, tau_x)].
Admitted.

Lemma ctx_inv_destruct_underarrarr: forall Gamma nst x t1 t2,
    ctx_inv nst ((x, Uty (t1 u--> t2)) :: Gamma) -> ctx_inv nst Gamma.
Proof with eauto.
  apply (rev_ind (fun Gamma => forall nst x t1 t2,
                      ctx_inv nst ((x, Uty (t1 u--> t2)) :: Gamma) -> ctx_inv nst Gamma))...
  - intros (a & tau_a) Gamma Hind nst x t1 t2 Hinv.
    rewrite app_comm_cons in Hinv... apply destruct_ctx_inv in Hinv... destruct Hinv as (H1 & H2 & H3).
    assert (ctx_inv nst Gamma) as Hinv...
    apply destructst_type_closed_ctx in H2. destruct H2 as (Hnst & Hfind & Hctx & Htau & Hwf)...
    constructor...
    + constructor... eapply st_type_closed_in_ctx_destruct_arrar_front...
    + intros. eapply H3... constructor...
      rewrite nstate_tystate_same_none...
      intros e_x He_xD. assert (exists c0 : constant, e_x -->* c0) as He_xE... apply inv_ctx_implies_head in H1. apply destruct_ctx_inv_one in H1. destruct H1... destruct H1...
      destruct  He_xE as (c0 & Hc0).
      eapply step_preserve_ctx_denotation... apply eta_drop_lete_not_bot...
Qed.

Lemma ctx_inv_destruct_underoarr: forall Gamma nst x a T phia tau_b,
    ctx_inv nst ((x, Uty (a o: ({{v:T | phia}}) o--> tau_b)) :: Gamma) -> ctx_inv nst Gamma.
Proof with eauto.
Admitted.

Lemma ctx_inv_front_destruct_over: forall st a T phi Gamma,
    ctx_inv st ((a, Oty ({{v:T | phi}})) :: Gamma) ->
    (forall c_x, tmR_aux st ({{v:T | phi}}) c_x -> ctx_inv (a |-> (c_x, T); st) Gamma).
Admitted.

Global Hint Resolve ctx_inv_front_destruct_over: core.


Lemma ctx_inv_implies_fresh_binding_last:
  forall Gamma x (tau_x: underty), ctx_inv empty (Gamma <l> x :l: Uty tau_x) -> ~ appear_free_in_underty x tau_x.
Admitted.

Lemma ctx_inv_implies_type_closed_last:
  forall st Gamma x (tau_x: underty),
    ctx_inv st (Gamma <l> x :l: Uty tau_x) -> st_type_closed_in_ctx (st\_ st _/) Gamma tau_x.
Admitted.

Lemma ctx_inv_implies_mem_fresh_and_close:
  forall Gamma1 x (tau_x: underty) Gamma2,
    ctx_inv empty (Gamma1 ++ ((x, Uty tau_x)::nil) ++ Gamma2) ->
    st_type_closed_in_ctx empty (Gamma1 ++ ((x, Uty tau_x)::nil)) tau_x /\
      ~ appear_free_in_underty x tau_x.
Admitted.

Global Hint Resolve ctx_inv_implies_mem_fresh_and_close: core.

(* Lemma ctx_inv_implies_postfix: forall a aty Gamma tau, *)
(*   l_find_right_most Gamma a = None -> *)
(*   name_not_free_in_ctx_and_ty a Gamma tau -> *)
(*   ctx_inv ((a, aty)::Gamma) -> *)
(*   ctx_inv Gamma. *)
(* Admitted. *)


(* Lemma ctx_inv_denotation_right_destruct: forall Gamma x (xty: underty) (tau: underty) e, *)
(*     ctx_inv (Gamma ++ ((x, Uty xty)::nil)) -> *)
(*     tmR_in_ctx_all_st (Gamma ++ ((x, Uty xty)::nil)) tau e -> *)
(*     (name_not_free_in_ctx_and_ty x Gamma tau) -> *)
(*     (exists e_x, tmR_in_ctx_all_st Gamma xty e_x /\ tmR_in_ctx_all_st Gamma tau (tlete x e_x e)). *)
(* Proof with eauto. *)
(*   intros Gamma. *)
(*   induction Gamma; simpl; intros. *)
(*   - inversion H0; subst. *)
(*     + exfalso. apply H7... *)
(*     + clear H7. destruct H8 as (e_x & HexD & HH). exists e_x. split... *)
(*     + destruct H7 as (e_x & HexD & HH). exists e_x. split... *)
(*     + destruct H7 as (e_x & HexD & HH). exists e_x. split... *)
(*   - destruct a as (a, aty). destruct (eqb_spec a x). admit. (* a!= x*) destruct H1 as (Haty & H1). *)
(*     inversion H0; subst. *)
(*     + (* overtype *) admit. *)
(*     + exfalso. admit. *)
(*     + admit. *)
(*     + destruct H7 as (e_a & Ha & HH). *)

(*       destruct H8 as (e_x & Hex & HH). eapply free_ctx_inv_drop in H7... *)
(*       eapply IHGamma in H7... destruct H7 as (e_a & Ha & HHH). *)
(*       assert () *)
(*       inversion H; subst. *)
(*     destruct H1 as (Haty & HH). *)
(*     destruct (eqb_spec x0 x). *)

(*     destruct H7 as (e_x & HexD & HH). exists e_x. split... *)
(*       constructor... constructor... inversion HH; subst. inversion H2; subst. *)
(*       constructor... constructor...  *)

Lemma ctx_inv_drop_last_weakening: forall st Gamma x xty,
    ctx_inv st ((x, xty)::Gamma) ->
    (forall tau,
        st_type_closed_in_ctx (st\_ st _/) Gamma tau ->
        (forall e, tmR_in_ctx_aux st Gamma tau e -> tmR_in_ctx_aux st ((x, xty)::Gamma) tau e)).
Proof with eauto.
  intros st Gamma x xty H tau Hwf e HH.
  eapply tmR_in_ctx_pre_weakening... apply app_one_is_cons.
Qed.

Lemma inv_implies_type_closed_last: forall st Gamma x tau_x,
  ctx_inv st (Gamma ++ ((x, tau_x)::nil)) -> st_type_closed_in_ctx (st\_ st _/) Gamma tau_x.
Admitted.

Definition well_formed (Gamma: context) (tau: underty) := ctx_inv empty Gamma /\ st_type_closed_in_ctx empty Gamma tau.
