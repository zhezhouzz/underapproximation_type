Set Warnings "-notation-overridden,-parsing".
From CT Require Import Maps.
From CT Require Import CoreLang.
From CT Require Import NormalTypeSystem.
From CT Require Import LinearContext.
From CT Require Import RfTypeDef.
From CT Require Import TypeClosed.
From CT Require Import Denotation.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Lists.List.

Import CoreLang.
Import LinearContext.
Import NoDup.
Import Ax.
Import TypeClosed.
Import Denotation.
Import ListNotations.

(* define the non-empty nst *)

(* It only makes sense when there is no duplicate bindings. *)
Inductive ctx_inv: state -> context -> Prop:=
| ctx_inv_nil: forall nst, ctx_inv nst nil
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
  inversion H; subst. apply app_one_eq_nil in H2. inversion H2.
  apply app_inj_tail in H0. inversion H0; subst. inversion H5; subst. split...
Qed.

Lemma destruct_ctx_inv_one: forall nst x tau,
    ctx_inv nst (((x, tau)::nil)) ->
      st_type_closed_ctx (st\_ nst _/) (((x, tau)::nil)) /\
      (forall e, tmR_in_ctx_aux nst [] tau e -> (exists (c: constant), e -->* c)).
Proof with eauto.
  intros. inversion H; subst. apply app_list_unit_eq_unit in H0. inversion H0; subst. inversion H3; subst.
  split...
Qed.

Lemma ctx_inv_implies_prefix_ctx_inv: forall Gamma2 st Gamma1, ctx_inv st (Gamma1 ++ Gamma2) -> ctx_inv st Gamma1.
Proof with eauto.
  intro Gamma2.
  induction Gamma2; intros st Gamma1 H.
  - rewrite app_nil_r in H...
  - setoid_rewrite <- app_one_is_cons in H... rewrite app_assoc in H. apply IHGamma2 in H. inversion H; subst...
    + destruct a. apply app_one_eq_nil in H2. inversion H2.
    + apply app_inj_tail in H0. destruct H0; subst...
Qed.

Global Hint Resolve ctx_inv_implies_prefix_ctx_inv: core.

Lemma ctx_inv_implies_type_closed_ctx: forall Gamma st,
    ctx_inv st Gamma -> st_type_closed_ctx (st\_ st _/) Gamma.
Proof with eauto.
  apply (rev_ind (fun Gamma => forall st, ctx_inv st Gamma -> st_type_closed_ctx (st\_ st _/) Gamma))...
  intros (x, xty) Gamma H st Hconcat. inversion Hconcat; subst.
  + apply app_one_eq_nil in H2. inversion H2.
  + apply app_inj_tail in H0. destruct H0; subst...
Qed.

Global Hint Resolve ctx_inv_implies_type_closed_ctx: core.

Lemma tmR_in_ctx_pre_weakening: forall nst Gamma1 Gamma2 (tau: overunderty),
    ctx_inv nst (Gamma1 ++ Gamma2) ->
    st_type_closed_in_ctx (state_to_tystate nst) Gamma2 tau ->
    (forall e, tmR_in_ctx_aux nst Gamma2 tau e -> tmR_in_ctx_aux nst (Gamma1 ++ Gamma2) tau e).
Proof with eauto.
  intros.
  induction Gamma1...
  - destruct a.
    rewrite <- app_comm_cons in H.
    assert (type_ctx_no_dup (st\_ nst _/) ((s, o) :: Gamma1 ++ Gamma2))...
    apply type_ctx_no_dup_implies_head_free in H2. apply l_find_right_most_none_neq_hd in H2. exfalso...
Qed.

Lemma tmR_in_ctx_post_weakening: forall nst Gamma1 Gamma2 (tau: overunderty),
    ctx_inv nst (Gamma1 ++ Gamma2) ->
    st_type_closed_in_ctx (st\_ nst _/) Gamma1 tau ->
    (forall e, tmR_in_ctx_aux nst Gamma1 tau e -> tmR_in_ctx_aux nst (Gamma1 ++ Gamma2) tau e).
Proof with eauto.
  intros.
  induction Gamma1... simpl. eapply tmR_in_ctx_pre_weakening in H1. rewrite app_nil_r in H1. apply H1. rewrite app_nil_r ...
  auto.
  - destruct a.
    rewrite <- app_comm_cons in H.
    assert (type_ctx_no_dup (st\_ nst _/) ((s, o) :: Gamma1 ++ Gamma2))...
    apply type_ctx_no_dup_implies_head_free in H2. apply l_find_right_most_none_neq_hd in H2. exfalso...
Qed.

Lemma ctx_inv_implies_no_dup: forall st Gamma,
    ctx_inv st Gamma -> type_ctx_no_dup (st\_ st _/) Gamma.
Proof with eauto.
  intros. destruct H...
Qed.

Global Hint Resolve ctx_inv_implies_no_dup: core.

Definition state_tystate_same_none: forall nst x, nst x = None <-> (st\_ nst _/) x = None.
Proof with eauto.
  intros.
  split; intros... unfold state_to_tystate. rewrite H... unfold state_to_tystate in H. destruct (nst x)...
  inversion H.
Qed.

Global Hint Rewrite state_tystate_same_none: core.

Lemma ctx_inv_destruct_underbase: forall Gamma st id T phi e_x (v_x_hat: constant),
    ctx_inv st ((id, Uty ([[v:T | phi]])) :: Gamma) ->
    tmR_aux st ([[v:T | phi]]) e_x ->
    e_x -->* v_x_hat ->
    ctx_inv (id |-> v_x_hat; st) Gamma.
Proof with eauto.
  apply (rev_ind (fun Gamma => forall  st id T phi e_x (v_x_hat: constant),
                      ctx_inv st ((id, Uty ([[v:T | phi]])) :: Gamma) ->
                      tmR_aux st ([[v:T | phi]]) e_x ->
                      e_x -->* v_x_hat ->
                      ctx_inv (id |-> v_x_hat; st) Gamma)).
  - intros  st id T phi e_x v_x_hat Hinv He_xD HE.
    apply destruct_ctx_inv_one in Hinv. destruct Hinv as (Hclosed & Hnotbot).
    assert (empty \N- e_x \Tin (T)) as He_xT... eapply tmR_has_type in He_xD...
  - intros (a & tau_a) Gamma Hind st id T phi e_x v_x_hat Hinv He_xD HE.
    assert (type_ctx_no_dup (st\_ st _/) ((id, Uty ([[v:T | phi]])) :: Gamma ++ ((a, tau_a)::nil)))...
    apply type_ctx_no_dup_implies_head_free in H. apply l_find_right_most_none_neq_hd in H. exfalso...
Qed.

(* Lemma ctx_inv_destruct_underbase: forall Gamma st x T phi (e_x: constant), *)
(*     ctx_inv st ((id, Uty ([[v:T | phi]])) :: Gamma) -> *)
(*     tmR_aux st ([[v:T | phi]]) e_x -> *)
(*     e_x -->* v_x_hat -> *)
(*     ctx_inv (id |-> v_x_hat; st) Gamma. *)
(* Proof with eauto. *)
(*   apply (rev_ind (fun Gamma => forall nst x T phi (e_x: constant), *)
(*                       ctx_inv nst ((x, Uty ([[v:T | phi]])) :: Gamma) -> *)
(*                       (forall v_x, tmR_aux nst ([[v: T | phi]]) v_x -> *)
(*                               ctx_inv (x |-> e_x; nst) Gamma))). *)
(*   - intros nst x T phi e_x Hinv v_x He_xD. *)
(*     apply destruct_ctx_inv_one in Hinv. destruct Hinv as (Hclosed & Hnotbot). *)
(*     assert (empty \N- e_x \Tin (T)) as He_xT... eapply tmR_has_type in He_xD... *)
(*   - intros (a & tau_a) Gamma Hind nst x phi e_x Hinv He_xD. *)
(*     rewrite app_comm_cons in Hinv... apply destruct_ctx_inv in Hinv... destruct Hinv as (H1 & H2 & H3). *)
(*     assert (ctx_inv nst ((x, Uty ([[v:(T) | phi]]))::nil)) as Hfst... *)
(*     { rewrite <- app_one_is_cons in H1... } *)
(*     apply destruct_ctx_inv_one in Hfst. destruct Hfst as (Hwf & Hclosed & Hnotbot). *)
(*     (* assert (type_ctx_no_dup nst (((x, Uty ([[v:T | phi]])) :: Gamma) ++ ((a, tau_a)::nil))) as Hnodup... *) *)
(*     assert (type_ctx_no_dup (st\_ nst _/) (((x, Uty ([[v: (T) | phi]])) :: Gamma))) as Hnodup... *)
(*     constructor... *)
(*     + simpl. rewrite nstate_to_tystate_hd. eapply st_type_closed_ctx_destruct_front... *)
(*     + intros e HeD. *)
(*       apply H3. constructor... rewrite state_tystate_same_none... *)
(*       exists e_x. split... intros ee HeeD c'. *)
(*       destruct tau_a; split... *)
(*       { assert (exists c0 : constant, e_x0 -->* c0)... *)
(*         eapply step_preserve_ctx_denotation... apply eta_drop_lete_not_bot... } *)
(*       { eapply step_preserve_ctx_denotation_over... apply eta_drop_lete_not_bot... } *)
(* Qed. *)

(* Global Hint Resolve ctx_inv_destruct_front: core. *)

Global Hint Resolve l_find_right_most_none_neq_tl: core.


Lemma inv_ctx_preserve_denotation: forall nst Gamma tau e,
    ctx_inv nst Gamma ->
    tmR_aux nst tau e ->
    tmR_in_ctx_aux nst Gamma tau e.
Proof with eauto.
  intros.
  induction Gamma. constructor...
  - destruct a. assert (type_ctx_no_dup (st\_ nst _/) ((s, o) :: Gamma))...
    apply type_ctx_no_dup_implies_head_free in H1. apply l_find_right_most_none_neq_hd in H1. exfalso...
Qed.

(* Lemma inv_ctx_implies_construct_arrarr: forall nst x t1 t2 Gamma tau e, *)
(*     ctx_inv nst ((x, Uty (t1 u--> t2)) :: Gamma) -> *)
(*     tmR_in_ctx_aux nst Gamma tau e -> *)
(*     tmR_in_ctx_aux nst ((x, Uty (t1 u--> t2)) :: Gamma) tau e. *)

Lemma inv_ctx_implies_head: forall nst x tau_x Gamma,
    ctx_inv nst ((x, tau_x) :: Gamma) -> ctx_inv nst [(x, tau_x)].
Proof with eauto.
  intros.
  induction Gamma...
  - destruct a. assert (type_ctx_no_dup (st\_ nst _/) ((x, tau_x) :: (s, o) :: Gamma))...
    apply type_ctx_no_dup_implies_head_free in H0. apply l_find_right_most_none_neq_hd in H0. exfalso...
Qed.

Lemma ctx_inv_destruct_underarrarr: forall Gamma nst x t1 t2,
    ctx_inv nst ((x, Uty (t1 u--> t2)) :: Gamma) -> ctx_inv nst Gamma.
Proof with eauto.
  apply (rev_ind (fun Gamma => forall nst x t1 t2,
                      ctx_inv nst ((x, Uty (t1 u--> t2)) :: Gamma) -> ctx_inv nst Gamma))...
  - intros (a & tau_a) Gamma Hind nst x t1 t2 Hinv.
    assert (type_ctx_no_dup (st\_ nst _/) ((x, Uty (t1 u--> t2)) :: Gamma <l> a :l: tau_a)) as Hnodup. apply ctx_inv_implies_no_dup in Hinv...
    rewrite app_comm_cons in Hinv... apply destruct_ctx_inv in Hinv... destruct Hinv as (H1 & H2 & H3).
    assert (ctx_inv nst Gamma) as Hinv...
    apply destructst_type_closed_ctx in H2. destruct H2 as (Hnst & Hfind & Hctx & Htau & Hwf)...
    constructor...
    + constructor; auto. inversion Hnodup... eapply st_type_closed_in_ctx_destruct_arrar_front in Htau...
    + intros. eapply H3... constructor...
      (* rewrite state_tystate_same_none... *)
      (* assert (l_find_right_most Gamma x = None) as Hfindx... *)
      intros e_x He_xD. assert (exists c0 : constant, e_x -->* c0) as He_xE... apply inv_ctx_implies_head in H1. apply destruct_ctx_inv_one in H1. destruct H1...
      destruct tau_a.
      { eapply step_preserve_ctx_denotation... apply eta_drop_lete_not_bot... }
      { eapply step_preserve_ctx_denotation_over... apply eta_drop_lete_not_bot... }
Qed.

Lemma ctx_inv_destruct_underoarr: forall Gamma nst x a T phia tau_b,
    ctx_inv nst ((x, Uty (a o: ({{v:T | phia}}) o--> tau_b)) :: Gamma) -> ctx_inv nst Gamma.
Proof with eauto.
  apply (rev_ind (fun Gamma => forall nst x a T phia tau_b,
                      ctx_inv nst ((x, Uty (a o: ({{v:T | phia}}) o--> tau_b)) :: Gamma) -> ctx_inv nst Gamma))...
  - intros (a & tau_a) Gamma Hind nst x y T phiy tau_b Hinv.
    assert (type_ctx_no_dup (st\_ nst _/) ((x, Uty (y o: {{v:T | phiy}} o--> tau_b)) :: Gamma <l> a :l: tau_a)) as Hnodup. apply ctx_inv_implies_no_dup in Hinv...
    rewrite app_comm_cons in Hinv... apply destruct_ctx_inv in Hinv... destruct Hinv as (H1 & H2 & H3).
    assert (ctx_inv nst Gamma) as Hinv...
    apply destructst_type_closed_ctx in H2. destruct H2 as (Hnst & Hfind & Hctx & Htau & Hwf)...
    constructor...
    + constructor; auto. inversion Hnodup... eapply st_type_closed_in_ctx_destruct_oarr_front in Htau...
    + intros. eapply H3... constructor...
      (* rewrite state_tystate_same_none... *)
      (* assert (l_find_right_most Gamma x = None) as Hfindx... *)
      intros e_x He_xD. assert (exists c0 : constant, e_x -->* c0) as He_xE... apply inv_ctx_implies_head in H1. apply destruct_ctx_inv_one in H1. destruct H1...
      destruct tau_a.
      { eapply step_preserve_ctx_denotation... apply eta_drop_lete_not_bot... }
      { eapply step_preserve_ctx_denotation_over... apply eta_drop_lete_not_bot... }
Qed.

Lemma ctx_inv_front_destruct_over: forall st a T phi Gamma,
    ctx_inv st ((a, Oty ({{v:T | phi}})) :: Gamma) ->
    (forall (c_x: constant), tmR_aux st ({{v:T | phi}}) c_x -> ctx_inv (a |-> c_x; st) Gamma).
Proof with eauto.
  intros.
  induction Gamma. inversion H; subst.
  assert (type_ctx_no_dup (st\_ st _/) ((x, tau):: nil)). constructor...
  apply type_ctx_no_dup_implies_head_free in H4. apply l_find_right_most_none_neq_hd in H4. exfalso...
  assert (type_ctx_no_dup (st\_ st _/) ((a, Oty ({{v:T | phi}})) :: nil) ). constructor...
  apply type_ctx_no_dup_implies_head_free in H1. apply l_find_right_most_none_neq_hd in H1. exfalso...
Qed.

Global Hint Resolve ctx_inv_front_destruct_over: core.


Lemma ctx_inv_implies_fresh_binding_last:
  forall Gamma x (tau_x: underty), ctx_inv empty (Gamma <l> x :l: Uty tau_x) -> ~ appear_free_in_underty x tau_x.
Proof with eauto.
  intros. apply destruct_ctx_inv in H. destruct H. destruct H0...
  apply destructst_type_closed_ctx in H0.
  destruct H0 as (Ha & Hb & Hc & Hd & He)...
  inversion Hd.
  assert (type_ctx_no_dup empty ((x, Uty tau_x) :: nil) ). constructor...
  apply type_ctx_no_dup_implies_head_free in H3. apply l_find_right_most_none_neq_hd in H3. exfalso...
Qed.


Lemma ctx_inv_implies_type_closed_last:
  forall st Gamma x (tau_x: underty),
    ctx_inv st (Gamma <l> x :l: Uty tau_x) -> st_type_closed_in_ctx (st\_ st _/) Gamma tau_x.
Proof with eauto.
  intros. apply destruct_ctx_inv in H. destruct H. destruct H0...
  apply destructst_type_closed_ctx in H0.
  destruct H0 as (Ha & Hb & Hc & Hd & He)...
Qed.


Lemma ctx_inv_implies_mem_fresh_and_close:
  forall Gamma1 x (tau_x: underty) Gamma2,
    ctx_inv empty (Gamma1 ++ ((x, Uty tau_x)::nil) ++ Gamma2) ->
    st_type_closed_in_ctx empty (Gamma1 ++ ((x, Uty tau_x)::nil)) tau_x /\
      ~ appear_free_in_underty x tau_x.
Proof with eauto.
  intros.
  assert (type_ctx_no_dup empty ((x, Uty tau_x) :: nil) ). constructor...
  apply type_ctx_no_dup_implies_head_free in H0. apply l_find_right_most_none_neq_hd in H0. exfalso...
Qed.

Global Hint Resolve ctx_inv_implies_mem_fresh_and_close: core.

(* Lemma ctx_inv_implies_postfix: forall a aty Gamma tau, *)
(*   l_find_right_most Gamma a = None -> *)
(*   name_not_free_in_ctx_and_ty a Gamma tau -> *)
(*   ctx_inv ((a, aty)::Gamma) -> *)
(*   ctx_inv Gamma. *)


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
  rewrite <- app_one_is_cons.
  eapply tmR_in_ctx_pre_weakening...
Qed.

Lemma inv_implies_type_closed_last: forall st Gamma x tau_x,
  ctx_inv st (Gamma ++ ((x, tau_x)::nil)) -> st_type_closed_in_ctx (st\_ st _/) Gamma tau_x.
Proof with eauto.
  intros.
  inversion H... apply app_one_eq_nil in H2. inversion H2.
  apply app_inj_tail  in H0. destruct H0. subst...
  assert (type_ctx_no_dup (st\_ st _/) ((x, tau_x)::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H0. apply l_find_right_most_none_neq_hd in H0. exfalso...
Qed.


Definition well_formed (Gamma: context) (tau: underty) := ctx_inv empty Gamma /\ st_type_closed_in_ctx empty Gamma tau.
