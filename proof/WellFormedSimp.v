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
Import TypeClosedSimp.
Import DenotationSimp.
Import ListNotations.

(* It only makes sense when there is no duplicate bindings. *)
Inductive ctx_inv: context -> Prop:=
| ctx_inv_nil: ctx_inv nil
| ctx_inv_cons_under: forall Gamma x (tau: overunderty),
    ctx_inv Gamma ->
    type_closed_ctx (Gamma ++ ((x, tau)::nil)) ->
    (forall e, tmR_in_ctx_all_st Gamma tau e -> (exists (v: value), e -->* v)) ->
    ctx_inv (Gamma ++ ((x, tau)::nil)).

Global Hint Constructors ctx_inv: core.

(* (* reverse induction principle. *) *)
(* Lemma ctx_inv_rev_inversion: forall Gamma x tau, *)
(*     ctx_inv (Gamma ++ ((x, tau)::nil)) -> *)
(*     (well_formed_ctx (Gamma ++ ((x, tau)::nil)) /\ *)
(*        ctx_inv Gamma /\ *)
(*        l_find_right_most Gamma x = None /\ *)
(*        (forall e, tmR_in_ctx_all_st Gamma tau e -> (exists (v: value), e -->* v))). *)
(* Proof. *)
(*   intros. *)
(*   inversion H. *)
(*   - apply app_one_eq_nil in H1. inversion H1. *)
(*   - apply app_inj_tail in H0. destruct H0. inversion H5; subst. repeat split; auto. *)
(* Qed. *)

Lemma ctx_inv_implies_prefix_ctx_inv: forall Gamma2 Gamma1, ctx_inv (Gamma1 ++ Gamma2) -> ctx_inv Gamma1.
Proof with eauto.
  intro Gamma2.
  induction Gamma2; intros Gamma1 H.
  - rewrite app_nil_r in H...
  - setoid_rewrite <- app_one_is_cons in H... rewrite app_assoc in H. apply IHGamma2 in H. inversion H...
    + symmetry in H1. apply app_eq_nil in H1. destruct H1. inversion H1.
    + apply app_inj_tail in H0. destruct H0; subst...
Qed.

Lemma ctx_inv_implies_type_closed_ctx: forall Gamma, ctx_inv Gamma -> type_closed_ctx Gamma.
Proof with eauto.
  apply (rev_ind (fun Gamma => ctx_inv Gamma -> type_closed_ctx Gamma))...
  intros (x, xty) Gamma H Hconcat. inversion Hconcat. symmetry in H1. apply app_eq_nil in H1. destruct H1. inversion H1.
  apply app_inj_tail in H0.
  destruct H0; subst...
Qed.

Lemma ctx_inv_implies_fresh_binding_last:
  forall Gamma x (tau_x: underty), ctx_inv (Gamma <l> x :l: Uty tau_x) -> ~ appear_free_in_underty x tau_x.
Admitted.

Lemma ctx_inv_implies_type_closed_last:
  forall Gamma x (tau_x: underty), ctx_inv (Gamma <l> x :l: Uty tau_x) -> type_closed Gamma tau_x.
Admitted.

Lemma ctx_inv_implies_mem_fresh_and_close:
  forall Gamma1 x (tau_x: underty) Gamma2, ctx_inv (Gamma1 ++ ((x, Uty tau_x)::nil) ++ Gamma2) ->
                                      type_closed (Gamma1 ++ ((x, Uty tau_x)::nil)) tau_x /\ ~ appear_free_in_underty x tau_x.
Admitted.

Global Hint Resolve ctx_inv_implies_mem_fresh_and_close: core.

(* Lemma ctx_inv_implies_postfix: forall a aty Gamma tau, *)
(*   l_find_right_most Gamma a = None -> *)
(*   name_not_free_in_ctx_and_ty a Gamma tau -> *)
(*   ctx_inv ((a, aty)::Gamma) -> *)
(*   ctx_inv Gamma. *)
(* Admitted. *)

Lemma lete_ctx_inv_implies_safe_dropping_1_to_1: forall Gamma x tau_x tau,
    ~ appear_free_in_underty x tau_x ->
    ctx_inv (Gamma ++ ((x, Uty tau_x)::nil)) ->
    (forall e, tmR_in_ctx_all_st (Gamma ++ ((x, Uty tau_x)::nil)) tau e ->
          (forall e_x, tmR_in_ctx_all_st Gamma tau_x e_x -> tmR_in_ctx_all_st Gamma tau (tlete x e_x e))).
Admitted.

Lemma tletbiop_ctx_inv_implies_safe_dropping_1_to_1: forall Gamma x tau,
    (forall e op (v1 v2: cid),
        ctx_inv (Gamma ++ ((x, Uty (mk_op_retty_from_cids op v1 v2))::nil)) ->
        tmR_in_ctx_all_st (Gamma ++ ((x, Uty (mk_op_retty_from_cids op v1 v2))::nil)) tau e ->
        tmR_in_ctx_all_st (Gamma <l> x :l: ((mk_op_retty_from_cids op v1 v2))) tau e ->
        tmR_in_ctx_all_st Gamma tau (tletbiop x op v1 v2 e)).
Admitted.

Lemma tletapp_oarr_ctx_inv_implies_safe_dropping_1_to_1: forall Gamma x tau_x tau,
    (forall e (v1: value) (v2: cid) a T phi1,
        tmR_in_ctx_all_st Gamma (a o: {{v: T | phi1}} o--> tau_x) v1 ->
        tmR_in_ctx_all_st Gamma ([[v: T | phi1]]) v2 ->
        ctx_inv (Gamma ++ ((x, Uty (under_subst_cid a v2 tau_x))::nil)) ->
        tmR_in_ctx_all_st (Gamma ++ ((x, Uty (under_subst_cid a v2 tau_x))::nil)) tau e ->
        tmR_in_ctx_all_st Gamma tau (tletapp x v1 v2 e)).
Admitted.

Lemma tletapp_arrarr_ctx_inv_implies_safe_dropping_1_to_1: forall Gamma x tau_x tau,
    ~ appear_free_in_underty x tau_x ->
    ctx_inv (Gamma ++ ((x, Uty tau_x)::nil)) ->
    (forall e, tmR_in_ctx_all_st (Gamma ++ ((x, Uty tau_x)::nil)) tau e ->
          (forall (v1 v2: value) t1,
              tmR_in_ctx_all_st Gamma (t1 u--> tau_x) v1 ->
              tmR_in_ctx_all_st Gamma t1 v2 ->
              tmR_in_ctx_all_st Gamma tau (tletapp x v1 v2 e))).
Admitted.


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

Lemma ctx_inv_drop_last_weakening: forall Gamma x xty,
    ctx_inv ((x, xty)::Gamma) ->
    (forall tau,
        type_closed Gamma tau ->
        (forall e, tmR_in_ctx_all_st Gamma tau e -> tmR_in_ctx_all_st ((x, xty)::Gamma) tau e)).
Proof with eauto.
  intros Gamma x xty H tau Hwf e st st'.
  eapply tmR_in_ctx_pre_weakening... apply app_one_is_cons.
Qed.

(* Lemma tmR_in_ctx_post_weakening: forall Gamma (tau: underty), *)
(*     well_formed l_empty tau -> *)
(*     (forall e, tmR_in_ctx_all_st l_empty tau e -> tmR_in_ctx_all_st Gamma tau e). *)
(* Proof with eauto. *)
(* Admitted. *)


(*       Lemma bigstep_lete: forall x e_x e (v: value), *)
(*     ((tlete x e_x e) -->* v) <-> (exists (v_x: value), e_x -->* v_x /\ [x := v_x] e -->* v). *)
(* Proof. *)
(* Admitted. *)

(* Lemma lete_preverse_denotation: forall tau x e_x e, *)
(*     ~ (x \FVty tau) -> *)
(*     under_tmR tau (tlete x e_x e) <-> *)
(*       (exists (v_x: value), e_x -->* v_x -> under_tmR tau (subst x v_x e)). *)
(* Proof with eauto. *)
(*   intro tau. *)
(*   induction tau; split; simpl; intros. *)
(*   - inversion H. clear H. destruct H1 as (Hclose & HH). inversion H0; subst. *)
(*     setoid_rewrite bigstep_lete in HH. *)
(*     assert (empty |- v_x \Vin T1). eapply preservation_value... *)
(*     split... eapply substitution_preserves_typing... *)
(*     split... intros. apply bigstep_lete with (x:=x) (e:=e) (v:=c) in H0. *)
(*     constructor. simpl. eapply substitution_preserves_typing... *)
(*     constructor... simpl. eapply substitution_preserves_typing... *)
(*     split... intros. *)
(*     assert (tlete x v_x e -->* c)... inversion H6; subst. *)

Definition well_formed (Gamma: context) (tau: underty) := ctx_inv Gamma /\ type_closed Gamma tau.
