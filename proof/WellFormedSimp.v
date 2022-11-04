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
    st_type_closed_ctx nst (Gamma ++ ((x, tau)::nil)) ->
    (forall e, tmR_in_ctx_aux nst Gamma tau e -> (exists (c: constant), e -->* c)) ->
    ctx_inv nst (Gamma ++ ((x, tau)::nil)).

Global Hint Constructors ctx_inv: core.

Lemma empty_ctx_inv_implies_all: forall Gamma x tau_x,
    ctx_inv empty (Gamma <l> x :l: tau_x) -> (forall st, ctx_inv st (Gamma <l> x :l: tau_x)).
Admitted.

Global Hint Resolve empty_ctx_inv_implies_all: core.

Lemma ctx_inv_front_destruct_over: forall st a T phi Gamma,
    ctx_inv st ((a, Oty ({{v:T | phi}})) :: Gamma) ->
    (forall c_x, tmR_aux st ({{v:T | phi}}) c_x -> ctx_inv (a |-> c_x; st) Gamma).
Admitted.

Global Hint Resolve ctx_inv_front_destruct_over: core.

Lemma ctx_inv_implies_no_dup: forall st Gamma, ctx_inv st Gamma -> type_ctx_no_dup Gamma.
Admitted.

Global Hint Resolve ctx_inv_implies_no_dup: core.

(* Lemma ctx_inv_front_destruct_under: forall st a T phi Gamma, *)
(*     ctx_inv st ((a, Ity ([[v:T | phi]])) :: Gamma) -> *)
(*     (forall c_x, overbase_tmR_aux st ({{v:T | phi}}) c_x -> ctx_inv (a |-> c_x; st) Gamma). *)
(* Admitted. *)

Lemma ctx_inv_implies_prefix_ctx_inv: forall Gamma2 st Gamma1, ctx_inv st (Gamma1 ++ Gamma2) -> ctx_inv st Gamma1.
Proof with eauto.
  intro Gamma2.
  induction Gamma2; intros st Gamma1 H.
  - rewrite app_nil_r in H...
  - setoid_rewrite <- app_one_is_cons in H... rewrite app_assoc in H. apply IHGamma2 in H. inversion H; subst...
    + destruct a. apply app_one_eq_nil in H0. inversion H0.
    + apply app_inj_tail in H0. destruct H0; subst...
Qed.

Lemma ctx_inv_implies_type_closed_ctx: forall Gamma st, ctx_inv st Gamma -> st_type_closed_ctx st Gamma.
Proof with eauto.
  apply (rev_ind (fun Gamma => forall st, ctx_inv st Gamma -> st_type_closed_ctx st Gamma))...
  intros (x, xty) Gamma H st Hconcat. inversion Hconcat; subst.
  + apply app_one_eq_nil in H0. inversion H0.
  + apply app_inj_tail in H0. destruct H0; subst...
Qed.

Global Hint Resolve ctx_inv_implies_type_closed_ctx: core.

Lemma ctx_inv_implies_fresh_binding_last:
  forall Gamma x (tau_x: underty), ctx_inv empty (Gamma <l> x :l: Uty tau_x) -> ~ appear_free_in_underty x tau_x.
Admitted.

Lemma ctx_inv_implies_type_closed_last:
  forall st Gamma x (tau_x: underty), ctx_inv st (Gamma <l> x :l: Uty tau_x) -> st_type_closed_in_ctx st Gamma tau_x.
Admitted.

Lemma ctx_inv_implies_mem_fresh_and_close:
  forall Gamma1 x (tau_x: underty) Gamma2, ctx_inv empty (Gamma1 ++ ((x, Uty tau_x)::nil) ++ Gamma2) ->
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
        st_type_closed_in_ctx st Gamma tau ->
        (forall e, tmR_in_ctx_aux st Gamma tau e -> tmR_in_ctx_aux st ((x, xty)::Gamma) tau e)).
Proof with eauto.
  intros st Gamma x xty H tau Hwf e HH.
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

(* The same as empty *)
Definition well_formed (Gamma: context) (tau: underty) := ctx_inv empty Gamma /\ st_type_closed_in_ctx empty Gamma tau.
