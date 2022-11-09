Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From PLF Require Import CoreLangSimp.
From PLF Require Import NormalTypeSystemSimp.
From PLF Require Import LinearContext.
From PLF Require Import RfTypeDef.
From PLF Require Import WellFormedSimp.
From PLF Require Import DenotationSimp.
From PLF Require Import CtxInvariantSimp.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Lists.List.

Import CoreLangSimp.
Import LinearContext.
Import WellFormedSimp.
Import DenotationSimp.
Import CtxInvariantSimp.
Import ListNotations.

Lemma denotation_spec: forall (tau tau_x: underty) x e,
    ~ appear_free_in_underty x tau ->
    tmR_in_ctx [(x, Uty tau_x)] tau e ->
    (forall e_x, under_tmR tau_x e_x -> (exists v : value, e_x -->* v)) ->
    (forall e_x,
        under_tmR tau_x e_x ->
        under_tmR tau (tlete x e_x e)
    ).
Proof with eauto.
  intros tau tau_x x e Hfree HeD Hnoterr e_x HexD.
  inversion HeD; simpl; subst.
  - destruct H4 as (e_x_hat & HexhatD & HH).
    assert (empty |- e_x_hat \in T)... apply under_tmR_has_type in HexhatD...
    apply Hnoterr in HexhatD.
    destruct HexhatD as (v_x_hat & HEv_x_hat).
    assert (empty |- v_x_hat \Vin T) as HTc_x_hat. apply preservation_value with (t:=e_x_hat)...
    assert (exists (c_x_hat: constant), v_x_hat = c_x_hat). apply basic_value_const_exists in HTc_x_hat... destruct H0 as (c_x_hat & H0)...
    subst.
    eapply HH with (Gamma':=nil) (tau':=tau) in HexD...
    + inversion HexD; subst. inversion H0; subst...
    + apply ty_subst_under_empty_ctx_not_free...
  - apply H4 in HexD. inversion HexD; subst. inversion H; subst...
Qed.

Lemma ctx_inv_implies_drop_right_most: forall Gamma (x: string) (tau_x: underty) (tau: underty),
    ctx_inv (Gamma ++ ((x, Uty tau_x)::nil)) ->
    ~ appear_free_in_underty x tau ->
    (forall e, tmR_in_ctx (Gamma ++ ((x, Uty tau_x)::nil)) tau e ->
          (forall e_x, tmR_in_ctx Gamma tau_x e_x ->
                  tmR_in_ctx Gamma tau (tlete x e_x e)
          )
    ).
Proof with eauto.
  intro Gamma.
  induction Gamma; simpl; intros x tau_x tau Hinv Hfree e HeD e_x HexD.
  - inversion Hinv; subst. apply app_eq_unit in H. destruct H; destruct H; subst.
    + inversion H4; subst. apply H3 in HexD. repeat constructor... unfold under_tmR.
      destruct H3 as (e_x & HexD & Hev). exists e_x. intros H
