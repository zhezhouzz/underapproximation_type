Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From PLF Require Import CoreLangSimp.
From PLF Require Import NormalTypeSystemSimp.
From PLF Require Import LinearContext.
From PLF Require Import RfTypeDef.
From PLF Require Import TypeClosedSimp.
From PLF Require Import TermOrdering.
From PLF Require Import DenotationSimp.
From PLF Require Import WellFormedSimp.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Lists.List.
From Coq Require Import FunInd.
From Coq Require Import Recdef.

Import CoreLangSimp.
Import NormalTypeSystemSimp.
Import LinearContext.
Import RfTypeDef.
Import TypeClosedSimp.
Import DenotationSimp.
Import WellFormedSimp.
Import ListNotations.

Global Hint Constructors well_formed_type: core.

Lemma tmR_in_ctx_id_eq_c: forall st Gamma (id: string) c,
    tmR_in_ctx_aux st Gamma (mk_eq_constant c) id ->
    (forall e tau, tmR_in_ctx_aux st Gamma tau e <-> tmR_in_ctx_aux st Gamma tau (subst id c e)).
Admitted.

Lemma tmR_in_ctx_state_implies_subst: forall (st: state) a (c_a: constant) Gamma tau,
    st a = c_a ->
    l_find_right_most Gamma a = None ->
    (forall e, tmR_in_ctx_aux st Gamma tau e <-> tmR_in_ctx_aux st Gamma tau ([a := c_a] e)).
Admitted.


(* Assume there is no dup in the context to simplify the proof. *)
Lemma tmR_in_ctx_preserve_oarr: forall Gamma st x (tau_x: overbasety) e (tau: underty),
    type_ctx_no_dup (Gamma <l> x :l: Oty tau_x) ->
    tmR_in_ctx_aux st (Gamma <l> x :l: Oty tau_x) tau e ->
    tmR_in_ctx_aux st Gamma (x o: tau_x o--> tau) (vlam x (o\_ tau_x _/) e).
Proof with eauto.
  intro Gamma.
  induction Gamma; simpl; intros st x tau_x e tau Hctx HD.
  - setoid_rewrite app_nil_l in HD. inversion HD; subst. constructor... constructor... constructor...
    split.
    + simpl. constructor... constructor... apply tmR_in_ctx_aux_implies_has_type in HD.
      unfold lcontxt_to_basic_ctx in HD. unfold ncontxt_to_basic_ctx in HD. simpl in HD...
    + intros c_x Hc_xD e3 Happ. inversion Happ; subst. simpl.
      eapply step_preserve_under_denotation... eapply eta_reduction...
      eapply step_preserve_under_denotation... eapply eta_application_const_to_lete_const...
      assert (tmR_in_ctx_aux (x !-> c_x; st) [] tau (tlete x c_x e)) as HH... inversion HH; subst. inversion H0...
  - destruct a as (a & tau_a).
    inversion HD; subst.
    + constructor...
      (* apply denotation_ctx_implies_last_well_formed_type in HD... inversion HD; subst. *)
      intros c_x Hc_xD.
      eapply step_preserve_ctx_denotation. apply eta_lete_const_to_subst.
      simpl... destruct (eqb_spec a x)... exfalso. eapply type_ctx_no_dup_fst_last_diff_name...
      eapply step_preserve_ctx_denotation... apply eta_lete_const_to_subst_in_lam...
      eapply IHGamma...
      apply type_ctx_no_dup_ctx_sub with (Gamma1 := (a, Oty ({{v:T | phi}}))::nil)...
    + constructor...
      destruct H7 as (e_x_hat & He_x_hatD & HH).
      exists e_x_hat. split...
      intros e_x He_xD c_x Hc_xT Hc_xE.
      eapply step_preserve_ctx_denotation... eapply eta_closed_term_can_captured_by_lam...
      apply IHGamma...
      apply type_ctx_no_dup_ctx_sub with (Gamma1 := (a, Uty ([[v:T | phi]]))::nil)...
    + constructor...
      intros e_x He_xD.
      eapply step_preserve_ctx_denotation... eapply eta_closed_term_can_captured_by_lam...
      apply IHGamma...
      apply type_ctx_no_dup_ctx_sub with (Gamma1 := (a, Uty tau_x0)::nil)...
Qed.

Lemma tmR_in_ctx_preserve_arrarr: forall Gamma st x (tau_x: underty) e (tau: underty),
    type_ctx_no_dup (Gamma <l> x :l: Uty tau_x) ->
    tmR_in_ctx_aux st (Gamma <l> x :l: Uty tau_x) tau e ->
    tmR_in_ctx_aux st Gamma (tau_x u--> tau) (vlam x (u\_ tau_x _/) e).
Proof with eauto.
  intro Gamma.
  induction Gamma; simpl; intros st x tau_x e tau Hctx HD.
  - setoid_rewrite app_nil_l in HD. setoid_rewrite app_nil_l in Hctx.
Admitted.

Lemma tmR_in_ctx_preserve_arrarr_application: forall Gamma st x (v1 v2: value) tau tau1,
    (* ctx_inv Gamma -> *)
    tmR_in_ctx_aux st Gamma (tau1 u--> tau) v1 ->
    tmR_in_ctx_aux st Gamma tau1 v2 ->
    tmR_in_ctx_aux st Gamma tau (tletapp x v1 v2 x).
Proof with eauto.
  intro Gamma.
  induction Gamma; simpl; intros st x v1 v2 tau tau_x Hv1D Hv2D.
  - inversion Hv1D; subst. inversion Hv2D; subst. clear Hv1D Hv2D. constructor... inversion H; subst. inversion H3; subst. clear H H3. inversion H0; subst. clear H0.
    destruct H2 as (Hv1T & HH). destruct (value_value_is_application x v1 v2) as (e3 & (He3app & He3E)).
    eapply HH with (e3:=e3) in H4... constructor... eapply step_preserve_under_denotation... destruct He3E...
  - destruct a as (a & tau_a).
    inversion Hv1D; subst.
    + constructor... { inversion H3; subst... } inversion Hv2D; subst.
      intros c_x He_xD.
      (* make HHv1 and HHv2 to value. *)
      assert (tmR_in_ctx_aux (a !-> c_x; st) Gamma (tau_x u--> tau) ([a := c_x] v1)) as HHv1...
      { eapply step_preserve_ctx_denotation... apply eta_lete_const_to_subst. } clear H7.
      assert (tmR_in_ctx_aux (a !-> c_x; st) Gamma tau_x ([a := c_x] v2)) as HHv2...
      { eapply step_preserve_ctx_denotation... apply eta_lete_const_to_subst. } clear H11.
      eapply step_preserve_ctx_denotation. apply eta_lete_const_to_subst. rewrite subst_letapp_penetrate.
      eapply IHGamma...
    + constructor... { inversion H3; subst... }
      destruct H7 as (e_x_hat & He_x_hatD & HH).
      exists e_x_hat. split...
      intros e_x He_xD c_x Hc_xT Hc_xE.
      (* (need context inv) *)
Admitted.

Lemma tmR_in_ctx_preserve_oarr_c_application: forall Gamma st x (v1: value) (c2: constant) tau a T phi,
    tmR_in_ctx_aux st Gamma (a o: {{v: T | phi}} o--> tau) v1 ->
    tmR_in_ctx_aux st Gamma ([[v: T | phi]]) c2 ->
    tmR_in_ctx_aux st Gamma (under_subst_c a c2 tau) (tletapp x v1 c2 x).
Admitted.

Lemma tmR_in_ctx_preserve_oarr_var_application: forall Gamma st x (v1: value) (name2: string) tau a T phi,
    tmR_in_ctx_aux st Gamma (a o: {{v: T | phi}} o--> tau) v1 ->
    tmR_in_ctx_aux st Gamma ([[v: T | phi]]) name2 ->
    tmR_in_ctx_aux st Gamma (under_subst_id a name2 tau) (tletapp x v1 name2 x).
Admitted.

Lemma tmR_in_ctx_preserve_matchb_true: forall Gamma st (v: value) e1 e2 tau,
    tmR_in_ctx_aux st Gamma (mk_eq_constant true) v ->
    tmR_in_ctx_aux st Gamma tau e1 ->
    tmR_in_ctx_aux st Gamma tau (tmatchb v e1 e2).
Proof with eauto.
  intros Gamma st v e1 e2 tau Hv He1.
  assert ((exists n : bool, v = n) \/ (exists name : string, v = name)) as HH. apply tmR_in_ctx_aux_implies_has_type in Hv. inversion Hv; subst. apply bool_value_cid_exists in H1...
  destruct HH.
  - destruct H as (b & Hb); subst. apply mk_eq_constant_is_itsefl_in_ctx in Hv. rewrite Hv.
    eapply step_preserve_ctx_denotation... apply eta_matchb_true...
  - destruct H as (id & Hid); subst. rewrite tmR_in_ctx_id_eq_c... simpl. rewrite eqb_refl.
    apply step_preserve_ctx_denotation with (e:= ([id := true] e1))... apply eta_matchb_true...
    rewrite <- tmR_in_ctx_id_eq_c...
Qed.


Lemma tmR_in_ctx_preserve_matchb_false: forall st Gamma (v: value) e1 e2 tau,
    tmR_in_ctx_aux st Gamma (mk_eq_constant false) v ->
    tmR_in_ctx_aux st Gamma tau e2 ->
    tmR_in_ctx_aux st Gamma tau (tmatchb v e1 e2).
Proof with eauto.
  intros st Gamma v e1 e2 tau Hv He1.
  assert ((exists n : bool, v = n) \/ (exists name : string, v = name)) as HH. apply tmR_in_ctx_aux_implies_has_type in Hv. inversion Hv; subst. apply bool_value_cid_exists in H1...
  destruct HH.
  - destruct H as (b & Hb); subst. apply mk_eq_constant_is_itsefl_in_ctx in Hv. rewrite Hv.
    eapply step_preserve_ctx_denotation... apply eta_matchb_false...
  - destruct H as (id & Hid); subst. rewrite tmR_in_ctx_id_eq_c... simpl. rewrite eqb_refl.
    apply step_preserve_ctx_denotation with (e:= ([id := false] e2))... apply eta_matchb_false...
    rewrite <- tmR_in_ctx_id_eq_c...
Qed.

