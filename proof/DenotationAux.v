Set Warnings "-notation-overridden,-parsing".

From CT Require Import Maps.
From CT Require Import CoreLang.
From CT Require Import NormalTypeSystem.
From CT Require Import LinearContext.
From CT Require Import RfTypeDef.
From CT Require Import TypeClosed.
From CT Require Import Denotation.
From CT Require Import TermMeet.
From CT Require Import WellFormed.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Lists.List.
From Coq Require Import FunInd.
From Coq Require Import Recdef.

Import CoreLang.
Import NormalTypeSystem.
Import LinearContext.
Import Ax.
Import NoDup.
Import RfTypeDef.
Import TypeClosed.
Import Denotation.
Import TermMeet.
Import WellFormed.
Import ListNotations.

Ltac tmR_implies_has_type :=
  match goal with | H : tmR_aux _ _ ?e |- has_type empty ?e _ =>
                      solve [eapply tmR_has_type in H; eauto]
  end.

Ltac tmR_implies_no_dup :=
  match goal with | H : tmR_in_ctx_aux ?st ?Gamma _ |- type_ctx_no_dup ?st ?Gamma =>
                      solve [eapply tmR_in_ctx_aux_implies_no_dup; auto]
  end.

Lemma c_to_c: forall (c1 c2: constant), c1 -->* c2 -> c2 = c1.
Proof with eauto.
  intros. inversion H; subst... inversion H0.
Qed.

Global Hint Resolve c_to_c: core.

Lemma mk_eq_c_is_c_empty: forall st (c c_x: constant),
    tmR_in_ctx_aux st [] (mk_eq_constant c) c_x -> c = c_x.
Proof with eauto.
  intros. rewrite tmR_in_ctx_to_under in H. inversion H; subst.
  destruct H1.
  assert (c_x -->* c)...
Qed.

Lemma mk_eq_c_is_c: forall Gamma st (c c_x: constant),
    tmR_in_ctx_aux st Gamma (mk_eq_constant c) c_x -> c = c_x.
Proof with eauto.
  intro Gamma.
  induction Gamma; intros. eapply mk_eq_c_is_c_empty...
  destruct a as (a & tau_a).
    apply tmR_in_ctx_aux_implies_no_dup in H.
    apply type_ctx_no_dup_implies_head_free in H. apply l_find_right_most_none_neq_hd in H. exfalso...
Qed.

Lemma mk_eq_tm_ruduce_to_c: forall st Gamma (c: constant) (c_x: tm),
    ctx_inv st Gamma ->
    tmR_in_ctx_aux st Gamma (mk_eq_constant c) c_x -> c_x -->* c.
Proof with eauto.
  intros st Gamma c c_x Hinv H.
  induction Gamma.
  - inversion H; subst. inversion H0; subst. inversion H3; subst. destruct H2. apply H4...
  - intros. destruct a.
    apply tmR_in_ctx_aux_implies_no_dup in H.
    apply type_ctx_no_dup_implies_head_free in H. apply l_find_right_most_none_neq_hd in H. exfalso...
Qed.

Lemma under_variable_has_same_type_in_ctx: forall st Gamma x (tau: underty),
    ctx_inv st Gamma ->
    tmR_in_ctx_aux st (Gamma ++ ((x, Uty tau)::nil)) tau x.
Proof with eauto.
  intros st Gamma x tau Hinv.
  induction Gamma.
  - simpl.
    assert (type_ctx_no_dup empty ((x, Uty tau)::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H. apply l_find_right_most_none_neq_hd in H. exfalso...
  -  destruct a.
     assert (type_ctx_no_dup empty ((s, o) :: nil)). constructor...
     apply type_ctx_no_dup_implies_head_free in H. apply l_find_right_most_none_neq_hd in H. exfalso...
Qed.

Lemma over_variable_has_eq_type_in_ctx: forall st Gamma x T phi,
    ctx_inv st Gamma ->
    tmR_in_ctx_aux st (Gamma ++ ((x, Oty ({{v: T | phi}}))::nil)) (mk_eq_var T x) x.
Proof with eauto.
  intros st Gamma x T phi Hinv.
  induction Gamma.
  - simpl.
    assert (type_ctx_no_dup empty ((x, Oty ({{v: T | phi}}))::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H. apply l_find_right_most_none_neq_hd in H. exfalso...
  -  destruct a.
     assert (type_ctx_no_dup empty ((s, o) :: nil)). constructor...
     apply type_ctx_no_dup_implies_head_free in H. apply l_find_right_most_none_neq_hd in H. exfalso...
Qed.

Lemma under_variable_has_eq_type_in_ctx: forall st Gamma x T phi,
    ctx_inv st Gamma ->
    tmR_in_ctx_aux st (Gamma ++ ((x, Uty ([[v: T | phi]]))::nil)) (mk_eq_var T x) x.
Proof with eauto.
  intros st Gamma x T phi Hinv.
  induction Gamma.
  - simpl.
    assert (type_ctx_no_dup empty ((x, Oty ({{v: T | phi}}))::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H. apply l_find_right_most_none_neq_hd in H. exfalso...
  -  destruct a.
     assert (type_ctx_no_dup empty ((s, o) :: nil)). constructor...
     apply type_ctx_no_dup_implies_head_free in H. apply l_find_right_most_none_neq_hd in H. exfalso...
Qed.

Lemma tmR_head_not_base_ty: forall st a a0 T phi tau_b Gamma c (id :string),
    tmR_in_ctx_aux st ((a, Uty (a0 o: {{v:T | phi}} o--> tau_b)) :: Gamma) (mk_eq_constant c) id -> a <> id.
Proof with eauto.
 intros. induction Gamma.
 - inversion H; subst.
  assert (type_ctx_no_dup empty ((a, Uty (mk_eq_constant c))::nil)). constructor...
  apply type_ctx_no_dup_implies_head_free in H0. apply l_find_right_most_none_neq_hd in H0. exfalso...
 - apply tmR_in_ctx_aux_implies_no_dup in H.
   apply type_ctx_no_dup_implies_head_free in H. apply l_find_right_most_none_neq_hd in H. exfalso...
Qed.

Lemma tmR_head_not_base_ty2: forall st a t1 t2 Gamma c (id :string),
    ctx_inv st Gamma ->
    tmR_in_ctx_aux st ((a, Uty (t1 u--> t2)) :: Gamma) (mk_eq_constant c) id -> a <> id.
Proof with eauto.
 intros st a t1 t2 Gamma c id Hinv H. induction Gamma.
 - inversion H; subst.
  assert (type_ctx_no_dup empty ((a, Uty (mk_eq_constant c))::nil)). constructor...
  apply type_ctx_no_dup_implies_head_free in H0. apply l_find_right_most_none_neq_hd in H0. exfalso...
 - apply tmR_in_ctx_aux_implies_no_dup in H.
   apply type_ctx_no_dup_implies_head_free in H. apply l_find_right_most_none_neq_hd in H. exfalso...
Qed.

Lemma tmR_in_ctx_id_match: forall Gamma st (id: string) c,
    ctx_inv st Gamma ->
    tmR_in_ctx_aux st Gamma (mk_eq_constant c) id ->
    (forall e1 e2 (tau: underty), tmR_in_ctx_aux st Gamma tau (tmatchb c e1 e2) ->
                             tmR_in_ctx_aux st Gamma tau (tmatchb id e1 e2)).
Proof with eauto.
  intro Gamma.
  induction Gamma; intros st id c Hinv Hc e1 e2 tau.
  rewrite tmR_in_ctx_to_under in Hc. inversion Hc; subst. destruct H0. inversion H0; subst. inversion H4; subst. inversion H5.
  intro HD.
  - destruct a as (a & tau_a).
    assert (type_ctx_no_dup (st\_ st _/) ((a, tau_a) :: Gamma)) as Hnodup. eapply tmR_in_ctx_aux_implies_no_dup in HD...
    inversion HD; subst.
    + constructor...
      intros c_x Hc_xD.
      assert (tmR_in_ctx_aux (a |-> c_x; st) Gamma tau (tlete a c_x (tmatchb id e1 e2)))...
      inversion Hc; subst...
      assert (tmR_in_ctx_aux (a |-> c_x; st) Gamma (mk_eq_constant c) (tlete a c_x id))...
      destruct (eqb_spec a id).
      { subst...
        assert (tmR_in_ctx_aux (id |-> c_x; st) Gamma (mk_eq_constant c) c_x)...
        eapply step_preserve_ctx_denotation... apply eta_a2. apply mk_eq_c_is_c in H0. subst.
        eapply step_preserve_ctx_denotation... apply eta_a3. }
      { assert (tmR_in_ctx_aux (a |-> c_x; st) Gamma tau (tmatchb c (tlete a c_x e1) ( tlete a c_x e2)))...
        eapply step_preserve_ctx_denotation... apply eta_match1.
        eapply IHGamma with (id:=id) in H0...
        eapply step_preserve_ctx_denotation... apply eta_match2...
        eapply step_preserve_ctx_denotation... apply eta_a4... }
    + constructor...
      inversion Hc; subst...
      destruct H11 as (e_x_hat2 & He_x_hat2D & HH2).
      destruct H7 as (e_x_hat1 & He_x_hat1D & HH1).
      destruct (eqb_spec a id).
      { subst...
        exists e_x_hat1. split...
        intros e_x He_xD v_x_hat HvE.
        assert (tmR_in_ctx_aux (id |-> v_x_hat; st) Gamma tau (tlete id e_x (tmatchb c e1 e2))). apply HH1...
        assert (tmR_in_ctx_aux (id |-> v_x_hat; st) Gamma (mk_eq_constant c) (tlete id e_x id))...
        assert (tmR_in_ctx_aux (id |-> v_x_hat; st) Gamma (mk_eq_constant c) e_x)...
        eapply step_preserve_ctx_denotation... apply eta_a2. apply mk_eq_tm_ruduce_to_c in H1.
        eapply step_preserve_ctx_denotation... apply eta_match3... eapply ctx_inv_destruct_underbase in Hinv...
      }
      {
        exists e_x_hat2. split...
        intros e_x He_xD v_x_hat HvE.
        assert (tmR_in_ctx_aux (a |-> v_x_hat; st) Gamma tau (tlete a e_x (tmatchb c e1 e2)))...
        assert (tmR_in_ctx_aux (a |-> v_x_hat; st) Gamma tau (tmatchb c (tlete a e_x e1) (tlete a e_x e2)))...
        eapply step_preserve_ctx_denotation... eapply eta_match1.
        eapply IHGamma with (id:=id) (c:=c) in H0...
        eapply step_preserve_ctx_denotation... apply eta_match2... eapply ctx_inv_destruct_underbase in Hinv...
        (* assert (tmR_in_ctx_aux (a |-> v_x_hat; st) Gamma (mk_eq_constant c) (tlete a e_x id))... *)
        eapply step_preserve_ctx_denotation... apply eta_a4...
      }
    + constructor...
      assert (a <> id). eapply tmR_head_not_base_ty...
      intros.
      assert (tmR_in_ctx_aux st Gamma tau (tlete a e_x (tmatchb c e1 e2)))...
      assert (tmR_in_ctx_aux st Gamma tau (tmatchb c (tlete a e_x e1) (tlete a e_x e2)))...
      eapply step_preserve_ctx_denotation... eapply eta_match1.
      eapply IHGamma with (id:=id) (c:=c) in H2...
      eapply step_preserve_ctx_denotation... apply eta_match2... eapply ctx_inv_destruct_underoarr in Hinv...
      inversion Hc; subst.
      eapply step_preserve_ctx_denotation... apply eta_a4...
    + constructor...
      assert (a <> id). eapply tmR_head_not_base_ty2 in Hc... eapply ctx_inv_destruct_underarrarr in Hinv...
      intros.
      assert (tmR_in_ctx_aux st Gamma tau (tlete a e_x (tmatchb c e1 e2)))...
      assert (tmR_in_ctx_aux st Gamma tau (tmatchb c (tlete a e_x e1) (tlete a e_x e2)))...
      eapply step_preserve_ctx_denotation... eapply eta_match1.
      eapply IHGamma with (id:=id) (c:=c) in H2...
      eapply step_preserve_ctx_denotation... apply eta_match2... eapply ctx_inv_destruct_underarrarr in Hinv...
      inversion Hc; subst.
      eapply step_preserve_ctx_denotation... apply eta_a4...
Qed.


Lemma st_type_closed_subst_c: forall x (c_x: constant) st (tau:underty),
    st_type_closed (st\_ x |-> c_x; st _/) tau ->
    st_type_closed (st\_ st _/) (<u[ x |c-> c_x ]> tau).
Proof with auto.
  intros.
  assert (type_ctx_no_dup empty ((x, Uty tau)::nil)). constructor...
  apply type_ctx_no_dup_implies_head_free in H0. apply l_find_right_most_none_neq_hd in H0. exfalso...
Qed.

Lemma oarr_name_unique: forall st s o tau e,
    under_tmR_aux st (s o: o o--> tau) e -> st s = None.
Proof with auto.
  intros.
  assert (type_ctx_no_dup empty ((s, Uty tau)::nil)). constructor...
  apply type_ctx_no_dup_implies_head_free in H0. apply l_find_right_most_none_neq_hd in H0. exfalso...
Qed.

Lemma under_subst_preserve_ty: forall x c_x tau,
    (u\_ <u[ x |c-> c_x ]> tau _/) = (u\_ tau _/).
Proof with eauto.
  intros. induction tau...
  - simpl. destruct o. destruct (eqb_spec x s); subst...
    simpl. rewrite IHtau...
  - simpl. rewrite IHtau1.  rewrite IHtau2...
Qed.

Lemma map_none_implies_hd {A: Type}: forall x (c_x:A) st s,
    (x |-> c_x; st) s = None -> x <> s.
Proof with eauto.
  intros.
  intro HH. subst. rewrite update_eq in H. inversion H.
Qed.

Global Hint Resolve map_none_implies_hd: core.

Lemma under_tmR_aux_st_update: forall x (c_x: constant) tau st,
    (forall e, under_tmR_aux (x |-> c_x; st) tau e <-> under_tmR_aux st (<u[ x |c-> c_x ]> tau) e).
Proof with eauto.
  intros x c_x.
  induction tau; split; intros.
  - inversion H; subst... destruct H1. constructor... apply st_type_closed_subst_c in H0...
  - assert (type_ctx_no_dup empty ((x, Uty([[v:b | r]]))::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H0. apply l_find_right_most_none_neq_hd in H0. exfalso...
  - inversion H; subst... destruct H1. destruct o.
    simpl.
    assert (x <> s). eapply oarr_name_unique in H...
    destruct (eqb_spec x s); subst... exfalso...
    assert (type_ctx_no_dup empty ((x, Oty ({{v:b | <r[ x |c-> c_x ]> r}}))::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H4.
    apply l_find_right_most_none_neq_hd in H4.
    exfalso. apply H4...
  -
    assert (type_ctx_no_dup empty ((x, Uty (s o: o o--> tau))::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H0. apply l_find_right_most_none_neq_hd in H0. exfalso. apply H0...
  -
    assert (type_ctx_no_dup empty ((x, Uty tau1)::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H0. apply l_find_right_most_none_neq_hd in H0. exfalso. apply H0...
  -
    assert (type_ctx_no_dup empty ((x, Uty tau1)::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H0. apply l_find_right_most_none_neq_hd in H0. exfalso. apply H0...
Qed.


Lemma over_head_denotation_implies_forall: forall st a T0 phi0 Gamma T phi (c_x: constant) c,
    tmR_aux st ({{v:T0 | phi0}}) c_x ->
    (forall e' : tm, tmR_in_ctx_aux st ((a, Oty ({{v:T0 | phi0}})) :: Gamma) ([[v:T | phi]]) e' -> e' -->* c) ->
    (forall e' : tm, tmR_in_ctx_aux (a |-> c_x; st) Gamma ([[v:T | phi]]) e' -> e' -->* c).
Proof with eauto.
  intros.
  destruct (exists_not_free_var_in_tm c).
  assert (type_ctx_no_dup empty ((x, Uty ([[v:T0 | phi0]]))::nil)). constructor...
  apply type_ctx_no_dup_implies_head_free in H3. apply l_find_right_most_none_neq_hd in H3. exfalso...
Qed.

Global Hint Resolve over_head_denotation_implies_forall: core.

Lemma under_head_denotation_implies_exists: forall st a T0 phi0 Gamma T phi c,
    (forall e' : tm, tmR_in_ctx_aux st ((a, Uty ([[v:T0 | phi0]])) :: Gamma) ([[v:T | phi]]) e' -> e' -->* c) ->
    (exists e_a, (forall e' : tm, tmR_in_ctx_aux (a |-> e_a; st) Gamma ([[v:T | phi]]) e' -> e' -->* c)).
Proof with eauto.
  intros.
  destruct (exists_not_free_var_in_tm c).
  assert (type_ctx_no_dup empty ((x, Uty ([[v:T0 | phi0]]))::nil)). constructor...
  apply type_ctx_no_dup_implies_head_free in H1. apply l_find_right_most_none_neq_hd in H1. exfalso...
Qed.

Lemma under_denotation_can_reduce_to_over: forall st T phi e (c: constant),
    tmR_aux st ([[v:T | phi]]) e -> tmR_aux st ({{v:T | phi}}) c -> e -->* c.
Proof with eauto.
  intros.
  inversion H; subst. inversion H3; subst. destruct H2. apply H4...
  apply tmR_has_type in H0... inversion H0...
  destruct (exists_not_free_var_in_tm c).
  assert (type_ctx_no_dup empty ((x, Uty ([[v:T | phi]]))::nil)). constructor...
  apply type_ctx_no_dup_implies_head_free in H6. apply l_find_right_most_none_neq_hd in H6. exfalso...
Qed.

Global Hint Resolve under_denotation_can_reduce_to_over: core.

Lemma constant_denotation_under_implies_over: forall st Gamma T phi (c: constant),
    tmR_in_ctx_aux st Gamma ([[v:T | phi]]) c -> tmR_in_ctx_aux st Gamma ({{v:T | phi}}) c.
Proof with eauto.
  intros.
  induction Gamma. inversion H; subst. constructor... inversion H0; subst. constructor... inversion H3; subst.
  destruct H2. constructor... inversion H1; subst. constructor... simpl in H2... inversion H2...
  destruct (exists_not_free_var_in_tm c).
  assert (type_ctx_no_dup empty ((x, Uty ([[v:T | phi]]))::nil)). constructor...
  apply type_ctx_no_dup_implies_head_free in H6. apply l_find_right_most_none_neq_hd in H6. exfalso...
  destruct a.
  assert (type_ctx_no_dup (st\_ st _/) ((s, o) :: Gamma)). apply tmR_in_ctx_aux_implies_no_dup in H...
  apply type_ctx_no_dup_implies_head_free in H0. apply l_find_right_most_none_neq_hd in H0. exfalso...
Qed.

Global Hint Resolve constant_denotation_under_implies_over: core.

Lemma denotation_last_var_to_const: forall st Gamma T phi a0 (id2: string) tau,
    (tmR_in_ctx_aux st Gamma ([[v:T | phi]]) id2) ->
    (forall e, (forall c2 : constant, tmR_in_ctx_aux st Gamma ({{v:T | phi}}) c2 -> tmR_in_ctx_aux st Gamma (<u[ a0 |c-> c2 ]> tau) e) ->
          tmR_in_ctx_aux st Gamma (<u[ a0 |id-> id2 ]> tau) e).
Proof with eauto.
  intros...
  assert (type_ctx_no_dup empty ((a0, Uty (<u[ a0 |id-> id2 ]> tau) )::nil)). constructor...
  apply type_ctx_no_dup_implies_head_free in H1. apply l_find_right_most_none_neq_hd in H1. exfalso...
Qed.

Lemma op_type_safe: forall op Gamma x1 e1 x2 e2 x,
    x1 <> x2 -> ~ x1 \FVtm e2 ->
    Gamma \N- e1 \Tin (fst_ty_of_op op) ->
    Gamma \N- e2 \Tin (fst_ty_of_op op) ->
    Gamma \N- tlete x1 e1 (tlete x2 e2 (tletbiop x op x1 x2 x)) \Tin ret_ty_of_op op.
Proof with eauto.
  intros...
  destruct (exists_not_free_var_in_tm e1).
  assert (type_ctx_no_dup empty ((x0, Uty (mk_op_retty_from_cids op x1 x2) )::nil)). constructor...
  apply type_ctx_no_dup_implies_head_free in H4. apply l_find_right_most_none_neq_hd in H4. exfalso...
Qed.

Lemma op_c_denoation_safe: forall st Gamma x op (c1 c2: constant),
    tmR_in_ctx_aux st Gamma (mk_op_retty_from_cids op c1 c2) (tletbiop x op c1 c2 x).
Proof with eauto.
  intros...
  induction Gamma.
  destruct (exists_not_free_var_in_tm c1).
  assert (type_ctx_no_dup empty ((x0, Uty (mk_op_retty_from_cids op c1 c2) )::nil)). constructor...
  apply type_ctx_no_dup_implies_head_free in H0. apply l_find_right_most_none_neq_hd in H0. exfalso...
  destruct (exists_not_free_var_in_tm c2).
  assert (type_ctx_no_dup empty ((x0, Uty (mk_op_retty_from_cids op c1 c2) )::nil)). constructor...
  apply type_ctx_no_dup_implies_head_free in H0. apply l_find_right_most_none_neq_hd in H0. exfalso...
Qed.

Lemma op_c_well_formed_type: forall op (c1 c2: constant), well_formed_type (mk_op_retty_from_cids op c1 c2).
Proof with eauto.
  intros... destruct op; constructor...
Qed.

Global Hint Resolve op_c_well_formed_type: core.

Lemma tmR_in_ctx_overhead_not_free: forall st a T phi Gamma tau (c_a: constant) e,
    ~ a \FVtm e ->
    tmR_aux st ({{v:T | phi}}) c_a ->
    tmR_in_ctx_aux st ((a, Oty ({{v:T | phi}})) :: Gamma) tau e ->
    tmR_in_ctx_aux (a |->  c_a; st) Gamma tau e.
Proof with eauto.
  intros.
  assert (type_ctx_no_dup (st\_ st _/) ((a, Oty ({{v:T | phi}})) :: Gamma)).
  apply tmR_in_ctx_aux_implies_no_dup in H1...
  apply type_ctx_no_dup_implies_head_free in H2. apply l_find_right_most_none_neq_hd in H2. exfalso...
Qed.

Lemma tmR_in_ctx_oarrhead_not_free: forall st x a Ta phia taub Gamma tau e,
    ~ x \FVtm e ->
    tmR_in_ctx_aux st ((x, Uty (a o: ({{v: Ta | phia}}) o--> taub)) :: Gamma) tau e ->
    tmR_in_ctx_aux st Gamma tau e.
Proof with eauto.
  intros.
  assert (type_ctx_no_dup (st\_ st _/) ((x, Uty (a o: ({{v: Ta | phia}}) o--> taub)) :: Gamma)).
  apply tmR_in_ctx_aux_implies_no_dup in H0...
  apply type_ctx_no_dup_implies_head_free in H1. apply l_find_right_most_none_neq_hd in H1. exfalso...
Qed.

Lemma tmR_in_ctx_arrarrhead_not_free: forall st x t1 t2 Gamma tau e,
    ~ x \FVtm e ->
    tmR_in_ctx_aux st ((x, Uty (t1 u--> t2)) :: Gamma) tau e ->
    tmR_in_ctx_aux st Gamma tau e.
Proof with eauto.
  intros.
  assert (type_ctx_no_dup (st\_ st _/) ((x, Uty (t1 u--> t2)) :: Gamma)).
  apply tmR_in_ctx_aux_implies_no_dup in H0...
  apply type_ctx_no_dup_implies_head_free in H1. apply l_find_right_most_none_neq_hd in H1. exfalso...
Qed.


Lemma denotation_last_var_to_const2: forall st Gamma op phi1 phi2 (v1 v2: cid) x,
    (tmR_in_ctx_aux st Gamma ([[v:fst_ty_of_op op | phi1]]) v1) ->
    (tmR_in_ctx_aux st Gamma ([[v:snd_ty_of_op op | phi2]]) v2) ->
    (forall c1 c2 : constant,
        tmR_in_ctx_aux st Gamma ({{v:fst_ty_of_op op | phi1}}) c1 ->
        tmR_in_ctx_aux st Gamma ({{v:snd_ty_of_op op | phi2}}) c2 ->
        tmR_in_ctx_aux st Gamma (mk_op_retty_from_cids op c1 c2) (tletbiop x op v1 v2 x))
    -> tmR_in_ctx_aux st Gamma (mk_op_retty_from_cids op v1 v2) (tletbiop x op v1 v2 x).
Proof with eauto.
  intros.
  induction Gamma; intros... inversion H; subst. destruct (exists_not_free_var_in_tm v1).
  assert (type_ctx_no_dup empty ((x, Uty ([[v:fst_ty_of_op op | phi1]]))::nil)). constructor...
          apply type_ctx_no_dup_implies_head_free in H4. apply l_find_right_most_none_neq_hd in H4. exfalso...
  destruct a.
  assert (type_ctx_no_dup  (st\_ st _/) ((s, o) :: Gamma)). apply tmR_in_ctx_aux_implies_no_dup in H.
  apply type_ctx_no_dup_implies_head_free in H. apply l_find_right_most_none_neq_hd in H. exfalso...
  apply type_ctx_no_dup_implies_head_free in H2. apply l_find_right_most_none_neq_hd in H2. exfalso...
Qed.

Lemma closed_op_rty_implies_fst_rty_exists: forall st Gamma op v1 v2,
    st_type_closed_in_ctx (st\_ st _/) Gamma (mk_op_retty_from_cids op v1 v2) ->
    (exists phi1, tmR_in_ctx_aux st Gamma ([[v: fst_ty_of_op op | phi1]]) v1).
Proof with eauto.
  intros.
  induction Gamma; intros... inversion H. destruct (exists_not_free_var_in_tm v1).
  assert (type_ctx_no_dup empty ((x, Uty (mk_op_retty_from_cids op v1 v2))::nil)). constructor...
          apply type_ctx_no_dup_implies_head_free in H3. apply l_find_right_most_none_neq_hd in H3. exfalso...
  inversion H.
  assert (type_ctx_no_dup  (st\_ st _/) (a :: Gamma))... destruct a. apply type_ctx_no_dup_implies_head_free in H2. apply l_find_right_most_none_neq_hd in H2. exfalso...
Qed.

Lemma closed_op_rty_implies_snd_rty_exists: forall st Gamma op v1 v2,
    st_type_closed_in_ctx (st\_ st _/) Gamma (mk_op_retty_from_cids op v1 v2) ->
    (exists phi2, tmR_in_ctx_aux st Gamma ([[v: snd_ty_of_op op | phi2]]) v2).
Proof with eauto.
  intros.
  induction Gamma; intros... inversion H. destruct (exists_not_free_var_in_tm v1).
  assert (type_ctx_no_dup empty ((x, Uty (mk_op_retty_from_cids op v1 v2))::nil)). constructor...
          apply type_ctx_no_dup_implies_head_free in H3. apply l_find_right_most_none_neq_hd in H3. exfalso...
  inversion H.
  assert (type_ctx_no_dup  (st\_ st _/) (a :: Gamma))... destruct a. apply type_ctx_no_dup_implies_head_free in H2. apply l_find_right_most_none_neq_hd in H2. exfalso...
Qed.

Lemma constraint_phi_implies_order: forall st x (c_x c_x': constant) T phi,
    overbase_tmR_aux (x |-> c_x; st) ({{v:T | well_founded_constraint x phi}}) c_x' -> const_order c_x' c_x.
Proof with eauto.
  intros.
    assert (type_ctx_no_dup empty ((x, Oty ({{v:T | well_founded_constraint x phi}}))::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H0. apply l_find_right_most_none_neq_hd in H0. exfalso...
Qed.

Lemma constraint_phi_implies_subtyping: forall st T phi x c_x c_x',
    st_type_closed_in_ctx (state_to_tystate st) nil ({{v:T | phi}}) ->
    overbase_tmR_aux (x |-> c_x; st) ({{v:T | well_founded_constraint x phi}}) c_x' ->
    overbase_tmR_aux st ({{v:T | phi}}) c_x'.
Proof with eauto.
  intros.
    assert (type_ctx_no_dup empty ((x, Oty ({{v:T | well_founded_constraint x phi}}))::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H1. apply l_find_right_most_none_neq_hd in H1. exfalso...
Qed.

Lemma st_type_close_arrarr: forall st x (t1 t2 tau: underty) e,
    tmR_in_ctx_aux st [(x, Uty (t1 u--> t2))] tau e ->
    st_type_closed (st\_ st _/) ((t1 u--> t2) u--> tau).
Proof with eauto.
  intros st x t1 t2 tau e HD.
  constructor...
  - constructor...
    apply tmR_in_ctx_aux_implies_closed_ctx in HD...
  - apply tmR_in_ctx_aux_implies_fst_closed in HD...
  - apply tmR_in_ctx_aux_implies_closed in HD... apply st_type_closed_in_ctx_destruct_arrar_front in HD...
    inversion HD...
Qed.

Lemma st_type_close_arrarr_ctx: forall Gamma st x (t1 t2 tau: underty) e,
    tmR_in_ctx_aux st (Gamma ++ ((x, Uty (t1 u--> t2))::nil)) tau e ->
    st_type_closed_in_ctx (st\_ st _/) Gamma ((t1 u--> t2) u--> tau).
Proof with eauto.
  intros.
  induction Gamma. inversion H; subst. constructor...
  eapply st_type_close_arrarr...
  - apply tmR_in_ctx_aux_implies_no_dup in H. rewrite <- app_comm_cons in H.
    assert (type_ctx_no_dup empty ((x, Uty tau)::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H0. apply l_find_right_most_none_neq_hd in H0. exfalso...
Qed.

Lemma st_type_close_oarr: forall st x T phi (tau: underty) e,
    tmR_in_ctx_aux st [(x, Oty ({{v:T | phi}}))] tau e ->
    st_type_closed (st\_ st _/) (x o: {{v:T | phi}} o--> tau).
Proof with eauto.
  intros st x T phi tau e HD.
  constructor...
  - apply tmR_in_ctx_aux_implies_closed_ctx in HD... apply st_type_closed_ctx_implies_head_closed in HD.
    destruct HD... destruct H0. inversion H0; subst...
  - apply tmR_in_ctx_aux_implies_closed in HD... apply st_type_closed_in_ctx_destruct_overbase_front in HD...
    inversion HD...
Qed.

Lemma st_type_close_oarr_ctx: forall Gamma st x T phi (tau: underty) e,
    tmR_in_ctx_aux st (Gamma ++ ((x, Oty ({{v:T | phi}}))::nil)) tau e ->
    st_type_closed_in_ctx (st\_ st _/) Gamma (x o: {{v:T | phi}} o--> tau).
Proof with eauto.
  intros.
  induction Gamma. inversion H; subst. constructor...
  eapply st_type_close_oarr...
  - apply tmR_in_ctx_aux_implies_no_dup in H. rewrite <- app_comm_cons in H.
    assert (type_ctx_no_dup empty ((x, Uty tau)::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H0. apply l_find_right_most_none_neq_hd in H0. exfalso...
Qed.

Lemma meet_of_two_terms_implies_denotation_aux: forall (tau: underty) st  e1 e2 e3,
    under_tmR_aux st tau e1 -> under_tmR_aux st tau e2 -> empty \N- e3 \Tin ou\_ tau _/ ->
    (forall c, e3 -->* c <-> e1 -->* c /\ e2 -->* c) -> under_tmR_aux st tau e3.
Proof with eauto.
  induction tau; intros.
  - constructor... inversion H... split... intros.
    inversion H0; subst... destruct H6. rewrite H2. split... inversion H; subst... destruct H9...
  - inversion H; subst... inversion H0; subst...
    constructor... split... intros...
    destruct H4. inversion H8; subst.
    assert (under_tmR_aux (s |-> c_x; st) tau (tlete x1 e1 (tlete x2 c_x (tletapp x x1 x2 x)))).
    eapply H9...
    assert (under_tmR_aux (s |-> c_x; st) tau (tlete x1 e2 (tlete x2 c_x (tletapp x x1 x2 x)))).
    eapply H6...
    assert (type_ctx_no_dup empty ((s, Uty tau)::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H14. apply l_find_right_most_none_neq_hd in H14. exfalso...
  - inversion H; subst... inversion H0; subst...
    constructor... split... intros...
    destruct H4. inversion H8; subst.
    assert (type_ctx_no_dup empty ((x, Uty tau1)::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H12. apply l_find_right_most_none_neq_hd in H12. exfalso...
Qed.

Lemma meet_of_three_terms_implies_denotation_aux: forall st (tau: underty) e1 e2 e3 e,
    under_tmR_aux st tau e1 -> under_tmR_aux st tau e2 -> under_tmR_aux st tau e3 -> empty \N- e \Tin ou\_ tau _/ ->
    (forall c, e -->* c <-> e1 -->* c /\ e2 -->* c /\ e3 -->* c) -> under_tmR_aux st tau e.
Proof with eauto.
  induction tau; intros.
  - inversion H;subst. inversion H0;subst. inversion H1;subst.
    destruct H5. destruct H7. destruct H9.
    constructor... split... intros... rewrite H3. split...
  - inversion H; subst... inversion H0; subst...
    constructor... split... intros...
    inversion H9; subst.
    assert (type_ctx_no_dup empty ((s, Uty tau)::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H12. apply l_find_right_most_none_neq_hd in H12. exfalso...
  - inversion H; subst... inversion H0; subst...
    constructor... split... intros...
    inversion H9; subst.
    assert (type_ctx_no_dup empty ((x, Uty tau1)::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H12. apply l_find_right_most_none_neq_hd in H12. exfalso...
Qed.


Lemma meet_of_four_terms_implies_denotation_aux: forall st (tau: underty) e1 e2 e3 e4 e,
    under_tmR_aux st tau e1 -> under_tmR_aux st tau e2 -> under_tmR_aux st tau e3 -> under_tmR_aux st tau e4 -> empty \N- e \Tin u\_ tau _/ ->
    (forall c, e -->* c <-> e1 -->* c /\ e2 -->* c /\ e3 -->* c /\ e4 -->* c) -> under_tmR_aux st tau e.
Proof with eauto.
  induction tau; intros.
  - inversion H;subst. inversion H0;subst. inversion H1;subst. inversion H2;subst.
    destruct H6. destruct H8. destruct H10.  destruct H12.
    constructor... split... intros... rewrite H4. split... split...
  - inversion H; subst... inversion H0; subst...
    constructor... split... intros...
    inversion H10; subst.
    assert (type_ctx_no_dup empty ((s, Uty tau)::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H13. apply l_find_right_most_none_neq_hd in H13. exfalso...
  - inversion H; subst... inversion H0; subst...
    constructor... split... intros...
    inversion H10; subst.
    assert (type_ctx_no_dup empty ((x, Uty tau1)::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H13. apply l_find_right_most_none_neq_hd in H13. exfalso...
Qed.

Lemma meet_of_two_terms_implies_denotation: forall st (tau: underty) e1 e2 e3,
    tmR_aux st tau e1 -> tmR_aux st tau e2 -> empty \N- e3 \Tin ou\_ tau _/ ->
    (forall c, e3 -->* c <-> e1 -->* c /\ e2 -->* c) -> tmR_aux st tau e3.
Proof with eauto.
  intros.
  inversion H; subst. inversion H0; subst.
  constructor... eapply meet_of_two_terms_implies_denotation_aux in H2...
Qed.

Lemma meet_of_three_terms_implies_denotation: forall st (tau: underty) e1 e2 e3 e,
    tmR_aux st tau e1 -> tmR_aux st tau e2 -> tmR_aux st tau e3 -> empty \N- e \Tin ou\_ tau _/ ->
    (forall c, e -->* c <-> e1 -->* c /\ e2 -->* c /\ e3 -->* c) -> tmR_aux st tau e.
Proof with eauto.
  intros.
  inversion H; subst. inversion H0; subst. inversion H1; subst.
  constructor... eapply meet_of_three_terms_implies_denotation_aux in H3...
Qed.

Lemma meet_of_four_terms_implies_denotation: forall st (tau: underty) e1 e2 e3 e4 e,
    tmR_aux st tau e1 -> tmR_aux st tau e2 -> tmR_aux st tau e3 -> tmR_aux st tau e4 -> empty \N- e \Tin u\_ tau _/ ->
    (forall c, e -->* c <-> e1 -->* c /\ e2 -->* c /\ e3 -->* c /\ e4 -->* c) -> tmR_aux st tau e.
Proof with eauto.
  intros.
  inversion H; subst. inversion H0; subst. inversion H1; subst. inversion H2; subst.
  constructor... eapply meet_of_four_terms_implies_denotation_aux in H4...
Qed.


