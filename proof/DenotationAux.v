Set Warnings "-notation-overridden,-parsing".

From PLF Require Import Maps.
From PLF Require Import CoreLangSimp.
From PLF Require Import NormalTypeSystemSimp.
From PLF Require Import LinearContext.
From PLF Require Import RfTypeDef.
From PLF Require Import TypeClosedSimp.
From PLF Require Import TermOrdering.
From PLF Require Import DenotationSimp.
From PLF Require Import TermMeet.
From PLF Require Import WellFormedSimp.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Lists.List.
From Coq Require Import FunInd.
From Coq Require Import Recdef.

Import CoreLangSimp.
Import NormalTypeSystemSimp.
Import LinearContext.
(* Import Nstate. *)
Import NoDup.
Import RfTypeDef.
Import TypeClosedSimp.
Import DenotationSimp.
Import TermMeet.
Import WellFormedSimp.
Import ListNotations.

Ltac tmR_implies_has_type :=
  match goal with | H : tmR_aux _ _ ?e |- has_type empty ?e _ =>
                      solve [eapply tmR_has_type in H; eauto]
  end.

Ltac tmR_implies_no_dup :=
  match goal with | H : tmR_in_ctx_aux ?st ?Gamma _ |- type_ctx_no_dup ?st ?Gamma =>
                      solve [eapply tmR_in_ctx_aux_implies_no_dup; auto]
  end.

(* Lemma eta_a1: forall id a (c: constant) c_x e, *)
(*     id <> a -> *)
(*     [id := c] tlete a c_x e <=< tlete a c_x ([id := c] e). *)
(* Admitted. *)

Lemma eta_a2: forall id c_x, tlete id c_x id <-< c_x.
Admitted.

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
  inversion H; subst.
Admitted.

(* closed; ctx inv *)
Lemma mk_eq_tm_ruduce_to_c: forall st Gamma (c: constant) (c_x: tm),
    tmR_in_ctx_aux st Gamma (mk_eq_constant c) c_x -> c_x -->* c.
Admitted.

Lemma eta_a3: forall id (c_x: constant) e1 e2,
  tlete id c_x (tmatchb c_x e1 e2) <-< tlete id c_x (tmatchb id e1 e2).
Admitted.

Lemma eta_a4: forall id a (c_x: tm),
    id <> a -> tlete a c_x id <-< id.
Admitted.

Lemma eta_match1: forall a c_x (c: constant) e1 e2,
    tlete a c_x (tmatchb c e1 e2) <-< tmatchb c (tlete a c_x e1) (tlete a c_x e2).
Admitted.

Lemma eta_match2: forall (a id: string) (c_x: tm) e1 e2,
    id <> a ->
    tmatchb id (tlete a c_x e1) (tlete a c_x e2) <-< tlete a c_x (tmatchb id e1 e2).
Admitted.

Lemma eta_match3: forall (id: string) (c: constant) (e_x: tm) e1 e2,
    tlete id e_x (tmatchb c e1 e2) <-< tlete id e_x (tmatchb id e1 e2).
Admitted.

Lemma tmR_head_not_base_ty: forall st a a0 T phi tau_b Gamma c (id :string),
    tmR_in_ctx_aux st ((a, Uty (a0 o: {{v:T | phi}} o--> tau_b)) :: Gamma) (mk_eq_constant c) id -> a <> id.
Admitted.

Lemma tmR_head_not_base_ty2: forall st a t1 t2 Gamma c (id :string),
    tmR_in_ctx_aux st ((a, Uty (t1 u--> t2)) :: Gamma) (mk_eq_constant c) id -> a <> id.
Admitted.

Lemma tmR_in_ctx_id_match: forall Gamma st (id: string) c,
    tmR_in_ctx_aux st Gamma (mk_eq_constant c) id ->
    (forall e1 e2 (tau: underty), tmR_in_ctx_aux st Gamma tau (tmatchb c e1 e2) ->
                             tmR_in_ctx_aux st Gamma tau (tmatchb id e1 e2)).
Proof with eauto.
  intro Gamma.
  induction Gamma; intros st id c Hc e1 e2 tau.
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
      destruct H15 as (e_x_hat2 & He_x_hat2D & HH2).
      destruct H9 as (e_x_hat1 & He_x_hat1D & HH1).
      destruct (eqb_spec a id).
      { subst...
        exists e_x_hat1. split...
        intros e_x He_xD v_x_hat HvE.
        assert (tmR_in_ctx_aux (id |-> v_x_hat; st) Gamma tau (tlete id e_x (tmatchb c e1 e2))). apply HH1...
        assert (tmR_in_ctx_aux (id |-> v_x_hat; st) Gamma (mk_eq_constant c) (tlete id e_x id))...
        assert (tmR_in_ctx_aux (id |-> v_x_hat; st) Gamma (mk_eq_constant c) e_x)...
        eapply step_preserve_ctx_denotation... apply eta_a2. apply mk_eq_tm_ruduce_to_c in H1.
        eapply step_preserve_ctx_denotation... apply eta_match3...
      }
      {
        exists e_x_hat2. split...
        intros e_x He_xD v_x_hat HvE.
        assert (tmR_in_ctx_aux (a |-> v_x_hat; st) Gamma tau (tlete a e_x (tmatchb c e1 e2)))...
        assert (tmR_in_ctx_aux (a |-> v_x_hat; st) Gamma tau (tmatchb c (tlete a e_x e1) (tlete a e_x e2)))...
        eapply step_preserve_ctx_denotation... eapply eta_match1.
        eapply IHGamma with (id:=id) (c:=c) in H0...
        eapply step_preserve_ctx_denotation... apply eta_match2...
        (* assert (tmR_in_ctx_aux (a |-> v_x_hat; st) Gamma (mk_eq_constant c) (tlete a e_x id))... *)
        eapply step_preserve_ctx_denotation... apply eta_a4...
      }
    + constructor...
      assert (a <> id). eapply tmR_head_not_base_ty...
      intros.
      assert (tmR_in_ctx_aux st Gamma tau (tlete a e_x (tmatchb c e1 e2)))...
      assert (tmR_in_ctx_aux st Gamma tau (tmatchb c (tlete a e_x e1) (tlete a e_x e2)))...
      eapply step_preserve_ctx_denotation... eapply eta_match1.
      eapply IHGamma with (id:=id) (c:=c) in H4...
      eapply step_preserve_ctx_denotation... apply eta_match2...
      inversion Hc; subst.
      eapply step_preserve_ctx_denotation... apply eta_a4...
    + constructor...
      assert (a <> id). eapply tmR_head_not_base_ty2...
      intros.
      assert (tmR_in_ctx_aux st Gamma tau (tlete a e_x (tmatchb c e1 e2)))...
      assert (tmR_in_ctx_aux st Gamma tau (tmatchb c (tlete a e_x e1) (tlete a e_x e2)))...
      eapply step_preserve_ctx_denotation... eapply eta_match1.
      eapply IHGamma with (id:=id) (c:=c) in H4...
      eapply step_preserve_ctx_denotation... apply eta_match2...
      inversion Hc; subst.
      eapply step_preserve_ctx_denotation... apply eta_a4...
Qed.

Lemma st_type_closed_subst_c: forall x (c_x: constant) st (tau:underty),
    st_type_closed (st\_ x |-> c_x; st _/) tau ->
    st_type_closed (st\_ st _/) (<u[ x |c-> c_x ]> tau).
Admitted.

Lemma oarr_name_unique: forall st s o tau e,
    under_tmR_aux st (s o: o o--> tau) e -> st s = None.
Admitted.

Lemma under_subst_preserve_ty: forall x c_x tau,
    (u\_ <u[ x |c-> c_x ]> tau _/) = (u\_ tau _/).
Admitted.

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
  - inversion H; subst... destruct H1. admit.
  - inversion H; subst... destruct H1. destruct o.
    simpl.
    assert (x <> s). eapply oarr_name_unique in H...
    destruct (eqb_spec x s); subst... exfalso...
    constructor...
    + apply st_type_closed_subst_c in H0... simpl in H0.
      destruct (eqb_spec x s); subst... exfalso...
    + split...  simpl. rewrite under_subst_preserve_ty...
      intros c_x' Hc_x'D e3 Happ.
      inversion Happ; subst.
      apply IHtau... rewrite update_permute... apply H2... admit.
  - admit.
Admitted.


(* Lemma empty_under_denotation_const_to_over: forall st T phi (c: constant), *)
(*     (forall e' : tm, tmR_in_ctx_aux st [] ([[v:T | phi]]) e' -> e' -->* c) -> overbase_tmR_aux st ({{v:T | phi}}) c. *)
(* Admitted. *)

(* Global Hint Resolve empty_under_denotation_const_to_over: core. *)

Lemma over_head_denotation_implies_forall: forall st a T0 phi0 Gamma T phi (c_x: constant) c,
    tmR_aux st ({{v:T0 | phi0}}) c_x ->
    (forall e' : tm, tmR_in_ctx_aux st ((a, Oty ({{v:T0 | phi0}})) :: Gamma) ([[v:T | phi]]) e' -> e' -->* c) ->
    (forall e' : tm, tmR_in_ctx_aux (a |-> c_x; st) Gamma ([[v:T | phi]]) e' -> e' -->* c).
Admitted.

Global Hint Resolve over_head_denotation_implies_forall: core.

Lemma under_head_denotation_implies_exists: forall st a T0 phi0 Gamma T phi c,
    (forall e' : tm, tmR_in_ctx_aux st ((a, Uty ([[v:T0 | phi0]])) :: Gamma) ([[v:T | phi]]) e' -> e' -->* c) ->
    (exists e_a, (forall e' : tm, tmR_in_ctx_aux (a |-> e_a; st) Gamma ([[v:T | phi]]) e' -> e' -->* c)).
Proof with eauto.
Admitted.

Lemma under_denotation_can_reduce_to_over: forall st T phi e (c: constant),
    tmR_aux st ([[v:T | phi]]) e -> tmR_aux st ({{v:T | phi}}) c -> e -->* c.
Admitted.

Global Hint Resolve under_denotation_can_reduce_to_over: core.

Lemma constant_denotation_under_implies_over: forall st Gamma T phi (c: constant),
    tmR_in_ctx_aux st Gamma ([[v:T | phi]]) c -> tmR_in_ctx_aux st Gamma ({{v:T | phi}}) c.
Admitted.

Global Hint Resolve constant_denotation_under_implies_over: core.

Lemma denotation_last_var_to_const: forall st Gamma T phi a0 (id2: string) tau,
    (tmR_in_ctx_aux st Gamma ([[v:T | phi]]) id2) ->
    (forall e, (forall c2 : constant, tmR_in_ctx_aux st Gamma ({{v:T | phi}}) c2 -> tmR_in_ctx_aux st Gamma (<u[ a0 |c-> c2 ]> tau) e) ->
          tmR_in_ctx_aux st Gamma (<u[ a0 |id-> id2 ]> tau) e).
Admitted.

Lemma op_type_safe: forall Gamma x1 e1 x2 e2 x op,
    x1 <> x2 -> ~ x1 \FVtm e2 ->
    Gamma \N- e1 \Tin (fst_ty_of_op op) ->
    Gamma \N- e2 \Tin (fst_ty_of_op op) ->
    Gamma \N- tlete x1 e1 (tlete x2 e2 (tletbiop x op x1 x2 x)) \Tin ret_ty_of_op op.
Admitted.

Lemma op_c_denoation_safe: forall st Gamma x op (c1 c2: constant),
    tmR_in_ctx_aux st Gamma (mk_op_retty_from_cids op c1 c2) (tletbiop x op c1 c2 x).
Admitted.

Lemma op_c_well_formed_type: forall op (c1 c2: constant), well_formed_type (mk_op_retty_from_cids op c1 c2).
Admitted.

Global Hint Resolve op_c_well_formed_type: core.

Lemma tmR_in_ctx_overhead_not_free: forall st a T phi Gamma tau (c_a: constant) e,
    ~ a \FVtm e ->
    tmR_aux st ({{v:T | phi}}) c_a ->
    tmR_in_ctx_aux st ((a, Oty ({{v:T | phi}})) :: Gamma) tau e ->
    tmR_in_ctx_aux (a |->  c_a; st) Gamma tau e.
Admitted.

Lemma tmR_in_ctx_oarrhead_not_free: forall st x a Ta phia taub Gamma tau e,
    ~ x \FVtm e ->
    tmR_in_ctx_aux st ((x, Uty (a o: ({{v: Ta | phia}}) o--> taub)) :: Gamma) tau e ->
    tmR_in_ctx_aux st Gamma tau e.
Admitted.

Lemma tmR_in_ctx_arrarrhead_not_free: forall st x t1 t2 Gamma tau e,
    ~ x \FVtm e ->
    tmR_in_ctx_aux st ((x, Uty (t1 u--> t2)) :: Gamma) tau e ->
    tmR_in_ctx_aux st Gamma tau e.
Admitted.

Lemma denotation_last_var_to_const2: forall st Gamma op phi1 phi2 (v1 v2: cid) x,
    (tmR_in_ctx_aux st Gamma ([[v:fst_ty_of_op op | phi1]]) v1) ->
    (tmR_in_ctx_aux st Gamma ([[v:snd_ty_of_op op | phi2]]) v2) ->
    (forall c1 c2 : constant,
        tmR_in_ctx_aux st Gamma ({{v:fst_ty_of_op op | phi1}}) c1 ->
        tmR_in_ctx_aux st Gamma ({{v:snd_ty_of_op op | phi2}}) c2 ->
        tmR_in_ctx_aux st Gamma (mk_op_retty_from_cids op c1 c2) (tletbiop x op v1 v2 x))
    -> tmR_in_ctx_aux st Gamma (mk_op_retty_from_cids op v1 v2) (tletbiop x op v1 v2 x).
Admitted.

Lemma closed_op_rty_implies_fst_rty_exists: forall st Gamma op v1 v2,
    st_type_closed_in_ctx (st\_ st _/) Gamma (mk_op_retty_from_cids op v1 v2) ->
    (exists phi1, tmR_in_ctx_aux st Gamma ([[v: fst_ty_of_op op | phi1]]) v1).
Admitted.

Lemma closed_op_rty_implies_snd_rty_exists: forall st Gamma op v1 v2,
    st_type_closed_in_ctx (st\_ st _/) Gamma (mk_op_retty_from_cids op v1 v2) ->
    (exists phi2, tmR_in_ctx_aux st Gamma ([[v: snd_ty_of_op op | phi2]]) v2).
Admitted.

Lemma constraint_phi_implies_order: forall st x (c_x c_x': constant) T phi,
    overbase_tmR_aux (x |-> c_x; st) ({{v:T | well_founded_constraint x phi}}) c_x' -> const_order c_x' c_x.
Admitted.

Lemma constraint_phi_implies_subtyping: forall st T phi x c_x c_x',
    st_type_closed_in_ctx (state_to_tystate st) nil ({{v:T | phi}}) ->
    overbase_tmR_aux (x |-> c_x; st) ({{v:T | well_founded_constraint x phi}}) c_x' ->
    overbase_tmR_aux st ({{v:T | phi}}) c_x'.
Admitted.
