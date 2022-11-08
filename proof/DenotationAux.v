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
Import Nstate.
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
  match goal with | H : tmR_aux ?st ?Gamma _ |- type_ctx_no_dup ?st ?Gamma =>
                      solve [eapply tmR_in_ctx_aux_implies_no_dup; eauto]
  end.

Lemma tmR_in_ctx_id_eq_c: forall st Gamma (id: string) c,
    tmR_in_ctx_aux st Gamma (mk_eq_constant c) id ->
    (forall e tau, tmR_in_ctx_aux st Gamma tau e <-> tmR_in_ctx_aux st Gamma tau (subst id c e)).
Admitted.

Lemma tmR_in_ctx_state_implies_subst: forall (st: nstate) a T (c_a: constant) Gamma tau,
    st a = Some (tvalue c_a, T) ->
    l_find_right_most Gamma a = None ->
    (forall e, tmR_in_ctx_aux st Gamma tau e <-> tmR_in_ctx_aux st Gamma tau ([a := c_a] e)).
Admitted.

Lemma under_tmR_aux_st_update: forall x (c_x: constant) (T:base_ty) st tau,
    empty \N- c_x \Tin T ->
    (forall e, under_tmR_aux (x |-> (tvalue c_x, T); st) tau e <->
            under_tmR_aux st (<u[ x |c-> c_x ]> tau) e).
Admitted.

Lemma empty_under_denotation_const_to_over: forall st T phi (c: constant),
    (forall e' : tm, tmR_in_ctx_aux st [] ([[v:T | phi]]) e' -> e' -->* c) -> overbase_tmR_aux st ({{v:T | phi}}) c.
Admitted.

Global Hint Resolve empty_under_denotation_const_to_over: core.

Lemma over_head_denotation_implies_forall: forall st a T0 phi0 Gamma T phi c_x c,
    tmR_aux st ({{v:T0 | phi0}}) c_x ->
    (forall e' : tm, tmR_in_ctx_aux st ((a, Oty ({{v:T0 | phi0}})) :: Gamma) ([[v:T | phi]]) e' -> e' -->* c) ->
    (forall e' : tm, tmR_in_ctx_aux (a |-> (c_x, T0); st) Gamma ([[v:T | phi]]) e' -> e' -->* c).
Admitted.

Global Hint Resolve over_head_denotation_implies_forall: core.

Lemma term_order_implies_nstate_order: forall e1 e2 T a st Gamma tau e,
    e1 <-< e2 -> tmR_in_ctx_aux (a |-> (e2, T); st) Gamma tau e ->
    tmR_in_ctx_aux (a |-> (e1, T); st) Gamma tau e.
Admitted.

Global Hint Resolve term_order_implies_nstate_order: core.

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
    tmR_in_ctx_aux (a |-> (tvalue c_a, T); st) Gamma tau e.
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
