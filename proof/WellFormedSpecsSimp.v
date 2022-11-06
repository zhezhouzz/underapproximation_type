Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From PLF Require Import CoreLangSimp.
From PLF Require Import NormalTypeSystemSimp.
From PLF Require Import LinearContext.
From PLF Require Import RfTypeDef.
From PLF Require Import TypeClosedSimp.
From PLF Require Import DenotationSimp.
From PLF Require Import WellFormedSimp.
From PLF Require Import DenotationSpecsSimp.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Lists.List.

Import CoreLangSimp.
Import LinearContext.
Import TypeClosedSimp.
Import DenotationSimp.
Import TermOrdering.
Import Nstate.
Import NoDup.
Import WellFormedSimp.
Import DenotationSpecsSimp.
Import ListNotations.

Lemma tmR_nst_no_free_implies_eq: forall st x e_x_hat (tau: underty),
    ~ appear_free_in_underty x tau -> (forall e, tmR_aux (x |-> e_x_hat; st) tau e <-> tmR_aux st tau e).
Admitted.

Global Hint Rewrite tmR_nst_no_free_implies_eq: core.
Global Hint Resolve tmR_has_type: core.

Lemma eta_lete_neq: forall x a c_x e_x e,
    a <> x ->
    tlete x (tlete a c_x e_x) (tlete a c_x e) <-< tlete a c_x (tlete x e_x e).
Admitted.

Lemma lete_ctx_inv_implies_safe_dropping_1_to_1: forall Gamma st x tau_x tau,
    ~ appear_free_in_underty x tau ->
    ctx_inv st (Gamma ++ ((x, Uty tau_x)::nil)) ->
    (forall e, tmR_in_ctx_aux st (Gamma ++ ((x, Uty tau_x)::nil)) tau e ->
          (forall e_x, tmR_in_ctx_aux st Gamma tau_x e_x -> tmR_in_ctx_aux st Gamma tau (tlete x e_x e))).
Proof with eauto.
  intro Gamma. induction Gamma; simpl; intros st x tau_x tau Hfree Hinv e HeD e_x He_xD.
  - constructor... inversion He_xD; subst. inversion HeD; subst.
    + destruct H10 as (e_x_hat & He_x_hatD & HH).
      assert (empty \N- e_x_hat \Tin T) as He_x_hatT. eapply tmR_has_type in He_x_hatD...
      assert (tmR_in_ctx_aux (x |-> (e_x_hat, T); st) [] tau (tlete x e_x e)) as HD... inversion HD; subst...
      rewrite <- tmR_nst_no_free_implies_eq...
    + assert (tmR_in_ctx_aux st [] tau (tlete x e_x e)) as HD... inversion HD; subst...
    + assert (tmR_in_ctx_aux st [] tau (tlete x e_x e)) as HD... inversion HD; subst...
  - destruct a as (a & tau_a).
    assert (type_ctx_no_dup (st\_ st _/) ((a, tau_a) :: Gamma ++ ((x, Uty tau_x)::nil))) as Hnodup...
    inversion HeD; subst.
    + constructor...
      intros c_x Hc_xD.
      assert (empty \N- c_x \Tin T) as He_xT. eapply tmR_has_type in Hc_xD...
      assert (tmR_in_ctx_aux (a |-> (tvalue c_x, T); st) (Gamma ++ ((x, Uty tau_x)::nil)) tau (tlete a c_x e))...
      inversion He_xD; subst...
      assert (tmR_in_ctx_aux (a |-> (tvalue c_x, T); st) Gamma tau_x (tlete a c_x e_x))...
      assert (tmR_in_ctx_aux (a |-> (tvalue c_x, T); st) Gamma tau (tlete x (tlete a c_x e_x) (tlete a c_x e))). eapply IHGamma...
      eapply step_preserve_ctx_denotation... eapply eta_lete_neq...
    + constructor...
      destruct H9 as (e_x_hat1 & He_x_hatD1 & HH1).
      inversion He_xD; subst...
      destruct H14 as (e_x_hat2 & He_x_hatD2 & HH2).
      destruct (meet_of_two_terms_exists e_x_hat1 e_x_hat2 T) as (e_x_hat3 & HT3 & HE3)... apply tmR_has_type in He_x_hatD1... apply tmR_has_type in He_x_hatD2...
      exists e_x_hat3. split... eapply meet_of_two_terms_implies_denotation in HE3...
      assert (tmR_aux st ([[v:T | phi]]) e_x_hat3)... eapply meet_of_two_terms_implies_denotation in HE3...
      intros e_x' He_xD'.
      assert (tmR_in_ctx_aux (a |-> (e_x_hat1, T); st) (Gamma ++ ((x, Uty tau_x)::nil)) tau (tlete a e_x' e)). apply HH1...
      assert (tmR_in_ctx_aux (a |-> (e_x_hat2, T); st) Gamma tau_x (tlete a e_x' e_x)). apply HH2...
      apply meet_of_two_terms_term_order in HE3... destruct HE3 as (HEE1 & HEE2).
      assert (tmR_in_ctx_aux (a |-> (e_x_hat3, T); st) Gamma tau (tlete x (tlete a e_x' e_x) (tlete a e_x' e)))...
      eapply IHGamma with (tau_x:= tau_x)... eapply ctx_inv_destruct_underbase...
      eapply step_preserve_ctx_denotation... eapply eta_lete_neq...
    + constructor...
      inversion He_xD; subst.
      intros e_x' He_xD'.
      assert (tmR_in_ctx_aux st (Gamma ++ ((x, Uty tau_x)::nil)) tau (tlete a e_x' e))...
      assert (tmR_in_ctx_aux st Gamma tau_x (tlete a e_x' e_x))...
      assert (tmR_in_ctx_aux st Gamma tau (tlete x (tlete a e_x' e_x) (tlete a e_x' e)))...
      eapply IHGamma... eapply ctx_inv_destruct_underoarr...
      eapply step_preserve_ctx_denotation... eapply eta_lete_neq...
    + constructor...
      inversion He_xD; subst.
      intros e_x' He_xD'.
      assert (tmR_in_ctx_aux st (Gamma ++ ((x, Uty tau_x)::nil)) tau (tlete a e_x' e))...
      assert (tmR_in_ctx_aux st Gamma tau_x (tlete a e_x' e_x))...
      assert (tmR_in_ctx_aux st Gamma tau (tlete x (tlete a e_x' e_x) (tlete a e_x' e)))...
      eapply IHGamma... eapply ctx_inv_destruct_underarrarr...
      eapply step_preserve_ctx_denotation... eapply eta_lete_neq...
Qed.

Lemma eta7: forall x v1 v2 e, (tlete x (tletapp x v1 v2 x) e) <=< (tletapp x v1 v2 e).
Admitted.


Lemma tletapp_arrarr_ctx_inv_implies_safe_dropping_1_to_1: forall Gamma st x tau_x tau,
    ~ appear_free_in_underty x tau ->
    ctx_inv st (Gamma ++ ((x, Uty tau_x)::nil)) ->
    (forall e, tmR_in_ctx_aux st (Gamma ++ ((x, Uty tau_x)::nil)) tau e ->
          (forall (v1 v2: value) t1,
              tmR_in_ctx_aux st Gamma (t1 u--> tau_x) v1 ->
              tmR_in_ctx_aux st Gamma t1 v2 ->
              tmR_in_ctx_aux st Gamma tau (tletapp x v1 v2 e))).
Proof with eauto.
  apply (rev_ind (fun Gamma => forall st x tau_x tau,
                      ~ appear_free_in_underty x tau ->
                      ctx_inv st (Gamma ++ ((x, Uty tau_x)::nil)) ->
                      (forall e, tmR_in_ctx_aux st (Gamma ++ ((x, Uty tau_x)::nil)) tau e ->
                            (forall (v1 v2: value) t1,
                                tmR_in_ctx_aux st Gamma (t1 u--> tau_x) v1 ->
                                tmR_in_ctx_aux st Gamma t1 v2 ->
                                tmR_in_ctx_aux st Gamma tau (tletapp x v1 v2 e)))
        )).
  - intros st x tau_x tau Hfree Hinv e HeD v1 v2 t1 Hv1D Hv2D.
    assert (tmR_in_ctx_aux st [] tau (tlete x (tletapp x v1 v2 x) e)).
    + eapply lete_ctx_inv_implies_safe_dropping_1_to_1...
      eapply tmR_in_ctx_preserve_arrarr_application...
    + eapply step_preserve_ctx_denotation... eapply eta7...
  - intros (a & tau_a) Gamma IHGamma st x tau_x tau Hfree Hinv e HeD v1 v2 t1 Hv1D Hv2D.
    assert (tmR_in_ctx_aux st (Gamma ++ ((a, tau_a)::nil)) tau (tlete x (tletapp x v1 v2 x) e)).
    + eapply lete_ctx_inv_implies_safe_dropping_1_to_1...
      eapply tmR_in_ctx_preserve_arrarr_application...
    + eapply step_preserve_ctx_denotation... eapply eta7...
Qed.

Lemma tletapp_oarr_ctx_inv_implies_safe_dropping_1_to_1: forall Gamma st x tau_x tau,
    ~ appear_free_in_underty x tau ->
    (forall e (v1: value) (v2: cid) a T phi1,
        tmR_in_ctx_aux st Gamma (a o: {{v: T | phi1}} o--> tau_x) v1 ->
        tmR_in_ctx_aux st Gamma ([[v: T | phi1]]) v2 ->
        ctx_inv st (Gamma ++ ((x, Uty (under_subst_cid a v2 tau_x))::nil)) ->
        tmR_in_ctx_aux st (Gamma ++ ((x, Uty (under_subst_cid a v2 tau_x))::nil)) tau e ->
        tmR_in_ctx_aux st Gamma tau (tletapp x v1 v2 e)).
Proof with eauto.
  apply (rev_ind (fun Gamma => forall st x tau_x tau,
                      ~ appear_free_in_underty x tau ->
                      (forall e (v1: value) (v2: cid) a T phi1,
                          tmR_in_ctx_aux st Gamma (a o: {{v: T | phi1}} o--> tau_x) v1 ->
                          tmR_in_ctx_aux st Gamma ([[v: T | phi1]]) v2 ->
                          ctx_inv st (Gamma ++ ((x, Uty (under_subst_cid a v2 tau_x))::nil)) ->
                          tmR_in_ctx_aux st (Gamma ++ ((x, Uty (under_subst_cid a v2 tau_x))::nil)) tau e ->
                          tmR_in_ctx_aux st Gamma tau (tletapp x v1 v2 e)))
        ).
  - intros st x tau_x tau Hfree e v1 v2 a T phia Hv1D Hv2D Hinv HeD.
    assert (tmR_in_ctx_aux st [] tau (tlete x (tletapp x v1 v2 x) e)).
    + eapply lete_ctx_inv_implies_safe_dropping_1_to_1...
      eapply tmR_in_ctx_preserve_oarr_application...
    + eapply step_preserve_ctx_denotation... eapply eta7...
  - intros (y & tau_y) Gamma IHGamma st x tau_x tau Hfree e v1 v2 a T phia Hv1D Hv2D Hinv HeD.
    assert (tmR_in_ctx_aux st (Gamma ++ ((y, tau_y)::nil)) tau (tlete x (tletapp x v1 v2 x) e)).
    + eapply lete_ctx_inv_implies_safe_dropping_1_to_1...
      eapply tmR_in_ctx_preserve_oarr_application...
    + eapply step_preserve_ctx_denotation... eapply eta7...
Qed.

Lemma eta8: forall x op v1 v2 e, (tlete x (tletbiop x op v1 v2 x) e) <=< (tletbiop x op v1 v2 e).
Admitted.

Lemma inv_implies_type_closed_last: forall st Gamma x tau_x,
  ctx_inv st (Gamma ++ ((x, tau_x)::nil)) -> st_type_closed_in_ctx (st\_ st _/) Gamma tau_x.
Admitted.

Global Hint Resolve inv_implies_type_closed_last: core.


Lemma tletbiop_ctx_inv_implies_safe_dropping_1_to_1: forall Gamma st x tau,
    ~ appear_free_in_underty x tau ->
    (forall e op (v1 v2: cid),
        ctx_inv st (Gamma ++ ((x, Uty (mk_op_retty_from_cids op v1 v2))::nil)) ->
        tmR_in_ctx_aux st (Gamma ++ ((x, Uty (mk_op_retty_from_cids op v1 v2))::nil)) tau e ->
        tmR_in_ctx_aux st (Gamma <l> x :l: ((mk_op_retty_from_cids op v1 v2))) tau e ->
        tmR_in_ctx_aux st Gamma tau (tletbiop x op v1 v2 e)).
Proof with eauto.
  apply (rev_ind (fun Gamma => forall st x tau,
                      ~ appear_free_in_underty x tau ->
                      (forall e op (v1 v2: cid),
                          ctx_inv st (Gamma ++ ((x, Uty (mk_op_retty_from_cids op v1 v2))::nil)) ->
                          tmR_in_ctx_aux st (Gamma ++ ((x, Uty (mk_op_retty_from_cids op v1 v2))::nil)) tau e ->
                          tmR_in_ctx_aux st (Gamma <l> x :l: ((mk_op_retty_from_cids op v1 v2))) tau e ->
                          tmR_in_ctx_aux st Gamma tau (tletbiop x op v1 v2 e)))).
  - intros st x tau Hfree e op v1 v2 Hinv Hv1D Hv2D.
    assert (tmR_in_ctx_aux st [] tau (tlete x (tletbiop x op v1 v2 x) e)).
    + eapply lete_ctx_inv_implies_safe_dropping_1_to_1...
      eapply tmR_in_ctx_preserve_biop_application...
    + eapply step_preserve_ctx_denotation... eapply eta8...
  - intros (y & tau_y) Gamma IHGamma st x tau Hfree e op v1 v2 Hinv Hv1D Hv2D.
    assert (tmR_in_ctx_aux st (Gamma ++ ((y, tau_y)::nil)) tau (tlete x (tletbiop x op v1 v2 x) e)).
    + eapply lete_ctx_inv_implies_safe_dropping_1_to_1...
      eapply tmR_in_ctx_preserve_biop_application...
    + eapply step_preserve_ctx_denotation... eapply eta8...
Admitted.

