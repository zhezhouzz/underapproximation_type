Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From PLF Require Import CoreLangSimp.
From PLF Require Import NormalTypeSystemSimp.
From PLF Require Import LinearContext.
From PLF Require Import RfTypeDef.
From PLF Require Import WellFormedSimp.
From PLF Require Import DenotationSimp.
From PLF Require Import SubtypingSimp.
From PLF Require Import TypingRules.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Lists.List.

Import CoreLangSimp.
Import LinearContext.
Import WellFormedSimp.
Import DenotationSimp.
Import CtxInvariantSimp.
Import TypeDisj.
Import SubtypingSimp.
Import TypingRules.
Import ListNotations.

(* Lemma under_base_representative: forall T phi, exists e, *)
(*     under_tmR ([[v:T | phi]]) e /\ (forall (c: constant), e -->* c <-> (forall st, phi st = (fun c' => c = c'))). *)
(* Admitted. *)

Lemma rf_nat_value_n_exists:
  forall Gamma (v: cid) phi,
    ctx_inv Gamma ->
    Gamma \C- v \Vin ([[v:TNat | phi]]) ->
    tmR_in_ctx Gamma ([[v:TNat | phi]]) v -> ((exists n, v = cnat n) \/ (exists x, v = vvar x)).
Proof.
  intros Gamma v phi Hinv H HD.
  apply UT_Value in H. apply type_judgement_implies_basic_type_judgement in H. simpl in H.
  destruct v.
  - left. inversion H; subst. apply any_ctx_const_nat_typed_is_nat in H2. destruct H2 as (n & Hn). exists n... inversion Hn... reflexivity.
  - right. exists s... reflexivity.
Qed.

Global Hint Resolve ctx_inv_implies_fresh_binding_last: core.
Global Hint Resolve ctx_inv_implies_well_found_last: core.

Theorem soundness: forall (Gamma: context) (e: tm) (uty:underty),
    Gamma \C- e \Tin uty -> tmR_in_ctx Gamma uty e.
Proof with eauto.
  apply (term_under_type_chek_rec
           (fun Gamma v uty H => tmR_in_ctx Gamma uty (tvalue v))
           (fun Gamma e uty H => tmR_in_ctx Gamma uty e)); intros Gamma.
  (* constant *)
  - intros c Hinv.
    apply tmR_in_ctx_pre_weakening with (Gamma1 := Gamma) (Gamma2 := l_empty); auto. rewrite app_nil_r...
  (* var under *)
  - intros x tau Hinv Hfind. apply l_find_right_most_some_spec in Hfind.
    destruct Hfind as (Gamma1 & Gamma2 & Hctxconcat & HGamma2). subst.
    rewrite app_assoc... rewrite app_assoc in Hinv.
    eapply tmR_in_ctx_post_weakening with (Gamma2 := Gamma2)...
    + apply ctx_inv_implies_prefix_ctx_inv in Hinv...
    + apply under_variable_has_same_type_in_ctx...
   (* var over *)
  - intros T x Hinv (phi & Hfind). apply l_find_right_most_some_spec in Hfind.
    destruct Hfind as (Gamma1 & Gamma2 & Hctxconcat & HGamma2). subst.
    rewrite app_assoc. rewrite app_assoc in Hinv.
    eapply tmR_in_ctx_post_weakening with (Gamma1 := (Gamma1 ++ ((x, Oty ({{v:T | phi}}))::nil))) (Gamma2 := Gamma2)...
    apply over_variable_has_eq_type_in_ctx.
  (* lam *)
  - intros. apply tmR_in_ctx_preserve_oarr...
  - intros. apply tmR_in_ctx_preserve_arrarr...
  (* value *)
  - auto.
  (* err *)
  - intros T Hinv.
    assert (tmR_in_ctx l_empty (mk_bot T) texn)... unfold mk_bot.
    repeat constructor... intros. inversion H0.
    apply tmR_in_ctx_pre_weakening with (Gamma1 := Gamma) (Gamma2 := l_empty)... rewrite app_nil_r...
    constructor...
  (* sub *)
  - intros e tau1 tau2 Ht1 Hd1 Hsub Hwf2 Hctx.
    apply is_subtype_spec with (e:=e) in Hsub...
    eapply tmR_in_ctx_pre_weakening... apply app_nil_r.
  (* eq *)
  - intros e tau1 tau2 He HeD Hsub1 Hsub2 Hwf Hinv.
    eapply is_subtype_spec...
  (* merge *)
  - intros e tau1 tau2 tau3 He1 He1D He2 He2D Hdisj Hwf. unfold disjunct in Hdisj. rewrite <- Hdisj...
  (* lete *)
  - intros x e_x e tau tau_x Hex HexD He HeD Hwf.
    assert (ctx_inv (Gamma <l> x :l: tau_x)). eapply type_judgement_implies_inv...
    apply lete_ctx_inv_implies_safe_dropping_1_to_1 with (x:=x) (tau:=tau) (e:=e) (tau_x := tau_x)...
  (* letop *)
  - intros x op v1 v2 e tau phi1 phi2 Hv1 Hv1D Hv2 Hv2D He HeD Hwf.
    assert (ctx_inv (Gamma <l> x :l: (mk_op_retty_from_cids op v1 v2))). eapply type_judgement_implies_inv...
    assert (ctx_inv Gamma). eapply type_judgement_implies_inv... constructor...
    apply tletbiop_ctx_inv_implies_safe_dropping_1_to_1...
  (* letapp_arrarr *)
  - intros x v1 v2 e tau tauarg tau_x Hv1 Hv1D Hv2 Hv2D He HeD Hwf.
    assert (ctx_inv (Gamma <l> x :l: tau_x)). eapply type_judgement_implies_inv...
    assert (ctx_inv Gamma). eapply type_judgement_implies_inv... constructor...
    eapply tletapp_arrarr_ctx_inv_implies_safe_dropping_1_to_1...
  (* letapp_const *)
  - intros x v1 c2 e tau T phi a tau_x Hv1 Hv1D Hv2 Hv2D He HeD Hwf.
    assert (ctx_inv (Gamma <l> x :l: (under_subst_cid a c2 tau_x))). eapply type_judgement_implies_inv...
    assert (ctx_inv Gamma). eapply type_judgement_implies_inv... constructor...
    eapply tletapp_oarr_ctx_inv_implies_safe_dropping_1_to_1...
Qed.

Lemma coverage: forall (e: tm) (T:basic_ty) (phi: constant -> Prop),
    l_empty \C- e \Tin [[v: T | fun _ c => phi c ]] ->
    forall (c: constant), empty |- (vconst c) \Vin T -> phi c -> e -->* vconst c .
Proof with eauto.
  intros.
  apply soundness in H.
  inversion H; subst.
  inversion H2; subst. simpl in H2, H4.
  destruct H4 as (_ & _ & Hprop).
  apply Hprop; auto.
Qed.
