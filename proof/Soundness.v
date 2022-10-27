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
Import SubtypingSimp.
Import TypingRules.
Import ListNotations.

(* constant *)
Lemma mk_eq_constant_implies_emptyctx_denotation: forall (c : constant),
    tmR_in_ctx l_empty (mk_eq_constant c) (vconst c).
Proof with eauto.
  intro c. destruct c...
Qed.

Lemma mk_eq_constant_implies_denotation: forall Gamma (c : constant),
    tmR_in_ctx Gamma (mk_eq_constant c) (vconst c).
Proof with eauto.
  intros Gamma c.
  apply tmR_in_ctx_pre_weakening with (Gamma1 := Gamma) (Gamma2 := l_empty); auto. rewrite app_nil_r...
Qed.

(* err *)
Lemma err_typecheck_implies_err_denotation: forall Gamma,
   forall (T : basic_ty), well_formed_ctx Gamma -> tmR_in_ctx Gamma (mk_bot T) texn.
Proof with eauto.
  intros.
  assert (tmR_in_ctx l_empty (mk_bot T) texn). unfold mk_bot.
  repeat constructor... intros. inversion H1.
  apply tmR_in_ctx_pre_weakening with (Gamma1 := Gamma) (Gamma2 := l_empty)... rewrite app_nil_r...
  constructor...
Qed.

Lemma under_base_representative: forall T phi, exists e,
    under_tmR ([[v:T | phi]]) e /\ (forall (c: constant), e -->* c <-> (forall st, phi st = (fun c' => c = c'))).
Admitted.

(* under var *)
Lemma under_var_implies_denotation: forall (Gamma: context),
  forall (x : string) (tau : underty),
    well_formed_ctx Gamma -> l_find_right_most Gamma x = Some (Uty tau) -> tmR_in_ctx Gamma tau x.
Proof with eauto.
  intros Gamma x tau Hwf Hfind. apply l_find_right_most_some_spec in Hfind.
  destruct Hfind as (Gamma1 & Gamma2 & Hctxconcat & HGamma2). subst.
  generalize dependent x. generalize dependent tau. generalize dependent Gamma2.
  induction Gamma1; simpl; intros Gamma2 tau x Hwf Hfind.
  - destruct tau.
    + constructor.
      destruct (under_base_representative b r) as (e_x & H1 & H2).
      exists e_x. split... intros c_x Gamma' tau' H3 H4 H5. rewrite H2 in H4. simpl. rewrite eqb_refl.
      rewrite ty_subst_under_ctx_not_in_ctx in H5...
      assert (is_closed_refinement b r). eapply constant_refinement_is_closed. apply H4.
      inversion H5; subst...
      erewrite refinement_subst_c_closed_rf...
      apply tmR_in_ctx_pre_weakening with (Gamma1 := Gamma2) (Gamma2 := l_empty)...
      apply app_nil_r.
      constructor. simpl. constructor...
      constructor... constructor... constructor... constructor... split... intros. rewrite H4 in H6. subst...
    + destruct u.
      {
        constructor. admit.
      }
      {
        constructor. admit.
      }
  - admit.
Admitted.

(* sub *)
Lemma sub_implies_denotation: forall (Gamma: context),
  forall (e : tm) (tau1 tau2 : underty),
  l_empty \C- e \Tin tau1 ->
  tmR_in_ctx l_empty tau1 e ->
  l_empty \C- tau1 \<: tau2 -> well_formed l_empty tau2 -> well_formed_ctx Gamma -> tmR_in_ctx Gamma tau2 e.
Proof with eauto.


Lemma disj_typecheck_implies_disj_denotation: forall Gamma,
  forall (e : tm) (tau1 tau2 tau3 : underty),
  Gamma \C- e \Tin tau1 ->
  tmR_in_ctx Gamma tau1 e ->
  Gamma \C- e \Tin tau2 ->
  tmR_in_ctx Gamma tau2 e -> Gamma \C- tau1 \tyor tau2 \tyeq tau3 -> well_formed l_empty tau3 -> tmR_in_ctx Gamma tau3 e.
Proof with eauto.
  unfold disjunct. intros. rewrite <- H3...
Qed.

Theorem soundness: forall (Gamma: context) (e: tm) (uty:underty),
    Gamma \C- e \Tin uty -> tmR_in_ctx Gamma uty e.
Proof with eauto.
  apply (term_under_type_chek_rec
           (fun Gamma v uty H => tmR_in_ctx Gamma uty (tvalue v))
           (fun Gamma e uty H => tmR_in_ctx Gamma uty e)); intros Gamma.
  (* constant *)
  - intros. apply mk_eq_constant_implies_denotation.
  (* var *) (* some context trick *)
  - apply under_var_implies_denotation.
  - admit.
  (* lam *) (* some context trick *)
  - admit. - admit.
  (* value *)
  - auto.
  (* err *)
  - apply err_typecheck_implies_err_denotation.
  (* sub *) (* some context trick *)
  - intros e tau1 tau2 Ht1 Hd1 Hsub Hwf2 Hwfctx. unfold 
  (* merge *)
  - unfold disjunct. intros. rewrite <- d...
  (* lete *)
  - admit.
  (* letop *)
  - admit.
  (* letapp_funcarg *)
  - admit.
  (* letapp_const *)
  - admit.
  (* letapp_var *)
  - admit.
Admitted.

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
Qed
