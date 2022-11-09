(** * MoreStlc: More on the Simply Typed Lambda-Calculus *)

Set Warnings "-notation-overridden,-parsing".
From CT Require Import Maps.
From CT Require Import CoreLang.
From CT Require Import NormalTypeSystem.
From CT Require Import LinearContext.
From CT Require Import RfTypeDef.
From CT Require Import TypeClosed.
From CT Require Import Denotation.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Lists.List.
From Coq Require Import FunInd.
From Coq Require Import Recdef.

Import CoreLang.
Import NormalTypeSystem.
Import LinearContext.
Import RfTypeDef.
Import TypeClosed.
Import NoDup.
Import Ax.
Import Denotation.
Import ListNotations.

Definition erase_remove_dup (ctx: lcontxt) : lcontxt :=
  match ctx with
  | nil => nil
  | (a, Ta) :: ctx' =>
      match ctx' with
      | nil => ctx
      | (b, Tb) :: ctx' =>
          if String.eqb a b
          then (a, Ta) :: ctx'
          else (b, Tb) :: (a, Ta) :: ctx'
      end
  end.

Lemma no_dup_same_remove_dup: forall (Gamma: lcontxt),
    type_ctx_no_dup empty Gamma ->
    (erase_ctx (erase_remove_dup Gamma)) = (erase_ctx Gamma).
Proof with eauto.
  intros.
  induction H...
  destruct Gamma; simpl...
  destruct p.
  simpl in IHtype_ctx_no_dup. apply l_find_right_most_none_neq_hd in H0.
  destruct (eqb_spec x s); subst. exfalso. apply H0...
  simpl. rewrite state_permute...
Qed.

Fixpoint remove_arr_ty_bindings (ctx: lcontxt) : lcontxt :=
  match ctx with
  | nil => nil
  | (a, Ta) :: ctx' =>
      match Ta with
      | {{v:TNat | phi}} => (a, Oty ({{v:TNat | phi}})) :: (remove_arr_ty_bindings ctx')
      | {{v:TBool | phi}} => (a, Oty ({{v:TBool | phi}})) :: (remove_arr_ty_bindings ctx')
      | [[v:TNat | phi]] => (a, Uty ([[v:TNat | phi]])) :: (remove_arr_ty_bindings ctx')
      | [[v:TBool | phi]] => (a, Uty ([[v:TBool | phi]])) :: (remove_arr_ty_bindings ctx')
      | _ => remove_arr_ty_bindings ctx'
      end
  end.

(* Lemma erase_arr_is_eq: forall st Gamma, *)
(*     type_ctx_no_dup st Gamma -> *)
(*     (remove_arr_ty_bindings Gamma) = erase_remove_dup (remove_arr_ty_bindings Gamma). *)
(* Proof. *)
(*   intros. rewrite <- erase_remove_dup... *)

Definition flip_head (ctx: lcontxt) : lcontxt :=
  match ctx with
  | nil => nil
  | (a, Ta) :: ctx' =>
      match Ta with
      | {{v:TNat | phi}} => (fresh_var_gen a, Uty ([[v:TBool | phi]])) :: (a, Oty ({{v:TNat | phi}})) :: nil
      | {{v:TBool | phi}} => (fresh_var_gen a, Uty ([[v:TNat | phi]])) :: (a, Oty ({{v:TBool | phi}})) :: nil
      | [[v:TNat | phi]] => (fresh_var_gen a, Oty ({{v:TBool | phi}})) :: (a, Uty ([[v:TNat | phi]])) :: nil
      | [[v:TBool | phi]] => (fresh_var_gen a, Oty ({{v:TNat | phi}})) :: (a, Uty ([[v:TBool | phi]])) :: nil
      | _ => ctx
      end
  end.

Global Hint Resolve fresh_var_gen_is_fresh: core.

Lemma flip_head_has_no_dup: forall Gamma,
    type_ctx_no_dup empty Gamma -> type_ctx_no_dup empty (flip_head Gamma).
Proof with eauto.
  intros.
  destruct Gamma... destruct p. destruct o. simpl. destruct u. destruct b.
  -  constructor...
  simpl. assert (s <> fresh_var_gen s)... destruct (eqb_spec s (fresh_var_gen s)); subst... exfalso...
  -  constructor...
  simpl. assert (s <> fresh_var_gen s)... destruct (eqb_spec s (fresh_var_gen s)); subst... exfalso...
  - inversion H; subst. constructor...
  - apply H.
  - destruct o... simpl. destruct b.
    constructor...
  simpl. assert (s <> fresh_var_gen s)... destruct (eqb_spec s (fresh_var_gen s)); subst... exfalso...
    constructor...
  simpl. assert (s <> fresh_var_gen s)... destruct (eqb_spec s (fresh_var_gen s)); subst... exfalso...
Qed.

Lemma no_dup_state_implies_no_dup: forall Gamma st,
    type_ctx_no_dup st Gamma -> type_ctx_no_dup empty Gamma.
Proof with eauto.
  induction Gamma...
  intros. inversion H; subst. constructor...
Qed.

Lemma flip_head_is_eq: forall st Gamma,
    type_ctx_no_dup st Gamma ->
    erase_ctx (flip_head Gamma) = erase_ctx (erase_remove_dup (flip_head Gamma)).
Proof.
  intros. apply no_dup_state_implies_no_dup in H. apply flip_head_has_no_dup in H.
  apply no_dup_same_remove_dup in H. auto.
Qed.

Lemma erase_included: forall Gamma, includedin empty (erase_ctx Gamma).
Proof with eauto.
  intros. induction Gamma... simpl... unfold includedin . intros...
  simpl. destruct a. unfold includedin . intros... inversion H.
Qed.

Global Hint Resolve erase_included: core.

Lemma no_dup_implies_ctx_lift: forall Gamma x tau_x e tau,
    type_ctx_no_dup empty (Gamma <l> x :l: tau_x) ->
    (erase_ctx (Gamma <l> x :l: tau_x)) \N- e \Tin tau <-> (erase_ctx ((x, tau_x) :: Gamma)) \N- e \Tin tau.
Proof with eauto.
  induction Gamma; intros...
  - setoid_rewrite app_nil_l. setoid_rewrite app_nil_l in H. simpl. simpl in H. split...
  - destruct a.
    simpl.
    setoid_rewrite <- app_comm_cons in H.
    apply type_ctx_no_dup_implies_head_free in H. apply l_find_right_most_none_neq_hd in H. exfalso...
Qed.

Global Hint Resolve no_dup_implies_ctx_lift: core.

Lemma over_eq_var_in_Gamma_has_type: forall Gamma x T phi,
    l_find_right_most Gamma x = Some (Oty ({{v: T | phi }})) -> (erase_ctx Gamma) \N- x \Vin T.
Proof with eauto.
  induction Gamma; intros...
  - simpl in H. inversion H.
  - destruct a. destruct (eqb_spec s x); subst. simpl. constructor... rewrite update_eq.
    assert (type_ctx_no_dup empty ((x, o)::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H0. apply l_find_right_most_none_neq_hd in H0. exfalso...
    assert (type_ctx_no_dup empty ((x, o)::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H0. apply l_find_right_most_none_neq_hd in H0. exfalso...
Qed.

Global Hint Resolve over_eq_var_in_Gamma_has_type: core.

Lemma under_eq_var_in_Gamma_has_type: forall Gamma x T phi,
    l_find_right_most Gamma x = Some (Uty ([[v: T | phi ]])) -> (erase_ctx Gamma) \N- x \Vin T.
Proof with eauto.
  induction Gamma; intros...
  - simpl in H. inversion H.
  - destruct a. destruct (eqb_spec s x); subst. simpl. constructor... rewrite update_eq.
    assert (type_ctx_no_dup empty ((x, o)::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H0. apply l_find_right_most_none_neq_hd in H0. exfalso...
    assert (type_ctx_no_dup empty ((x, o)::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H0. apply l_find_right_most_none_neq_hd in H0. exfalso...
Qed.

Global Hint Resolve under_eq_var_in_Gamma_has_type: core.

Lemma var_in_Gamma_has_type: forall Gamma x tau,
    l_find_right_most Gamma x = Some tau -> (erase_ctx Gamma) \N- x \Vin (ou\_ tau _/).
Proof with eauto.
  induction Gamma; intros...
  - simpl in H. inversion H.
  - destruct a.
    destruct tau. destruct u.
    + eapply under_eq_var_in_Gamma_has_type...
    + constructor...
      assert (type_ctx_no_dup empty ((x, o)::nil)). constructor...
      apply type_ctx_no_dup_implies_head_free in H0. apply l_find_right_most_none_neq_hd in H0. exfalso...
    + constructor...
      assert (type_ctx_no_dup empty ((x, o)::nil)). constructor...
      apply type_ctx_no_dup_implies_head_free in H0. apply l_find_right_most_none_neq_hd in H0. exfalso...
    + constructor...
      assert (type_ctx_no_dup empty ((x, o)::nil)). constructor...
      apply type_ctx_no_dup_implies_head_free in H0. apply l_find_right_most_none_neq_hd in H0. exfalso...
Qed.

Global Hint Resolve var_in_Gamma_has_type: core.

Lemma tmR_in_ctx_aux_implies_has_type: forall Gamma st tau e,
    tmR_in_ctx_aux st Gamma tau e -> (erase_ctx Gamma) \N- e \Tin ou\_ tau _/.
Proof with eauto.
  intros.
  induction Gamma. inversion H; subst...
  - destruct a.
    apply tmR_in_ctx_aux_implies_no_dup in H.
    apply type_ctx_no_dup_implies_head_free in H.
    simpl in H.
    destruct (l_find_right_most Gamma s). inversion H.
    rewrite eqb_refl in H. inversion H.
 Qed.


Global Hint Resolve tmR_in_ctx_aux_implies_has_type: core.
