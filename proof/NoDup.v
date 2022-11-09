Set Warnings "-notation-overridden,-parsing".
From CT Require Import Maps.
From Coq Require Import Lists.List.
From Coq Require Import Strings.String.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Classical.
From CT Require Import RfTypeDef.
From CT Require Import LinearContext.
From CT Require Import Ax.

Import ListNotations.
Import CoreLang.
Import NormalTypeSystem.
Import Ax.

Definition tystate := string -> option base_ty.

Definition state_to_tystate (nst: state) :=
  fun x => (match nst x with
         | None => None
         | Some c => Some (ty_of_const c)
         end).

Notation " 'st\_' st '_/' " := (state_to_tystate st) (at level 40).

Global Hint Unfold state_to_tystate: core.

Definition state_in_tystate (st: state) (tyst: tystate): Prop :=
  forall x Tx, tyst x = Some Tx -> (exists c, st x = Some c).

Notation " st '\TYSTin' nst " := (state_in_tystate st nst) (at level 40).

Definition nstate_to_tystate_hd: forall nst x e_x,
    (st\_ x |-> e_x; nst _/) = (x |-> (ty_of_const e_x); (st\_ nst _/)).
Proof with eauto.
  intros. apply functional_extensionality. intros x'. unfold state_to_tystate.
  destruct (eqb_spec x x'); subst...
  - rewrite update_eq. unfold state_to_tystate. rewrite update_eq. reflexivity.
  - rewrite update_neq... unfold state_to_tystate. rewrite update_neq...
Qed.

Global Hint Rewrite nstate_to_tystate_hd: core.

Definition nstate_to_tystate_empty: (st\_ empty _/) = empty.
Proof with eauto.
  apply functional_extensionality...
Qed.

Global Hint Rewrite nstate_to_tystate_empty: core.

Definition lcontxt := linear_context overunderty.

Inductive type_ctx_no_dup: tystate -> lcontxt -> Prop :=
| type_ctx_no_dup_nil: forall tyst, type_ctx_no_dup tyst nil
| type_ctx_no_dup_cons: forall tyst x tau_x Gamma,
    tyst x = None ->
    l_find_right_most Gamma x = None ->
    type_ctx_no_dup tyst Gamma ->
    type_ctx_no_dup tyst ((x, tau_x) :: Gamma).

Global Hint Constructors type_ctx_no_dup: core.

Lemma type_ctx_no_dup_cannot_find_fst: forall tyst Gamma b tau_b,
    type_ctx_no_dup tyst ((b, tau_b)::Gamma) -> l_find_right_most Gamma b = None.
Proof. intros. inversion H. auto. Qed.

Global Hint Resolve type_ctx_no_dup_cannot_find_fst: core.

Lemma type_ctx_no_dup_cannot_find_fst_in_nst: forall tyst Gamma b tau_b,
    type_ctx_no_dup tyst ((b, tau_b)::Gamma) -> tyst b = None.
Proof. intros. inversion H. auto. Qed.

Global Hint Resolve type_ctx_no_dup_cannot_find_fst_in_nst: core.

Lemma type_ctx_no_dup_cannot_find_last: forall Gamma tyst b tau_b,
    type_ctx_no_dup tyst (Gamma ++ ((b, tau_b)::nil)) -> l_find_right_most Gamma b = None.
Proof with eauto.
  induction Gamma; intros tyst b tau_b H...
  - destruct a; subst. inversion H; subst.
    assert (l_find_right_most Gamma b = None)... simpl. rewrite H0.
    assert (l_find_right_most  [(b, tau_b)] s = None)... inversion H1.
    destruct (eqb_spec s b); subst... rewrite eqb_refl. rewrite eqb_refl in H3. inversion H3.
Qed.

Global Hint Resolve type_ctx_no_dup_cannot_find_last: core.

Lemma type_ctx_no_dup_ctx_pre: forall Gamma1 tyst Gamma2,
    type_ctx_no_dup tyst (Gamma1 ++ Gamma2) -> type_ctx_no_dup tyst Gamma1.
Proof with eauto.
  induction Gamma1; intros tyst Gamma2 H...
  - inversion H...
Qed.

Global Hint Resolve type_ctx_no_dup_ctx_pre: core.

Lemma type_ctx_no_dup_ctx_post: forall Gamma1 tyst Gamma2,
    type_ctx_no_dup tyst (Gamma1 ++ Gamma2) -> type_ctx_no_dup tyst Gamma2.
Proof with eauto.
  induction Gamma1; intros tyst Gamma2 H...
  - inversion H...
Qed.

Global Hint Resolve type_ctx_no_dup_ctx_post: core.

Lemma type_ctx_no_dup_cannot_find_last_in_nst: forall Gamma tyst b tau_b,
    type_ctx_no_dup tyst (Gamma ++ ((b, tau_b)::nil)) -> tyst b = None.
Proof with eauto.
  induction Gamma; intros tyst b tau_b H...
Qed.

Global Hint Resolve type_ctx_no_dup_ctx_post: core.

Lemma type_ctx_no_dup_implies_tail: forall st s0 o Gamma,
    type_ctx_no_dup st ((s0, o) :: Gamma) -> type_ctx_no_dup st Gamma.
Proof with eauto.
  intros... rewrite <- app_one_is_cons in H...
Qed.

Global Hint Resolve type_ctx_no_dup_implies_tail: core.

Lemma type_ctx_no_dup_fst_last_diff_name: forall Gamma tyst a tau_a b tau_b,
    type_ctx_no_dup tyst ((a, tau_a)::Gamma ++ ((b, tau_b)::nil)) -> a <> b.
Proof with eauto.
  induction Gamma; intros tyst b tau_b H...
Qed.

Global Hint Resolve type_ctx_no_dup_fst_last_diff_name: core.

Lemma nodup_update: forall Gamma st s b r,
    type_ctx_no_dup st ((s, Uty ([[v:b | r]])) :: Gamma) ->
    type_ctx_no_dup (s |-> b; st) Gamma.
Proof with eauto.
  induction Gamma; simpl; intros st s b r H...
  - inversion H; subst... destruct a. constructor.
    rewrite update_neq... eapply l_find_right_most_none_neq_hd in H5...
    eauto.
    apply IHGamma with (r := r)...
Qed.

Lemma nodup_update_over: forall Gamma st s b r,
    type_ctx_no_dup st ((s, Oty ({{v:b | r}})) :: Gamma) ->
    type_ctx_no_dup (s |-> b; st) Gamma.
Proof with eauto.
  induction Gamma; simpl; intros st s b r H...
  - inversion H; subst... destruct a. constructor.
    rewrite update_neq... eapply l_find_right_most_none_neq_hd in H5...
    eauto.
    apply IHGamma with (r := r)...
Qed.

Lemma nodup_permute: forall tyst a tau_a b tau_b Gamma,
    a <> b ->
    type_ctx_no_dup tyst ((a, tau_a) :: (b, tau_b) :: Gamma) ->
    type_ctx_no_dup tyst ((b, tau_b) :: (a, tau_a) :: Gamma).
Proof with eauto.
  intros.
  inversion H0;subst. constructor...
  inversion H7;subst.
  eapply l_find_right_most_none_neq_hd in H6...
  simpl... rewrite H9. destruct (eqb_spec a b); subst... exfalso...
Qed.

Fixpoint erase_ctx (Gamma: lcontxt) :=
  match Gamma with
  | nil => empty
  | (x, tau)::Gamma => update (erase_ctx Gamma) x (ou\_ tau _/)
  end.

Lemma type_ctx_no_dup_implies_head_free: forall st a tau_a Gamma,
    type_ctx_no_dup st ((a, tau_a) :: Gamma) -> l_find_right_most ((a, tau_a) :: Gamma) a = None.
Proof with eauto.
  intros.
  inversion H; subst.
  assert (erase_ctx ((a, Uty (mk_bot TBool)) :: (a, Uty (mk_bot TNat)) :: nil) =
            erase_ctx ((a, Uty (mk_bot TNat)) :: (a, Uty (mk_bot TBool)) :: nil)).
  simpl. rewrite state_permute. apply functional_extensionality. intros.
  destruct (eqb_spec a x); subst. rewrite update_eq...
  rewrite update_neq...
  assert ((erase_ctx ((a, Uty (mk_bot TBool)) :: (a, Uty (mk_bot TNat)) :: nil)) a = Some (TBase TBool)).
  simpl. rewrite update_eq...
  assert ((erase_ctx ((a, Uty (mk_bot TNat)) :: (a, Uty (mk_bot TBool)) :: nil)) a = Some (TBase TNat)).
  simpl. rewrite update_eq...
  rewrite H0 in H1. rewrite H1 in H2. inversion H2.
Qed.

Lemma nodup_dropfst: forall st s tau_s Gamma,
    type_ctx_no_dup st ((s, tau_s) :: Gamma) ->
    type_ctx_no_dup st Gamma.
Proof with eauto.
  intros. rewrite <- app_one_is_cons in H...
Qed.

Lemma nodup_append: forall a tau_a Gamma nst,
    nst a = None ->
    l_find_right_most Gamma a = None ->
    type_ctx_no_dup nst Gamma ->
    type_ctx_no_dup nst (Gamma <l> a :l: tau_a).
Proof with eauto.
  intros a tau_a.
  induction Gamma.
  - intros nst Hnst Hfind H. setoid_rewrite app_nil_l. constructor...
  - intros nst Hnst Hfind H... destruct a0. constructor.
    + eauto.
    + inversion H; subst. apply l_find_right_most_none_neq_hd in Hfind...
      apply find_none_append...
    + apply IHGamma; auto. apply l_find_right_most_none_neq_tl in Hfind...
      apply type_ctx_no_dup_implies_tail in H...
Qed.


Definition fresh_var_gen (x: string) := append "1" x.
Lemma fresh_var_gen_is_fresh: forall x, fresh_var_gen x <> x.
Proof.
  intros.
  destruct (classic (fresh_var_gen x = x)); auto.
  exfalso. assert ((length (fresh_var_gen x)) = (length x)). rewrite H... reflexivity.
  simpl in H0.
  induction (length x). inversion H0. apply IHn. inversion H0... rewrite H2. rewrite H2. reflexivity.
Qed.

Lemma exist_fresh_var: forall (x: string), exists y, y <> x.
Proof with eauto.
  intros. exists (fresh_var_gen x).
  remember (fresh_var_gen_is_fresh x).
  destruct (eqb_spec (fresh_var_gen x) x); subst... apply fresh_var_gen_is_fresh.
Qed.
