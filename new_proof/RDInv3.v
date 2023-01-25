From stdpp Require Import mapset.
From stdpp Require Import natmap.
From stdpp Require Import fin_map_dom.
From CT Require Import CoreLangProp.
From CT Require Import OperationalSemanticsProp.
From CT Require Import BasicTypingProp.
From CT Require Import SyntaxSugar.
From CT Require Import Refinement.
From CT Require Import RefinementTac.
From CT Require Import RefinementDenotation.
From CT Require Import RefinementDenotationTac.
From CT Require Import RefinementDenotationProp.
From CT Require Import RDInv.
From CT Require Import RDInv2.
From CT Require Import TermOrdering.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Classical.

Import Atom.
Import CoreLang.
Import CoreLangProp.
Import Tactics.
Import NamelessTactics.
Import ListCtx.
Import OperationalSemantics.
Import OperationalSemanticsProp.
Import BasicTyping.
Import SyntaxSugar.
Import Refinement.
Import RefinementTac.
Import RefinementDenotation.
Import RefinementDenotationTac.
Import RefinementDenotationProp.
Import NamelessTactics.
Import TermOrdering.
Import RDInv.
Import RDInv2.

Global Hint Resolve mk_eq_constant_is_not_overbasety: core.
Global Hint Resolve mk_eq_var_is_not_overbasety: core.
Global Hint Resolve is_arr_implies_not_overbasety: core.
Global Hint Constructors ok_dctx: core.
Global Hint Resolve rR_implies_no_free: core.
Global Hint Resolve ctxrR_tlete_drop_halt_lhs: core.
Global Hint Resolve rR_implies_reduction_no_free: core.
Global Hint Resolve is_arr_implies_not_overbasety: core.
Global Hint Resolve under_not_overbasety: core.

Lemma rR_arrow_value_lam_exists_oarr:
  ∀ (v : value) b n d ϕ τ m bst st,
    {m;bst;st}⟦-:{v: b | n | d | ϕ}⤑ τ⟧ v ->
    (∃ e : tm, v = vlam b e) ∨ (∃ e : tm, v = vfix (b ⤍ ⌊ τ ⌋) (vlam b e)).
Proof.
  intros.
  apply empty_basic_typing_arrow_value_lam_exists. refinement_solver.
Qed.

Lemma rR_arrow_value_lam_exists_arrarr:
  ∀ (v : value) τ_x τ m bst st,
    {m;bst;st}⟦τ_x ⤑ τ⟧ v ->
    (∃ e : tm, v = vlam ⌊ τ_x ⌋ e) ∨ (∃ e : tm, v = vfix (⌊ τ_x ⌋ ⤍ ⌊ τ ⌋) (vlam ⌊ τ_x ⌋ e)).
Proof.
  intros.
  apply empty_basic_typing_arrow_value_lam_exists. refinement_solver.
Qed.

Lemma is_arr_open_trans: forall τ_x (v2: value),
  is_arr τ_x -> is_arr (τ_x ^r^ v2).
Proof.
  intros. destruct τ_x; auto.
Qed.

Global Hint Resolve is_arr_open_trans: core.

Ltac wf_regular_solver :=
  match goal with
  | [H: wf_ctxrR ?st ?Γ |- ok_dctx _ ?Γ] => apply wf_ctxrR_regular1 in H; auto
  end.

Ltac lc_simpl4 :=
  match goal with
  | [H: context [{?x := ?v }t ({_ ~t> (vfvar ?x)} ?e)] |- _ ] =>
      assert (x ∉ stale e) as Htmp by fast_set_solver;
      rewrite open_subst_same_tm in H; auto; try clear Htmp
  | [|- context [{?x := ?v }t ({_ ~t> (vfvar ?x)} ?e)]] =>
      assert (x ∉ stale e) as Htmp by fast_set_solver;
      rewrite open_subst_same_tm; auto; try clear Htmp
  | [H: context [{?x := ?v }v ({_ ~v> (vfvar ?x)} ?e)] |- _ ] =>
      assert (x ∉ stale e) as Htmp by fast_set_solver;
      rewrite open_subst_same_value in H; auto; try clear Htmp
  | [|- context [{?x := ?v }v ({_ ~v> (vfvar ?x)} ?e)]] =>
      assert (x ∉ stale e) as Htmp by fast_set_solver;
      rewrite open_subst_same_value; auto; try clear Htmp
  end.

Lemma tletapp_typable
     : ∀ Γ (v1 v2 : value) (e : tm) (x : atom) (T1 T2 T: ty),
         x ∉ fv_tm e -> Γ ⊢t v1 ⋮v (T2 ⤍ T1) -> Γ ⊢t v2 ⋮v T2
         → (Γ ++ [(x, T1)]) ⊢t e ^t^ x ⋮t T -> Γ ⊢t tletapp v1 v2 e ⋮t T.
Proof.
  intros. auto_exists_L; intros.
  apply basic_has_type_renaming with (x0:=x0) in H2; auto.
  lc_simpl4. fast_set_solver. basic_typing_solver.
Qed.

Ltac inv_rd_simpl3 :=
  repeat match goal with
    | [H: context [⌊[v:_|_|_|_]⌋] |- _ ] => simpl in H
    | [H: context [⌊{v:_|_|_|_}⌋] |- _ ] => simpl in H
    | [H: context [⌊-:{v: _|_|_|_}⤑ _⌋] |- _ ] => simpl in H
    | [H: context [⌊ _ ⤑ _⌋] |- _ ] => simpl in H
    | [H: context [ ⌊ (_, _) :: _ ⌋* ] |- _ ] => simpl in H
    | [|- context [⌊[v:_|_|_|_]⌋] ] => simpl
    | [|- context [⌊{v:_|_|_|_}⌋] ] => simpl
    | [|- context [⌊-:{v: _|_|_|_}⤑ _⌋] ] => simpl
    | [|- context [⌊ _ ⤑ _⌋] ] => simpl
    | [|- context [ ⌊ (_, _) :: _ ⌋* ] ] => simpl
    end.

Ltac auto_ty_exfalso5 :=
  match goal with
  | [H: is_arr ?τ_x, H': {v:_|_|_|_} = ?τ_x ^r^ ?v2 |- _ ] =>
      assert (is_arr (τ_x ^r^ v2)) as Htmp by auto;
      rewrite <- H' in Htmp; auto_ty_exfalso2
  | [H: is_arr ?τ_x, H': [v:_|_|_|_] = ?τ_x ^r^ ?v2 |- _ ] =>
      assert (is_arr (τ_x ^r^ v2)) as Htmp by auto;
      rewrite <- H' in Htmp; auto_ty_exfalso2
  end.

Ltac instantiation_simp :=
   repeat match goal with
    | [H: instantiation [] _ |- _ ] => invclear H
    | [H: context [tm_msubst [] _ ] |- _ ] => simpl in H
    | [|- context [tm_msubst [] _ ] ] => simpl
    end.

Lemma rR_open_trans_empty': forall τ st (c: constant) e,
    {1;<b[↦c]> b∅;st}⟦τ⟧ e -> ({0;b∅;st}⟦τ ^r^ c⟧) e.
Proof.
  intros. rewrite <- rR_open_trans' in H; mydestr; eauto.
  unfold bst_eq. intros. assert (i = 0). lia. subst. simpl. unfold bstate_insert. dec_solver2.
Qed.

Lemma rR_base_implies_constant: forall m bst st b1 n1 d1 ϕ1 (c2: value),
  ({m;bst;st}⟦[v:b1|n1|d1|ϕ1]⟧) c2 -> (exists c: constant, c2 = c).
Proof.
  intros. apply rR_regular1 in H. mydestr. invclear H0. invclear H3; eauto. invclear H1.
Qed.

Global Hint Resolve rR_base_implies_constant: core.

Lemma wf_implies_base_phi_sat: forall m bst st b1 n1 d1 ϕ1 (c2: constant),
  (∀ e_wf : tm, ({m;bst;st}⟦[v:b1|n1|d1|ϕ1]⟧) e_wf → ∃ v_wf : value, e_wf ↪* v_wf) ->
  ({m;bst;st}⟦[v:b1|n1|d1|ϕ1]⟧) c2 -> ϕ1 bst st c2 /\ [] ⊢t c2 ⋮v b1.
Proof.
  intros. invclear H0; mydestr.
  destruct (classic (exists c: constant, ϕ1 bst st c /\ [] ⊢t c ⋮v b1)).
  - mydestr. assert (c2 ↪* x); auto. reduction_simpl1; subst. invclear H5; auto.
  - neg_simpl. assert ({m;bst;st}⟦[v:b1|n1|d1|ϕ1]⟧ terr).
    do 2 constructor; auto. intros. apply H3 in H5. exfalso. apply H5; auto.
    apply H in H4. mydestr. auto_reduction_exfalso1.
Qed.

Lemma tm_open_then_msubst_k: forall Γ env,
    instantiation Γ env ->
    (forall e k v, tm_msubst env ({k ~t> v} e) = ({k ~t> (value_msubst env v)} (tm_msubst env e))).
Proof.
  intros Γ env Hi; induction Hi; simpl; intros; auto.
  rewrite subst_open_tm; basic_typing_solver6.
Qed.

Lemma mk_app_v_v_reduce_to_letapp:
  ∀ (Γ : listctx ty) (Tx T2 : ty) (v1 v2 : value),
    Γ ⊢t v2 ⋮v Tx -> Γ ⊢t v1 ⋮v (Tx ⤍ T2) ->
    (mk_app v1 v2) <-<{ Γ; T2} (tletapp v1 v2 (vbvar 0)).
Proof.
  intros. constructor.
  - eapply mk_app_typable; eauto.
  - auto_exists_L; simpl; intros. basic_typing_vfavr_solver.
  - unfold termRraw. intros Γv. intros. unfold mk_app in H2. reduction_simpl1.
    rewrite lete_step_spec in H2; simpl; mydestr. simpl in H4. reduction_simpl1.
    rewrite lete_step_spec in H4; simpl; mydestr. simpl in H6. reduction_simpl1.
Qed.

Lemma mk_app_v_v_reduce_to_letapp':
  ∀ (Γ : listctx ty) (Tx T2 : ty) (v1 v2 : value),
    Γ ⊢t v2 ⋮v Tx -> Γ ⊢t v1 ⋮v (Tx ⤍ T2) ->
    (tletapp v1 v2 (vbvar 0)) <-<{ Γ; T2} (mk_app v1 v2).
Proof.
  intros. constructor.
  - auto_exists_L; simpl; intros. basic_typing_vfavr_solver.
  - eapply mk_app_typable; eauto.
  - unfold termRraw. intros Γv. intros. unfold mk_app. reduction_simpl1.
    rewrite lete_step_spec. split; auto. apply mk_app_body. reduction_solver1.
    exists (value_msubst Γv v1). split; eauto. reduction_solver1.
    simpl. reduction_simpl1.
    rewrite lete_step_spec. split; auto. reduction_solver1. reduction_simpl1.
    exists (value_msubst Γv v2). split; eauto. reduction_solver1. reduction_simpl1.
Qed.

(* Lemma mk_app_v_v_reduce_to_letapp_e: *)
(*   ∀ (Γ : listctx ty) (Tx T2 T: ty) (v1 v2 : value) e, *)
(*     Γ ⊢t v2 ⋮v Tx -> Γ ⊢t v1 ⋮v (Tx ⤍ T2) -> *)
(*     (exists (L: aset), ∀ x : atom, x ∉ L → (Γ ++ [(x, T2)]) ⊢t e ^t^ x ⋮t T) -> *)
(*     (tlete (mk_app v1 v2) e) <-<{ Γ; T} (tletapp v1 v2 e). *)
(* Proof. *)
(*   intros. constructor. *)
(*   - mydestr. econstructor; eauto. eapply mk_app_typable; eauto. *)
(*   - mydestr. econstructor; eauto. *)
(*   - unfold termRraw. intros Γv. intros. unfold mk_app in H3. reduction_simpl1. *)
(*     rewrite lete_step_spec in H3; simpl; mydestr. simpl in H4. reduction_simpl1. *)
(*     rewrite lete_step_spec in H4; simpl; mydestr. simpl in H7. reduction_simpl1. *)
(*     rewrite lete_step_spec in H7; simpl; mydestr. simpl in H9. reduction_simpl1. *)
(*     reduction_simpl1. *)
(* Qed. *)

Lemma lete_0_reduce_to_self_aux: ∀ (Γ : listctx ty) e,
    termRraw Γ e (tlete e (vbvar 0)).
Proof.
  intros. unfold termRraw. intros Γv. intros. reduction_simpl1.
  rewrite lete_step_spec. split; auto. eexists; split; eauto. reduction_solver1.
Qed.

Lemma lete_0_reduce_to_self_aux': ∀ (Γ : listctx ty) e,
    termRraw Γ (tlete e (vbvar 0)) e.
Proof.
  intros. unfold termRraw. intros Γv. intros. reduction_simpl1.
  rewrite lete_step_spec in H0; mydestr. simpl in H2. reduction_simpl1; auto.
Qed.

Lemma lete_0_reduce_to_self: ∀ (Γ : listctx ty) (T : ty) e,
    Γ ⊢t e ⋮t T -> e <-<{ Γ; T} (tlete e (vbvar 0)).
Proof.
  intros. constructor; auto.
  - auto_exists_L; simpl; intros. basic_typing_vfavr_solver.
  - unfold termRraw. intros Γv. intros. reduction_simpl1.
     rewrite lete_step_spec. split; auto. eexists; split; eauto. reduction_solver1.
Qed.

Lemma lete_0_reduce_to_self': ∀ (Γ : listctx ty) (T : ty) e,
    Γ ⊢t e ⋮t T -> (tlete e (vbvar 0)) <-<{ Γ; T} e.
Proof.
  intros. constructor; auto.
  - auto_exists_L; simpl; intros. basic_typing_vfavr_solver.
  - unfold termRraw. intros Γv. intros. reduction_simpl1.
    rewrite lete_step_spec in H1; mydestr. simpl in H3. reduction_simpl1; auto.
Qed.

(* Lemma basic_typing_implies_open_value: forall Γ (T1 T2: ty) (v1: value) (e2: value), *)
(*     Γ ⊢t v1 ⋮v T1 -> Γ ⊢t e2 ⋮v T1 ⤍ T2 -> Γ ⊢t (e2 ^v^ v1) ⋮v T2. *)
(* Proof. *)
(*   intros. invclear H0. *)

Lemma vfix_implies_open_tyable:
  ∀ (Γ : context) (v1 : value) (Tx T : ty),
    Γ ⊢t vfix (Tx ⤍ T) v1 ⋮v Tx ⤍ T → Γ ⊢t v1 ^v^ (vfix (Tx ⤍ T) v1) ⋮v Tx ⤍ T.
Proof.
  intros. invclear H. auto_pose_fv f.
  assert ((Γ ++ [(f, Tx0 ⤍ T)]) ⊢t (vlam Tx0 e) ^t^ f ⋮t (Tx0 ⤍ T)) by fast_set_solver.
  invclear H0.
  apply basic_typing_subst_value_pre with (u:=(vfix (Tx0 ⤍ T) (vlam Tx0 e))) in H4; eauto.
  simpl in H4. simpl. lc_simpl4.
Qed.

Lemma letapp_fix_reduce_to_open:
  ∀ (Γ : listctx ty) (Tx T2 : ty) e1 (v2 : value),
    Γ ⊢t v2 ⋮v Tx -> Γ ⊢t (vfix (Tx ⤍ T2) (vlam Tx e1)) ⋮v (Tx ⤍ T2) ->
    (tletapp (vfix (Tx ⤍ T2) (vlam Tx e1)) v2 (vbvar 0)) <-<{ Γ; T2}
      (({1 ~t> (vfix (Tx ⤍ T2) (vlam Tx e1))} e1) ^t^ v2).
Proof.
  intros. constructor.
  - auto_exists_L; simpl; intros. basic_typing_vfavr_solver.
  - apply vfix_implies_open_tyable in H0. simpl in H0.
    apply vlam_implies_open_tyable with (v2:= v2) in H0; auto.
  - unfold termRraw. intros Γv. intros. unfold mk_app in H2. msubst_simpl.
    rewrite letapp_step_spec in H2; mydestr. destruct H5; mydestr; subst; invclear H5. simpl in H6.
    rewrite letapp_step_spec in H6; mydestr. destruct H8; mydestr; subst; invclear H8. simpl in H9.
    eapply lete_0_reduce_to_self_aux'; eauto. msubst_simpl.
    setoid_rewrite tm_open_then_msubst_k; eauto.
    setoid_rewrite tm_open_then_msubst_k; eauto. msubst_simpl. auto.
Qed.

Lemma letapp_fix_reduce_to_open':
  ∀ (Γ : listctx ty) (Tx T2 : ty) e1 (v2 : value),
    Γ ⊢t v2 ⋮v Tx -> Γ ⊢t (vfix (Tx ⤍ T2) (vlam Tx e1)) ⋮v (Tx ⤍ T2) ->
    (({1 ~t> (vfix (Tx ⤍ T2) (vlam Tx e1))} e1) ^t^ v2) <-<{ Γ; T2} (tletapp (vfix (Tx ⤍ T2) (vlam Tx e1)) v2 (vbvar 0)).
Proof.
  intros. constructor.
  - apply vfix_implies_open_tyable in H0. simpl in H0.
    apply vlam_implies_open_tyable with (v2:= v2) in H0; auto.
  - auto_exists_L; simpl; intros. basic_typing_vfavr_solver.
  - unfold termRraw. intros Γv. intros. msubst_simpl.
    rewrite letapp_step_spec. repeat split; auto.
    { assert (lc (vfix (Tx ⤍ T2) (vlam Tx e1))). basic_typing_solver.
      assert (lc (value_msubst Γv (vfix (Tx ⤍ T2) (vlam Tx e1)))).
      eapply instantiation_implies_value_msubst_lc; eauto. msubst_simpl. auto. }
    reduction_solver1.
    right. do 2 eexists; split; eauto. simpl.
    rewrite letapp_step_spec. repeat split; auto.
    { apply vfix_implies_open_tyable in H0. simpl in H0.
      assert (lc (vlam Tx ({1 ~t> vfix (Tx ⤍ T2) (vlam Tx e1)} e1))). basic_typing_solver.
      eapply instantiation_implies_value_msubst_lc in H3; eauto. msubst_simpl.
      setoid_rewrite tm_open_then_msubst_k in H3; eauto. msubst_simpl. auto. }
    reduction_solver1.
    left. do 2 eexists; split; eauto. simpl.
    eapply lete_0_reduce_to_self_aux in H2; eauto. msubst_simpl.
    setoid_rewrite tm_open_then_msubst_k in H2; eauto.
    setoid_rewrite tm_open_then_msubst_k in H2; eauto. msubst_simpl. auto.
Qed.

Lemma termR_trans_better:
  ∀ (Γ : context) (T : ty) (e1 e2 e3 : tm),
    e1 <-<{ Γ; T} e2 → e2 <-<{ Γ; T} e3 → e1 <-<{ Γ; T} e3.
Proof.
  intros. eapply termR_trans; eauto.
  - invclear H; auto.
  - invclear H; auto.
  - invclear H0; auto.
Qed.

Lemma mk_app_reduce_to_open_fix:
  ∀ (Γ : listctx ty) (Tx T2 : ty) e1 (v2 : value),
    Γ ⊢t v2 ⋮v Tx -> Γ ⊢t (vfix (Tx ⤍ T2) (vlam Tx e1)) ⋮v (Tx ⤍ T2) ->
    (mk_app (vfix (Tx ⤍ T2) (vlam Tx e1)) v2) <-<{ Γ; T2}
      (({1 ~t> (vfix (Tx ⤍ T2) (vlam Tx e1))} e1) ^t^ v2).
Proof.
  intros. apply termR_trans_better with (e2:= tletapp (vfix (Tx ⤍ T2) (vlam Tx e1)) v2 (vbvar 0)).
  - eapply mk_app_v_v_reduce_to_letapp; eauto.
  - eapply letapp_fix_reduce_to_open; eauto.
Qed.

Lemma mk_app_reduce_to_open_fix':
  ∀ (Γ : listctx ty) (Tx T2 : ty) e1 (v2 : value),
    Γ ⊢t v2 ⋮v Tx -> Γ ⊢t (vfix (Tx ⤍ T2) (vlam Tx e1)) ⋮v (Tx ⤍ T2) ->
     (({1 ~t> (vfix (Tx ⤍ T2) (vlam Tx e1))} e1) ^t^ v2) <-<{ Γ; T2} (mk_app (vfix (Tx ⤍ T2) (vlam Tx e1)) v2).
Proof.
  intros. apply termR_trans_better with (e2:= tletapp (vfix (Tx ⤍ T2) (vlam Tx e1)) v2 (vbvar 0)).
  - eapply letapp_fix_reduce_to_open'; eauto.
  - eapply mk_app_v_v_reduce_to_letapp'; eauto.
Qed.

Lemma rR_letapp_overbase_underbase:
  forall st τ_x (v1: value) (c2: constant) b1 n1 d1 ϕ1 b2 n2 d2 ϕ2,
  τ_x = [v:b2|n2|d2|ϕ2] ->
  (∀ e_wf : tm, ({0;b∅;st}⟦[v:b1|n1|d1|ϕ1]⟧) e_wf → ∃ v_wf : value, e_wf ↪* v_wf) ->
  ({0;b∅;st}⟦[v:b1|n1|d1|ϕ1]⟧) c2 ->
  ({0;b∅;st}⟦-:{v:b1|n1|d1|ϕ1}⤑ τ_x⟧) v1 ->
  (exists e1, ({0;b∅;st}⟦ τ_x ^r^ c2 ⟧) (e1 ^t^ c2) /\ (e1 ^t^ c2) <-<{ []; b2} (mk_app v1 c2)).
Proof.
  intros; subst.
  assert (ϕ1 b∅ st c2 /\ [] ⊢t c2 ⋮v b1) as (Hz & HzT). eapply wf_implies_base_phi_sat; eauto.
  destruct (rR_arrow_value_lam_exists_oarr _ _ _ _ _ _ _ _ _ H2); mydestr; subst.
  - rename x into e1. exists e1. split.
    + invclear H2; invclear H1; mydestr. apply H5 in Hz; auto.
      apply rR_open_trans_empty' in Hz; auto.
      eapply termR_perserve_rR; eauto. refinement_solver. simpl.
      apply mk_app_reduce_to_open; auto. basic_typing_solver.
    + apply mk_app_reduce_to_open'; auto. refinement_solver.
  - rename x into e1. exists (({1 ~t> (vfix (b1 ⤍ b2) (vlam b1 e1))} e1)). split.
    + invclear H2; invclear H1; mydestr. apply H5 in Hz; auto.
      apply rR_open_trans_empty' in Hz; auto.
      eapply termR_perserve_rR; eauto. refinement_solver. simpl.
      apply mk_app_reduce_to_open_fix; auto. refinement_solver.
    + apply mk_app_reduce_to_open_fix'; auto. refinement_solver.
Qed.

(* Lemma rR_letapp_overbase_underbase_lam: *)
(*   forall st τ_x e1 (c2: constant) b1 n1 d1 ϕ1 b2 n2 d2 ϕ2, *)
(*   τ_x = [v:b2|n2|d2|ϕ2] -> *)
(*   (∀ e_wf : tm, ({0;b∅;st}⟦[v:b1|n1|d1|ϕ1]⟧) e_wf → ∃ v_wf : value, e_wf ↪* v_wf) -> *)
(*   ({0;b∅;st}⟦[v:b1|n1|d1|ϕ1]⟧) c2 -> *)
(*   ({0;b∅;st}⟦-:{v:b1|n1|d1|ϕ1}⤑ τ_x⟧) (vlam b1 e1) -> *)
(*   ({0;b∅;st}⟦ τ_x ^r^ c2 ⟧) (e1 ^t^ c2). *)
(* Proof. *)
(*   intros; subst. *)
(*   assert (ϕ1 b∅ st c2 /\ [] ⊢t c2 ⋮v b1) as (Hz & HzT). eapply wf_implies_base_phi_sat; eauto. *)
(*   - invclear H2; invclear H1; mydestr. apply H5 in Hz; auto. *)
(*     apply rR_open_trans_empty' in Hz; auto. *)
(*     eapply termR_perserve_rR; eauto. refinement_solver. simpl. *)
(*     apply mk_app_reduce_to_open; auto. basic_typing_solver. *)
(* Qed. *)

Lemma rR_letapp_overbase_lam:
  forall st τ_x e1 (c2: constant) b1 n1 d1 ϕ1,
  (∀ e_wf : tm, ({0;b∅;st}⟦[v:b1|n1|d1|ϕ1]⟧) e_wf → ∃ v_wf : value, e_wf ↪* v_wf) ->
  ({0;b∅;st}⟦[v:b1|n1|d1|ϕ1]⟧) c2 ->
  ({0;b∅;st}⟦-:{v:b1|n1|d1|ϕ1}⤑ τ_x⟧) (vlam b1 e1) ->
  ({0;b∅;st}⟦ τ_x ^r^ c2 ⟧) (e1 ^t^ c2).
Proof.
  intros; subst.
  assert (ϕ1 b∅ st c2 /\ [] ⊢t c2 ⋮v b1) as (Hz & HzT). eapply wf_implies_base_phi_sat; eauto.
  invclear H0; invclear H1; mydestr. apply H4 in Hz; auto.
  apply rR_open_trans_empty' in Hz; auto.
  eapply termR_perserve_rR; eauto. refinement_solver. refinement_solver.
  apply mk_app_reduce_to_open; auto.
  inv_rd_simpl3. inv_rd_simpl1.
Qed.

Lemma rR_letapp_overbase_fix:
  forall st τ_x e1 (c2: constant) b1 n1 d1 ϕ1,
  (∀ e_wf : tm, ({0;b∅;st}⟦[v:b1|n1|d1|ϕ1]⟧) e_wf → ∃ v_wf : value, e_wf ↪* v_wf) ->
  ({0;b∅;st}⟦[v:b1|n1|d1|ϕ1]⟧) c2 ->
  ({0;b∅;st}⟦-:{v:b1|n1|d1|ϕ1}⤑ τ_x⟧) (vfix (b1 ⤍ ⌊ τ_x ⌋ ) (vlam b1 e1)) ->
  ({0;b∅;st}⟦ τ_x ^r^ c2 ⟧) (({1 ~t> (vfix (b1 ⤍ ⌊ τ_x ⌋) (vlam b1 e1))} e1) ^t^ c2).
Proof.
  intros; subst.
  assert (ϕ1 b∅ st c2 /\ [] ⊢t c2 ⋮v b1) as (Hz & HzT). eapply wf_implies_base_phi_sat; eauto.
  invclear H0; invclear H1; mydestr. apply H4 in Hz; auto.
  apply rR_open_trans_empty' in Hz; auto.
  eapply termR_perserve_rR; eauto. refinement_solver. refinement_solver.
  inv_rd_simpl1.
  apply mk_app_reduce_to_open_fix; auto.
Qed.

(* Lemma rR_letapp_overbase_underbase_fix: *)
(*   forall st τ_x e1 (c2: constant) b1 n1 d1 ϕ1 b2 n2 d2 ϕ2, *)
(*   τ_x = [v:b2|n2|d2|ϕ2] -> *)
(*   (∀ e_wf : tm, ({0;b∅;st}⟦[v:b1|n1|d1|ϕ1]⟧) e_wf → ∃ v_wf : value, e_wf ↪* v_wf) -> *)
(*   ({0;b∅;st}⟦[v:b1|n1|d1|ϕ1]⟧) c2 -> *)
(*   ({0;b∅;st}⟦-:{v:b1|n1|d1|ϕ1}⤑ τ_x⟧) (vfix (b1 ⤍ b2) (vlam b1 e1)) -> *)
(*   ({0;b∅;st}⟦ τ_x ^r^ c2 ⟧) (({1 ~t> (vfix (b1 ⤍ b2) (vlam b1 e1))} e1) ^t^ c2). *)
(* Proof. *)
(*   intros; subst. *)
(*   assert (ϕ1 b∅ st c2 /\ [] ⊢t c2 ⋮v b1) as (Hz & HzT). eapply wf_implies_base_phi_sat; eauto. *)
(*   invclear H2; invclear H1; mydestr. apply H5 in Hz; auto. *)
(*       apply rR_open_trans_empty' in Hz; auto. *)
(*       eapply termR_perserve_rR; eauto. refinement_solver. simpl. *)
(*       apply mk_app_reduce_to_open_fix; auto. refinement_solver. *)
(* Qed. *)

Lemma wf_implies_base_phi_sat_v: forall m bst st b1 n1 d1 ϕ1 (v2: value),
  (∀ e_wf : tm, ({m;bst;st}⟦[v:b1|n1|d1|ϕ1]⟧) e_wf → ∃ v_wf : value, e_wf ↪* v_wf) ->
  ({m;bst;st}⟦[v:b1|n1|d1|ϕ1]⟧) v2 ->
  (exists c2: constant, v2 = c2 /\ ϕ1 bst st c2 /\ [] ⊢t c2 ⋮v b1).
Proof.
  intros. assert (exists c2: constant, v2 = c2) by eauto. mydestr; subst.
  eapply wf_implies_base_phi_sat in H0; eauto.
Qed.

Check wf_implies_ctxrR_tlete_ubase.

Lemma wf_implies_ctxrR_tlete_ubase_better_drop
     : ∀ (Γ : list (atom * rty)) (st : state) (x : atom) (b : base_ty)
         (n : nat) (d : aset) (ϕ : refinement) (e_x e : tm) (τ : rty),
    x ∉ stale e ∪ stale τ -> closed_rty 0 (ctxdom ⦑Γ⦒ ∪ dom aset st) τ ->
         wf_ctxrR st (Γ ++ [(x, [v:b|n|d|ϕ])])
         → not_overbasety τ
           → (⅋{st}⟦[v:b|n|d|ϕ]⟧{Γ}) e_x
             → (⅋{st}⟦τ⟧{Γ ++ [(x, [v:b|n|d|ϕ])] }) (e ^t^ x)
               → (⅋{st}⟦τ⟧{Γ}) (tlete e_x e).
Proof.
  intros. rewrite <- (close_open_var_tm e x 0); try fast_set_solver.
  assert ((⅋{st}⟦τ⟧{Γ ++ [(x, [v:b|n|d|ϕ])] }) (tlete e_x (x \t\ (e ^t^ x)))).
  eapply wf_implies_ctxrR_tlete_ubase; eauto.
  eapply inv_implies_ctxrR_drop_last; eauto.
  - fast_set_solver.
  - assert (x ∉ fv_tm e_x). inv_rd_solver1.
    assert (x ∉ fv_tm (x \t\ (e ^t^ x))) by apply close_rm_fv_tm.
    set_solver.
Qed.

Lemma wf_implies_ctxrR_tlete_ubase_better
     : ∀ (Γ : list (atom * rty)) (st : state) (x : atom) (b : base_ty)
         (n : nat) (d : aset) (ϕ : refinement) (e_x e : tm) (τ : rty),
    x ∉ stale e ->
         wf_ctxrR st (Γ ++ [(x, [v:b|n|d|ϕ])])
         → not_overbasety τ
           → (⅋{st}⟦[v:b|n|d|ϕ]⟧{Γ}) e_x
             → (⅋{st}⟦τ⟧{Γ ++ [(x, [v:b|n|d|ϕ])] }) (e ^t^ x)
               → (⅋{st}⟦τ⟧{Γ ++ [(x, [v:b|n|d|ϕ])] }) (tlete e_x e).
Proof.
  intros. rewrite <- (close_open_var_tm e x 0); try fast_set_solver.
  eapply wf_implies_ctxrR_tlete_ubase; eauto.
Qed.

Lemma wf_implies_ctxrR_tlete_is_arr_better:
  ∀ (Γ : list (atom * rty)) (st : state) (x : atom) (τ_x : rty)
    (e_x : value) (e : tm) (τ : rty),
    x ∉ stale e ->
    wf_ctxrR st (Γ ++ [(x, τ_x)])
    → not_overbasety τ
      → is_arr τ_x
        → (⅋{st}⟦τ_x⟧{Γ}) e_x
          → (⅋{st}⟦τ⟧{Γ ++ [(x, τ_x)] }) (e ^t^ x)
          → (⅋{st}⟦τ⟧{Γ ++ [(x, τ_x)] }) (tlete e_x e).
Proof.
  intros. rewrite <- (close_open_var_tm e x 0); try fast_set_solver.
  eapply wf_implies_ctxrR_tlete_is_arr; eauto.
Qed.

Lemma wf_implies_ctxrR_tlete_ubase_better_empty
  : ∀ (st : state) (x : atom) (b : base_ty)
      (n : nat) (d : aset) (ϕ : refinement) (e_x e : tm) (τ : rty),
    x ∉ stale e -> wf_ctxrR st ([(x, [v:b|n|d|ϕ])])
    → not_overbasety τ
    → ({0;b∅;st}⟦[v:b|n|d|ϕ]⟧ e_x)
    → (⅋{st}⟦τ⟧{ [(x, [v:b|n|d|ϕ])] }) (e ^t^ x)
    → (⅋{st}⟦τ⟧{ [(x, [v:b|n|d|ϕ])] }) (tlete e_x e).
Proof.
  intros. assert ((⅋{st}⟦τ⟧{ [] ++ [(x, [v:b|n|d|ϕ])] }) (tlete e_x e)).
  apply wf_implies_ctxrR_tlete_ubase_better; RD_simp2; auto. listctx_set_simpl.
Qed.

Lemma wf_implies_ctxrR_tlete_is_arr_better_empty:
  ∀ (st : state) (x : atom) (τ_x : rty)
    (e_x : value) (e : tm) (τ : rty),
    x ∉ stale e ->
    wf_ctxrR st ([(x, τ_x)])
    → not_overbasety τ
      → is_arr τ_x
        → ({0;b∅;st}⟦τ_x⟧ e_x)
          → (⅋{st}⟦τ⟧{[(x, τ_x)] }) (e ^t^ x)
          → (⅋{st}⟦τ⟧{[(x, τ_x)] }) (tlete e_x e).
Proof.
  intros. assert ((⅋{st}⟦τ⟧{ [] ++ [(x, τ_x)] }) (tlete e_x e)).
  apply wf_implies_ctxrR_tlete_is_arr_better; RD_simp2; auto. listctx_set_simpl.
Qed.

Lemma termR_perserve_inv_ctxrR: forall Γ τ (e e': tm),
    not_overbasety τ ->
    valid_rty τ ->
    e <-<{ ⌊ Γ ⌋* ;  ⌊ τ ⌋ } e' -> (forall st, ⅋{st}⟦τ⟧{Γ} e -> ⅋{st}⟦τ⟧{Γ} e').
Proof.
  induction Γ; intros; invclear H2.
  - constructor. intros. eapply termR_perserve_rR; eauto.
  - constructor; auto. termR_solver.
    intros. apply IHΓ with (e:= ({x := c_x }t e)); eauto. denotation_simp.
    apply termR_tm_subst with (Tx := B); eauto. refinement_solver. denotation_simp3.
    termR_solver.
  - constructor; auto. termR_solver.
    destruct H11 as (e_x_hat & He_x_hat & HH). exists e_x_hat. split; auto.
    intros. auto_under v_x. eapply IHΓ; eauto.
    eapply termR_tm_subst with (Tx := b); eauto. denotation_simp. refinement_solver.
    simpl in H1. termR_solver.
  - constructor; auto. termR_solver. termR_solver. auto_under v_x.
    eapply IHΓ; eauto.
    eapply termR_tm_subst with (Tx := ⌊τ_x⌋); eauto. denotation_simp. refinement_solver.
    simpl in H1. termR_solver.
Qed.

Lemma termR_tletapp_fix: forall Γ e1 (v2: value) e b x1 T,
    Γ ⊢t v2 ⋮v b -> Γ ⊢t vfix (b ⤍ x1) (vlam b e1) ⋮v b ⤍ x1 ->
    (exists (L: aset), ∀ y : atom, y ∉ L → (Γ ++ [(y, x1)]) ⊢t e ^t^ y ⋮t T) ->
  (tlete (({1 ~t> vfix (b ⤍ x1) (vlam b e1)} e1) ^t^ v2) e) <-<{ Γ; T} (tletapp (vfix (b ⤍ x1) (vlam b e1)) v2 e).
Proof.
  intros. constructor; auto.
  - mydestr. econstructor; eauto. apply vfix_implies_open_tyable in H0.
    simpl in H0. apply vlam_implies_open_tyable with (v2:=v2) in H0; auto.
  - mydestr. auto_pose_fv x'. eapply tletapp_typable with (x:=x'); eauto; fast_set_solver.
  - unfold termRraw. intros Γv; intros. reduction_simpl1.
    rewrite letapp_step_spec. repeat split; auto.
    + assert (lc (value_msubst Γv (vfix (b ⤍ x1) (vlam b e1)))).
      eapply instantiation_implies_value_msubst_lc; eauto. basic_typing_solver.
      msubst_simpl; auto.
    + reduction_solver1.
    + apply multi_step_regular in H3; mydestr. lc_solver.
    + setoid_rewrite tm_open_then_msubst_k in H3; eauto.
      setoid_rewrite tm_open_then_msubst_k in H3; eauto.
      right. do 2 eexists; split; eauto.
      rewrite letapp_step_spec. repeat split; auto.
      { assert (body (vlam b e1)). basic_typing_solver.
        assert (body (tm_msubst Γv (vlam b e1))). eapply msubst_preserves_body_tm; eauto.
        msubst_simpl.
        eapply open_lc_tm with (u:= vfix (b ⤍ x1) (vlam b (tm_msubst Γv e1))) in H5.
        msubst_simpl; auto.
        assert (lc (value_msubst Γv (vfix (b ⤍ x1) (vlam b e1)))).
      eapply instantiation_implies_value_msubst_lc; eauto. basic_typing_solver.
      msubst_simpl; auto.
      }
      reduction_solver1.
      apply multi_step_regular in H3; mydestr. lc_solver.
      left. do 2 eexists; split; eauto. msubst_simpl; auto.
Qed.


Lemma termR_tletapp_lam: forall Γ e1 (v2: value) e Tx Ty T,
    Γ ⊢t v2 ⋮v Tx -> Γ ⊢t (vlam Tx e1) ⋮v Tx ⤍ Ty ->
    (exists (L: aset), ∀ y : atom, y ∉ L → (Γ ++ [(y, Ty)]) ⊢t e ^t^ y ⋮t T) ->
    (tlete (e1 ^t^ v2) e) <-<{Γ; T} (tletapp (vlam Tx e1) v2 e).
Proof.
  intros. constructor; auto.
  - mydestr. econstructor; eauto. invclear H0.
    auto_pose_fv x'.
    assert ((Γ ++ [(x', Tx)]) ⊢t e1 ^t^ x' ⋮t Ty). apply H4; fast_set_solver.
    apply basic_typing_subst_tm_pre with (u:=v2) in H2; auto. lc_simpl4.
  - mydestr. auto_pose_fv x'. eapply tletapp_typable with (x:=x'); eauto; fast_set_solver.
  - unfold termRraw. intros Γv; intros. reduction_simpl1.
    rewrite letapp_step_spec. repeat split; auto.
    + assert (lc (value_msubst Γv (vlam Tx e1))).
      eapply instantiation_implies_value_msubst_lc; eauto. basic_typing_solver.
      msubst_simpl; auto.
    + reduction_solver1.
    + apply multi_step_regular in H3; mydestr. lc_solver.
    + left. do 2 eexists; split; eauto.
      setoid_rewrite tm_open_then_msubst_k in H3; eauto.
Qed.

Lemma termR_tletapp_lam_y: forall Γ e1 y (v2: value) e Tx Ty T,
    Γ ⊢t v2 ⋮v Tx -> Γ ⊢t (vlam Tx e1) ⋮v Tx ⤍ Ty -> y # e ->
    (Γ ++ [(y, Ty)]) ⊢t e ^t^ y ⋮t T ->
    (tlete (e1 ^t^ v2) e) <-<{Γ; T} (tletapp (vlam Tx e1) v2 e).
Proof.
  intros. constructor; auto.
  - econstructor; eauto. instantiate (1:=Ty). basic_typing_solver.
    auto_exists_L_intros.
    apply basic_has_type_renaming with (x0:=x) in H2; try listctx_set_solver.
    lc_simpl4. basic_typing_solver.
  - eapply tletapp_typable with (x:=y); eauto; fast_set_solver.
  - unfold termRraw. intros Γv; intros. reduction_simpl1.
    rewrite letapp_step_spec. repeat split; auto.
    + assert (lc (value_msubst Γv (vlam Tx e1))).
      eapply instantiation_implies_value_msubst_lc; eauto. basic_typing_solver.
      msubst_simpl; auto.
    + reduction_solver1.
    + apply multi_step_regular in H4; mydestr. lc_solver.
    + left. do 2 eexists; split; eauto.
      setoid_rewrite tm_open_then_msubst_k in H4; eauto.
Qed.

Lemma letapp_aux_typable: forall st τ x τ_x e y,
    x <> y -> x ∉ stale e ->
    (⅋{st}⟦τ⟧{ [(x, τ_x)] } (e ^t^ x)) -> ([(x, ⌊ τ_x ⌋)] ++ [(y, ⌊ τ_x ⌋)]) ⊢t e ^t^ y ⋮t ⌊τ⌋.
Proof.
  intros.
  apply inv_ctxrR_regular in H1; mydestr. simpl in H1.
  assert (([] ++ [(x, ⌊τ_x⌋)]) ⊢t (e ^t^ x) ⋮t ⌊τ⌋); auto; try listctx_set_solver.
  apply basic_has_type_renaming with (x0:=y) in H5; try listctx_set_solver.
  lc_simpl4. simpl in H5.
  apply basic_typing_weaken_tm_post; auto; listctx_set_solver.
Qed.

Definition tm_to_value (Tx: ty) (e:tm): value := vlam Tx (mk_app e (vbvar 1)).

Lemma tm_to_value_typable: forall Γ e Tx T,
    Γ ⊢t e ⋮t Tx ⤍ T -> Γ ⊢t (tm_to_value Tx e) ⋮v Tx ⤍ T.
Proof.
  unfold tm_to_value. intros.
  auto_exists_L. intros. unfold mk_app.
  apply mk_app_typable with (T1:=Tx); fold _open_tm.
  - lc_simpl. basic_typing_solver.
  - dec_solver1. basic_typing_vfavr_solver.
Qed.

Global Hint Resolve tm_to_value_typable: core.

Lemma mk_app_open: forall e v,
    lc e -> (mk_app e (vbvar 1) ^t^ v) = mk_app e v.
Proof.
  intros. simpl. unfold mk_app. lc_simpl.
Qed.

Lemma termR_tm_to_value: forall Γ e (v: value) Tx T,
    Γ ⊢t e ⋮t Tx ⤍ T -> Γ ⊢t v ⋮v Tx ->
    (mk_app e v) <-<{Γ; T} (tletapp (tm_to_value Tx e) v (vbvar 0)).
Proof.
  intros.
  assert (Γ ⊢t tletapp (tm_to_value Tx e) v (vbvar 0) ⋮t T) as HT.
  { auto_pose_fv x. eapply tletapp_typable with (x:=x); eauto. fast_set_solver.
    simpl. basic_typing_vfavr_solver. }
  constructor; auto.
  - eapply mk_app_typable; eauto.
  - unfold termRraw. intros Γv; intros.
    unfold tm_to_value. reduction_simpl1.
    rewrite letapp_step_spec; repeat split; auto.
    { assert (lc (tm_to_value Tx e)). basic_typing_solver.
      eapply instantiation_implies_value_msubst_lc in H3; eauto.
      unfold tm_to_value in H3. msubst_simpl; auto. }
    reduction_solver1.
    left. do 2 eexists; split; eauto.
    setoid_rewrite <- tm_open_then_msubst_k; eauto.
    rewrite mk_app_open; basic_typing_solver.
    rewrite lete_step_spec. split; auto. exists v0. split; auto. simpl.
    apply multistep_refl. op_solver1.
Qed.

Lemma termR_tm_to_value': forall Γ e (v: value) Tx T,
    Γ ⊢t e ⋮t Tx ⤍ T -> Γ ⊢t v ⋮v Tx ->
    (tletapp (tm_to_value Tx e) v (vbvar 0)) <-<{Γ; T} (mk_app e v).
Proof.
  intros.
  assert (Γ ⊢t tletapp (tm_to_value Tx e) v (vbvar 0) ⋮t T) as HT.
  { auto_pose_fv x. eapply tletapp_typable with (x:=x); eauto. fast_set_solver.
    simpl. basic_typing_vfavr_solver. }
  constructor; auto.
  - eapply mk_app_typable; eauto.
  - unfold termRraw. intros Γv; intros.
    unfold tm_to_value in H2. reduction_simpl1.
    rewrite letapp_step_spec in H2; mydestr. destruct H5; mydestr; invclear H5.
    setoid_rewrite <- tm_open_then_msubst_k in H6; eauto.
    rewrite mk_app_open in H6; basic_typing_solver.
    eapply lete_0_reduce_to_self_aux'; eauto. msubst_simpl; auto.
Qed.

Lemma termR_tm_to_value_mk_app: forall Γ e (v: value) Tx T,
    Γ ⊢t e ⋮t Tx ⤍ T -> Γ ⊢t v ⋮v Tx ->
    (mk_app e v) <-<{Γ; T} (mk_app (tm_to_value Tx e) v).
Proof.
  intros.
  apply termR_trans_better with (tletapp (tm_to_value Tx e) v (vbvar 0)).
  - eapply termR_tm_to_value; auto.
  - eapply mk_app_v_v_reduce_to_letapp'; eauto.
Qed.

Lemma termR_tm_to_value_mk_app': forall Γ e (v: value) Tx T,
    Γ ⊢t e ⋮t Tx ⤍ T -> Γ ⊢t v ⋮v Tx ->
    (mk_app (tm_to_value Tx e) v) <-<{Γ; T} (mk_app e v).
Proof.
  intros.
  apply termR_trans_better with (tletapp (tm_to_value Tx e) v (vbvar 0)).
  - eapply mk_app_v_v_reduce_to_letapp; eauto.
  - eapply termR_tm_to_value'; auto.
Qed.

Definition arg_ty (τ: rty): ty :=
  match τ with
  | -:{v: b | n | d | ϕ}⤑ τ_x => b
  | τ1 ⤑ τ_x => ⌊ τ1 ⌋
  | _ => ⌊ τ ⌋
  end.

Lemma rR_is_arr_refine: forall m bst st τ_x e,
    is_arr τ_x ->
    {m;bst;st}⟦τ_x⟧ e ->
    {m;bst;st}⟦τ_x⟧ (tm_to_value (arg_ty τ_x) e).
Proof.
  intros. destruct τ_x; auto_ty_exfalso.
  - invclear H0; mydestr.
    constructor; auto. denotation_simp. constructor; auto.
    intros. apply H2 in H4; auto.
    eapply termR_perserve_rR; eauto; refinement_solver.
    apply termR_tm_to_value_mk_app; auto.
  - invclear H0; mydestr.
    constructor; auto. denotation_simp. constructor; auto.
    intros. auto_under v_x.
    eapply termR_perserve_rR; eauto; refinement_solver.
    apply termR_tm_to_value_mk_app; auto. refinement_solver.
Qed.

Lemma termR_elete_better
  : ∀ (γ : listctx ty) (Tx T : ty) (e_x e_x' e e' : tm) (x : atom),
    x ∉ stale e ∪ stale e' ->
    e_x <-<{ γ; Tx} e_x'
    → (e ^t^ x) <-<{ γ ++ [(x, Tx)]; T}(e' ^t^ x)
    → (tlete e_x e) <-<{ γ; T} (tlete e_x' e').
Proof.
  intros.
  eapply termR_elete in H1; eauto.
  rewrite close_open_var_tm in H1; try fast_set_solver.
  rewrite close_open_var_tm in H1; try fast_set_solver.
Qed.

Lemma termR_elete_lhs
  : ∀ (γ : listctx ty) (Tx T : ty) (e_x e_x' e : tm) (x : atom),
    x ∉ stale e ->
    e_x <-<{ γ; Tx} e_x' -> (γ ++ [(x, Tx)]) ⊢t (e ^t^ x) ⋮t T
    → (tlete e_x e) <-<{ γ; T} (tlete e_x' e).
Proof.
  intros. eapply termR_elete_better; eauto. fast_set_solver.
Qed.

Lemma rR_letapp_base': forall τ_x τ st x (v1 v2: value) (e: tm) b n d ϕ,
    x ∉ fv_tm e ∪ rty_fv τ ->
    not_overbasety τ -> not_overbasety τ_x ->
    (∀ e_wf : tm, ({0;b∅;st}⟦[v:b|n|d|ϕ]⟧) e_wf → ∃ v_wf : value, e_wf ↪* v_wf) ->
    wf_ctxrR st [(x, τ_x ^r^ v2)] ->
    {0;b∅;st}⟦-:{v: b | n | d | ϕ}⤑ τ_x⟧ v1 ->
    {0;b∅;st}⟦[v:b|n|d|ϕ]⟧ v2 ->
    (⅋{st}⟦τ⟧{[(x, τ_x ^r^ v2)] }) (e ^t^ x) ->
    {0;b∅;st}⟦τ⟧ (tletapp v1 v2 e).
    (* (⅋{st}⟦τ⟧{[(x, τ_x ^r^ v2)] }) (tletapp v1 v2 e). *)
Proof.
  intros.
  assert (exists c2: constant, v2 = c2 /\ ϕ b∅ st c2 /\ [] ⊢t c2 ⋮v b); mydestr; subst.
  { eapply wf_implies_base_phi_sat_v in H2; eauto. }
  destruct (decide (is_arr τ_x)); intros.
  - destruct (rR_arrow_value_lam_exists_oarr _ _ _ _ _ _ _ _ _ H4); mydestr; subst.
    + rename x1 into e1.
      assert (({0;b∅;st}⟦τ_x ^r^ x0⟧) (e1 ^t^ x0)) as Hz.
      { eapply rR_letapp_overbase_lam in H4; eauto. }
      invclear H6; try auto_ty_exfalso5.
      simpl in H4; mydestr.
      constructor; auto. admit. admit. admit.
      apply rR_is_arr_refine in Hz; auto.
      apply wf_implies_ctxrR_tlete_is_arr_better_empty with (x:=x) (e:=e) (τ:=τ) in Hz; eauto; try fast_set_solver.
      eapply termR_perserve_inv_ctxrR; eauto.
      { invclear Hz; refinement_solver. }
      simpl. inv_rd_simpl1.
      apply termR_trans_better with ((tlete (e1 ^t^ x0) e)).
      { apply termR_elete_lhs with (x:=x) (Tx := ⌊τ_x⌋). fast_set_solver.

        eapply termR_elete. }
      admit.
      apply termR_tletapp_lam with (Ty := ⌊τ_x ⌋ ); eauto.
      basic_typing_solver. refinement_solver. basic_typing_solver.
      auto_exists_L. intros.
      eapply letapp_aux_typable in H6; inv_rd_simpl1; eauto.
    + admit.
  - destruct τ_x; try auto_ty_exfalso. invclear H1.
    destruct (rR_arrow_value_lam_exists_oarr _ _ _ _ _ _ _ _ _ H4); mydestr; subst.
    + rename x5 into e1.
      assert (({0;b∅;st}⟦[v:x1|x2|x3|x4] ^r^ x0⟧) (e1 ^t^ x0)) as Hz.
      { eapply rR_letapp_overbase_lam in H4; eauto. }
      apply wf_implies_ctxrR_tlete_ubase_better_empty with (x:=x) (e:=e) (τ:=τ) in Hz; eauto; try fast_set_solver.
      eapply termR_perserve_inv_ctxrR; eauto.
      { invclear Hz; refinement_solver. }
      simpl. apply termR_tletapp_lam with (Ty := x1); eauto.
      basic_typing_solver. refinement_solver. basic_typing_solver.
      auto_exists_L. intros.
      eapply letapp_aux_typable in H6; eauto; fast_set_solver.
    + rename x5 into e1.
      assert (({0;b∅;st}⟦[v:x1|x2|x3|x4] ^r^ x0⟧) (({1 ~t> vfix (b ⤍ x1) (vlam b e1)} e1) ^t^ x0)) as Hz.
      { eapply rR_letapp_overbase_fix in H4; eauto. }
      apply wf_implies_ctxrR_tlete_ubase_better_empty with (x:=x) (e:=e) (τ:=τ) in Hz; eauto; try fast_set_solver.
      eapply termR_perserve_inv_ctxrR; eauto.
      { invclear Hz; refinement_solver. }
      simpl. apply termR_tletapp_fix; auto.
      basic_typing_solver. refinement_solver. basic_typing_solver.
      auto_exists_L. intros.
      eapply letapp_aux_typable in H6; eauto; fast_set_solver.
Admitted.




Lemma rR_letapp_base: forall τ st τ_x (v1 v2 v_x: value) (e: tm) b n d ϕ,
  not_overbasety τ -> not_overbasety τ_x ->
  (∀ e_wf : tm, ({0;b∅;st}⟦[v:b|n|d|ϕ]⟧) e_wf → ∃ v_wf : value, e_wf ↪* v_wf) ->
  (∀ e_wf : tm, ({0;b∅;st}⟦τ_x ^r^ v2⟧) e_wf → ∃ v_wf : value, e_wf ↪* v_wf) ->
  ({0;b∅;st}⟦[v:b|n|d|ϕ]⟧) v2 ->
  ({0;b∅;st}⟦-:{v: b | n | d | ϕ}⤑ τ_x⟧) v1 ->
  ({0;b∅;st}⟦τ_x ^r^ v2⟧) v_x ->
  ({0;b∅;st}⟦τ⟧) (e ^t^ v_x) ->
  ({0;b∅;st}⟦τ⟧) (tletapp v1 v2 e).
Proof.
  intros. apply termR_perserve_rR with (e ^t^ v_x); auto; refinement_solver.
  constructor; refinement_solver.
  - inv_rd_simpl1. admit.
  - unfold termRraw. intros. instantiation_simp.
    simpl in H4. mydestr.
    assert (exists (c2: constant), v2 = c2); mydestr; subst. { admit. }
    assert (ϕ b∅ st x). admit.
    apply H9 in H10; refinement_solver.
    apply rR_open_trans_empty' in H10.
Admitted.

Lemma rR_letapp_base_lam: forall τ st τ_x x e1 (v2: value) (e: tm) b n d ϕ,
    x ∉ fv_tm e ∪ rty_fv τ ->
    not_overbasety τ -> not_overbasety τ_x ->
    (∀ e_wf : tm, ({0;b∅;st}⟦[v:b|n|d|ϕ]⟧) e_wf → ∃ v_wf : value, e_wf ↪* v_wf) ->
    (∀ e_wf : tm, ({0;b∅;st}⟦τ_x ^r^ v2⟧) e_wf → ∃ v_wf : value, e_wf ↪* v_wf) ->
    {0;b∅;st}⟦-:{v: b | n | d | ϕ}⤑ τ_x⟧ (vlam b e1) ->
    {0;b∅;st}⟦[v:b|n|d|ϕ]⟧ v2 ->
    {0;b∅;st}⟦τ_x ^r^ v2⟧ (e1 ^t^ v2).
Admitted.

Lemma rR_letapp_base_fun: forall τ st τ_x x (v1 v2: value) (e: tm) b n d ϕ,
    x ∉ fv_tm e ∪ rty_fv τ ->
    not_overbasety τ -> not_overbasety τ_x ->
    (∀ e_wf : tm, ({0;b∅;st}⟦[v:b|n|d|ϕ]⟧) e_wf → ∃ v_wf : value, e_wf ↪* v_wf) ->
    (∀ e_wf : tm, ({0;b∅;st}⟦τ_x ^r^ v2⟧) e_wf → ∃ v_wf : value, e_wf ↪* v_wf) ->
    {0;b∅;st}⟦-:{v: b | n | d | ϕ}⤑ τ_x⟧ v1 ->
    {0;b∅;st}⟦[v:b|n|d|ϕ]⟧ v2 ->
    (exists v_x: value, {0;b∅;st}⟦τ_x ^r^ v2⟧ v_x /\ v_x <-<{ []; b ⤍ ⌊ τ_x ⌋} (mk_app v1 v2)).
Admitted.

Lemma rR_letapp_base_app_x_arr: forall τ st τ_x x (v1 v2: value) (v_x: value) (e: tm),
    x ∉ fv_tm e ∪ rty_fv τ ->
    not_overbasety τ -> is_arr τ_x ->
    {0;b∅;st}⟦τ_x⟧ v_x ->
    v_x <-<{ []; b ⤍ ⌊ τ_x ⌋} (mk_app v1 v2) ->
    (⅋{st}⟦τ⟧{[(x, τ_x)] }) (e ^t^ x) ->
    {0;b∅;st}⟦τ⟧ (tletapp v1 v2 e).
Proof.

Lemma rR_letapp_base': forall τ st τ_x x (v1 v2: value) (e: tm) b n d ϕ,
    x ∉ fv_tm e ∪ rty_fv τ ->
    not_overbasety τ -> not_overbasety τ_x ->
    (∀ e_wf : tm, ({0;b∅;st}⟦[v:b|n|d|ϕ]⟧) e_wf → ∃ v_wf : value, e_wf ↪* v_wf) ->
    (∀ e_wf : tm, ({0;b∅;st}⟦τ_x ^r^ v2⟧) e_wf → ∃ v_wf : value, e_wf ↪* v_wf) ->
    {0;b∅;st}⟦-:{v: b | n | d | ϕ}⤑ τ_x⟧ v1 ->
    {0;b∅;st}⟦[v:b|n|d|ϕ]⟧ v2 ->
    (⅋{st}⟦τ⟧{[(x, τ_x ^r^ v2)] }) (e ^t^ x) ->
    (⅋{st}⟦τ⟧{[(x, τ_x ^r^ v2)] }) (tletapp v1 v2 e).
Proof.
  intros. eapply rR_letapp_base_fun in H4; eauto.
  mydestr. invclear H6. admit. constructor; auto. admit.
  exists x0. split; auto. admit. intros.

Lemma rR_letapp_base': forall τ st τ_x x e1 (v2: value) (e: tm) b n d ϕ,
    x ∉ fv_tm e ∪ rty_fv τ ->
    not_overbasety τ -> not_overbasety τ_x ->
    (∀ e_wf : tm, ({0;b∅;st}⟦[v:b|n|d|ϕ]⟧) e_wf → ∃ v_wf : value, e_wf ↪* v_wf) ->
    (∀ e_wf : tm, ({0;b∅;st}⟦τ_x ^r^ v2⟧) e_wf → ∃ v_wf : value, e_wf ↪* v_wf) ->
    {0;b∅;st}⟦-:{v: b | n | d | ϕ}⤑ τ_x⟧ (vlam b e1) ->
    {0;b∅;st}⟦[v:b|n|d|ϕ]⟧ v2 ->
    (⅋{st}⟦τ⟧{[(x, τ_x ^r^ v2)] }) (e ^t^ x) ->
    (⅋{st}⟦τ⟧{[(x, τ_x ^r^ v2)] }) (tletapp (vlam b e1) v2 e).
Proof.
  intros. eapply rR_letapp_base_fun in H4; eauto.
  assert ((⅋{st}⟦τ⟧{[] ++ [(x, τ_x ^r^ v2)] }) (tlete (e1 ^t^ v2) (x \t\ (e ^t^ x)))).
  Check wf_implies_ctxrR_tlete_is_arr.
  apply wf_implies_ctxrR_tlete_is_arr.

  invclear H6; try auto_ty_exfalso5. admit. (e1^v2)

  assert
    ((∃ e : tm, v1 = vlam b e) ∨ (∃ e : tm, v1 = vfix (b ⤍ ⌊ τ_x ⌋) (vlam b e))).
  eapply rR_arrow_value_lam_exists_oarr; eauto. destruct H7; mydestr; subst.



  apply termR_perserve_rR with (e ^t^ v_x); auto; refinement_solver.
  constructor; refinement_solver.
  - inv_rd_simpl1. admit.
  - unfold termRraw. intros. instantiation_simp.
    simpl in H4. mydestr.
    assert (exists (c2: constant), v2 = c2); mydestr; subst. { admit. }
    assert (ϕ bst st x). admit.
    apply H9 in H10; refinement_solver.
Admitted.

Lemma inv_rRctx_letapp_oarr: forall Γ st (τ: rty) b n d ϕ x τ_x (v1 v2: value) e,
    x ∉ fv_tm e ∪ rty_fv τ -> not_overbasety τ -> not_overbasety τ_x ->
    closed_rty 0 (ctxdom ⦑Γ⦒ ∪ dom aset st) τ ->
    wf_ctxrR st (Γ ++ [(x, [v:b|n|d|ϕ])]) ->
    wf_ctxrR st (Γ ++ [(x, τ_x ^r^ v2)]) ->
    (⅋{st}⟦-:{v: b | n | d | ϕ}⤑ τ_x⟧{Γ}) v1 ->
    (⅋{st}⟦[v:b|n|d|ϕ]⟧{Γ}) v2 ->
    (⅋{st}⟦τ⟧{Γ ++ [(x, τ_x ^r^ v2)] }) (e ^t^ x) ->
    ⅋{st}⟦τ⟧{Γ ++ [(x, τ_x ^r^ v2)] } (tletapp v1 v2 e).
Proof.
    induction Γ; intros.
    - mydestr. inv_rd_simpl1.
      invclear H5. destruct (decide (is_arr τ_x)).
      + constructor; auto. wf_regular_solver. denotation_simp.
        inv_rd_simpl2. inv_rd_simpl1. inv_rd_simpl3. inv_rd_simpl1.
        apply basic_typing_weaken_tm_empty; eauto. listctx_set_solver.
        eapply tletapp_typable with (x:=x) (Γ:=[]); eauto; basic_typing_solver.
        intros. constructor; auto. RD_simp2; try auto_ty_exfalso5. clear H25.
    rewrite subst_fresh_tm. lc_simpl4.
    assert (({0;b∅;st}⟦τ_x ^r^ v2⟧) (v1 ) )

    specialize (H19 _ H8).  RD_simp2. lc_simpl4. simpl.
        rewrite subst_fresh_value. rewrite subst_fresh_value.
        rewrite subst_fresh_tm.
        admit. fast_set_solver.
        
      rewrite open_subst_same_tm in H6; auto. admit. fast_set_solver. try clear Htmp

        admit.
      + auto_ty_exfalso. RD_simp2. mydestr. constructor; auto.
        admit. exists x5. split; auto. intros. auto_under v_x. RD_simp2. dsimp1. admit.
    - RD_simp2.
      + constructor; auto. admit. intros. auto_under c_x.
        assert ((⅋{<[a:=c_x]> st}⟦τ⟧{Γ ++ [(x, τ_x ^r^ ({a := c_x }v v2))] })
                  (tletapp ({a := c_x }v v1) ({a := c_x }v v2) ({a := c_x }t e))).
        eapply IHΓ; eauto. admit. admit. admit. admit. admit.
      + constructor; auto. admit.
        mydestr. auto_meet_exists Hmeet. auto_meet_reduce. auto_under v_x.
        admit.
      + constructor; auto. denotation_simp. admit.
        intros. auto_under v_x.


    + denotation_simp. assert ([] ⊢t tletbiop op v1 v2 e ⋮t ⌊τ⌋). eapply tletbiop_typable; refinement_solver.
      basic_typing_solver.
    + assert ((exists (m1: nat), v1 = m1)) by (eapply fst_ty_of_op_implies_nat; eauto). mydestr; subst. rename x2 into v1.
      assert ((exists (m2: nat), v2 = m2)) by (eapply snd_ty_of_op_implies_nat; eauto). mydestr; subst. rename x2 into v2.
      exists (eval_op_under_bound_tm op v1 v2). split. apply rR_eval_op_under_bound_tm.
      intros. assert (x1 ↪* v_x) as Hz by (eapply rR_eval_op_under_bound_tm_min; eauto).
      auto_under v_x. RD_simp2. simpl.
      rewrite subst_fresh_tm; auto. rewrite open_subst_same_tm in H10; auto.
      eapply termR_perserve_rR; refinement_solver.
      eapply letapp_eval_op_under_bound_tm_termR; eauto; refinement_solver.


