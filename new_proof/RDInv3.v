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

Lemma is_arr_open_trans: forall τ_x k (v2: value),
  is_arr τ_x -> is_arr ({k ~r> v2} τ_x).
Proof.
  intros. destruct τ_x; auto.
Qed.

Global Hint Resolve is_arr_open_trans: core.

Ltac wf_regular_solver :=
  match goal with
  | [H: wf_ctxrR ?st ?Γ |- ok_dctx _ ?Γ] => apply wf_ctxrR_regular1 in H; auto
  end.

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
  | [H: is_arr ?τ_x, H': {v:_|_|_|_} = {?k ~r> ?v2} ?τ_x |- _ ] =>
      assert (is_arr ({k ~r> v2} τ_x)) as Htmp by auto;
      rewrite <- H' in Htmp; auto_ty_exfalso2
  | [H: is_arr ?τ_x, H': [v:_|_|_|_] = {?k ~r> ?v2} ?τ_x |- _ ] =>
      assert (is_arr ({k ~r> v2} τ_x)) as Htmp by auto;
      rewrite <- H' in Htmp; auto_ty_exfalso2
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

Ltac reduction_simpl0 :=
  repeat match goal with
  | [H: tvalue _ ↪* tvalue _ |- _ ] =>
      rewrite value_reduce_to_value_implies_same in H; mydestr; subst
  end.

Lemma rR_letapp_overbase_underbase:
  forall st τ_x (v1: value) (c2: constant) b1 n1 d1 ϕ1 b2 n2 d2 ϕ2,
  τ_x = [v:b2|n2|d2|ϕ2] ->
  (∀ e_wf : tm, ({0;b∅;st}⟦[v:b1|n1|d1|ϕ1]⟧) e_wf → ∃ v_wf : value, e_wf ↪* v_wf) ->
  ({0;b∅;st}⟦[v:b1|n1|d1|ϕ1]⟧) c2 ->
  ({0;b∅;st}⟦-:{v:b1|n1|d1|ϕ1}⤑ τ_x⟧) v1 ->
  (exists e1, ({0;b∅;st}⟦ τ_x ^r^ c2 ⟧) e1 /\ e1 <-<{ []; b2} (mk_app v1 c2)).
Proof.
  intros; subst.
  assert (ϕ1 b∅ st c2 /\ [] ⊢t c2 ⋮v b1) as (Hz & HzT). eapply wf_implies_base_phi_sat; eauto.
  destruct (rR_arrow_value_lam_exists_oarr _ _ _ _ _ _ _ _ _ H2); mydestr; subst.
  - rename x into e1. exists (e1 ^t^ c2). split.
    + invclear H2; invclear H1; mydestr. reduction_simpl0.
      apply H6 in Hz; auto.
      apply rR_open_trans_empty' in Hz; auto.
      eapply termR_perserve_rR; eauto. refinement_solver. simpl.
      apply mk_app_reduce_to_open; auto. basic_typing_solver.
    + apply mk_app_reduce_to_open'; auto. refinement_solver.
  - rename x into e1. exists (({1 ~t> c2} e1) ^t^ (vfix (b1 ⤍ b2) (vlam b1 e1))). split.
    + invclear H2; invclear H1; mydestr. reduction_simpl0. apply H6 in Hz; auto.
      apply rR_open_trans_empty' in Hz; auto.
      eapply termR_perserve_rR; eauto. refinement_solver. simpl.
      apply mk_app_reduce_to_open_fix; auto. refinement_solver.
    + apply mk_app_reduce_to_open_fix'; auto. refinement_solver.
Qed.

Lemma rR_letapp_overbase_lam:
  forall st τ_x e1 (c2: constant) b1 n1 d1 ϕ1,
  (∀ e_wf : tm, ({0;b∅;st}⟦[v:b1|n1|d1|ϕ1]⟧) e_wf → ∃ v_wf : value, e_wf ↪* v_wf) ->
  ({0;b∅;st}⟦[v:b1|n1|d1|ϕ1]⟧) c2 ->
  ({0;b∅;st}⟦-:{v:b1|n1|d1|ϕ1}⤑ τ_x⟧) (vlam b1 e1) ->
  ({0;b∅;st}⟦ τ_x ^r^ c2 ⟧) (e1 ^t^ c2).
Proof.
  intros; subst.
  assert (ϕ1 b∅ st c2 /\ [] ⊢t c2 ⋮v b1) as (Hz & HzT). eapply wf_implies_base_phi_sat; eauto.
  invclear H0; invclear H1; mydestr. reduction_simpl0. apply H6 in Hz; auto.
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
  ({0;b∅;st}⟦ τ_x ^r^ c2 ⟧) (({1 ~t> c2} e1) ^t^ (vfix (b1 ⤍ ⌊ τ_x ⌋) (vlam b1 e1))).
Proof.
  intros; subst.
  assert (ϕ1 b∅ st c2 /\ [] ⊢t c2 ⋮v b1) as (Hz & HzT). eapply wf_implies_base_phi_sat; eauto.
  invclear H0; invclear H1; mydestr. reduction_simpl0. apply H6 in Hz; auto.
  apply rR_open_trans_empty' in Hz; auto.
  eapply termR_perserve_rR; eauto. refinement_solver. refinement_solver.
  inv_rd_simpl1.
  apply mk_app_reduce_to_open_fix; auto.
Qed.

Lemma wf_implies_base_phi_sat_v: forall m bst st b1 n1 d1 ϕ1 (v2: value),
  (∀ e_wf : tm, ({m;bst;st}⟦[v:b1|n1|d1|ϕ1]⟧) e_wf → ∃ v_wf : value, e_wf ↪* v_wf) ->
  ({m;bst;st}⟦[v:b1|n1|d1|ϕ1]⟧) v2 ->
  (exists c2: constant, v2 = c2 /\ ϕ1 bst st c2 /\ [] ⊢t c2 ⋮v b1).
Proof.
  intros. assert (exists c2: constant, v2 = c2) by eauto. mydestr; subst.
  eapply wf_implies_base_phi_sat in H0; eauto.
Qed.

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

(* Definition tm_to_value (Tx: ty) (e:tm): value := vlam Tx (mk_app e (vbvar 1)). *)

(* Lemma tm_to_value_typable: forall Γ e Tx T, *)
(*     Γ ⊢t e ⋮t Tx ⤍ T -> Γ ⊢t (tm_to_value Tx e) ⋮v Tx ⤍ T. *)
(* Proof. *)
(*   unfold tm_to_value. intros. *)
(*   auto_exists_L. intros. unfold mk_app. *)
(*   apply mk_app_typable with (T1:=Tx); fold _open_tm. *)
(*   - lc_simpl. basic_typing_solver. *)
(*   - dec_solver1. basic_typing_vfavr_solver. *)
(* Qed. *)

(* Global Hint Resolve tm_to_value_typable: core. *)

(* Lemma termR_tm_to_value: forall Γ e (v: value) Tx T, *)
(*     Γ ⊢t e ⋮t Tx ⤍ T -> Γ ⊢t v ⋮v Tx -> *)
(*     (mk_app e v) <-<{Γ; T} (tletapp (tm_to_value Tx e) v (vbvar 0)). *)
(* Proof. *)
(*   intros. *)
(*   assert (Γ ⊢t tletapp (tm_to_value Tx e) v (vbvar 0) ⋮t T) as HT. *)
(*   { auto_pose_fv x. eapply tletapp_typable with (x:=x); eauto. fast_set_solver. *)
(*     simpl. basic_typing_vfavr_solver. } *)
(*   constructor; auto. *)
(*   - eapply mk_app_typable; eauto. *)
(*   - unfold termRraw. intros Γv; intros. *)
(*     unfold tm_to_value. reduction_simpl1. *)
(*     rewrite letapp_step_spec; repeat split; auto. *)
(*     { assert (lc (tm_to_value Tx e)). basic_typing_solver. *)
(*       eapply instantiation_implies_value_msubst_lc in H3; eauto. *)
(*       unfold tm_to_value in H3. msubst_simpl; auto. } *)
(*     reduction_solver1. *)
(*     left. do 2 eexists; split; eauto. *)
(*     setoid_rewrite <- tm_open_then_msubst_k; eauto. *)
(*     rewrite mk_app_open; basic_typing_solver. *)
(*     rewrite lete_step_spec. split; auto. exists v0. split; auto. simpl. *)
(*     apply multistep_refl. op_solver1. *)
(* Qed. *)

(* Lemma termR_tm_to_value': forall Γ e (v: value) Tx T, *)
(*     Γ ⊢t e ⋮t Tx ⤍ T -> Γ ⊢t v ⋮v Tx -> *)
(*     (tletapp (tm_to_value Tx e) v (vbvar 0)) <-<{Γ; T} (mk_app e v). *)
(* Proof. *)
(*   intros. *)
(*   assert (Γ ⊢t tletapp (tm_to_value Tx e) v (vbvar 0) ⋮t T) as HT. *)
(*   { auto_pose_fv x. eapply tletapp_typable with (x:=x); eauto. fast_set_solver. *)
(*     simpl. basic_typing_vfavr_solver. } *)
(*   constructor; auto. *)
(*   - eapply mk_app_typable; eauto. *)
(*   - unfold termRraw. intros Γv; intros. *)
(*     unfold tm_to_value in H2. reduction_simpl1. *)
(*     rewrite letapp_step_spec in H2; mydestr. destruct H5; mydestr; invclear H5. *)
(*     setoid_rewrite <- tm_open_then_msubst_k in H6; eauto. *)
(*     rewrite mk_app_open in H6; basic_typing_solver. *)
(*     eapply lete_0_reduce_to_self_aux'; eauto. msubst_simpl; auto. *)
(* Qed. *)

(* Lemma termR_tm_to_value_mk_app: forall Γ e (v: value) Tx T, *)
(*     Γ ⊢t e ⋮t Tx ⤍ T -> Γ ⊢t v ⋮v Tx -> *)
(*     (mk_app e v) <-<{Γ; T} (mk_app (tm_to_value Tx e) v). *)
(* Proof. *)
(*   intros. *)
(*   apply termR_trans_better with (tletapp (tm_to_value Tx e) v (vbvar 0)). *)
(*   - eapply termR_tm_to_value; auto. *)
(*   - eapply mk_app_v_v_reduce_to_letapp'; eauto. *)
(* Qed. *)

(* Lemma termR_tm_to_value_mk_app': forall Γ e (v: value) Tx T, *)
(*     Γ ⊢t e ⋮t Tx ⤍ T -> Γ ⊢t v ⋮v Tx -> *)
(*     (mk_app (tm_to_value Tx e) v) <-<{Γ; T} (mk_app e v). *)
(* Proof. *)
(*   intros. *)
(*   apply termR_trans_better with (tletapp (tm_to_value Tx e) v (vbvar 0)). *)
(*   - eapply mk_app_v_v_reduce_to_letapp; eauto. *)
(*   - eapply termR_tm_to_value'; auto. *)
(* Qed. *)

Definition arg_ty (τ: rty): ty :=
  match τ with
  | -:{v: b | n | d | ϕ}⤑ τ_x => b
  | τ1 ⤑ τ_x => ⌊ τ1 ⌋
  | _ => ⌊ τ ⌋
  end.

(* Lemma rR_is_arr_refine: forall m bst st τ_x e, *)
(*     is_arr τ_x -> *)
(*     {m;bst;st}⟦τ_x⟧ e -> *)
(*     {m;bst;st}⟦τ_x⟧ (tm_to_value (arg_ty τ_x) e). *)
(* Proof. *)
(*   intros. destruct τ_x; auto_ty_exfalso. *)
(*   - invclear H0; mydestr. *)
(*     constructor; auto. denotation_simp. constructor; auto. *)
(*     intros. apply H2 in H4; auto. *)
(*     eapply termR_perserve_rR; eauto; refinement_solver. *)
(*     apply termR_tm_to_value_mk_app; auto. *)
(*   - invclear H0; mydestr. *)
(*     constructor; auto. denotation_simp. constructor; auto. *)
(*     eexists (tm_to_value (arg_ty (τ_x1 ⤑ τ_x2)) e). split; auto. admit. *)
(*     intros. auto_under v_x. *)
(*     eapply termR_perserve_rR; eauto; refinement_solver. *)
(*     apply termR_tm_to_value_mk_app; auto. refinement_solver. *)
(* Qed. *)

Lemma rR_arr_halt: forall m bst st τ e,
    is_arr τ -> {m;bst;st}⟦τ⟧ e -> (exists (v: value), {m;bst;st}⟦τ⟧ v /\ e ↪* v).
Proof.
  intros. destruct τ; try auto_ty_exfalso.
  - simpl in H0; mydestr. exists x; split; auto.
    constructor; auto. reduction_solver1. split; auto.
    eexists; split; eauto. reduction_solver1.
  - simpl in H0; mydestr. exists x; split; auto.
    constructor; auto. reduction_solver1. split; auto.
    eexists; split; eauto. reduction_solver1.
Qed.

Lemma rR_letapp_base': forall τ_x τ st x (v1 v2: value) (e: tm) b n d ϕ,
    x ∉ fv_tm e ∪ rty_fv τ ->
    not_overbasety τ -> not_overbasety τ_x ->
    (∀ e_wf : tm, ({0;b∅;st}⟦[v:b|n|d|ϕ]⟧) e_wf → ∃ v_wf : value, e_wf ↪* v_wf) ->
    wf_ctxrR st [(x, τ_x ^r^ v2)] ->
    {0;b∅;st}⟦-:{v: b | n | d | ϕ}⤑ τ_x⟧ v1 ->
    {0;b∅;st}⟦[v:b|n|d|ϕ]⟧ v2 ->
    (⅋{st}⟦τ⟧{[(x, τ_x ^r^ v2)] }) (e ^t^ x) ->
    (* {0;b∅;st}⟦τ⟧ (tletapp v1 v2 e). *)
    (⅋{st}⟦τ⟧{[(x, τ_x ^r^ v2)] }) (tletapp v1 v2 e).
Proof.
  intros.
  assert (exists c2: constant, v2 = c2 /\ ϕ b∅ st c2 /\ [] ⊢t c2 ⋮v b); mydestr; subst.
  { eapply wf_implies_base_phi_sat_v in H2; eauto. }
  destruct (decide (is_arr τ_x)); intros.
  - destruct (rR_arrow_value_lam_exists_oarr _ _ _ _ _ _ _ _ _ H4); mydestr; subst.
    + rename x1 into e1.
      assert (({0;b∅;st}⟦τ_x ^r^ x0⟧) (e1 ^t^ x0)) as Hz.
      { eapply rR_letapp_overbase_lam in H4; eauto. }
      assert (is_arr (τ_x ^r^ x0)) as Hzz; auto.
      pose (rR_arr_halt _ _ _ _ _ Hzz Hz). mydestr.
      apply wf_implies_ctxrR_tlete_is_arr_better_empty with (x:=x) (e:=e) (τ:=τ) in H7; eauto; try fast_set_solver.
      eapply termR_perserve_inv_ctxrR; eauto.
      { invclear H7; refinement_solver. }
      apply termR_trans_better with (tlete (e1 ^t^ x0) e).
      simpl. apply termR_weakening. listctx_set_solver.
      eapply termR_elete_lhs with (x:=x); eauto; try inv_rd_simpl1.
      apply termR_value_tm; auto; refinement_solver.
      simpl. apply termR_tletapp_lam with (Ty := ⌊ τ_x ⌋); auto; inv_rd_simpl1.
      basic_typing_solver. refinement_solver. basic_typing_solver.
      auto_exists_L. intros.
      eapply letapp_aux_typable in H6; eauto; inv_rd_simpl1.
    + rename x1 into e1.
      assert (({0;b∅;st}⟦τ_x ^r^ x0⟧) (({1 ~t> x0} e1) ^t^ (vfix (b ⤍ ⌊τ_x⌋) (vlam b e1)))) as Hz.
      { eapply rR_letapp_overbase_fix in H4; eauto. }
      assert (is_arr (τ_x ^r^ x0)) as Hzz; auto.
      pose (rR_arr_halt _ _ _ _ _ Hzz Hz). mydestr.
      apply wf_implies_ctxrR_tlete_is_arr_better_empty with (x:=x) (e:=e) (τ:=τ) in H7; eauto; try fast_set_solver.
      eapply termR_perserve_inv_ctxrR; eauto.
      { invclear H7; refinement_solver. }
      apply termR_trans_better with (tlete (({1 ~t> x0} e1) ^t^ (vfix (b ⤍ ⌊τ_x⌋) (vlam b e1))) e).
      apply termR_weakening. listctx_set_solver.
      eapply termR_elete_lhs with (x:=x); eauto; try inv_rd_simpl1.
      apply termR_value_tm; auto; refinement_solver.
      simpl. apply termR_tletapp_fix; auto.
      basic_typing_solver. refinement_solver. basic_typing_solver.
      auto_exists_L. intros.
      eapply letapp_aux_typable in H6; eauto; inv_rd_simpl1.
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
      assert (({0;b∅;st}⟦[v:x1|x2|x3|x4] ^r^ x0⟧) (({1 ~t> x0} e1) ^t^ (vfix (b ⤍ x1) (vlam b e1)))) as Hz.
      { eapply rR_letapp_overbase_fix in H4; eauto. }
      apply wf_implies_ctxrR_tlete_ubase_better_empty with (x:=x) (e:=e) (τ:=τ) in Hz; eauto; try fast_set_solver.
      eapply termR_perserve_inv_ctxrR; eauto.
      { invclear Hz; refinement_solver. }
      simpl. apply termR_tletapp_fix; auto.
      basic_typing_solver. refinement_solver. basic_typing_solver.
      auto_exists_L. intros.
      eapply letapp_aux_typable in H6; eauto; fast_set_solver.
Qed.
