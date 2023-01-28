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
From CT Require Import RDInv3.
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
Import RDInv3.

Global Hint Resolve mk_eq_constant_is_not_overbasety: core.
Global Hint Resolve mk_eq_var_is_not_overbasety: core.
Global Hint Resolve is_arr_implies_not_overbasety: core.
Global Hint Constructors ok_dctx: core.
Global Hint Resolve rR_implies_no_free: core.
Global Hint Resolve ctxrR_tlete_drop_halt_lhs: core.
Global Hint Resolve rR_implies_reduction_no_free: core.
Global Hint Resolve is_arr_implies_not_overbasety: core.
Global Hint Resolve under_not_overbasety: core.

Lemma is_arr_open_trans': forall τ_x k (v2: value),
  is_arr ({k ~r> v2} τ_x) -> is_arr τ_x.
Proof.
  intros. destruct τ_x; auto.
Qed.

Global Hint Resolve is_arr_open_trans': core.

Ltac auto_ty_exfalso6:=
  match goal with
  | [H: is_arr ?τ, H': ~ is_arr ({?k ~r> ?v} ?τ) |- _ ] =>
      assert (is_arr ({k ~r> v} τ)) by auto; exfalso; auto
  | [H: ~ is_arr ?τ, H': is_arr ({?k ~r> ?v} ?τ) |- _ ] =>
      assert (is_arr τ) by eauto; exfalso; auto
  end || auto_ty_exfalso5.

Lemma state_subst_implies_rty_subst_ok_dctx_all_v_to_c
  : ∀ (a: atom) (Γ : list (atom * rty)) (L : aset) (c_x : constant) (x : atom)
      (k : nat) τ,
    a ∉ stale Γ ∪ {[x]} ->
    closed_rty 0 (ctxdom ⦑ Γ ⦒ ∪ L) ({k ~r> c_x} τ) ->
    ok_dctx L (Γ ++ [(x, {k ~r> a} τ)]) ->
    ok_dctx L (Γ ++ [(x, {k ~r> c_x} τ)]).
Proof.
  intros a.
  induction Γ; intros.
  - denotation_simp.
    destruct (decide (is_arr τ)); invclear H1; try auto_ty_exfalso6.
    + apply ok_dctx_cons_arr; auto; refinement_solver.
    + constructor; auto; refinement_solver.
  - denotation_simp. invclear H1.
    + constructor; auto. listctx_set_solver. apply IHΓ; auto. fast_set_solver.
      dec_solver2. refinement_solver.
    + apply ok_dctx_cons_arr; auto; refinement_solver. apply IHΓ; auto. fast_set_solver.
      dec_solver2.
Qed.

Lemma state_subst_implies_rty_subst_ok_dctx_all_c_to_v
  : ∀ (a: atom) (Γ : list (atom * rty)) (L : aset) (c_x : constant) (x : atom)
      (k : nat) τ,
    a ∉ stale Γ ∪ {[x]} ->
    closed_rty 0 (ctxdom ⦑ Γ ⦒ ∪ L) ({k ~r> a} τ) ->
    ok_dctx L (Γ ++ [(x, {k ~r> c_x} τ)]) ->
    ok_dctx L (Γ ++ [(x, {k ~r> a} τ)]).
Proof.
  intros a.
  induction Γ; intros.
  - denotation_simp.
    destruct (decide (is_arr τ)); invclear H1; try auto_ty_exfalso6.
    + apply ok_dctx_cons_arr; auto; refinement_solver.
    + constructor; auto; refinement_solver.
  - denotation_simp. invclear H1.
    + constructor; auto. listctx_set_solver. eapply IHΓ; eauto. fast_set_solver.
      dec_solver2. refinement_solver.
    + apply ok_dctx_cons_arr; auto; refinement_solver. eapply IHΓ; eauto. fast_set_solver.
      dec_solver2.
Qed.

Lemma state_subst_implies_rty_subst_ok_dctx_all
  : ∀ (a v : atom) (Γ : list (atom * rty)) (L : aset) (c_x : constant) (x : atom)
      (k : nat) τ,
    a ∉ stale Γ ∪ {[x]} ->
    closed_rty 0 (ctxdom ⦑ Γ ⦒ ∪ L) ({k ~r> {a := c_x }v v} τ) ->
    ok_dctx L (Γ ++ [(x, {k ~r> v} τ)]) ->
    ok_dctx L (Γ ++ [(x, {k ~r> {a := c_x }v v} τ)]).
Proof.
  intros. destruct (decide (a = v)); subst; auto; simpl; dec_solver2.
  eapply state_subst_implies_rty_subst_ok_dctx_all_v_to_c; eauto.
  simpl in H0. rewrite decide_True in H0; auto.
Qed.

Lemma state_subst_implies_rty_subst_ok_dctx_all'
  : ∀ (a v : atom) (Γ : list (atom * rty)) (L : aset) (c_x : constant) (x : atom)
      (k : nat) τ,
    a ∉ stale Γ ∪ {[x]} ->
    closed_rty 0 (ctxdom ⦑ Γ ⦒ ∪ L) ({k ~r> v} τ) ->
    ok_dctx L (Γ ++ [(x, {k ~r> {a := c_x }v v} τ)]) ->
    ok_dctx L (Γ ++ [(x, {k ~r> v} τ)]).
Proof.
  intros. destruct (decide (a = v)); subst; auto.
  - simpl in H1. rewrite decide_True in H1; auto.
    eapply state_subst_implies_rty_subst_ok_dctx_all_c_to_v; eauto.
  - simpl in H1. dec_solver2.
Qed.

(* Lemma rty_open_oarr_eq: forall k c_x B n d ϕ τ, *)
(* ({k ~r> c_x} (-:{v: B | n | d | ϕ}⤑ τ)) = τ. *)
(* Proof. *)
(*   intros. simpl. *)

Lemma state_subst_implies_r_susbt_eq_forward
     : ∀ (k : nat) (a : atom) (ϕ : refinement) (bst : bstate) (st : state)
         (c_x v : constant),
         refinement_open k a ϕ bst (<[a:=c_x]> st) v ->
         refinement_open k c_x ϕ bst (<[a:=c_x]> st) v.
Proof.
  intros. rewrite <- state_subst_implies_r_susbt_eq; auto.
Qed.

Global Hint Resolve state_subst_implies_r_susbt_eq_forward: core.

Lemma state_subst_implies_r_susbt_eq_backward
     : ∀ (k : nat) (a : atom) (ϕ : refinement) (bst : bstate) (st : state)
         (c_x v : constant),
    refinement_open k c_x ϕ bst (<[a:=c_x]> st) v ->
    refinement_open k a ϕ bst (<[a:=c_x]> st) v.
Proof.
  intros. rewrite state_subst_implies_r_susbt_eq; auto.
Qed.

Global Hint Resolve state_subst_implies_r_susbt_eq_backward: core.

Lemma state_subst_implies_rty_subst_rR_all_vc
  : ∀ τ (a: atom) m st bst (c_x : constant) (k : nat) e,
    (closed_rty m (dom aset (<[a:=c_x]> st)) ({k ~r> c_x} τ) /\
       {m;bst;(<[a:=c_x]> st)}⟦ {k ~r> a} τ⟧ e) <->
    (closed_rty m (dom aset (<[a:=c_x]> st)) ({k ~r> a} τ) /\
       {m;bst;(<[a:=c_x]> st)}⟦ {k ~r> c_x} τ ⟧ e).
Proof.
  induction τ; split; intros; mydestr.
  - simpl in H0; mydestr. constructor; auto. constructor; auto.
    split; auto.
    eexists; do 2 (split; eauto).
  - simpl in H0; mydestr. constructor; auto. constructor; auto.
    split; auto.
    eexists; do 2 (split; eauto).
  - simpl in H0; mydestr. constructor; auto. constructor; auto.
  - simpl in H0; mydestr. constructor; auto. constructor; auto.
  - simpl in H0; mydestr. constructor; auto. constructor; auto. inv_rd_simpl1.
    constructor; auto.
    eexists; split; eauto. intros.
    simpl in H. rewrite closed_rty_destruct_oarr in H; mydestr.
    apply IHτ; auto.
  - simpl in H0; mydestr. constructor; auto. constructor; auto. inv_rd_simpl1.
    constructor; auto.
    eexists; split; eauto. intros.
    simpl in H. rewrite closed_rty_destruct_oarr in H; mydestr.
    apply IHτ; auto.
  - simpl in H0; mydestr. constructor; auto. constructor; auto. inv_rd_simpl1.
    constructor; auto.
    eexists; split; eauto. intros.
    simpl in H. rewrite closed_rty_destruct_arrarr in H; mydestr.
    rewrite closed_rty_destruct_arrarr in H1; mydestr.
    apply IHτ2; eauto. split; auto.
    apply H3; auto. apply IHτ1; auto.
  - simpl in H0; mydestr. constructor; auto. constructor; auto. inv_rd_simpl1.
    constructor; auto.
    eexists; split; eauto. intros.
    simpl in H. rewrite closed_rty_destruct_arrarr in H; mydestr.
    rewrite closed_rty_destruct_arrarr in H1; mydestr.
    apply IHτ2; eauto. split; auto.
    apply H3; auto. apply IHτ1; auto.
Qed.

Lemma state_subst_implies_rty_subst_rR_all_v_to_c
  : ∀ τ (a: atom) m st bst (c_x : constant) (k : nat) e,
    closed_rty m (dom aset (<[a:=c_x]> st)) ({k ~r> c_x} τ) ->
    {m;bst;(<[a:=c_x]> st)}⟦ {k ~r> a} τ⟧ e ->
    {m;bst;(<[a:=c_x]> st)}⟦ {k ~r> c_x} τ ⟧ e.
Proof.
  intros. assert (closed_rty m (dom aset (<[a:=c_x]> st)) ({k ~r> c_x} τ) /\
       {m;bst;(<[a:=c_x]> st)}⟦ {k ~r> a} τ⟧ e); auto.
  rewrite state_subst_implies_rty_subst_rR_all_vc in H1; mydestr; auto.
Qed.

Lemma state_subst_implies_rty_subst_rR_all_c_to_v
  : ∀ τ (a: atom) m st bst (c_x : constant) (k : nat) e,
    closed_rty m (dom aset (<[a:=c_x]> st)) ({k ~r> a} τ) ->
    {m;bst;(<[a:=c_x]> st)}⟦ {k ~r> c_x} τ⟧ e ->
    {m;bst;(<[a:=c_x]> st)}⟦ {k ~r> a} τ ⟧ e.
Proof.
  intros. assert (closed_rty m (dom aset (<[a:=c_x]> st)) ({k ~r> a} τ) /\
       {m;bst;(<[a:=c_x]> st)}⟦ {k ~r> c_x} τ⟧ e); auto.
  rewrite <- state_subst_implies_rty_subst_rR_all_vc in H1; mydestr; auto.
Qed.

Lemma unfold_rty_obase: forall B n d ϕ k v,
    {v:B|n `min` k|refinement_set_open k v d|refinement_open k v ϕ} =
      {k ~r> v} {v:B|n|d|ϕ}.
Proof.
  intros; auto.
Qed.

Lemma unfold_rty_ubase: forall B n d ϕ k v,
    [v:B|n `min` k|refinement_set_open k v d|refinement_open k v ϕ] =
      {k ~r> v} [v:B|n|d|ϕ].
Proof.
  intros; auto.
Qed.


Lemma state_subst_implies_rty_subst_wf_ctxrR_all_v_to_c
    : ∀ (a: atom) (Γ : list (atom * rty)) st (c_x : constant) (x : atom)
      (k : nat) τ,
  a ∉ (stale st ∪ stale Γ ∪ {[x]}) ->
  closed_rty 0 (ctxdom ⦑Γ⦒ ∪ dom aset (<[a:=c_x]> st)) ({k ~r> c_x} τ) ->
  wf_ctxrR (<[a:=c_x]> st) (Γ ++ [(x, {k ~r> a} τ)]) ->
  wf_ctxrR (<[a:=c_x]> st) (Γ ++ [(x, {k ~r> c_x} τ)]).
Proof.
  intros a. induction Γ; intros.
  - assert (ok_dctx (dom aset (<[a:=c_x]> st)) [(x, {k ~r> c_x} τ)]) as Hokd.
    { apply wf_ctxrR_regular1 in H1.
      eapply state_subst_implies_rty_subst_ok_dctx_all_v_to_c in H1; eauto.
      fast_set_solver. }
    listctx_set_simpl.
    destruct τ; invclear H1; try auto_ty_exfalso6; try auto_ty_exfalso.
    + constructor; auto. intros.
      rewrite unfold_rty_obase in H1.
      apply state_subst_implies_rty_subst_rR_all_c_to_v in H1; auto.
      invclear H5; auto.
    + constructor; auto.
      { intros. apply H10. rewrite unfold_rty_ubase in H1.
        apply state_subst_implies_rty_subst_rR_all_c_to_v in H1; auto.
        invclear H6; auto. }
      mydestr. exists x0; split; eauto.
      rewrite unfold_rty_ubase. apply state_subst_implies_rty_subst_rR_all_v_to_c; auto.
      refinement_solver.
    + constructor; auto. intros.
      apply H8.
      apply state_subst_implies_rty_subst_rR_all_c_to_v in H1; auto.
      invclear H7; auto.
    + constructor; auto. intros.
      apply H8.
      apply state_subst_implies_rty_subst_rR_all_c_to_v in H1; auto.
      invclear H7; auto.
 - assert (ok_dctx (dom aset (<[a:=c_x]> st)) ((a0 :: Γ) ++ [(x, {k ~r> c_x} τ)])) as Hokd.
    { apply wf_ctxrR_regular1 in H1.
      eapply state_subst_implies_rty_subst_ok_dctx_all_v_to_c in H1; eauto.
      fast_set_solver. }
   invclear H1.
   + constructor; auto. intros. auto_under c_x0.
     setoid_rewrite insert_commute in H6; try fast_set_solver.
     setoid_rewrite insert_commute; try fast_set_solver.
     apply IHΓ; refinement_solver.
   + constructor; auto. mydestr. exists x1; split; eauto.
     intros. auto_under e_x. dsimp1.
     setoid_rewrite insert_commute in H2; try fast_set_solver.
     setoid_rewrite insert_commute; try fast_set_solver.
     apply IHΓ; refinement_solver.
   + constructor; auto. apply IHΓ; refinement_solver.
Qed.

Lemma state_subst_implies_rty_subst_wf_ctxrR_all_c_to_v
    : ∀ (a: atom) (Γ : list (atom * rty)) st (c_x : constant) (x : atom)
      (k : nat) τ,
  a ∉ (stale st ∪ stale Γ ∪ {[x]}) ->
  closed_rty 0 (ctxdom ⦑Γ⦒ ∪ dom aset (<[a:=c_x]> st)) ({k ~r> a} τ) ->
  wf_ctxrR (<[a:=c_x]> st) (Γ ++ [(x, {k ~r> c_x} τ)]) ->
  wf_ctxrR (<[a:=c_x]> st) (Γ ++ [(x, {k ~r> a} τ)]).
Proof.
  intros a. induction Γ; intros.
  - assert (ok_dctx (dom aset (<[a:=c_x]> st)) [(x, {k ~r> a} τ)]) as Hokd.
    { apply wf_ctxrR_regular1 in H1.
      eapply state_subst_implies_rty_subst_ok_dctx_all_c_to_v in H1; eauto.
      fast_set_solver. }
    listctx_set_simpl.
    destruct τ; invclear H1; try auto_ty_exfalso6; try auto_ty_exfalso.
    + constructor; auto. intros.
      rewrite unfold_rty_obase in H1.
      apply state_subst_implies_rty_subst_rR_all_v_to_c in H1; auto.
      invclear H5; auto.
    + constructor; auto.
      { intros. apply H10. rewrite unfold_rty_ubase in H1.
        apply state_subst_implies_rty_subst_rR_all_v_to_c in H1; auto.
        invclear H6; auto. }
      mydestr. exists x0; split; eauto.
      rewrite unfold_rty_ubase. apply state_subst_implies_rty_subst_rR_all_c_to_v; auto.
      refinement_solver.
    + constructor; auto. intros.
      apply H8.
      apply state_subst_implies_rty_subst_rR_all_v_to_c in H1; auto.
      invclear H7; auto.
    + constructor; auto. intros.
      apply H8.
      apply state_subst_implies_rty_subst_rR_all_v_to_c in H1; auto.
      invclear H7; auto.
 - assert (ok_dctx (dom aset (<[a:=c_x]> st)) ((a0 :: Γ) ++ [(x, {k ~r> a} τ)])) as Hokd.
    { apply wf_ctxrR_regular1 in H1.
      eapply state_subst_implies_rty_subst_ok_dctx_all_c_to_v in H1; eauto.
      fast_set_solver. }
   invclear H1.
   + constructor; auto. intros. auto_under c_x0.
     setoid_rewrite insert_commute in H6; try fast_set_solver.
     setoid_rewrite insert_commute; try fast_set_solver.
     apply IHΓ; refinement_solver.
   + constructor; auto. mydestr. exists x1; split; eauto.
     intros. auto_under e_x. dsimp1.
     setoid_rewrite insert_commute in H2; try fast_set_solver.
     setoid_rewrite insert_commute; try fast_set_solver.
     apply IHΓ; refinement_solver.
   + constructor; auto. apply IHΓ; refinement_solver.
Qed.

Lemma state_subst_implies_rty_subst_inv_ctxrR_all_v_to_c
    : ∀ (a: atom) (Γ : list (atom * rty)) st (c_x : constant) (x : atom)
      (k : nat) τ τ' e,
  a ∉ (stale st ∪ stale Γ ∪ {[x]}) ->
  closed_rty 0 (ctxdom ⦑Γ⦒ ∪ dom aset (<[a:=c_x]> st)) ({k ~r> c_x} τ) ->
   ⅋{<[a:=c_x]> st}⟦τ'⟧{Γ ++ [(x, {k ~r> a} τ)] } e ->
   ⅋{<[a:=c_x]> st}⟦τ'⟧{Γ ++ [(x, {k ~r> c_x} τ)] } e.
Proof.
  intros a. induction Γ; intros.
  - assert (ok_dctx (dom aset (<[a:=c_x]> st)) [(x, {k ~r> c_x} τ)]) as Hokd.
    { apply inv_ctxrR_regular1 in H1; mydestr.
      eapply state_subst_implies_rty_subst_ok_dctx_all_v_to_c in H1; eauto.
      fast_set_solver. }
    assert (closed_rty 0 (dom aset (<[a:=c_x]> st)) ({k ~r> a} τ)) as Hclosed.
    { apply inv_ctxrR_regular1 in H1; mydestr. unfold ctx_closed_rty in H2.
      assert ([] ++ [(x, {k ~r> a} τ)] = [] ++ [(x, {k ~r> a} τ)] ++ []) as Htmp by listctx_set_simpl.
      apply H2 in Htmp. refinement_solver. }
    listctx_set_simpl.
    destruct τ; invclear H1; try auto_ty_exfalso6; try auto_ty_exfalso.
    + constructor; auto. intros.
      rewrite unfold_rty_obase in H1.
      apply state_subst_implies_rty_subst_rR_all_c_to_v in H1; auto.
    + constructor; auto.
      mydestr. exists x0; split; eauto.
      rewrite unfold_rty_ubase. apply state_subst_implies_rty_subst_rR_all_v_to_c; auto.
      refinement_solver.
    + constructor; auto. inv_rd_simpl3. inv_rd_simpl1.
      intros. apply H12.
      apply state_subst_implies_rty_subst_rR_all_c_to_v in H1; auto.
    + constructor; auto. inv_rd_simpl3. inv_rd_simpl1.
      intros. apply H12.
      apply state_subst_implies_rty_subst_rR_all_c_to_v in H1; auto.
 - assert (ok_dctx (dom aset (<[a:=c_x]> st)) ((a0 :: Γ) ++ [(x, {k ~r> c_x} τ)])) as Hokd.
    { apply inv_ctxrR_regular1 in H1; mydestr.
      eapply state_subst_implies_rty_subst_ok_dctx_all_v_to_c in H1; eauto.
      fast_set_solver. }
    assert (closed_rty 0
              (ctxdom  ⦑(a0 :: Γ) ++ [(x, {k ~r> c_x} τ)]⦒ ∪ dom aset (<[a:=c_x]> st)) ({k ~r> a} τ)) as Hclosed.
    { apply inv_ctxrR_regular1 in H1; mydestr. auto. unfold ctx_closed_rty in H2.
      assert (((a0, r) :: Γ) ++ [(x, {k ~r> a} τ)] = ((a0, r) :: Γ) ++ [(x, {k ~r> a} τ)] ++ []) as Htmp by listctx_set_simpl.
      apply H2 in Htmp. refinement_solver. }
   invclear H1.
   + listctx_set_simpl. constructor; auto.
     { inv_rd_simpl1. }
     { inv_rd_simpl1. inv_rd_simpl3. inv_rd_simpl1. }
     intros. auto_under c_x0.
     setoid_rewrite insert_commute in H10; try fast_set_solver.
     setoid_rewrite insert_commute; try fast_set_solver.
     apply IHΓ; refinement_solver.
   + listctx_set_simpl. constructor; auto.
     { inv_rd_simpl1. }
     { inv_rd_simpl1. inv_rd_simpl3. inv_rd_simpl1. }
     mydestr. exists x1; split; eauto.
     intros. auto_under e_x. dsimp1.
     setoid_rewrite insert_commute in H2; try fast_set_solver.
     setoid_rewrite insert_commute; try fast_set_solver.
     apply IHΓ; refinement_solver.
   + listctx_set_simpl. constructor; auto.
     { inv_rd_simpl1. }
     { inv_rd_simpl1. inv_rd_simpl3. inv_rd_simpl1. }
     intros.
     apply IHΓ; refinement_solver.
Qed.

Lemma state_subst_implies_rty_subst_inv_ctxrR_all_c_to_v
    : ∀ (a: atom) (Γ : list (atom * rty)) st (c_x : constant) (x : atom)
      (k : nat) τ τ' e,
  a ∉ (stale st ∪ stale Γ ∪ {[x]}) ->
  closed_rty 0 (ctxdom ⦑Γ⦒ ∪ dom aset (<[a:=c_x]> st)) ({k ~r> a} τ) ->
   ⅋{<[a:=c_x]> st}⟦τ'⟧{Γ ++ [(x, {k ~r> c_x} τ)] } e ->
   ⅋{<[a:=c_x]> st}⟦τ'⟧{Γ ++ [(x, {k ~r> a} τ)] } e.
Proof.
  intros a. induction Γ; intros.
  - assert (ok_dctx (dom aset (<[a:=c_x]> st)) [(x, {k ~r> a} τ)]) as Hokd.
    { apply inv_ctxrR_regular1 in H1; mydestr.
      eapply state_subst_implies_rty_subst_ok_dctx_all_c_to_v in H1; eauto.
      fast_set_solver. }
    assert (closed_rty 0 (dom aset (<[a:=c_x]> st)) ({k ~r> c_x} τ)) as Hclosed.
    { apply inv_ctxrR_regular1 in H1; mydestr. unfold ctx_closed_rty in H2.
      assert ([] ++ [(x, {k ~r> c_x} τ)] = [] ++ [(x, {k ~r> c_x} τ)] ++ []) as Htmp by listctx_set_simpl.
      apply H2 in Htmp. refinement_solver. }
    listctx_set_simpl.
    destruct τ; invclear H1; try auto_ty_exfalso6; try auto_ty_exfalso.
    + constructor; auto. intros.
      rewrite unfold_rty_obase in H1.
      apply state_subst_implies_rty_subst_rR_all_v_to_c in H1; auto.
    + constructor; auto.
      mydestr. exists x0; split; eauto.
      rewrite unfold_rty_ubase. apply state_subst_implies_rty_subst_rR_all_c_to_v; auto.
      refinement_solver.
    + constructor; auto. inv_rd_simpl3. inv_rd_simpl1.
      intros. apply H12.
      apply state_subst_implies_rty_subst_rR_all_v_to_c in H1; auto.
    + constructor; auto. inv_rd_simpl3. inv_rd_simpl1.
      intros. apply H12.
      apply state_subst_implies_rty_subst_rR_all_v_to_c in H1; auto.
 - assert (ok_dctx (dom aset (<[a:=c_x]> st)) ((a0 :: Γ) ++ [(x, {k ~r> a} τ)])) as Hokd.
    { apply inv_ctxrR_regular1 in H1; mydestr.
      eapply state_subst_implies_rty_subst_ok_dctx_all_c_to_v in H1; eauto.
      fast_set_solver. }
    assert (closed_rty 0
              (ctxdom  ⦑(a0 :: Γ) ++ [(x, {k ~r> c_x} τ)]⦒ ∪ dom aset (<[a:=c_x]> st)) ({k ~r> a} τ)) as Hclosed.
    { apply inv_ctxrR_regular1 in H1; mydestr. auto. unfold ctx_closed_rty in H2.
      assert (((a0, r) :: Γ) ++ [(x, {k ~r> c_x} τ)] = ((a0, r) :: Γ) ++ [(x, {k ~r> c_x} τ)] ++ []) as Htmp by listctx_set_simpl.
      apply H2 in Htmp. refinement_solver. }
   invclear H1.
   + listctx_set_simpl. constructor; auto.
     { inv_rd_simpl1. }
     { inv_rd_simpl1. inv_rd_simpl3. inv_rd_simpl1. }
     intros. auto_under c_x0.
     setoid_rewrite insert_commute in H10; try fast_set_solver.
     setoid_rewrite insert_commute; try fast_set_solver.
     apply IHΓ; refinement_solver.
   + listctx_set_simpl. constructor; auto.
     { inv_rd_simpl1. }
     { inv_rd_simpl1. inv_rd_simpl3. inv_rd_simpl1. }
     mydestr. exists x1; split; eauto.
     intros. auto_under e_x. dsimp1.
     setoid_rewrite insert_commute in H2; try fast_set_solver.
     setoid_rewrite insert_commute; try fast_set_solver.
     apply IHΓ; refinement_solver.
   + listctx_set_simpl. constructor; auto.
     { inv_rd_simpl1. }
     { inv_rd_simpl1. inv_rd_simpl3. inv_rd_simpl1. }
     intros.
     apply IHΓ; refinement_solver.
Qed.

Lemma closed_rty_1_open_eq_0: forall τ (k: nat) (L:aset) (c: constant),
    closed_rty (S k) L τ -> closed_rty k L ({k ~r> c} τ).
Proof.
  induction τ; intros.
  - invclear H. constructor; auto.
  - invclear H. constructor; auto.
  - rewrite closed_rty_destruct_oarr in H; mydestr.
    simpl.
    rewrite closed_rty_destruct_oarr. split; auto.
    assert (closed_rty k L ({k ~r> c} {v:B|n|d|ϕ})); auto.
  - rewrite closed_rty_destruct_arrarr in H; mydestr.
    simpl.
    rewrite closed_rty_destruct_arrarr. split; auto.
Qed.

Lemma closed_rty_oarr_implies_closed_rty_retty: forall τ b n d ϕ (L:aset) (c: constant),
    closed_rty 0 L (-:{v: b | n | d | ϕ}⤑ τ) -> closed_rty 0 L ({0 ~r> c} τ).
Proof.
  intros. rewrite closed_rty_destruct_oarr in H; mydestr.
  apply closed_rty_1_open_eq_0; auto.
Qed.

Global Hint Resolve closed_rty_oarr_implies_closed_rty_retty: core.

Lemma state_subst_implies_rty_subst_wf_ctxrR_all_open_to_v
  : ∀ (a: atom) (v: value) (Γ : list (atom * rty)) st (c_x : constant) (x : atom)
      (k : nat) τ,
    ((exists (n: constant), v = n) \/ (exists (y: atom), v = y)) ->
    a ∉ (stale st ∪ stale Γ ∪ {[x]}) ->
    closed_rty 0 (ctxdom ⦑Γ⦒ ∪ dom aset (<[a:=c_x]> st)) ({k ~r> v} τ) ->
    wf_ctxrR (<[a:=c_x]> st) (Γ ++ [(x, {k ~r> {a := c_x}v v} τ)]) ->
    wf_ctxrR (<[a:=c_x]> st) (Γ ++ [(x, {k ~r> v} τ)]).
Proof.
  intros. destruct H; mydestr; subst; auto.
  destruct (decide (a = x0)); subst; auto.
  - simpl in H2. rewrite decide_True in H2; auto.
    eapply state_subst_implies_rty_subst_wf_ctxrR_all_c_to_v; auto.
  - simpl in H2. dec_solver2.
Qed.

Lemma state_subst_implies_rty_subst_wf_ctxrR_all_v_to_open
  : ∀ (a: atom) (v: value) (Γ : list (atom * rty)) st (c_x : constant) (x : atom)
      (k : nat) τ,
    ((exists (n: constant), v = n) \/ (exists (y: atom), v = y)) ->
    a ∉ (stale st ∪ stale Γ ∪ {[x]}) ->
    closed_rty 0 (ctxdom ⦑Γ⦒ ∪ dom aset (<[a:=c_x]> st)) ({k ~r> {a := c_x}v v} τ) ->
    wf_ctxrR (<[a:=c_x]> st) (Γ ++ [(x, {k ~r> v} τ)]) ->
    wf_ctxrR (<[a:=c_x]> st) (Γ ++ [(x, {k ~r> {a := c_x}v v} τ)]).
Proof.
  intros. destruct H; mydestr; subst; auto.
  destruct (decide (a = x0)); subst; auto.
  - simpl in H1. rewrite decide_True in H1; auto.
    simpl. rewrite decide_True; auto.
    eapply state_subst_implies_rty_subst_wf_ctxrR_all_v_to_c; auto.
  - dec_solver2.
Qed.

Lemma state_subst_implies_rty_subst_inv_ctxrR_all_open_to_v
  : ∀ (a: atom) (v: value) (Γ : list (atom * rty)) st (c_x : constant) (x : atom)
      (k : nat) τ τ' e,
    ((exists (n: constant), v = n) \/ (exists (y: atom), v = y)) ->
    a ∉ (stale st ∪ stale Γ ∪ {[x]}) ->
    closed_rty 0 (ctxdom ⦑Γ⦒ ∪ dom aset (<[a:=c_x]> st)) ({k ~r> v} τ) ->
    ⅋{<[a:=c_x]> st}⟦τ'⟧{Γ ++ [(x, {k ~r> {a := c_x}v v} τ)] } e ->
    ⅋{<[a:=c_x]> st}⟦τ'⟧{Γ ++ [(x, {k ~r> v} τ)] } e.
Proof.
  intros. destruct H; mydestr; subst; auto.
  destruct (decide (a = x0)); subst; auto.
  - simpl in H2. rewrite decide_True in H2; auto.
    eapply state_subst_implies_rty_subst_inv_ctxrR_all_c_to_v; auto.
  - simpl in H2. dec_solver2.
Qed.

Lemma state_subst_implies_rty_subst_inv_ctxrR_all_v_to_open
  : ∀ (a: atom) (v: value) (Γ : list (atom * rty)) st (c_x : constant) (x : atom)
      (k : nat) τ τ' e,
    ((exists (n: constant), v = n) \/ (exists (y: atom), v = y)) ->
    a ∉ (stale st ∪ stale Γ ∪ {[x]}) ->
    closed_rty 0 (ctxdom ⦑Γ⦒ ∪ dom aset (<[a:=c_x]> st)) ({k ~r> {a := c_x}v v} τ) ->
    ⅋{<[a:=c_x]> st}⟦τ'⟧{Γ ++ [(x, {k ~r> v} τ)] } e ->
    ⅋{<[a:=c_x]> st}⟦τ'⟧{Γ ++ [(x, {k ~r> {a := c_x}v v} τ)] } e.
Proof.
  intros. destruct H; mydestr; subst; auto.
  destruct (decide (a = x0)); subst; auto.
  - simpl in H1. rewrite decide_True in H1; auto.
    simpl. rewrite decide_True; auto.
    eapply state_subst_implies_rty_subst_inv_ctxrR_all_v_to_c; auto.
  - dec_solver2.
Qed.

Lemma ctx_closed_rty_last: forall (L: aset) Γ x τ_x,
    ctx_closed_rty L (Γ ++ [(x, τ_x)]) -> closed_rty 0 (ctxdom ⦑Γ⦒ ∪ L) τ_x.
Proof.
  intros. unfold ctx_closed_rty in H.
  assert (Γ ++ [(x, τ_x)] = Γ ++ [(x, τ_x)] ++ []) as Hz by listctx_set_simpl; eauto.
Qed.

Global Hint Resolve ctx_closed_rty_last: core.

Lemma closed_rty_subst_var: forall a (c_x: constant) m (L: aset) (v: value) τ_x,
    ((exists (n: constant), v = n) \/ (exists (y: atom), v = y)) ->
    closed_rty m L (τ_x ^r^ v) ->
    closed_rty m L (τ_x ^r^ c_x) ->
    closed_rty m L (τ_x ^r^ ({a := c_x }v v)).
Proof.
  intros. destruct H; mydestr; subst; auto.
  destruct (decide (a = x)); subst; auto.
  simpl. dec_solver2. simpl. dec_solver2.
Qed.

Lemma arr_type_in_tyctx_is_free: forall a Γ r (v: value) (b: base_ty),
    is_arr r -> ((a, ⌊r⌋) :: Γ) ⊢t v ⋮v b -> a # v.
Proof.
  intros. destruct v; invclear H0; auto.
  - set_solver.
  - destruct (decide (a = atom)); subst; try fast_set_solver.
    simpl in H4. rewrite decide_True in H4; auto.
    destruct r; invclear H4; try auto_ty_exfalso.
Qed.
