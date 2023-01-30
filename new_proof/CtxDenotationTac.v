From stdpp Require Import mapset.
From stdpp Require Import natmap.
From stdpp Require Import fin_map_dom.
From CT Require Import CoreLangProp.
From CT Require Import OperationalSemanticsProp.
From CT Require Import BasicTypingProp.
From CT Require Import SyntaxSugar.
From CT Require Import Refinement.
From CT Require Import RefinementTac.
From CT Require Import RefinementDenotationProp.
From CT Require Import CtxDenotation.
From CT Require Import TermOrdering.
From CT Require Import RDInv5.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Classical.

Import Atom.
Import CoreLang.
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
Import WFDenotation.
Import WFDenotationTac.
Import CtxDenotation.
Import TermOrdering.
Import RDInv.
Import RDInv2.
Import RDInv3.
Import RDInv5.

Lemma ctxrR_empty_iff_rR: forall st τ e, ({st}⟦τ⟧{ [] }) e <-> {st}⟦τ⟧ e.
Proof.
  split; intros; auto.
  - invclear H; auto.
  - constructor; auto.
Qed.

Lemma not_ctxrR_empty_iff_not_rR: forall st τ e, ~ ({st}⟦τ⟧{ [] }) e <-> ~ {st}⟦τ⟧ e.
Proof.
  split; intros; auto.
  - intros HF. apply H. constructor; auto.
  - intros HF. apply H. invclear HF; auto.
Qed.

Ltac RD_simp :=
  repeat (match goal with
          | [H: ({_}⟦ _ ⟧{ [] }) _ |- _ ] => invclear H
          | [|- ({_}⟦ _ ⟧{ [] }) _ ] => constructor
          | [H: ({_}⟦_⟧{ (_, {v:_|_|_|_}) :: _ }) _ |- _ ] => invclear H; try auto_ty_exfalso
          | [H: ({0;b∅;_}⟦{v:_|_|_|_}⟧) (tvalue ?u) |- _ ] =>
              match u with
              | vconst _ => idtac
              | _ => destruct (over_inhabitant_only_constant _ _ _ _ _ _ H); subst
              end
          | [H: ¬ ({_}⟦ _ ⟧{ [] }) _ |- _ ] => rewrite not_ctxrR_empty_iff_not_rR in H
          | [ |- ¬ ({_}⟦ _ ⟧{ [] }) _ ] => rewrite not_ctxrR_empty_iff_not_rR
          | [H: ({_}⟦?τ⟧{_}) _  |- closed_rty _ _ ?τ] => apply ctxrR_regular in H; mydestr
          | [H: ({_}⟦_⟧{?Γ}) _  |- ok_dctx _ ?Γ] => apply ctxrR_regular in H; mydestr; my_simplify_map_eq
          | [H: ({_}⟦?τ⟧{ _ }) _ |- closed_rty _ _ ?τ] =>
       apply ctxrR_regular in H; mydestr
          end; eauto).

Lemma inv_ctxrR_implies_ctxrR: forall Γ st τ e,
    not_overbasety τ -> ⅋{st}⟦τ⟧{Γ} e -> {st}⟦τ⟧{Γ} e.
Proof.
  induction Γ; intros. RD_simp2. RD_simp.
  RD_simp2.
  - constructor; auto.
  - constructor; auto. mydestr. auto_meet_exists Hmeet. auto_under v_x. dsimp1.
    apply IHΓ; auto.
    eapply termR_perserve_inv_ctxrR; eauto. refinement_solver.
    apply termR_trans_better with (tlete x0 (a \t\ e)).
    + apply termR_let_one_step_from_basic_type'. basic_typing_solver.
      refinement_solver. eapply let_store_typable; eauto. refinement_solver.
      rewrite basic_has_type_swap2_tm; auto.
    + eapply termR_elete_lhs with (x:=a) (Tx:=b); eauto.
      apply close_rm_fv_tm.
      apply termR_weakening. basic_typing_solver.
      apply termR_value_tm; auto.
      rewrite open_close_var_tm; basic_typing_solver.
      rewrite basic_has_type_swap2_tm; auto.
  - constructor; auto. auto_meet_exists Hmeet. reduction_simpl0. auto_under v_x.
    apply IHΓ; auto.
    erewrite state_insert_closed_value_arr; eauto; refinement_solver.
    eapply termR_perserve_inv_ctxrR; eauto. refinement_solver.
    apply termR_let_one_step_from_basic_type'. basic_typing_solver.
    refinement_solver. eapply let_store_typable; eauto. refinement_solver.
    rewrite basic_has_type_swap2_tm; auto.
Qed.

Lemma ctxrR2_implies_inv_ctxrR: forall Γ st τ1 τ2,
    ctxrR2 st Γ τ1 τ2 -> (forall e, ⅋{st}⟦ τ1 ⟧{ Γ } e -> ⅋{st}⟦ τ2 ⟧{ Γ } e).
Proof.
  induction Γ; intros. invclear H. RD_simp2. mydestr.
  invclear H.
  - RD_simp2. constructor; auto. rewrite <- H10; auto. intros. auto_under c_x.
    eapply IHΓ; eauto.
  - RD_simp2. constructor; auto. rewrite <- H10; auto. mydestr.
    auto_meet_exists Hmeet. auto_meet_reduce.
  - RD_simp2. constructor; auto. rewrite <- H11; auto. mydestr.
    assert ({st}⟦τ1⟧⊆⟦τ2⟧{Γ}).
    { assert (∃ v : value, ({0;b∅;st}⟦r⟧) v ∧ x ↪* v) by (apply rR_arr_halt; auto).
      mydestr. eapply H0 in H1; eauto.
      erewrite state_insert_closed_value_arr in H1; eauto. refinement_solver.
      apply multistep_refl. op_solver1. }
    intros. auto_under v_x. eapply IHΓ; eauto.
Qed.

Lemma rR_push_to_subst: forall (st: state) (x: atom) c_x τ e,
    x ∉ rty_fv τ ->
    closed_rty 0 ({[x]} ∪ dom aset st) (τ ^r^ x) ->
    {1;<b[↦c_x]> b∅;st}⟦τ⟧ e -> {0;b∅;<[x:=c_x]> st}⟦τ ^r^ x⟧ e.
Proof.
  intros.
  apply rR_open_trans_empty' in H1.
  rewrite <- rR_shadow_update_st_c with (a := x) (c := c_x) in H1.
  apply state_subst_implies_rty_subst_rR_all_c_to_v; auto.
  closed_rty_solver.
  refinement_solver. rewrite rty_fv_open_c_eq. auto.
Qed.

Lemma closed_rty_open_1: forall τ (L: aset) (c_x:constant),
    closed_rty 1 L τ -> closed_rty 0 L (τ ^r^ c_x).
Proof.
  induction τ; intros.
  - invclear H. constructor; auto.
  - invclear H. constructor; auto.
  - auto_dclosed_rty.
    assert (closed_rty 0 L ({v:B|n|d|ϕ} ^r^ c_x)). invclear H; constructor; auto.
    simpl. simpl in H2. auto_dclosed_rty.
  - simpl. auto_dclosed_rty.
Qed.

Lemma rR_push_to_subst': forall (st: state) (x: atom) c_x τ e,
    x ∉ rty_fv τ ->
    closed_rty 1 (dom aset st) (τ) ->
    {0;b∅;<[x:=c_x]> st}⟦τ ^r^ x⟧ e ->
    {1;<b[↦c_x]> b∅;st}⟦τ⟧ e.
Proof.
  intros.
  apply state_subst_implies_rty_subst_rR_all_v_to_c in H1; auto.
  apply rR_open_trans_empty; auto.
  rewrite rR_shadow_update_st_c with (a := x) (c := c_x) in H1; auto.
  rewrite rty_fv_open_c_eq. auto.
  apply closed_rty_open_1 with (c_x:=c_x)in H0. refinement_solver.
Qed.


Lemma inv_ctxrR2_oarr_rR: forall x st b n d ϕ1 ϕ2 τ1 τ2,
    x ∉ stale τ2 ∪ stale τ1 ∪ stale d ->
    closed_rty 0 (dom _ st) (-:{v: b | n | d | ϕ1}⤑ τ1) ->
    closed_rty 0 (dom _ st) (-:{v: b | n | d | ϕ2}⤑ τ2) ->
    {st}⟦{v:b|n|d|ϕ2}⟧⊆⟦{v:b|n|d|ϕ1}⟧{[] } ->
    {st}⟦τ1 ^r^ x⟧⊆⟦τ2 ^r^ x⟧{[(x, {v:b|n|d|ϕ2})] } ->
    {st}⟦-:{v: b | n | d | ϕ1}⤑ τ1⟧⊆⟦-:{v: b | n | d | ϕ2}⤑ τ2⟧{[] }.
Proof.
  intros. invclear H2. invclear H3; try auto_ty_exfalso.
  constructor; auto.
  { inv_rd_simpl0. rewrite H19; auto. }
  intros. simpl in H2; mydestr.
  constructor; auto.
  { inv_rd_simpl0. rewrite <- H19; auto. }
  constructor; auto. exists x0. split; auto. intros.
  assert (({0;b∅;st}⟦{v:b|n|d|ϕ2}⟧) c_x) as Hc_x2.
  { constructor; auto. constructor; auto. exists c_x; do 2 (split; eauto). }
  specialize (H7 _ Hc_x2) as Hc_x1.
  simpl in Hc_x1; mydestr; subst. invclear H21. rename x1 into c_x.
  apply H9 in H14; auto.
  specialize (H20 _ Hc_x2). invclear H20.
  apply rR_push_to_subst with (x:=x) in H14; try fast_set_solver; try closed_rty_solver.
  apply H24 in H14.
  eapply rR_push_to_subst'; eauto. fast_set_solver. auto_dclosed_rty.
Qed.

Lemma ctxrR2_implies_erase_eq: forall Γ st τ1 τ2,
  {st}⟦ τ1 ⟧⊆⟦ τ2 ⟧{Γ} -> ⌊ τ1 ⌋ = ⌊ τ2 ⌋.
Proof.
  intros. induction H; auto.
Qed.

Lemma ctxrR2_implies_closed_rty: forall Γ st τ1 τ2,
  {st}⟦ τ1 ⟧⊆⟦ τ2 ⟧{Γ} ->
  closed_rty 0 (ctxdom ⦑Γ⦒ ∪ dom aset st) τ1 /\ closed_rty 0 (ctxdom ⦑Γ⦒ ∪ dom aset st) τ2.
Proof.
  intros. induction H; split; auto; denotation_simp; try closed_rty_solver.
Qed.

Lemma ctxrR2_regular: forall Γ st τ1 τ2,
  {st}⟦ τ1 ⟧⊆⟦ τ2 ⟧{Γ} ->
  ⌊ τ1 ⌋ = ⌊ τ2 ⌋ /\ closed_rty 0 (ctxdom ⦑Γ⦒ ∪ dom aset st) τ1 /\ closed_rty 0 (ctxdom ⦑Γ⦒ ∪ dom aset st) τ2.
Proof.
  intros. split.
  eapply ctxrR2_implies_erase_eq; eauto.
  eapply ctxrR2_implies_closed_rty; eauto.
Qed.

Ltac closed_rty_solver2 :=
  match goal with
  | [H: closed_rty ?m ?L [v:?b|?n|?d|?ϕ] |- closed_rty ?m ?L {v:?b|?n|?d|?ϕ} ] =>
      rewrite closed_rty_under_iff_over; auto
  | [H: closed_rty ?m ?L {v:?b|?n|?d|?ϕ} |- closed_rty ?m ?L [v:?b|?n|?d|?ϕ] ] =>
      rewrite <- closed_rty_under_iff_over; auto
  end.

Lemma rR_implies_overbase_simpl: forall st b n d ϕ1 ϕ2,
    closed_rty 0 (dom aset st) {v:b|n|d|ϕ2} ->
    closed_rty 0 (dom aset st) {v:b|n|d|ϕ1} ->
    (∀ e : tm, ({0;b∅;st}⟦{v:b|n|d|ϕ1}⟧) e → ({0;b∅;st}⟦{v:b|n|d|ϕ2}⟧) e) <->
      (forall (c: constant), [] ⊢t c ⋮t b -> ϕ1 b∅ st c -> ϕ2 b∅ st c).
Proof.
  split; intros; mydestr.
  - intros. assert (({0;b∅;st}⟦{v:b|n|d|ϕ1}⟧) c). constructor; auto. constructor; auto.
    exists c. split; auto. basic_typing_solver.
    apply H1 in H4. invclear H4; mydestr; subst. invclear H8; auto.
  - simpl in H2; mydestr. invclear H6. constructor; auto. constructor; auto.
    exists x. split; auto.
Qed.

Definition not_eq_c (c: constant): tm :=
  match c with
  | cbool true => false
  | cbool false => true
  | cnat n =>
    tletbiop op_rannat (cnat 0) (cnat 0)
            (tletbiop op_eq n (vbvar 0)
               (tmatchb (vbvar 0) terr (vbvar 1)))
  end.

Lemma not_eq_c_typable: forall c, [] ⊢t not_eq_c c ⋮t ty_of_const c.
Proof.
  intros. destruct c; auto.
  - simpl. destruct b; eauto; constructor; constructor; auto.
  - simpl.
    econstructor; eauto. instantiate (1:=∅). intros. simpl.
    econstructor; eauto. repeat constructor; listctx_set_solver. repeat constructor; try listctx_set_solver. simpl. dec_solver1.
    instantiate (1:= {[x]}). intros. simpl.
    econstructor; eauto; try basic_typing_vfavr_solver.
    constructor; auto. listctx_set_solver.
    constructor; auto. basic_typing_vfavr_solver.
Qed.

Global Hint Resolve not_eq_c_typable: core.

Lemma not_eq_c_reduce_to_not_c: forall (c c': constant),
    ty_of_const c = ty_of_const c' ->
    c ≠ c' -> not_eq_c c ↪* c'.
Proof.
  intros. destruct c; destruct c'; invclear H.
  - simpl. destruct b; destruct b0; auto; auto_exfalso.
  - assert ([] ⊢t not_eq_c n ⋮t ty_of_const n) as HT by auto.
    assert (lc (not_eq_c n)) as Hlc by basic_typing_solver.
    simpl. simpl in Hlc. rewrite letbiop_lc_body in Hlc; mydestr.
    rewrite letbiop_step_spec. exists 0,0, n0. repeat (split; auto).
    simpl.
    rewrite letbiop_step_spec. exists n,n0,(n =? n0). repeat (split; auto).
    { unfold body. exists ∅. intros. simpl. lc_solver. }
    simpl. assert (n =? n0 = false). rewrite Nat.eqb_neq; auto. rewrite H3.
    eapply multistep_R. constructor; auto.
Qed.

Lemma not_eq_c_not_reduce_to_c: forall (c: constant), ~ not_eq_c c ↪* c.
Proof.
  intros. intro HF. destruct c; simpl in HF.
  - destruct b; invclear HF; invclear H.
  - rewrite letbiop_step_spec in HF; mydestr; subst.
    simpl in H3.
    rewrite letbiop_step_spec in H3; mydestr; subst.
    invclear H3. invclear H4. simpl in H7.
    invclear H7. invclear H3; try auto_reduction_exfalso.
    invclear H6. reduction_simpl0. invclear H3. rewrite Nat.eqb_neq in H8. neg_apply H8.
Qed.

Lemma rR_implies_underbase_simpl: forall st b n d ϕ1 ϕ2,
    closed_rty 0 (dom aset st) [v:b|n|d|ϕ2] ->
    closed_rty 0 (dom aset st) [v:b|n|d|ϕ1] ->
    (∀ e : tm, ({0;b∅;st}⟦[v:b|n|d|ϕ1]⟧) e → ({0;b∅;st}⟦[v:b|n|d|ϕ2]⟧) e) <->
      (forall (c: constant), [] ⊢t c ⋮t b -> ϕ2 b∅ st c -> ϕ1 b∅ st c).
Proof.
  split; intros; mydestr.
  - pbc. assert ({0;b∅;st}⟦[v:b|n|d|ϕ1]⟧ (not_eq_c c)).
    { constructor; auto. denotation_simp. constructor; auto. intros.
      apply not_eq_c_reduce_to_not_c. denotation_simp. neg_apply H4; subst; auto. }
    apply H1 in H5. simpl in H5; mydestr. apply H7 in H3; auto; basic_typing_solver.
    apply not_eq_c_not_reduce_to_c in H3. invclear H3.
  - simpl in H2; mydestr. constructor; auto.
Qed.

Lemma rR_implies_under_iff_over: forall st b n d ϕ1 ϕ2,
    closed_rty 0 (dom aset st) {v:b|n|d|ϕ1} ->
    closed_rty 0 (dom aset st) {v:b|n|d|ϕ2} ->
    (∀ e : tm, ({0;b∅;st}⟦[v:b|n|d|ϕ2]⟧) e → ({0;b∅;st}⟦[v:b|n|d|ϕ1]⟧) e) <->
      (∀ e : tm, ({0;b∅;st}⟦{v:b|n|d|ϕ1}⟧) e → ({0;b∅;st}⟦{v:b|n|d|ϕ2}⟧) e).
Proof.
  split; intros.
  - invclear H2; mydestr; subst. constructor; auto. constructor; auto.
    exists x. split; auto. split; auto.
    rewrite rR_implies_underbase_simpl in H1; eauto; closed_rty_solver2.
  - invclear H2; mydestr; subst. constructor; auto. constructor; auto. closed_rty_solver2.
    intros. rewrite rR_implies_overbase_simpl in H1; eauto; closed_rty_solver2.
Qed.

Lemma ctxrR2_under_over: forall Γ st b n d ϕ1 ϕ2,
  {st}⟦[v:b|n|d|ϕ2]⟧⊆⟦[v:b|n|d|ϕ1]⟧{Γ} <-> {st}⟦{v:b|n|d|ϕ1}⟧⊆⟦{v:b|n|d|ϕ2}⟧{Γ}.
Proof.
  induction Γ; split; intros.
  - invclear H; constructor; auto; try closed_rty_solver2.
    intros. rewrite rR_implies_under_iff_over in H3; auto; closed_rty_solver2.
  - invclear H; constructor; auto; try closed_rty_solver2.
    intros. rewrite <- rR_implies_under_iff_over in H3; auto; closed_rty_solver2.
  - invclear H; constructor; auto; try closed_rty_solver2.
    + intros. auto_under c_x. rewrite <- IHΓ; auto.
    + mydestr. exists x0; split; auto. intros. auto_under e_x. apply H0 in H7.
      rewrite <- IHΓ; auto.
    + mydestr. exists x0; split; auto. intros. auto_under e_x. apply H0 in H8.
      rewrite <- IHΓ; auto.
  - invclear H; constructor; auto; try closed_rty_solver2.
    + intros. auto_under c_x. rewrite IHΓ; auto.
    + mydestr. exists x0; split; auto. intros. auto_under e_x. apply H0 in H7.
      rewrite IHΓ; auto.
    + mydestr. exists x0; split; auto. intros. auto_under e_x. apply H0 in H8.
      rewrite IHΓ; auto.
Qed.

Lemma inv_ctxrR2_oarr: forall Γ x st b n d ϕ1 ϕ2 τ1 τ2,
    x ∉ stale τ2 ∪ stale τ1 ∪ stale d ∪ stale Γ ->
    closed_rty 0 (ctxdom ⦑Γ⦒ ∪ dom _ st) (-:{v: b | n | d | ϕ1}⤑ τ1) ->
    closed_rty 0 (ctxdom ⦑Γ⦒ ∪ dom _ st) (-:{v: b | n | d | ϕ2}⤑ τ2) ->
    {st}⟦{v:b|n|d|ϕ2}⟧⊆⟦{v:b|n|d|ϕ1}⟧{Γ} ->
    {st}⟦τ1^r^ x⟧⊆⟦τ2^r^ x⟧{Γ ++ [(x, {v:b|n|d|ϕ2})] } ->
    {st}⟦-:{v: b | n | d | ϕ1}⤑ τ1⟧⊆⟦-:{v: b | n |  d | ϕ2}⤑ τ2⟧{Γ}.
Proof.
  induction Γ; intros.
  { listctx_set_simpl.
    apply inv_ctxrR2_oarr_rR with (x:=x); eauto; try fast_set_solver; closed_rty_solver. }
  mydestr.
  assert (⌊ -:{v: b | n | d | ϕ1}⤑ τ1 ⌋ = ⌊ -:{v: b | n | d | ϕ2}⤑ τ2 ⌋) as HTeq.
  { simpl. apply ctxrR2_implies_erase_eq in H3. inv_rd_simpl0. rewrite H3; auto. }
  invclear H2; invclear H3; try auto_ty_exfalso.
  - constructor; auto. intros. auto_under c_x.
    apply IHΓ with (x:=x); try fast_set_solver; try closed_rty_solver.
  - constructor; auto. mydestr. auto_meet_exists HE. dsimp1.
    apply IHΓ with (x:=x); auto. fast_set_solver. closed_rty_solver. closed_rty_solver.
    + auto_meet_reduce. auto_under e_x; auto.
    + auto_meet_reduce. auto_under e_x; auto.
  - inv_rd_simpl1. constructor; auto. mydestr.
    assert ({st}⟦{v:b|n|d|ϕ2}⟧⊆⟦{v:b|n|d|ϕ1}⟧{Γ}).
    { auto_under x1.
      assert (∃ v : value, ({0;b∅;st}⟦r⟧) v ∧ x1 ↪* v) by (apply rR_arr_halt; auto).
      mydestr. assert ([] ⊢t x2 ⋮v ⌊ r ⌋ ). refinement_solver.
      eapply H5 in H15; eauto. denotation_simp. }
    exists x0. split; auto. intros. auto_under e_x.
    assert ([] ⊢t v_x ⋮v ⌊ r ⌋ ). refinement_solver. denotation_simp.
    apply IHΓ with (x:=x); try fast_set_solver.
Qed.

(* Lemma inv_ctxrR2_oarr: forall Γ x st b n d ϕ1 ϕ2 τ1 τ2, *)
(*     x ∉ stale τ2 ∪ stale τ1 ∪ stale d ∪ stale Γ -> *)
(*     closed_rty 0 (ctxdom ⦑Γ⦒ ∪ dom _ st) (-:{v: b | n | d | ϕ1}⤑ τ1) -> *)
(*     closed_rty 0 (ctxdom ⦑Γ⦒ ∪ dom _ st) (-:{v: b | n | d | ϕ2}⤑ τ2) -> *)
(*     wf_ctxrR st (Γ ++ [(x, {v:b|n|d|ϕ2})]) -> *)
(*     {st}⟦{v:b|n|d|ϕ2}⟧⊆⟦{v:b|n|d|ϕ1}⟧{Γ} -> *)
(*     {st}⟦τ1⟧⊆⟦τ2⟧{Γ ++ [(x, {v:b|n|d|ϕ2})] } -> *)
(*     {st}⟦-:{v: b | n | d | ϕ1}⤑ τ1⟧⊆⟦-:{v: b | n | d | ϕ2}⤑ τ2⟧{Γ}. *)
(* Proof. *)
(*   induction Γ; intros. *)
(*   - listctx_set_simpl. invclear H3. invclear H4. *)
(*     constructor; auto; try closed_rty_solver. *)
(*     + admit. *)
(*     + intros. simpl in H3; mydestr. constructor; auto. admit.  constructor; auto. admit. *)
(*       exists x0. split; auto. intros.  *)
(*   listctx_set_simpl. *)
(* Admitted. *)

(* Inductive inv_ctxrR2: state -> listctx rty -> rty -> rty -> Prop := *)
(* | inv_ctxrR2_nil: forall st τ1 τ2, *)
(*     closed_rty 0 (dom _ st) τ1 -> closed_rty 0 (dom _ st) τ2 -> *)
(*     (forall e, { st }⟦ τ1 ⟧ e -> { st }⟦ τ2 ⟧ e) -> *)
(*     inv_ctxrR2 st [] τ1 τ2 *)
(* | inv_ctxrR2_cons_over: forall st (x: atom) B n d ϕ Γ τ1 τ2, *)
(*     ok_dctx (dom _ st) ((x, {v: B | n | d | ϕ}) :: Γ) -> *)
(*     closed_rty 0 ({[x]} ∪ ctxdom ⦑Γ⦒ ∪ dom _ st) τ1 -> *)
(*     closed_rty 0 ({[x]} ∪ ctxdom ⦑Γ⦒ ∪ dom _ st) τ2 -> *)
(*     (forall (c_x: constant), {st}⟦ {v: B | n | d | ϕ} ⟧ c_x -> *)
(*                         inv_ctxrR2 (<[ x := c_x ]> st) Γ τ1 τ2) -> *)
(*     inv_ctxrR2 st ((x, {v: B | n | d | ϕ}) :: Γ) τ1 τ2 *)
(* | inv_ctxrR2_cons_under_base: forall st (x: atom) b d n ϕ τ1 τ2 Γ, *)
(*     ok_dctx (dom _ st) ((x, [v: b|n|d|ϕ]) :: Γ) -> *)
(*     closed_rty 0 ({[x]} ∪ ctxdom ⦑Γ⦒ ∪ dom _ st) τ1 -> *)
(*     closed_rty 0 ({[x]} ∪ ctxdom ⦑Γ⦒ ∪ dom _ st) τ2 -> *)
(*     (exists e_x_hat, {st}⟦ [v: b | n | d | ϕ] ⟧ e_x_hat /\ *)
(*                    (∀ (v_x: value), e_x_hat ↪* v_x -> *)
(*                                        inv_ctxrR2 ({ x ↦ v_x } st) Γ τ1 τ2)) -> *)
(*     inv_ctxrR2 st ((x, [v: b | n | d | ϕ]) :: Γ) τ1 τ2 *)
(* | inv_ctxrR2_cons_under_arr: forall st (x: atom) τ_x τ1 τ2 Γ, *)
(*     is_arr τ_x -> *)
(*     ok_dctx (dom _ st) ((x, τ_x) :: Γ) -> *)
(*     inv_ctxrR2 st Γ τ1 τ2 -> *)
(*     inv_ctxrR2 st ((x, τ_x) :: Γ) τ1 τ2. *)

(* Notation " '⅋{' st '}⟦' τ1 '⟧⊆⟦' τ2 '⟧{' Γ '}' " := (inv_ctxrR2 st Γ τ1 τ2) (at level 20, format "⅋{ st }⟦ τ1 ⟧⊆⟦ τ2 ⟧{ Γ }", st constr, τ1 constr, τ2 constr, Γ constr). *)

(* Lemma ctxrR2_implies_ctxrR: forall Γ st τ1 τ2, *)
(*     ctxrR2 st Γ τ1 τ2 -> (forall e, {st}⟦ τ1 ⟧{ Γ } e -> {st}⟦ τ2 ⟧{ Γ } e). *)
(* Proof. *)
(*   induction Γ; intros. invclear H. RD_simp. mydestr. *)
(*   invclear H. *)
(*   - RD_simp. constructor; auto. rewrite <- H10; auto. intros. auto_under c_x. *)
(*     eapply IHΓ; eauto. *)
(*   - RD_simp. invclear H0; try auto_ty_exfalso. constructor; auto. rewrite <- H10; auto. *)
(*     mydestr. *)
(*     auto_meet_exists Hmeet. auto_meet_reduce. *)
(*   - RD_simp. invclear H0; try auto_ty_exfalso. constructor; auto. rewrite <- H11; auto. *)
(*     mydestr. *)
(*     assert ({st}⟦τ1⟧⊆⟦τ2⟧{Γ}). *)
(*     { assert (∃ v : value, ({0;b∅;st}⟦r⟧) v ∧ x0 ↪* v) by (apply rR_arr_halt; auto). *)
(*       mydestr. *)
(*       eapply H2 in H1; eauto. *)
(*       erewrite state_insert_closed_value_arr in H1; eauto. refinement_solver. } *)
(*     exists x. split; auto. intros. *)
(*     assert (∃ v : value, ({0;b∅;st}⟦r⟧) v ∧ e_x ↪* v) by (apply rR_arr_halt; auto). *)
(*     mydestr. *)
(*     apply H0 with (v_x := x1) in H16; auto. *)
(*     erewrite state_insert_closed_value_arr in H16; eauto. *)
(*     erewrite state_insert_closed_value_arr; eauto. *)


(*     specialize (H0 _ H9). *)
(*     intros. auto_under v_x. eapply IHΓ; eauto. *)
(* Qed. *)


(*     apply H0 . *)
(*     intros. *)


(*     apply H16 in H1. eapply IHΓ; eauto. *)
(*     apply H0 with (v_x := v_x) in H1. *)
(*     eapply IHΓ; eauto. apply H0 with. *)

(*     auto_under c_x. *)
(*     eapply IHΓ; eauto. *)



(*       eapply let_store_typable; eauto. refinement_solver. *)


(*      Ltac ctxrR_shadow_update_st_tac_c := *)
(*   match goal with *)
(*   | [H: context [{0;b∅;<[?a0 := ?c]> ?st}⟦?τ⟧ _], *)
(*         H': ({0;b∅;?st}⟦?τ⟧) _ |- ?ret ] => *)
(*       (* idtac H' *) *)
(*       (rewrite <- lc_rR_shadow_update_st with (a:=a0) (c:=c) in H' *)
(*        ;eauto; refinement_solver7); *)
(*       match ret with *)
(*       | context [({_↦?v_x}) _] => eapply H with (v_x := v_x) in H'; eauto *)
(*       | _ => apply H in H' *)
(*       end *)
(*   end. *)

(* Lemma ctxrR_shadow_update_st_c: forall Γ st τ, *)
(*     closed_rty 0 (ctxdom ⦑ Γ ⦒ ∪ dom aset st) τ -> *)
(*     ok_dctx (dom aset st) Γ -> *)
(*     (forall (a: atom) c e, a ∉ (ctxdom Γ ∪ dom aset st) -> *)
(*                       ({<[a:=c]> st}⟦τ⟧{Γ}) e <-> ({st}⟦τ⟧{Γ}) e *)
(*     ). *)
(* Proof. *)
(*   induction Γ; split; simpl; intros; mydestr; subst. *)
(*   - invclear H2. constructor; auto; intros. *)
(*     rewrite <- rR_shadow_update_st_c; eauto; refinement_solver. *)
(*   - invclear H2. constructor; auto; intros. *)
(*     rewrite rR_shadow_update_st_c; eauto; refinement_solver. *)
(*   - invclear H0; invclear H2; mydestr; try auto_ty_exfalso4. *)
(*     + constructor; auto. constructor; auto. *)
(*       intros. *)
(*       ctxrR_shadow_update_st_tac_c. *)
(*       setoid_rewrite insert_commute in H0; try fast_set_solver. *)
(*       rewrite IHΓ in H0; auto; denotation_simp. *)
(*       refinement_solver. *)
(*     + constructor; auto. denotation_simp. constructor; auto. *)
(*       exists x. split. intros. rewrite <- rR_shadow_update_st_c; eauto. refinement_solver. *)
(*       intros; mydestr. *)
(*       ctxrR_shadow_update_st_tac_c. *)
(*       assert ([] ⊢t v_x ⋮v x0) by refinement_solver. denotation_simp2. *)
(*       rewrite subst_st_insert_commute in H3; try fast_set_solver. *)
(*       rewrite IHΓ in H3; auto; denotation_simp. *)
(*       refinement_solver. *)
(*     + apply ctxrR_cons_under_arr; auto. denotation_simp. apply ok_dctx_cons_arr; auto. *)
(*       exists x. split. intros. rewrite <- rR_shadow_update_st_c; eauto. refinement_solver. *)
(*       intros; mydestr. *)
(*       ctxrR_shadow_update_st_tac_c. *)
(*       assert ([] ⊢t v_x ⋮v ⌊r⌋) by refinement_solver. denotation_simp2. *)
(*       rewrite IHΓ in H3; auto; refinement_solver. *)
(*   - invclear H0; invclear H2; mydestr; try auto_ty_exfalso4. *)
(*     + constructor; auto; denotation_simp. refinement_solver. ok_dctx_solver_slow. *)
(*       intros. specialize (H16 c_x). *)
(*       assert ({0;b∅; st}⟦{v:B|0|d|ϕ}⟧ c_x) by *)
(*         (rewrite <- lc_rR_shadow_update_st_c with (a:=a0) (c:=c); eauto; refinement_solver). *)
(*       apply H16 in H3. *)
(*       setoid_rewrite insert_commute; refinement_solver. *)
(*       rewrite IHΓ; auto; denotation_simp; refinement_solver. *)
(*     + invclear H7; mydestr. *)
(*       constructor; auto. denotation_simp. refinement_solver. ok_dctx_solver_slow. *)
(*       exists x. split. rewrite rR_shadow_update_st_c; eauto; refinement_solver. *)
(*       intros; mydestr. *)
(*       rewrite lc_rR_shadow_update_st_c in H7; auto; try refinement_solver3. *)
(*       eapply H2 with (v_x := v_x) in H7; eauto. *)
(*       assert ([] ⊢t v_x ⋮v x0) by refinement_solver. denotation_simp. *)
(*       setoid_rewrite insert_commute; denotation_simp. *)
(*       rewrite IHΓ; auto; denotation_simp; refinement_solver. *)
(*     + invclear H7; mydestr. *)
(*       apply ctxrR_cons_under_arr; auto. refinement_solver. ok_dctx_solver_slow. *)
(*       exists x. split. intros. rewrite rR_shadow_update_st_c; eauto; refinement_solver. *)
(*       intros; mydestr. *)
(*       rewrite lc_rR_shadow_update_st_c in H7; auto; try refinement_solver. *)
(*       eapply H2 with (v_x := v_x) in H7; eauto. *)
(*       assert ([] ⊢t v_x ⋮v ⌊r⌋) by refinement_solver. denotation_simp. *)
(*       rewrite IHΓ; auto; denotation_simp; refinement_solver. *)
(* Qed. *)

(* Lemma ctxrR_shadow_update_st: forall Γ st τ, *)
(*     closed_rty 0 (ctxdom ⦑ Γ ⦒ ∪ dom aset st) τ -> *)
(*     ok_dctx (dom aset st) Γ -> *)
(*     (forall (a: atom) c e, a ∉ (ctxdom Γ ∪ dom aset st) -> closed_value c -> *)
(*                       ({ {a↦c} st}⟦τ⟧{Γ}) e <-> ({st}⟦τ⟧{Γ}) e *)
(*     ). *)
(* Proof. *)
(*   intros. apply state_insert_closed_value with (a:=a) (v:=c) (st:=st) in H2; eauto. *)
(*   destruct H2; mydestr; subst; auto; try (rewrite H2; auto). *)
(*   apply ctxrR_shadow_update_st_c; auto. *)
(* Qed. *)

(* Lemma termR_perserve_ctxrR: forall Γ τ (e e': tm), *)
(*     not_overbasety τ -> *)
(*     valid_rty τ -> *)
(*     e <-<{ ⌊ Γ ⌋* ;  ⌊ τ ⌋ } e' -> (forall st, {st}⟦τ⟧{Γ} e -> {st}⟦τ⟧{Γ} e'). *)
(* Proof. *)
(*   induction Γ; intros; invclear H2. *)
(*   - constructor. intros. eapply termR_perserve_rR; eauto. *)
(*   - constructor; auto. termR_solver. *)
(*     intros. apply IHΓ with (e:= ({x := c_x }t e)); eauto. denotation_simp. *)
(*     apply termR_tm_subst with (Tx := B); eauto. refinement_solver. denotation_simp3. *)
(*     termR_solver. *)
(*   - constructor; auto. termR_solver. *)
(*     destruct H13 as (e_x_hat & He_x_hat & HH). exists e_x_hat. split; auto. *)
(*     intros. apply IHΓ with (e:= (tlete e_x (x \t\ e))); eauto. *)
(*     apply termR_elete with (Tx := ⌊ τ_x ⌋ ); auto. constructor; auto; refinement_solver. *)
(*     simpl in H1. termR_solver. *)
(*   - apply ctxrR_cons_under_arr; auto. termR_solver. *)
(*     destruct H12 as (e_x_hat & He_x_hat & HH). exists e_x_hat. split; auto. *)
(*     intros. apply IHΓ with (e:= (tlete e_x (x \t\ e))); eauto. *)
(*     apply termR_elete with (Tx := ⌊ τ_x ⌋ ); auto. constructor; auto; refinement_solver. *)
(*     simpl in H1. termR_solver. *)
(* Qed. *)

(* Lemma ctxrR_err_overbase_forall_forward: forall Γ st τ x b n d ϕ, *)
(*     ¬ ({st}⟦τ⟧{(x, {v:b|n|d|ϕ}) :: Γ}) terr -> *)
(*     ok_dctx (dom aset st) ((x, {v:b|n|d|ϕ}) :: Γ) -> *)
(*     closed_rty 0 ({[x]} ∪ ctxdom ⦑Γ⦒ ∪ dom aset st) τ -> *)
(*     not_overbasety τ -> *)
(*     (exists (c: constant), {st}⟦{v:b|n|d|ϕ}⟧ c /\ ¬ ({<[x:=c]> st}⟦τ⟧{Γ}) terr). *)
(* Proof. *)
(*   intros. pbc. apply H. clear H. neg_simpl. *)
(*   constructor; auto; refinement_solver2. *)
(*   (* intros. *) *)
(*   (* assert (({<[x:=c_x]> st}⟦τ⟧{Γ}) terr); auto. *) *)
(*   (* apply termR_perserve_ctxrR with (e:= terr); auto; refinement_solver2. *) *)
(*   (* apply tyable_implies_terr_termR. *) *)
(*   (* econstructor; eauto; refinement_solver2; mydestr. *) *)
(*   (* apply basic_typing_weaken_value_empty; eauto. refinement_solver2. *) *)
(*   (* auto_exists_L_intros. do 2 constructor; refinement_solver2. *) *)
(* Qed. *)

(* Lemma ctxrR_err_overbase_forall_backward: forall Γ st τ x b n d ϕ, *)
(*     ok_dctx (dom aset st) ((x, {v:b|n|d|ϕ}) :: Γ) -> *)
(*     closed_rty 0 ({[x]} ∪ ctxdom ⦑Γ⦒ ∪ dom aset st) τ -> *)
(*     not_overbasety τ -> *)
(*     (exists (c: constant), {st}⟦{v:b|n|d|ϕ}⟧ c /\ ¬ ({<[x:=c]> st}⟦τ⟧{Γ}) terr) -> *)
(*     ¬ ({st}⟦τ⟧{(x, {v:b|n|d|ϕ}) :: Γ}) terr. *)
(* Proof. *)
(*   intros. mydestr. intro Hf. apply H3. clear H3. *)
(*   invclear Hf; try invclear H6. *)
(*   apply termR_perserve_ctxrR with (e:= (tlete x0 (x \t\ terr))); auto; refinement_solver2. *)
(*   eapply tyable_implies_terr_termR_terr; refinement_solver2. *)
(*   (* assert (({<[x:=x0]> st}⟦τ⟧{Γ}) terr); auto. constructor. *) *)
(*   apply termR_perserve_ctxrR with (e:= terr); auto; refinement_solver2. *)
(*   apply tyable_implies_terr_termR. *)
(*   apply let_store_typable with (Tu:=b); eauto. *)
(*   refinement_solver; basic_typing_solver. *)
(*   rewrite basic_has_type_head_to_tail_tm in H14; auto. *)
(* Qed. *)

(* Lemma ctxrR_err_overbase_forall: forall Γ st τ x b n d ϕ, *)
(*     ok_dctx (dom aset st) ((x, {v:b|n|d|ϕ}) :: Γ) -> *)
(*     closed_rty 0 ({[x]} ∪ ctxdom ⦑Γ⦒ ∪ dom aset st) τ -> *)
(*     not_overbasety τ -> *)
(*     ¬ ({st}⟦τ⟧{(x, {v:b|n|d|ϕ}) :: Γ}) terr <-> *)
(*       (exists (c: constant), {st}⟦{v:b|n|d|ϕ}⟧ c /\ ¬ ({<[x:=c]> st}⟦τ⟧{Γ}) terr). *)
(* Proof. *)
(*   split; intros. *)
(*   - eapply ctxrR_err_overbase_forall_forward; eauto. *)
(*   - eapply ctxrR_err_overbase_forall_backward; eauto. *)
(* Qed. *)

(* Lemma ctxrR_err_underbase_forall_forward: forall Γ st τ x b n d ϕ, *)
(*     ¬ ({st}⟦τ⟧{(x, [v:b|n|d|ϕ]) :: Γ}) terr -> *)
(*     ok_dctx (dom aset st) ((x, [v:b|n|d|ϕ]) :: Γ) -> *)
(*     closed_rty 0 ({[x]} ∪ ctxdom ⦑Γ⦒ ∪ dom aset st) τ -> *)
(*     not_overbasety τ -> *)
(*     (forall e_x_hat, {st}⟦[v:b|n|d|ϕ]⟧ e_x_hat -> *)
(*                 (exists (c: constant), [] ⊢t c ⋮v b /\ e_x_hat ↪* c /\ ¬ ({<[x:=c]> st}⟦τ⟧{Γ}) terr) *)
(*     ). *)
(* Proof. *)
(*   intros. pbc. apply H. clear H. neg_simpl. *)
(*   constructor; auto; refinement_solver2. *)
(*   exists (e_x_hat). split; auto. intros. denotation_simp1. *)
(*   apply termR_perserve_ctxrR with (e:= terr); auto; refinement_solver2. *)
(*   - apply tyable_implies_terr_termR. econstructor; eauto; refinement_solver2; mydestr. *)
(*     auto_exists_L_intros. do 2 constructor; refinement_solver2. *)
(*   - denotation_simp3. *)
(* Qed. *)

(* Lemma ctxrR_err_underbase_forall_backward: forall Γ st τ x b n d ϕ, *)
(*     ok_dctx (dom aset st) ((x, [v:b|n|d|ϕ]) :: Γ) -> *)
(*     closed_rty 0 ({[x]} ∪ ctxdom ⦑Γ⦒ ∪ dom aset st) τ -> *)
(*     not_overbasety τ -> *)
(*     (forall e_x_hat, {st}⟦[v:b|n|d|ϕ]⟧ e_x_hat -> *)
(*                 (exists (c: constant), [] ⊢t c ⋮v b /\ e_x_hat ↪* c /\ ¬ ({<[x:=c]> st}⟦τ⟧{Γ}) terr) *)
(*     ) -> *)
(*     ¬ ({st}⟦τ⟧{(x, [v:b|n|d|ϕ]) :: Γ}) terr. *)
(* Proof. *)
(*   intros. intro Hf. invclear Hf; mydestr; denotation_simp3. *)
(*   assert (∃ c : constant, [] ⊢t c ⋮v x1 ∧ x0 ↪* c ∧ ¬ ({<[x:=c]> st}⟦τ⟧{Γ}) terr); auto. *)
(*   mydestr. apply H11. clear H11. eapply H4 in H3; eauto. *)
(*   apply termR_perserve_ctxrR with (e:= (tlete x0 (x \t\ terr))); auto; refinement_solver2; simpl. *)
(*   eapply tyable_implies_terr_termR_terr; refinement_solver2. *)
(* Qed. *)

(* Lemma ctxrR_err_underbase_forall: forall Γ st τ x b n d ϕ, *)
(*     ok_dctx (dom aset st) ((x, [v:b|n|d|ϕ]) :: Γ) -> *)
(*     closed_rty 0 ({[x]} ∪ ctxdom ⦑Γ⦒ ∪ dom aset st) τ -> *)
(*     not_overbasety τ -> *)
(*     ¬ ({st}⟦τ⟧{(x, [v:b|n|d|ϕ]) :: Γ}) terr <-> *)
(*       (forall e_x_hat, {st}⟦[v:b|n|d|ϕ]⟧ e_x_hat -> *)
(*                   (exists (c: constant), [] ⊢t c ⋮v b /\ e_x_hat ↪* c /\ ¬ ({<[x:=c]> st}⟦τ⟧{Γ}) terr) *)
(*       ). *)
(* Proof. *)
(*   split; intros. *)
(*   - eapply ctxrR_err_underbase_forall_forward; eauto. *)
(*   - eapply ctxrR_err_underbase_forall_backward; eauto. *)
(* Qed. *)

(* Ltac reduction_solver2 := *)
(*   RD_simp; repeat (reduction_solver1 || refinement_solver5). *)

(* Lemma not_empty_ctxrR_inhabitant_err_implies_halt: forall τ st, *)
(*     valid_rty τ -> ~ ({st}⟦τ⟧{ [] }) terr -> ∀ e, ({st}⟦τ⟧) e -> (exists (v: value), e ↪* v). *)
(* Proof. *)
(*   intros. *)
(*   eapply rR_inhabitant_err_or_halt in H. destruct H; eauto. *)
(*   exfalso_apply H0. RD_simp; auto. *)
(* Qed. *)

(* Lemma ctx_trans_terr_inhabitant: forall Γ st τ x τ_x, *)
(*     not_overbasety τ -> *)
(*     ok_dctx (dom aset st) ((x, τ_x) :: Γ) -> *)
(*     ¬ ({st}⟦ τ ⟧{(x, τ_x) :: Γ}) terr -> ¬ ({st}⟦ τ ⟧{ Γ }) terr. *)
(* Proof. *)
(*   intros. neg_apply H1; RD_simp; denotation_simp1. *)
(*   destruct (classic (not_overbasety τ_x)); invclear H0; auto_ty_exfalso; denotation_simp. *)
(*   - constructor; refinement_solver6. *)
(*     exists (random_inhabitant ([v:x0|0|x2|x3])). *)
(*     assert (({0;b∅;st}⟦ ([v:x0|0|x2|x3]) ⟧) (random_inhabitant ([v:x0|0|x2|x3]))) by *)
(*       (apply random_inhabitant_in_any_under; refinement_solver5). *)
(*     split; auto. intros. *)
(*     assert ([] ⊢t (random_inhabitant ([v:x0|0|x2|x3])) ⋮t ⌊ ([v:x0|0|x2|x3]) ⌋); auto. *)
(*     assert ([] ⊢t v_x ⋮v x0); reduction_solver2. *)
(*     assert (({st}⟦τ⟧{Γ}) (tlete e_x (x \t\ terr))). *)
(*     apply termR_perserve_ctxrR with (e:= terr); refinement_solver6. *)
(*     apply tyable_implies_terr_termR. reduction_solver2. *)
(*     eapply ty_tlete_dummy; refinement_solver6. *)
(*     setoid_rewrite ctxrR_shadow_update_st; auto; refinement_solver. *)
(*   - apply ctxrR_cons_under_arr; auto; refinement_solver. *)
(*     exists (random_inhabitant τ_x). *)
(*     assert (({0;b∅;st}⟦ τ_x ⟧) (random_inhabitant τ_x)) by *)
(*       (apply random_inhabitant_in_any_under; refinement_solver5). *)
(*     split; auto. intros. assert ([] ⊢t v_x ⋮v ⌊ τ_x ⌋ ) by reduction_solver2. *)
(*     denotation_simp3. *)
(*     apply termR_perserve_ctxrR with (e:= terr); refinement_solver6. *)
(*     apply tyable_implies_terr_termR. reduction_solver2. *)
(*     assert (ok ⌊Γ⌋* ) by refinement_solver. *)
(*     eapply ty_tlete_dummy; refinement_solver6. basic_typing_solver. *)
(*   - invclear H0; auto_ty_exfalso3. constructor; refinement_solver6. *)
(*     intros. reduction_simpl1. *)
(*     setoid_rewrite ctxrR_shadow_update_st_c; auto; refinement_solver. *)
(* Qed. *)

(* Lemma is_arr_simpl_rewrite_ctxrR: forall r a st τ Γ e, *)
(*     is_arr r -> *)
(*     closed_rty 0 (dom aset st) r -> *)
(*     (∀ e_wf : tm, ({0;b∅;st}⟦r⟧) e_wf → ∃ v_wf : value, e_wf ↪* v_wf) -> *)
(*     (∃ e_x_hat : tm, ({0;b∅;st}⟦r⟧) e_x_hat *)
(*                      ∧ (∀ e_x : tm, ({0;b∅;st}⟦r⟧) e_x → *)
(*                                     ∀ v_x : value, e_x_hat ↪* v_x → *)
(*                                                    ({({a↦v_x}) st}⟦τ⟧{Γ}) (tlete e_x e))) <-> *)
(*       (∀ e_x : tm, ({0;b∅;st}⟦r⟧) e_x → ({st}⟦τ⟧{Γ}) (tlete e_x e)). *)
(* Proof. *)
(*   intros. *)
(*   eapply is_arr_simpl_rewrite with (P:= fun st e_x => ({st}⟦τ⟧{Γ}) (tlete e_x e)) in H1; eauto. *)
(* Qed. *)

(* Lemma is_arr_simpl_rewrite_wf_ctxrR_value: forall r a st Γ, *)
(*     is_arr r -> *)
(*     closed_rty 0 (dom aset st) r -> *)
(*     (∀ e_wf : tm, ({0;b∅;st}⟦r⟧) e_wf → ∃ v_wf : value, e_wf ↪* v_wf) -> *)
(*     (∃ e_x_hat : tm, ({0;b∅;st}⟦r⟧) e_x_hat *)
(*                      ∧ (∀ e_x : value, ({0;b∅;st}⟦r⟧) e_x → ∀ v_x : value, e_x_hat ↪* v_x → *)
(*                                                                         wf_ctxrR ({a↦v_x} st) Γ)) <-> *)
(*       (∀ e_x : value, ({0;b∅;st}⟦r⟧) e_x → wf_ctxrR st Γ). *)
(* Proof. *)
(*   intros. *)
(*   eapply is_arr_simpl_rewrite_value with (P:= fun st _ => wf_ctxrR st Γ) in H1; eauto. *)
(* Qed. *)

(* Lemma is_arr_simpl_rewrite_ctxrR_value: forall r a st τ Γ e, *)
(*     is_arr r -> *)
(*     closed_rty 0 (dom aset st) r -> *)
(*     (∀ e_wf : tm, ({0;b∅;st}⟦r⟧) e_wf → ∃ v_wf : value, e_wf ↪* v_wf) -> *)
(*     (∃ e_x_hat : tm, ({0;b∅;st}⟦r⟧) e_x_hat *)
(*                      ∧ (∀ e_x : value, ({0;b∅;st}⟦r⟧) e_x → *)
(*                                     ∀ v_x : value, e_x_hat ↪* v_x → *)
(*                                                    ({({a↦v_x}) st}⟦τ⟧{Γ}) (tlete e_x e))) <-> *)
(*       (∀ e_x : value, ({0;b∅;st}⟦r⟧) e_x → ({st}⟦τ⟧{Γ}) (tlete e_x e)). *)
(* Proof. *)
(*   intros. *)
(*   eapply is_arr_simpl_rewrite_value with (P:= fun st e_x => ({st}⟦τ⟧{Γ}) (tlete e_x e)) in H1; eauto. *)
(* Qed. *)

(* Ltac is_arr_simp := *)
(*   repeat match goal with *)
(*     | [H: ∃ e_x_hat, ({0;b∅;?st}⟦?r⟧) e_x_hat /\ *)
(*                        (∀ e_x, ({0;b∅;?st}⟦?r⟧) e_x -> ∀ v_x, e_x_hat ↪* (tvalue v_x) -> wf_ctxrR _ _) |- _] => *)
(*         rewrite is_arr_simpl_rewrite_wf_ctxrR in H; auto; refinement_solver7 *)
(*     | [H: ∃ e_x_hat, ({0;b∅;?st}⟦?r⟧) e_x_hat /\ *)
(*                        (∀ e_x, ({0;b∅;?st}⟦?r⟧) e_x -> ∀ v_x, e_x_hat ↪* (tvalue v_x) -> {({?a↦v_x}) ?st}⟦_⟧{_} _) |- _] => *)
(*         rewrite is_arr_simpl_rewrite_ctxrR in H; auto; refinement_solver7 *)
(*     | [|- ∃ e_x_hat, ({0;b∅;?st}⟦?r⟧) e_x_hat /\ *)
(*                       (∀ e_x, ({0;b∅;?st}⟦?r⟧) e_x -> ∀ v_x, e_x_hat ↪* (tvalue v_x) -> {({?a↦v_x}) ?st}⟦_⟧{_} _)] => *)
(*         rewrite is_arr_simpl_rewrite_ctxrR; auto; refinement_solver7 *)

(*     | [H: ∃ e_x_hat, ({0;b∅;?st}⟦?r⟧) e_x_hat /\ *)
(*                        (∀ e_x, ({0;b∅;?st}⟦?r⟧) (tvalue e_x) -> ∀ v_x, e_x_hat ↪* (tvalue v_x) -> wf_ctxrR _ _) |- _] => *)
(*         rewrite is_arr_simpl_rewrite_wf_ctxrR_value in H; auto; refinement_solver7 *)
(*     | [H: ∃ e_x_hat, ({0;b∅;?st}⟦?r⟧) e_x_hat /\ *)
(*                        (∀ e_x, ({0;b∅;?st}⟦?r⟧) (tvalue e_x) -> ∀ v_x, e_x_hat ↪* (tvalue v_x) -> {({?a↦v_x}) ?st}⟦_⟧{_} _) |- _] => *)
(*         rewrite is_arr_simpl_rewrite_ctxrR_value in H; auto; refinement_solver7 *)
(*     | [|- ∃ e_x_hat, ({0;b∅;?st}⟦?r⟧) e_x_hat /\ *)
(*                       (∀ e_x, ({0;b∅;?st}⟦?r⟧) (tvalue e_x) -> ∀ v_x, e_x_hat ↪* (tvalue v_x) -> {({?a↦v_x}) ?st}⟦_⟧{_} _)] => *)
(*         rewrite is_arr_simpl_rewrite_ctxrR_value; auto; refinement_solver7 *)
(*     end. *)

(* Lemma rRctx_pre_weakening: forall Γ1 Γ2 st τ, *)
(*     not_overbasety τ -> *)
(*     wf_ctxrR st (Γ1 ++ Γ2) -> *)
(*     (forall e, {st}⟦τ⟧{Γ1} e -> {st}⟦τ⟧{Γ1 ++ Γ2} e). *)
(* Proof. *)
(*   induction Γ1; intros; RD_simp; denotation_simp. *)
(*   - apply rRctx_pre_weakening_empty; auto. *)
(*   - invclear H1; invclear H0; auto_ty_exfalso. *)
(*     + constructor; refinement_solver. basic_typing_solver7. ctx_erase_simp5. *)
(*     + denotation_simp. invclear H9; try auto_ty_exfalso. *)
(*       constructor; auto. refinement_solver. ctx_erase_simp5. basic_typing_solver7. ctx_erase_simp5. *)
(*       auto_meet_exists Hmeet. auto_meet_reduce. *)
(*     + invclear H9; auto_ty_exfalso4. apply ctxrR_cons_under_arr; auto. *)
(*       refinement_solver. *)
(*       ctx_erase_simp. basic_typing_solver7. listctx_set_simpl. rewrite ok_pre_destruct. split; refinement_solver7. *)
(*       is_arr_simp. *)
(* Qed. *)

(* Lemma rRctx_pre_weakening_empty: forall Γ st τ, *)
(*     not_overbasety τ -> *)
(*     wf_ctxrR st Γ -> *)
(*     (forall e, {st}⟦τ⟧ e -> {st}⟦τ⟧{Γ} e). *)
(* Proof. *)
(*   induction Γ; intros; RD_simp; denotation_simp. *)
(*   invclear H0. *)
(*   - invclear H5; auto_ty_exfalso. constructor; refinement_solver7. *)
(*     intros. apply IHΓ; auto. *)
(*     rewrite subst_fresh_tm; eauto. *)
(*     rewrite rR_shadow_update_st_c; refinement_solver. *)
(*   - constructor; refinement_solver7. mydestr. auto_meet_exists Hmeet. *)
(*     apply IHΓ; eauto. *)
(*     rewrite rR_shadow_update_st; refinement_solver. *)
(*   - apply ctxrR_cons_under_arr; refinement_solver7. *)
(*      auto_meet_exists Hmeet. invclear H7; auto_ty_exfalso. *)
(*      rewrite ctxrR_shadow_update_st; refinement_solver. *)
(* Qed. *)
