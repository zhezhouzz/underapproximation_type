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

Global Hint Resolve mk_eq_constant_is_not_overbasety: core.
Global Hint Resolve mk_eq_var_is_not_overbasety: core.
Global Hint Resolve is_arr_implies_not_overbasety: core.
Global Hint Constructors ok_dctx: core.
Global Hint Resolve rR_implies_no_free: core.
Global Hint Resolve ctxrR_tlete_drop_halt_lhs: core.
Global Hint Resolve rR_implies_reduction_no_free: core.
Global Hint Resolve is_arr_implies_not_overbasety: core.
Global Hint Resolve under_not_overbasety: core.

Lemma has_basety_implies_constant_or_var: forall Γ (b: base_ty) (v: value),
    Γ ⊢t v ⋮v b -> ((exists (n: constant), v = n) \/ (exists (y: atom), v = y)).
Proof.
  intros.
  destruct v.
  + left. eexists; eauto.
  + right. eexists; eauto.
  + invclear H.
  + invclear H.
  + invclear H.
Qed.

Lemma inv_basety_implies_constant_or_var: forall Γ st b n d ϕ (v: value),
    (⅋{st}⟦[v:b|n|d|ϕ]⟧{Γ}) v -> ((exists (n: constant), v = n) \/ (exists (y: atom), v = y)).
Proof.
  intros. apply inv_ctxrR_regular in H; mydestr; eauto. invclear H.
  eapply has_basety_implies_constant_or_var; eauto.
Qed.

(* Lemma state_subst_implies_rty_subst_eq2_forward_closed_rty *)
(*     : ∀ (a v : atom) (Γ : list (atom * rty)) (L : aset) (c_x : constant) (x : atom) *)
(*       (k : nat) (b : base_ty) (n : nat) (d : aset) (ϕ : refinement), *)
(*   not_fv_in_refinement d ϕ -> a ∈ L -> *)
(*   bound_in_refinement (n `min` k) (refinement_open k c_x ϕ) -> *)
(*   closed_rty 0 L ({k ~r> v} [v:b|n|d|ϕ]) -> *)
(*   closed_rty 0 L ({k ~r> {a := c_x }v v} [v:b|n|d|ϕ]). *)
(* Proof. *)
(*   intros. destruct (decide (a = v)); subst; auto. *)
(*   assert ({v := c_x }v v = c_x) as Htmp. simpl; dec_solver2. rewrite Htmp. clear Htmp. *)
(*   invclear H2; auto_ty_exfalso. constructor; auto. *)
(*   invclear H3. constructor; auto. invclear H6. constructor; auto. *)
(*   + unfold not_fv_in_refinement. intros. unfold refinement_open. *)
(*     unfold not_fv_in_refinement in H2. unfold refinement_open in H2. *)
(*     unfold refinement_set_open in H6. *)
(*     apply H2 in H6. *)

(*     unfold not_fv_in_refinement in H2. intros. *)
(*     apply H2. intros. apply H7; auto. *)

(*     invclear H2. constructor; auto. invclear H7. constructor; auto. *)
(*       unfold not_fv_in_refinement. intros. apply H. intros. apply H7; auto. *)
(*     + invclear H3. constructor; auto. *)
(*     + simpl in H4. simpl. set_solver. *)

Lemma state_subst_implies_rty_subst_eq2_forward_ok_dctx
    : ∀ (a v : atom) (Γ : list (atom * rty)) (L : aset) (c_x : constant) (x : atom)
      (k : nat) (b : base_ty) (n : nat) (d : aset) (ϕ : refinement),
  not_fv_in_refinement d ϕ -> a ∈ L ->
  bound_in_refinement (n `min` k) (refinement_open k c_x ϕ) ->
  ok_dctx L (Γ ++ [(x, {k ~r> v} [v:b|n|d|ϕ])]) ->
  ok_dctx L (Γ ++ [(x, {k ~r> {a := c_x }v v} [v:b|n|d|ϕ])]).
Proof.
  intros a v. induction Γ; intros.
  - denotation_simp. destruct (decide (a = v)); subst; auto.
    invclear H2; auto_ty_exfalso. constructor; auto.
    invclear H7. constructor; auto.
    + invclear H2. constructor; auto. invclear H7. constructor; auto.
      unfold not_fv_in_refinement. intros. apply H. intros. apply H7; auto.
    + invclear H3. constructor; auto.
    + simpl in H4. simpl. set_solver.
  - mydestr. invclear H2; auto_ty_exfalso.
    + constructor; auto; fold (@app (atom * rty)). listctx_set_simpl.
      eapply IHΓ; eauto. set_solver.
    + apply ok_dctx_cons_arr; auto; fold (@app (atom * rty)). listctx_set_simpl.
Qed.

Lemma state_subst_implies_rty_subst_eq2_forward
  : ∀ (a v : atom) (Γ : list (atom * rty)) (st : state) (c_x : constant) (τ : rty) (x : atom)
      (k : nat) (b : base_ty) (n : nat) (d : aset) (ϕ : refinement) (e : tm),
    ok_dctx (dom aset (<[a:=c_x]> st)) (Γ ++ [(x, {k ~r> {a := c_x }v v} [v:b|n|d|ϕ])]) ->
    (⅋{<[a:=c_x]> st}⟦τ⟧{Γ ++ [(x, {k ~r> v} [v:b|n|d|ϕ])] }) e ->
    (⅋{<[a:=c_x]> st}⟦τ⟧{Γ ++ [(x, {k ~r> {a := c_x }v v} [v:b|n|d|ϕ])] }) e.
Proof.
  intros.
  assert (ok_dctx (dom aset (<[a:=c_x]> st)) (Γ ++ [(x, {k ~r> {a := c_x }v v} [v:b|n|d|ϕ])]) /\
         (⅋{<[a:=c_x]> st}⟦τ⟧{Γ ++ [(x, {k ~r> v} [v:b|n|d|ϕ])] }) e). split; auto.
  rewrite <- state_subst_implies_rty_subst_eq2 in H1; mydestr; auto.
Qed.

Lemma state_subst_implies_rty_subst_eq2_backward
  : ∀ (a v : atom) (Γ : list (atom * rty)) (st : state) (c_x : constant) (τ : rty) (x : atom)
      (k : nat) (b : base_ty) (n : nat) (d : aset) (ϕ : refinement) (e : tm),
    ok_dctx (dom aset (<[a:=c_x]> st)) (Γ ++ [(x, {k ~r> v} [v:b|n|d|ϕ])]) ->
    (⅋{<[a:=c_x]> st}⟦τ⟧{Γ ++ [(x, {k ~r> {a := c_x }v v} [v:b|n|d|ϕ])] }) e ->
    (⅋{<[a:=c_x]> st}⟦τ⟧{Γ ++ [(x, {k ~r> v} [v:b|n|d|ϕ])] }) e.
Proof.
  intros.
  assert (ok_dctx (dom aset (<[a:=c_x]> st)) (Γ ++ [(x, {k ~r> v} [v:b|n|d|ϕ])]) /\
         (⅋{<[a:=c_x]> st}⟦τ⟧{Γ ++ [(x, {k ~r> {a := c_x }v v} [v:b|n|d|ϕ])]  }) e). split; auto.
  rewrite state_subst_implies_rty_subst_eq2 in H1; mydestr; auto.
Qed.

Require Import Coq.Logic.FunctionalExtensionality.

Lemma bstate_insert_commute: forall n1 n2 c0 c x,
    n1 <> n2 ->
  (<b[n2:=c0]> (<b[n1:=c]> x)) = (<b[n1:=c]> (<b[n2:=c0]> x)).
Proof.
  unfold bstate_insert. intros.
  apply functional_extensionality; intros.
  dec_solver2.
Qed.

Lemma rty_open_commute: forall n1 n2 (v1 v2: value) b d n ϕ,
    lc v1 -> lc v2 -> n1 <> n2 ->
    {n1 ~r> v1} ({n2 ~r> v2} [v:b|d|n|ϕ]) = {n2 ~r> v2} ({n1 ~r> v1} [v:b|d|n|ϕ]).
Proof.
  intros. simpl. assert ((d `min` n2) `min` n1 = (d `min` n1) `min` n2). lia. rewrite H2. clear H2.
  assert (refinement_set_open n1 v1 (refinement_set_open n2 v2 n) = refinement_set_open n2 v2 (refinement_set_open n1 v1 n)).
  unfold refinement_set_open.
  destruct v1; destruct v2; auto. set_solver.
  rewrite H2. clear H2.
  assert (refinement_open n1 v1 (refinement_open n2 v2 ϕ) = refinement_open n2 v2 (refinement_open n1 v1 ϕ)).
  unfold refinement_open.
  do 3 (apply functional_extensionality; intros).
  destruct v1; destruct v2; try destruct (x0 !! atom); try destruct (x0 !! atom0); auto;
  try (rewrite bstate_insert_commute; auto).
  rewrite H2; auto.
Qed.

Lemma rty_open_commute_op: forall n1 n2 (v1 v2: value) op,
    lc v1 -> lc v2 -> n1 <> n2 ->
    {n1 ~r> v1} ({n2 ~r> v2} (mk_op_ret op)) = {n2 ~r> v2} ({n1 ~r> v1} (mk_op_ret op)).
Proof.
  intros. unfold mk_op_ret. rewrite rty_open_commute; auto.
Qed.

Lemma tletbiop_aux1: forall op (x1:atom),
    not_fv_in_refinement ({[x1]} ∪ ∅)
    (refinement_open 0 x1 (λ (bst : bstate) (_ : state) (v : constant), eval_op op (bst 0) (bst 1) v)).
Proof.
  intros. unfold not_fv_in_refinement. intros. unfold refinement_open.
  assert (m !! x1 = m' !! x1). apply H. set_solver.
  rewrite H0.
  destruct (m' !! x1); auto.
Qed.

Lemma tletbiop_aux2: forall op (x1:atom) (c_x:constant),
  bound_in_refinement 0
    (refinement_open 1 c_x
       (refinement_open 0 x1
          (λ (bst : bstate) (_ : state) (v : constant), eval_op op (bst 0) (bst 1) v))).
Proof.
  intros. unfold bound_in_refinement. intros. unfold refinement_open.
  destruct (st !! x1); auto.
Qed.

Lemma bstate_insert_insert:
  ∀ (n1 : nat) (c0 c : constant) (x : bstate), <b[n1:=c0]> (<b[n1:=c]> x) = <b[n1:=c0]> x.
Proof.
  intros. unfold bstate_insert. apply functional_extensionality; intros.
  dec_solver2.
Qed.

Lemma subst_var_same_value: forall (x: atom) (v: value), {x := v }v x = v.
Proof. intros. simpl. dec_solver2. Qed.
Lemma subst_var_same_tm: forall (x: atom) (v: value), {x := v }t x = v.
Proof. intros. simpl. dec_solver2. Qed.
Lemma subst_var_diff_value: forall (x y: atom) (v: value), x <> y -> {x := v }v y = y.
Proof. intros. simpl. dec_solver2. Qed.
Lemma subst_var_diff_tm: forall (x y: atom) (v: value), x <> y -> {x := v }t y = y.
Proof. intros. simpl. dec_solver2. Qed.
Lemma tletbiop_aux3: forall (x1 x0: atom) (c_x: value),
    lc c_x -> lc ({x1 := c_x }v x0).
Proof. intros. simpl. dec_solver2. Qed.
Lemma tletbiop_aux4: forall (x1 x0: atom) (c_x: constant),
    lc ({x1 := c_x }v x0).
Proof. intros. apply tletbiop_aux3. auto. Qed.
Global Hint Resolve tletbiop_aux4: core.
Lemma tletbiop_aux5: forall (x1: atom) (x0 c_x: constant),
    lc ({x1 := c_x }v x0).
Proof. intros. simpl. auto. Qed.
Global Hint Resolve tletbiop_aux5: core.

Lemma subst_over_constant: forall (x: atom) (v: value) (c_x:constant), ({x := v }v c_x) = c_x.
Proof. intros. auto. Qed.

Ltac biop_simpl:=
  repeat
    match goal with
    | [H: context [{?x := ?v }v (vfvar ?x)] |- _ ] => rewrite (subst_var_same_value x v) in H
    | [H: context [{?x := ?v }t (tvalue (vfvar ?x))] |- _ ] => rewrite (subst_var_same_tm x v) in H
    | [|- context [{?x := ?v }v (vfvar ?x)] ] => rewrite (subst_var_same_value x v)
    | [|- context [{?x := ?v }t (tvalue (vfvar ?x))] ] => rewrite (subst_var_same_tm x v)
    | [H: context [{?x := ?v }v (vfvar ?y)] |- _ ] =>
        assert (x <> y) as Htmp by auto;
        rewrite (subst_var_diff_value x y v Htmp) in H;
        try clear Htmp
    | [H: context [{?x := ?v }t (tvalue (vfvar ?y))] |- _ ] =>
        assert (x <> y) as Htmp by auto;
        rewrite (subst_var_diff_tm x y v Htmp) in H;
        try clear Htmp
    | [|- context [{?x := ?v }v (vfvar ?y)] ] =>
        assert (x <> y) as Htmp by auto;
        rewrite (subst_var_diff_value x y v Htmp);
        try clear Htmp
    | [|- context [{?x := ?v }t (tvalue (vfvar ?y))] ] =>
        assert (x <> y) as Htmp by auto;
        rewrite (subst_var_diff_tm x y v Htmp);
        try clear Htmp
    | [H: context [{?x := ?v }v (vconst ?c_x) ] |- _ ] =>
        rewrite (subst_over_constant x v c_x) in H
    | [|- context [{?x := ?v }v (vconst ?c_x) ] ] =>
        rewrite (subst_over_constant x v c_x)
    end.

Lemma state_subst_implies_rty_subst_eq2_forward2
  : ∀ (a: atom) (Γ : list (atom * rty)) (st : state) (c_x : constant) (τ : rty) (x : atom)
      (k : nat) (b : base_ty) (n : nat) (d : aset) (ϕ : refinement) (e : tm),
    not_fv_in_refinement d ϕ -> bound_in_refinement (n `min` k) (refinement_open k c_x ϕ) ->
    (⅋{<[a:=c_x]> st}⟦τ⟧{Γ ++ [(x, {k ~r> a} [v:b|n|d|ϕ])] }) e ->
    (⅋{<[a:=c_x]> st}⟦τ⟧{Γ ++ [(x, {k ~r> c_x} [v:b|n|d|ϕ])] }) e.
Proof.
  intros. apply state_subst_implies_rty_subst_eq2_forward in H1; biop_simpl; auto.
  apply inv_ctxrR_regular in H1; mydestr.
  apply state_subst_implies_rty_subst_eq2_forward_ok_dctx with (a:=a) (c_x:=c_x) in H2;  biop_simpl; eauto.
  rewrite dom_insert_simp. set_solver.
Qed.

Lemma bound_in_refinement_open_minus1: forall n (v1: value) ϕ,
    ((exists (n: constant), v1 = n) \/ (exists (y: atom), v1 = y)) ->
    bound_in_refinement (S n) ϕ -> bound_in_refinement n (refinement_open n v1 ϕ).
Proof.
  unfold bound_in_refinement; unfold refinement_open. intros. destruct H; mydestr; subst.
  - apply H0. apply bst_eq_insert_S; auto.
  - destruct (st !! x).
    + apply H0. apply bst_eq_insert_S; auto.
    + apply H0. apply bst_eq_insert_S; auto.
Qed.

Lemma state_subst_implies_rty_subst_eq2_forward_closed_rty
  : ∀ (a v: atom) (L d: aset) (c_x : constant)
      (k : nat) (b : base_ty) (m n : nat) (ϕ : refinement),
    not_fv_in_refinement d ϕ -> bound_in_refinement (n `min` k) (refinement_open k c_x ϕ) ->
    closed_rty m L ({k ~r> v} [v:b|n|d|ϕ]) ->
    closed_rty m L ({k ~r> {a := c_x }v v} [v:b|n|d|ϕ]).
Proof.
  intros. destruct (decide (a = v)); subst; auto; biop_simpl; auto.
  invclear H1. constructor; auto.
  - constructor; auto. constructor; auto.
    unfold not_fv_in_refinement. unfold refinement_open. intros.
    apply H; auto.
  - invclear H3. constructor; auto.
  - simpl in H4. simpl. fast_set_solver.
Qed.

Lemma state_subst_implies_rty_subst_eq2_rR
  : ∀ (a v: atom) st bst (c_x : constant)
      (k : nat) (b : base_ty) m (n : nat) (d : aset) (ϕ : refinement) e,
    (closed_rty m (dom aset (<[a:=c_x]> st)) ({k ~r> {a := c_x }v v} [v:b|n|d|ϕ]) /\
       {m;bst;(<[a:=c_x]> st)}⟦ {k ~r> v} [v:b|n|d|ϕ]⟧ e) <->
      (closed_rty m (dom aset (<[a:=c_x]> st)) ({k ~r> v} [v:b|n|d|ϕ]) /\
         {m;bst;(<[a:=c_x]> st)}⟦ {k ~r> {a := c_x }v v} [v:b|n|d|ϕ] ⟧ e).
Proof.
  split; intros; mydestr.
  - split; auto. refinement_solver.
    destruct (decide (a = v)); subst; auto; biop_simpl; auto.
    invclear H0; mydestr. constructor; auto. split; auto.
    intros. apply H2; auto. unfold refinement_open. my_simplify_map_eq3.
  - split; auto. refinement_solver.
    destruct (decide (a = v)); subst; auto; biop_simpl; auto.
    invclear H0; mydestr. constructor; auto. split; auto.
    intros. apply H2; auto. unfold refinement_open in H4.
    my_simplify_map_eq3.
Qed.

Lemma state_subst_implies_rty_subst_eq2_rR_forward
  : ∀ (a v: atom) st bst (c_x : constant)
      (k : nat) (b : base_ty) m (n : nat) (d : aset) (ϕ : refinement) e,
    closed_rty m (dom aset (<[a:=c_x]> st)) ({k ~r> {a := c_x }v v} [v:b|n|d|ϕ]) ->
    {m;bst;(<[a:=c_x]> st)}⟦ {k ~r> v} [v:b|n|d|ϕ]⟧ e ->
    {m;bst;(<[a:=c_x]> st)}⟦ {k ~r> {a := c_x }v v} [v:b|n|d|ϕ] ⟧ e.
Proof.
  intros.
  assert (closed_rty m (dom aset (<[a:=c_x]> st)) ({k ~r> {a := c_x }v v} [v:b|n|d|ϕ]) /\
         {m;bst;(<[a:=c_x]> st)}⟦ {k ~r> v} [v:b|n|d|ϕ]⟧ e); auto.
  rewrite state_subst_implies_rty_subst_eq2_rR in H1; mydestr; auto.
Qed.

Lemma state_subst_implies_rty_subst_eq2_rR_backward
  : ∀ (a v: atom) st bst (c_x : constant)
      (k : nat) (b : base_ty) m (n : nat) (d : aset) (ϕ : refinement) e,
    closed_rty m (dom aset (<[a:=c_x]> st)) ({k ~r> v} [v:b|n|d|ϕ]) ->
    {m;bst;(<[a:=c_x]> st)}⟦ {k ~r> {a := c_x }v v} [v:b|n|d|ϕ] ⟧ e ->
    {m;bst;(<[a:=c_x]> st)}⟦ {k ~r> v} [v:b|n|d|ϕ]⟧ e.
Proof.
  intros.
  assert (closed_rty m (dom aset (<[a:=c_x]> st)) ({k ~r> v} [v:b|n|d|ϕ]) /\
         {m;bst;(<[a:=c_x]> st)}⟦ {k ~r> {a := c_x }v v} [v:b|n|d|ϕ] ⟧ e); auto.
  rewrite <- state_subst_implies_rty_subst_eq2_rR in H1; mydestr; auto.
Qed.

Lemma state_insert_include_a: forall (a: atom) (st: state) (c_x: constant),
    a ∈ dom aset (<[a:=c_x]> st).
Proof.
  intros. rewrite dom_insert_simp; auto. set_solver.
Qed.

Global Hint Resolve state_insert_include_a: core.

Lemma state_subst_implies_rty_subst_eq2_forward_wf_ctxrR
    : ∀ (a v : atom) (Γ : list (atom * rty)) st (c_x : constant) (x : atom)
      (k : nat) (b : base_ty) (n : nat) (d : aset) (ϕ : refinement),
  not_fv_in_refinement d ϕ -> a ∉ (stale st ∪ stale Γ ∪ {[x]}) ->
  bound_in_refinement (n `min` k) (refinement_open k c_x ϕ) ->
  wf_ctxrR (<[a:=c_x]> st) (Γ ++ [(x, {k ~r> v} [v:b|n|d|ϕ])]) ->
  wf_ctxrR (<[a:=c_x]> st) (Γ ++ [(x, {k ~r> {a := c_x }v v} [v:b|n|d|ϕ])]).
Proof.
  intros a v. induction Γ; intros.
  - listctx_set_simpl.
    assert (ok_dctx (dom aset (<[a:=c_x]> st)) ([] ++ [(x, {k ~r> v} [v:b|n|d|ϕ])])).
    { apply wf_ctxrR_regular1 in H2; auto. }
    constructor; auto.
    + eapply state_subst_implies_rty_subst_eq2_forward_ok_dctx in H3; eauto.
    + invclear H2; auto; try auto_ty_exfalso. intros. apply H12.
      assert ({0;b∅;<[a:=c_x]> st}⟦ {k ~r> {a := c_x }v v} [v:b|n|d|ϕ] ⟧ e_wf); auto.
      apply state_subst_implies_rty_subst_eq2_rR_backward in H4; auto.
      refinement_solver.
    + invclear H2; auto; try auto_ty_exfalso. mydestr. exists x0. split; eauto.
       assert ({0;b∅;<[a:=c_x]> st}⟦ {k ~r> v} [v:b|n|d|ϕ] ⟧ x0); auto.
       apply state_subst_implies_rty_subst_eq2_rR_forward in H5; auto.
       apply state_subst_implies_rty_subst_eq2_forward_closed_rty; auto.
       refinement_solver.
  - RD_simp2.
    + constructor; auto.
      { rewrite app_comm_cons.
        apply state_subst_implies_rty_subst_eq2_forward_ok_dctx; auto. }
      intros. setoid_rewrite insert_commute; try fast_set_solver.
      apply IHΓ; auto. setoid_rewrite dom_insert_simp; auto. set_solver.
      setoid_rewrite insert_commute; try fast_set_solver.
    + mydestr. constructor; auto.
      { rewrite app_comm_cons.
        apply state_subst_implies_rty_subst_eq2_forward_ok_dctx; auto. }
      exists x0. split; auto. intros. eapply H3 in H4; eauto.
      assert (exists (c: constant), v_x = c). dsimp1; eauto. mydestr; subst.
      setoid_rewrite insert_commute; try fast_set_solver.
      apply IHΓ; auto. setoid_rewrite dom_insert_simp; auto. set_solver.
      setoid_rewrite insert_commute; try fast_set_solver.
    + constructor; auto.
      { rewrite app_comm_cons.
        apply state_subst_implies_rty_subst_eq2_forward_ok_dctx; auto. }
      apply IHΓ; auto. set_solver.
Qed.

Lemma state_subst_implies_rty_subst_eq2_forwardc_wf_ctxrR
    : ∀ (a : atom) (Γ : list (atom * rty)) st (c_x : constant) (x : atom)
      (k : nat) (b : base_ty) (n : nat) (d : aset) (ϕ : refinement),
  not_fv_in_refinement d ϕ -> a ∉ (stale st ∪ stale Γ ∪ {[x]}) ->
  bound_in_refinement (n `min` k) (refinement_open k c_x ϕ) ->
  wf_ctxrR (<[a:=c_x]> st) (Γ ++ [(x, {k ~r> a} [v:b|n|d|ϕ])]) ->
  wf_ctxrR (<[a:=c_x]> st) (Γ ++ [(x, {k ~r> c_x} [v:b|n|d|ϕ])]).
Proof.
  intros.
  apply state_subst_implies_rty_subst_eq2_forward_wf_ctxrR in H2; biop_simpl; auto.
Qed.

Lemma state_subst_implies_rty_subst_eq2_backward_wf_ctxrR
    : ∀ (a v : atom) (Γ : list (atom * rty)) st (c_x : constant) (x : atom)
      (k : nat) (b : base_ty) (n : nat) (d : aset) (ϕ : refinement),
  a ∉ (stale st ∪ stale Γ ∪ {[x]}) ->
  ok_dctx (dom aset (<[a:=c_x]> st)) (Γ ++ [(x, {k ~r> v} [v:b|n|d|ϕ])]) ->
  wf_ctxrR (<[a:=c_x]> st) (Γ ++ [(x, {k ~r> {a := c_x }v v} [v:b|n|d|ϕ])]) ->
  wf_ctxrR (<[a:=c_x]> st) (Γ ++ [(x, {k ~r> v} [v:b|n|d|ϕ])]).
Proof.
  intros a v. induction Γ; intros.
  - listctx_set_simpl.
    constructor; auto.
    + invclear H1; auto; try auto_ty_exfalso. intros. apply H10.
      assert ({0;b∅;<[a:=c_x]> st}⟦ {k ~r> {a := c_x }v v} [v:b|n|d|ϕ] ⟧ e_wf); auto.
      apply state_subst_implies_rty_subst_eq2_rR_forward in H1; auto.
      refinement_solver.
    + invclear H1; auto; try auto_ty_exfalso. mydestr. exists x0. split; eauto.
       assert ({0;b∅;<[a:=c_x]> st}⟦ {k ~r> {a := c_x }v v} [v:b|n|d|ϕ] ⟧ x0); auto.
       apply state_subst_implies_rty_subst_eq2_rR_backward in H3; auto.
       refinement_solver.
  - RD_simp2; invclear H0; try auto_ty_exfalso.
    + constructor; auto.
      intros. setoid_rewrite insert_commute; try fast_set_solver.
      apply IHΓ; auto. setoid_rewrite dom_insert_simp; auto. set_solver.
      setoid_rewrite insert_commute; try fast_set_solver. setoid_rewrite dom_insert_simp; auto.
      setoid_rewrite insert_commute; try fast_set_solver.
    + mydestr. constructor; auto.
      exists x0. split; auto. intros. eapply H1 in H2; eauto.
      assert (exists (c: constant), v_x = c). dsimp1; eauto. mydestr; subst.
      setoid_rewrite insert_commute; try fast_set_solver.
      apply IHΓ; auto. setoid_rewrite dom_insert_simp; auto. set_solver.
      setoid_rewrite insert_commute; try fast_set_solver. setoid_rewrite dom_insert_simp; auto.
      setoid_rewrite insert_commute; try fast_set_solver.
    + constructor; auto.
      apply IHΓ; auto. set_solver.
Qed.

Lemma tletbiop_aux6: forall op (v1 v2: value),
    ((exists (n: constant), v1 = n) \/ (exists (y: atom), v1 = y)) ->
    ((exists (n: constant), v2 = n) \/ (exists (y: atom), v2 = y)) ->
    bound_in_refinement 0
      (refinement_open 1 v1
         (refinement_open 0 v2
            (λ (bst : bstate) (_ : state) (v : constant), eval_op op (bst 0) (bst 1) v))).
Proof.
  unfold bound_in_refinement; unfold refinement_open. intros.
  destruct H; destruct H0; mydestr;
    try (try destruct (st !! x); auto; try destruct (st !! x0); subst; auto);
    try (destruct (st !! x); auto);
    try (destruct (st !! x0); auto).
Qed.

Lemma tletbiop_aux7: forall op (v1 v2: value),
    ((exists (n: constant), v1 = n) \/ (exists (y: atom), v1 = y)) ->
    ((exists (n: constant), v2 = n) \/ (exists (y: atom), v2 = y)) ->
    bound_in_refinement 0
      (refinement_open 0 v1
         (refinement_open 1 v2
            (λ (bst : bstate) (_ : state) (v : constant), eval_op op (bst 0) (bst 1) v))).
Proof.
  unfold bound_in_refinement; unfold refinement_open. intros.
  destruct H; destruct H0; mydestr;
    try (try destruct (st !! x); auto; try destruct (st !! x0); subst; auto);
    try (destruct (st !! x); auto);
    try (destruct (st !! x0); auto).
Qed.

Lemma tletbiop_aux8: forall (d: aset), (d ∪ ∅) = d.
Proof.
  intros. set_solver. Qed.

Lemma not_fv_in_refinement_aux: forall op (v: value) (n: nat),
    n ≤ 1 -> ((exists (n: constant), v = n) \/ (exists (y: atom), v = y)) ->
    not_fv_in_refinement (fv_value v)
      (refinement_open n v
         (λ (bst : bstate) (_ : state) (v : constant), eval_op op (bst 0) (bst 1) v)).
Proof.
  unfold not_fv_in_refinement; unfold refinement_open. intros.
  destruct H0; mydestr; subst; auto.
  assert (m !! x = m' !! x). apply H1; fast_set_solver. rewrite H0.
  destruct (m'!!x); auto.
Qed.

Lemma mk_op_ret_wf_ctxrR
    : ∀ st op (a : atom) (v1 v2: value) (Γ : list (atom * rty)) (c_x : constant) (x: atom),
    ((exists (n: constant), v1 = n) \/ (exists (y: atom), v1 = y)) ->
    ((exists (n: constant), v2 = n) \/ (exists (y: atom), v2 = y)) ->
    a ∉ stale st ∪ stale Γ ∪ {[x]} ->
    (ok_dctx (dom aset (<[a:=c_x]> st)) (Γ ++ [(x, {1 ~r> v2} (mk_op_ret op ^r^ v1))]) /\
       (wf_ctxrR (<[a:=c_x]> st)
          (Γ ++ [(x, {1 ~r> ({a := c_x }v v2)} (mk_op_ret op ^r^ ({a := c_x }v v1)))]))
    ) <-> (wf_ctxrR (<[a:=c_x]> st) (Γ ++ [(x, {1 ~r> v2} (mk_op_ret op ^r^ v1))])).
Proof.
  split; intros; mydestr; subst.
  - destruct H; destruct H0; mydestr; subst; auto.
    + destruct (decide (a = x0)); auto; subst; biop_simpl; auto.
      apply state_subst_implies_rty_subst_eq2_backward_wf_ctxrR; biop_simpl; auto.
    + destruct (decide (a = x1)); auto; subst; biop_simpl; auto.
      rewrite rty_open_commute_op; auto.
      rewrite rty_open_commute_op in H3; auto.
      rewrite rty_open_commute_op in H2; auto.
      apply state_subst_implies_rty_subst_eq2_backward_wf_ctxrR; biop_simpl; auto.
    + destruct (decide (a = x0)); auto; subst; auto.
      { biop_simpl.
        destruct (decide (x0 = x1)); auto; subst.
        - biop_simpl.
          assert (ok_dctx (dom aset (<[x1:=c_x]> st)) (Γ ++ [(x, {1 ~r> c_x} (mk_op_ret op ^r^ x1))])).
          { apply state_subst_implies_rty_subst_eq2_forward_ok_dctx with (a:=x1) (c_x :=c_x) in H2; eauto. biop_simpl; auto.
          unfold refinement_set_open. apply tletbiop_aux1; auto.
          assert ((2 `min` 0) `min` 1 = 0) by lia. setoid_rewrite H. clear H.
          apply tletbiop_aux2. }
          assert (wf_ctxrR (<[x1:=c_x]> st) (Γ ++ [(x, {1 ~r> c_x} (mk_op_ret op ^r^ x1))])).
          { rewrite rty_open_commute_op; auto.
            rewrite rty_open_commute_op in H3; auto.
            rewrite rty_open_commute_op in H; auto.
            apply state_subst_implies_rty_subst_eq2_backward_wf_ctxrR; biop_simpl; auto. }
          apply state_subst_implies_rty_subst_eq2_backward_wf_ctxrR; biop_simpl; auto.
        - apply state_subst_implies_rty_subst_eq2_backward_wf_ctxrR; biop_simpl; auto.
      }
      { rewrite rty_open_commute_op in H2; auto.
        rewrite rty_open_commute_op in H3; auto.
        rewrite rty_open_commute_op; auto.
        apply state_subst_implies_rty_subst_eq2_backward_wf_ctxrR; biop_simpl; auto.
 }
 - split. apply wf_ctxrR_regular1 in H2; mydestr; auto.
    destruct H; destruct H0; mydestr; subst; auto.
    + destruct (decide (a = x0)); auto; subst; biop_simpl; auto.
      apply state_subst_implies_rty_subst_eq2_forwardc_wf_ctxrR; biop_simpl; auto.
      pose (not_fv_in_refinement_aux op x1 0) as Hn; apply Hn; try lia; eauto.
      apply tletbiop_aux6; eauto.
    + destruct (decide (a = x1)); auto; subst; biop_simpl; auto.
      rewrite rty_open_commute_op; auto.
      rewrite rty_open_commute_op in H2; auto.
      apply state_subst_implies_rty_subst_eq2_forwardc_wf_ctxrR; biop_simpl; auto.
      pose (not_fv_in_refinement_aux op x0 1) as Hn; apply Hn; try lia; eauto.
      apply tletbiop_aux7; eauto.
    + destruct (decide (a = x0)); auto; subst; auto; biop_simpl.
      { destruct (decide (x0 = x1)); auto; subst; biop_simpl.
        - assert (wf_ctxrR (<[x1:=c_x]> st) (Γ ++ [(x, {1 ~r> x1} (mk_op_ret op ^r^ c_x))] )).
          rewrite rty_open_commute_op; auto.
          rewrite rty_open_commute_op in H2; auto.
          apply state_subst_implies_rty_subst_eq2_forwardc_wf_ctxrR; biop_simpl; auto.
          pose (not_fv_in_refinement_aux op x1 1) as Hn; simpl; rewrite tletbiop_aux8; apply Hn; try lia; eauto.
          simpl. apply tletbiop_aux7; eauto.
          apply state_subst_implies_rty_subst_eq2_forwardc_wf_ctxrR; biop_simpl; auto.
          pose (not_fv_in_refinement_aux op c_x 0) as Hn; simpl; try rewrite tletbiop_aux8; apply Hn; try lia; eauto.
          simpl. apply tletbiop_aux6; eauto.
        - apply state_subst_implies_rty_subst_eq2_forwardc_wf_ctxrR; biop_simpl; auto.
          pose (not_fv_in_refinement_aux op x1 0) as Hn; simpl; try rewrite tletbiop_aux8; apply Hn; try lia; eauto.
          simpl. apply tletbiop_aux6; eauto.
      }
      { destruct (decide (a = x1)); auto; subst; biop_simpl; auto.
        rewrite rty_open_commute_op; auto.
        rewrite rty_open_commute_op in H2; auto.
        apply state_subst_implies_rty_subst_eq2_forwardc_wf_ctxrR; biop_simpl; auto.
        pose (not_fv_in_refinement_aux op x0 1) as Hn; simpl; try rewrite tletbiop_aux8; apply Hn; try lia; eauto.
        simpl. apply tletbiop_aux7; eauto.
      }
Qed.

Lemma mk_op_ret_ok_dctx
    : ∀ st op (a : atom) (v1 v2: value) (Γ : list (atom * rty)) (c_x : constant) (x : atom) e τ,
    ((exists (n: constant), v1 = n) \/ (exists (y: atom), v1 = y)) ->
    ((exists (n: constant), v2 = n) \/ (exists (y: atom), v2 = y)) ->
    (ok_dctx (dom aset (<[a:=c_x]> st)) (Γ ++ [(x, {1 ~r> v2} (mk_op_ret op ^r^ v1))]) /\
    (⅋{<[a:=c_x]> st}⟦τ⟧{Γ ++ [(x, {1 ~r> ({a := c_x }v v2)} (mk_op_ret op ^r^ ({a := c_x }v v1)))] } e)) <->
       (⅋{<[a:=c_x]> st}⟦τ⟧{Γ ++ [(x, {1 ~r> v2} (mk_op_ret op ^r^ v1))] }) e.
Proof.
  split; intros; mydestr; subst.
  - destruct H; destruct H0; mydestr; subst; auto.
    + destruct (decide (a = x0)); auto; subst; biop_simpl; auto.
      apply state_subst_implies_rty_subst_eq2_backward; biop_simpl; auto.
    + destruct (decide (a = x1)); auto; subst; biop_simpl; auto.
      rewrite rty_open_commute_op; auto.
      rewrite rty_open_commute_op in H1; auto.
      rewrite rty_open_commute_op in H2; auto.
      apply state_subst_implies_rty_subst_eq2_backward; biop_simpl; auto.
    + destruct (decide (a = x0)); auto; subst; auto.
      { biop_simpl.
        destruct (decide (x0 = x1)); auto; subst.
        - biop_simpl.
          assert (ok_dctx (dom aset (<[x1:=c_x]> st)) (Γ ++ [(x, {1 ~r> c_x} (mk_op_ret op ^r^ x1))])).
          { apply state_subst_implies_rty_subst_eq2_forward_ok_dctx with (a:=x1) (c_x :=c_x) in H1; eauto. biop_simpl; auto.
          unfold refinement_set_open. apply tletbiop_aux1; auto.
          assert ((2 `min` 0) `min` 1 = 0) by lia. setoid_rewrite H. clear H.
          apply tletbiop_aux2. }
          assert ((⅋{<[x1:=c_x]> st}⟦τ⟧{Γ ++ [(x, {1 ~r> c_x} (mk_op_ret op ^r^ x1))] }) e).
          { rewrite rty_open_commute_op; auto.
            rewrite rty_open_commute_op in H; auto.
            rewrite rty_open_commute_op in H2; auto.
            apply state_subst_implies_rty_subst_eq2_backward; biop_simpl; auto. }
          apply state_subst_implies_rty_subst_eq2_backward; biop_simpl; auto.
        - apply state_subst_implies_rty_subst_eq2_backward; biop_simpl; auto.
      }
      { rewrite rty_open_commute_op in H1; auto.
        rewrite rty_open_commute_op in H2; auto.
        rewrite rty_open_commute_op; auto.
        apply state_subst_implies_rty_subst_eq2_backward; biop_simpl; auto. }
  - split. try (apply inv_ctxrR_regular in H1; mydestr; auto).
    destruct H; destruct H0; mydestr; subst; auto.
    + destruct (decide (a = x0)); auto; subst; biop_simpl; auto.
      apply state_subst_implies_rty_subst_eq2_forward2; biop_simpl; auto.
      pose (not_fv_in_refinement_aux op x1 0) as Hn; apply Hn; try lia; eauto.
      apply tletbiop_aux6; eauto.
    + destruct (decide (a = x1)); auto; subst; biop_simpl; auto.
      rewrite rty_open_commute_op; auto.
      rewrite rty_open_commute_op in H1; auto.
      apply state_subst_implies_rty_subst_eq2_forward2; biop_simpl; auto.
      pose (not_fv_in_refinement_aux op x0 1) as Hn; apply Hn; try lia; eauto.
      apply tletbiop_aux7; eauto.
    + destruct (decide (a = x0)); auto; subst; auto; biop_simpl.
      { destruct (decide (x0 = x1)); auto; subst; biop_simpl.
        - assert ((⅋{<[x1:=c_x]> st}⟦τ⟧{Γ ++ [(x, {1 ~r> x1} (mk_op_ret op ^r^ c_x))] }) e).
          rewrite rty_open_commute_op; auto.
          rewrite rty_open_commute_op in H1; auto.
          apply state_subst_implies_rty_subst_eq2_forward2; biop_simpl; auto.
          pose (not_fv_in_refinement_aux op x1 1) as Hn; simpl; rewrite tletbiop_aux8; apply Hn; try lia; eauto.
          simpl. apply tletbiop_aux7; eauto.
          apply state_subst_implies_rty_subst_eq2_forward2; biop_simpl; auto.
          pose (not_fv_in_refinement_aux op c_x 0) as Hn; simpl; try rewrite tletbiop_aux8; apply Hn; try lia; eauto.
          simpl. apply tletbiop_aux6; eauto.
        - apply state_subst_implies_rty_subst_eq2_forward2; biop_simpl; auto.
          pose (not_fv_in_refinement_aux op x1 0) as Hn; simpl; try rewrite tletbiop_aux8; apply Hn; try lia; eauto.
          simpl. apply tletbiop_aux6; eauto.
      }
      { destruct (decide (a = x1)); auto; subst; biop_simpl; auto.
        rewrite rty_open_commute_op; auto.
        rewrite rty_open_commute_op in H1; auto.
        apply state_subst_implies_rty_subst_eq2_forward2; biop_simpl; auto.
        pose (not_fv_in_refinement_aux op x0 1) as Hn; simpl; try rewrite tletbiop_aux8; apply Hn; try lia; eauto.
        simpl. apply tletbiop_aux7; eauto.
      }
Qed.

Lemma letebiop_aux9: forall op r, is_arr r -> ~ ⌊r⌋ = fst_ty_of_op op.
Proof.
  intros. destruct r; destruct op; simpl; invclear H; try (intro H; invclear H).
Qed.

Lemma inv_rRctx_tletbiop: forall Γ st (τ: rty) x op n1 d1 ϕ1 n2 d2 ϕ2 (v1 v2: value) e,
    x ∉ fv_tm e -> x ∉ rty_fv τ -> not_overbasety τ ->
    wf_ctxrR st (Γ ++ [(x, {1 ~r> v2} (mk_op_ret op ^r^ v1))]) ->
    (⅋{st}⟦[v:fst_ty_of_op op|n1|d1|ϕ1]⟧{Γ}) v1 ->
    (⅋{st}⟦[v:snd_ty_of_op op|n2|d2|ϕ2]⟧{Γ}) v2 ->
    (⅋{st}⟦τ⟧{Γ ++ [(x, {1 ~r> v2} (mk_op_ret op ^r^ v1))] }) (e ^t^ x) ->
    ⅋{st}⟦τ⟧{Γ ++ [(x, {1 ~r> v2} (mk_op_ret op ^r^ v1))] } (tletbiop op v1 v2 e).
Proof.
  induction Γ; intros; RD_simp2.
  - mydestr. inv_rd_simpl1. constructor; auto.
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
  - constructor; auto. denotation_simp. apply tletbiop_typable_ctx_dummy_cons; auto.
    intros. auto_under c_x. invclear H11; auto_ty_exfalso.
    assert ((exists (n: constant), v1 = n) \/ (exists (y: atom), v1 = y)).
    { invclear H20. eapply has_basety_implies_constant_or_var; eauto. }
    assert ((exists (n: constant), v2 = n) \/ (exists (y: atom), v2 = y)).
    { invclear H23. eapply has_basety_implies_constant_or_var; eauto. }
    rewrite <- mk_op_ret_ok_dctx; auto. split; auto. rewrite dom_insert_simp; auto.
    assert (a <> x) by (rewrite ctxdom_app_union in H16; inv_rd_simpl1).
    eapply IHΓ; eauto; fold value_subst; fold tm_subst.
    + pose (fv_of_subst_tm a c_x e). set_solver.
    + assert (wf_ctxrR (<[a:=c_x]> st)
                (Γ ++ [(x, {1 ~r> v2} (mk_op_ret op ^r^ v1))])); auto.
      rewrite <- mk_op_ret_wf_ctxrR in H10; mydestr; auto.
      inv_rd_simpl1.
    + assert ((⅋{<[a:=c_x]> st}⟦τ⟧{Γ ++
                       [(x, {1 ~r> v2} (mk_op_ret op ^r^ v1))] }) ({a := c_x }t (e ^t^ x))
           ); auto.
      rewrite subst_open_tm in H10; auto.
      assert ({a := c_x}v x = x). { simpl. dec_solver2. }
      rewrite H11 in H10.
      rewrite <- mk_op_ret_ok_dctx in H10; mydestr; auto.
  - constructor; auto. denotation_simp. apply tletbiop_typable_ctx_dummy_cons; auto.
    mydestr. auto_meet_exists Hmeet. auto_meet_reduce. auto_under v_x.
    assert ((exists (n: constant), v1 = n) \/ (exists (y: atom), v1 = y)).
    { invclear H20. eapply has_basety_implies_constant_or_var; eauto. }
    assert ((exists (n: constant), v2 = n) \/ (exists (y: atom), v2 = y)).
    { invclear H23. eapply has_basety_implies_constant_or_var; eauto. }
    assert (exists (c: constant), v_x = c). dsimp1; eauto. mydestr; subst.
    assert (({a↦x4}) st = <[a:=x4]> st) as Htz; auto. rewrite Htz.
    rewrite <- mk_op_ret_ok_dctx; auto. split; auto.
    { apply inv_ctxrR_regular in H12; mydestr; auto. }
    assert (a <> x). { apply ok_dctx_regular in H11; mydestr. basic_typing_solver. }
    eapply IHΓ; eauto; fold value_subst; fold tm_subst.
    + rewrite fv_of_subst_tm_closed; refinement_solver.
    + eapply H3 in H2; eauto.
      assert (wf_ctxrR (<[a:=x4]> st)
                (Γ ++ [(x, {1 ~r> v2} (mk_op_ret op ^r^ v1))])); auto.
      rewrite <- mk_op_ret_wf_ctxrR in H28; mydestr; auto.
      apply ok_dctx_regular in H11; mydestr. simpl in H29. listctx_set_simpl.
      set_solver.
    + assert ((⅋{<[a:=x4]> st}⟦τ⟧{Γ ++
                       [(x, {1 ~r> v2} (mk_op_ret op ^r^ v1))] }) ({a := x4 }t (e ^t^ x))
           ); auto.
      rewrite subst_open_tm in H28; auto.
      assert ({a := x4}v x = x). { simpl. dec_solver2. }
      rewrite H29 in H28.
      rewrite <- mk_op_ret_ok_dctx in H28; mydestr; auto.
  - constructor; auto. denotation_simp. apply tletbiop_typable_ctx_dummy_cons; auto.
    intros. auto_under v_x.
    assert ((exists (n: constant), v1 = n) \/ (exists (y: atom), v1 = y)).
    { invclear H23. eapply has_basety_implies_constant_or_var; eauto. }
    assert ((exists (n: constant), v2 = n) \/ (exists (y: atom), v2 = y)).
    { invclear H19. eapply has_basety_implies_constant_or_var; eauto. }
    assert ({a := v_x }t tletbiop op v1 v2 e =
           tletbiop op ({a := v_x }v v1) ({a := v_x }v v2) ({a := v_x }t e)) as Htz; auto.
    rewrite Htz.
    assert ({a := v_x }v v1 = v1).
    { destruct H4; mydestr; subst; auto. simpl. dec_solver2; subst.
      invclear H23. invclear H22. simpl in H26. rewrite decide_True in H26. invclear H26.
      apply letebiop_aux9 with (op:=op) in H6. exfalso. apply H6; auto. reflexivity.
    }
    assert ({a := v_x }v v2 = v2).
    { destruct H5; mydestr; subst; auto. simpl. dec_solver2; subst.
      invclear H19. invclear H26. simpl in H27. rewrite decide_True in H27. invclear H27.
      apply letebiop_aux9 with (op:=op) in H6. exfalso. apply H6; auto. reflexivity.
    }
    rewrite H17. rewrite H22.
    assert ({a := v_x }t v1 = v1) by (simpl; rewrite H17; auto).
    assert ({a := v_x }t v2 = v2) by (simpl; rewrite H22; auto).
    rewrite H26 in H24.
    rewrite H27 in H20.
    eapply IHΓ; eauto; fold tm_subst.
    + rewrite fv_of_subst_tm_closed; refinement_solver.
    + rewrite <- subst_open_var_tm; auto. denotation_simp. basic_typing_solver. refinement_solver.
Qed.

(* Lemma inv_rRctx_tletbiop: forall Γ st (τ: rty) x op n1 d1 ϕ1 n2 d2 ϕ2 (v1 v2: nat) e, *)
(*     x ∉ fv_tm e -> x ∉ rty_fv τ -> not_overbasety τ -> *)
(*     wf_ctxrR st (Γ ++ [(x, {1 ~r> v2} (mk_op_ret op ^r^ v1))]) -> *)
(*     (⅋{st}⟦[v:fst_ty_of_op op|n1|d1|ϕ1]⟧{Γ}) v1 -> *)
(*     (⅋{st}⟦[v:fst_ty_of_op op|n2|d2|ϕ2]⟧{Γ}) v2 -> *)
(*     (⅋{st}⟦τ⟧{Γ ++ [(x, {1 ~r> v2} (mk_op_ret op ^r^ v1))] }) (e ^t^ x) -> *)
(*     ⅋{st}⟦τ⟧{Γ ++ [(x, {1 ~r> v2} (mk_op_ret op ^r^ v1))] } (tletbiop op v1 v2 e). *)
(* Proof. *)
(*   induction Γ; intros; RD_simp2. *)
(*   - mydestr. inv_rd_simpl1. constructor; auto. *)
(*     + denotation_simp. assert ([] ⊢t tletbiop op v1 v2 e ⋮t ⌊τ⌋). eapply tletbiop_typable; refinement_solver. *)
(*       basic_typing_solver. *)
(*     + exists (eval_op_under_bound_tm op v1 v2). split. apply rR_eval_op_under_bound_tm. *)
(*       intros. assert (x1 ↪* v_x) as Hz by (eapply rR_eval_op_under_bound_tm_min; eauto). *)
(*       auto_under v_x. RD_simp2. simpl. *)
(*       rewrite subst_fresh_tm; auto. rewrite open_subst_same_tm in H10; auto. *)
(*       eapply termR_perserve_rR; refinement_solver. *)
(*       eapply letapp_eval_op_under_bound_tm_termR; eauto; refinement_solver. *)
(*   - constructor; auto. denotation_simp. apply tletbiop_typable_ctx_dummy_cons; auto. *)
(*     intros. auto_under c_x. simpl. eapply IHΓ; eauto. *)
(*     + rewrite fv_of_subst_tm_closed; refinement_solver. *)
(*     + rewrite <- subst_open_var_tm; auto. denotation_simp. basic_typing_solver. *)
(*   - constructor; auto. denotation_simp. apply tletbiop_typable_ctx_dummy_cons; auto. *)
(*     mydestr. auto_meet_exists Hmeet. auto_meet_reduce. auto_under v_x. eapply IHΓ; eauto; fold tm_subst. *)
(*     + rewrite fv_of_subst_tm_closed; refinement_solver. *)
(*     + rewrite <- subst_open_var_tm; auto. denotation_simp. basic_typing_solver. op_solver1. *)
(*   - constructor; auto. denotation_simp. apply tletbiop_typable_ctx_dummy_cons; auto. *)
(*     intros. auto_under v_x. eapply IHΓ; eauto; fold tm_subst. *)
(*     + rewrite fv_of_subst_tm_closed; refinement_solver. *)
(*     + rewrite <- subst_open_var_tm; auto. denotation_simp. basic_typing_solver. refinement_solver. *)
(* Qed. *)


