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
Import TermOrdering.

Lemma over_inhabitant_only_constant: forall (u: value) st b n d ϕ,
    ({0;b∅;st}⟦{v:b|n|d|ϕ}⟧) u -> (exists (c: constant), u = c).
Proof.
  intros. invclear H; mydestr. exists x. invclear H3. auto.
Qed.

Ltac RD_simp :=
  repeat match goal with
  | [H: ({_}⟦ _ ⟧{ [] }) _ |- _ ] => invclear H
  | [|- ({_}⟦ _ ⟧{ [] }) _ ] => constructor
  | [H: ({_}⟦_⟧{ (_, {v:_|_|_|_}) :: _ }) _ |- _ ] => invclear H; try auto_ty_exfalso
  | [H: ({0;b∅;_}⟦{v:_|_|_|_}⟧) (tvalue ?u) |- _ ] =>
      match u with
      | vconst _ => idtac
      | _ => destruct (over_inhabitant_only_constant _ _ _ _ _ _ H); subst
      end
  end.

(* Ltac my_simplify_map_eq_solver1 := *)
(*   repeat match goal with *)
(*   | [ |- context [(<[?x:=?c_x]> (<[?x:=?c_y]> ?m))] ] => *)
(*       setoid_rewrite insert_insert; auto *)
(*   | [ H: ?x <> ?y |- context [(<[?x:=?c_x]> (<[?y:=?c_y]> ?m))] ] => *)
(*       fail 1 *)
(*   | [ |- context [(<[?x:=?c_x]> (<[?y:=?c_y]> ?m))] ] => *)
(*       destruct (Atom.atom_dec x y); subst *)
(*   end. *)

(* Lemma closed_rty_subst_excluded_over_forward: forall (n1 n2: nat) (z: atom) (c: constant) B (d1 d2: aset) ϕ, *)
(*     z ∉ d1 -> *)
(*     closed_rty n1 ({[z]} ∪ d1) {v:B|n2|d2|ϕ} -> *)
(*     closed_rty n1 d1 {v:B|n2|refinement_set_subst z c d2|refinement_subst z c d2 ϕ}. *)
(* Proof. *)
(*   unfold closed_rty. intros; mydestr. repeat split. *)
(*   - denotation_simp. invclear H0. constructor. invclear H4. constructor; auto. *)
(*     + intros. *)
(*       unfold not_fv_in_refinement. intros. unfold refinement_subst. *)
(*       unfold refinement_set_subst in H4. *)
(*       amap_dec_solver; apply H0; intros; destruct (atom_dec x z); subst; my_simplify_map_eq3. *)
(*     + unfold bound_in_refinement in H2. unfold bound_in_refinement. intros. *)
(*       unfold refinement_subst. amap_dec_solver. *)
(*   - refinement_solver. *)
(*   - simpl in H2. simpl. unfold refinement_set_subst. *)
(*     amap_dec_solver; set_solver. *)
(* Qed. *)

(* Lemma closed_rty_subst_excluded_over_backward: forall (n1 n2: nat) (z: atom) (c: constant) B (d1 d2: aset) ϕ, *)
(*     z ∉ d1 -> valid_rty {v:B|n2|d2|ϕ} -> *)
(*     closed_rty n1 d1 {v:B|n2|refinement_set_subst z c d2| refinement_subst z c d2 ϕ} -> closed_rty n1 ({[z]} ∪ d1) {v:B|n2|d2|ϕ}. *)
(* Proof. *)
(*   unfold closed_rty. intros; mydestr. repeat split. *)
(*   - denotation_simp. *)
(*   - refinement_solver. *)
(*   - simpl in H3. simpl. unfold refinement_set_subst in H3. *)
(*     amap_dec_solver. rewrite (union_difference_singleton z d2); auto. set_solver. *)
(* Admitted. *)

(* Lemma closed_rty_subst_excluded_under_forward: forall (n1 n2: nat) (z: atom) (c: constant) B (d1 d2: aset) ϕ, *)
(*     z ∉ d1 -> *)
(*     closed_rty n1 ({[z]} ∪ d1) [v:B|n2|d2|ϕ] -> *)
(*     closed_rty n1 d1 [v:B|n2|refinement_set_subst z c d2|refinement_subst z c d2 ϕ]. *)
(* Proof. *)
(*   unfold closed_rty. intros; mydestr. repeat split. *)
(*   - denotation_simp. invclear H0. constructor. invclear H4. constructor; auto. *)
(*     + intros. *)
(*       unfold not_fv_in_refinement. intros. unfold refinement_subst. *)
(*       unfold refinement_set_subst in H4. *)
(*       amap_dec_solver; apply H0; intros; destruct (atom_dec x z); subst; my_simplify_map_eq3. *)
(*     + unfold bound_in_refinement in H2. unfold bound_in_refinement. intros. *)
(*       unfold refinement_subst. amap_dec_solver. *)
(*   - refinement_solver. *)
(*   - simpl in H2. simpl. unfold refinement_set_subst. *)
(*     amap_dec_solver; set_solver. *)
(* Qed. *)

Lemma wf_r_implies_state_dummy_insert: forall n2 d2 ϕ z,
    wf_r n2 d2 ϕ -> z ∉ d2 -> (forall st bst c v, (ϕ bst (<[z:=c]> st) v <-> ϕ bst st v)).
Proof.
  intros. invclear H. apply H1. intros.
  my_simplify_map_eq3.
Qed.

Lemma closed_rty_over_implies_state_dummy_insert: forall n1 d1 n2 d2 B ϕ z,
    closed_rty n1 d1 {v:B|n2|d2|ϕ} -> z ∉ d2 -> (forall st bst c v, (ϕ bst (<[z:=c]> st) v <-> ϕ bst st v)).
Proof.
  intros. eapply wf_r_implies_state_dummy_insert; eauto. refinement_solver3.
Qed.

Lemma closed_rty_under_implies_state_dummy_insert: forall n1 d1 n2 d2 B ϕ z,
    closed_rty n1 d1 [v:B|n2|d2|ϕ] -> z ∉ d2 -> (forall st bst c v, (ϕ bst (<[z:=c]> st) v <-> ϕ bst st v)).
Proof.
  intros. eapply wf_r_implies_state_dummy_insert; eauto. refinement_solver3.
Qed.

Lemma denotation_st_update_iff_subst:
  forall τ (z: atom) (c: constant) n bst (st: state) e,
    z ∉ (dom aset st) ->
    ({n;bst;<[z:=c]> st}⟦τ⟧ e) <-> (closed_rty n ({[z]} ∪ dom aset st) τ /\ {n;bst;st}⟦({z:=c}r) τ⟧ e).
Proof.
  induction τ; split; simpl; intros; mydestr; subst.
  - do 2 (split; denotation_simp; auto). split; auto.
    + eapply closed_rty_subst_excluded_forward in H1; eauto.
    + unfold refinement_subst. amap_dec_solver.
    repeat eexists; eauto. rewrite closed_rty_over_implies_state_dummy_insert in H3; eauto.
  - split; auto. split; auto.
    + denotation_simp.
    + repeat eexists; eauto.
      unfold refinement_subst in H4. unfold refinement_subst in H2. unfold refinement_set_subst in H2.
      amap_dec_solver.
      rewrite closed_rty_over_implies_state_dummy_insert; eauto.
  - split; auto; denotation_simp. split; auto. split; auto.
    + eapply closed_rty_subst_excluded_forward in H1; eauto.
    + unfold refinement_subst. amap_dec_solver.
      intros. apply H2; auto. rewrite closed_rty_under_implies_state_dummy_insert; eauto.
  - split; auto. split; auto.
    + denotation_simp.
    + repeat eexists; eauto. intros. apply H3; auto.
      unfold refinement_subst. unfold refinement_subst in H2. unfold refinement_set_subst in H2.
      amap_dec_solver.
      rewrite closed_rty_under_implies_state_dummy_insert in H5; eauto.
  - split; auto; denotation_simp. split; auto; ctx_erase_simp4. split; auto.
    + eapply closed_rty_subst_excluded_forward in H1; eauto.
    + intros. unfold refinement_subst in H4.
      assert (ϕ bst (<[z:=c]> st) c_x).
      { amap_dec_solver; auto. rewrite wf_r_implies_state_dummy_insert; eauto. refinement_solver3. }
      apply H2 in H5; auto. rewrite IHτ in H5; mydestr; auto.
  - split; auto; ctx_erase_simp4. split; auto.
    + denotation_simp.
    + intros. rewrite IHτ; auto. split; refinement_solver. apply H3; auto.
      unfold refinement_subst. amap_dec_solver.
      rewrite wf_r_implies_state_dummy_insert in H5; eauto. refinement_solver3.
  - split; auto; denotation_simp. split; auto; ctx_erase_simp4. split; auto.
    + eapply closed_rty_subst_excluded_forward in H1; eauto.
    + intros.
      assert (({n;bst;<[z:=c]> st}⟦τ1⟧) e_x) as HH. rewrite IHτ1; auto. split; refinement_solver.
      apply H2 in HH. rewrite IHτ2 in HH; auto. mydestr; auto.
  - split; auto; ctx_erase_simp4. split; auto; denotation_simp.
    + intros. rewrite IHτ2; auto. split; refinement_solver. apply H3. rewrite IHτ1 in H4; auto. mydestr; auto.
Qed.

(* Lemma ctx_denotation_st_update_iff_subst: *)
(*   forall Γ τ (z: atom) (c: constant) (st: state) e, *)
(*     z ∉ (dom aset st) -> *)
(*     ({<[z:=c]> st}⟦τ⟧{Γ} e) <-> (closed_rty 0 ({[z]} ∪ ctxdom Γ ∪ dom aset st) τ /\ {st}⟦({z:=c}r) τ⟧{Γ} e). *)
(* Proof. *)
(*   induction Γ; intros. *)
(*   - split; intros; simpl; RD_simp. *)
(*     + rewrite denotation_st_update_iff_subst in H1; mydestr; try split; auto; refinement_solver3. *)
(*       RD_simp; auto. *)
(*     + mydestr. RD_simp. rewrite denotation_st_update_iff_subst; auto. split; auto; refinement_solver3. *)
(*   - split; intros; mydestr; simpl; RD_simp. *)
(*     + invclear H0. split; refinement_solver3. constructor; auto; refinement_solver3. *)
(*       admit. admit. ctx_erase_simp4. intros. apply H10 in H0. *)

(*       rewrite IHΓ in H6. *)

Fixpoint random_inhabitant (τ: rty) :=
  match τ with
  | {v: _ | _ | _ | _ } => terr
  | [v: TNat | _ | _ | _ ] => nat-gen
  | [v: TBool | _ | _ | _ ] => bool-gen
  | -:{v: T1 | _ | _ | _ } ⤑ τ => vlam T1 (random_inhabitant τ)
  | τ1 ⤑ τ2 => vlam (rty_erase τ1) (random_inhabitant τ2)
  end.

Ltac lc_solver2 :=
  match goal with
    | [H: lc (tlete ?u _) |- lc ?u] => invclear H; auto
    end || lc_solver1.

Lemma subst_subst_tm: ∀ (x : atom) (u_x : value) (y : atom) (u_y: value) (e: tm),
    x ≠ y → y ∉ fv_value u_x →
    {x := u_x }t ({y := u_y }t e) = {y := {x := u_x }v u_y }t ({x := u_x }t e).
Proof.
  intros x u_x y u_y e Hxy Hyux. apply (tm_mutual_rec
           (fun (e: value) => {x := u_x }v ({y := u_y }v e) = {y := {x := u_x }v u_y }v ({x := u_x }v e))
           (fun (e: tm) => {x := u_x }t ({y := u_y }t e) = {y := {x := u_x }v u_y }t ({x := u_x }t e))
        ); simpl; intros; auto;
  try (rewrite H; auto; rewrite H0; auto; rewrite H1; auto);
  try (repeat var_dec_solver; rewrite subst_fresh_value; auto).
Qed.

Lemma subst_subst_value: ∀ (x : atom) (u_x : value) (y : atom) (u_y e : value),
    x ≠ y → y ∉ fv_value u_x →
    {x := u_x }v ({y := u_y }v e) = {y := {x := u_x }v u_y }v ({x := u_x }v e).
Proof.
  intros x u_x y u_y e Hxy Hyux. apply (value_mutual_rec
           (fun (e: value) => {x := u_x }v ({y := u_y }v e) = {y := {x := u_x }v u_y }v ({x := u_x }v e))
           (fun (e: tm) => {x := u_x }t ({y := u_y }t e) = {y := {x := u_x }v u_y }t ({x := u_x }t e))
        ); simpl; intros; auto;
  try (rewrite H; auto; rewrite H0; auto; rewrite H1; auto);
  try (repeat var_dec_solver; rewrite subst_fresh_value; auto).
Qed.

Lemma fv_of_subst_value: forall x (u e: value), fv_value ({x := u }v e) ⊆ (fv_value e ∖ {[x]}) ∪ fv_value u.
Proof.
  intros x u. apply (value_mutual_rec
           (fun (e: value) => fv_value ({x := u }v e) ⊆ fv_value e ∖ {[x]} ∪ fv_value u)
           (fun (e: tm) => fv_tm ({x := u }t e) ⊆ fv_tm e ∖ {[x]} ∪ fv_value u)
        ); simpl; intros; auto; repeat var_dec_solver; set_solver.
Qed.

Lemma fv_of_subst_tm: forall x (u : value) (e: tm), fv_tm ({x := u }t e) ⊆ (fv_tm e ∖ {[x]}) ∪ fv_value u.
Proof.
  intros x u. apply (tm_mutual_rec
           (fun (e: value) => fv_value ({x := u }v e) ⊆ fv_value e ∖ {[x]} ∪ fv_value u)
           (fun (e: tm) => fv_tm ({x := u }t e) ⊆ fv_tm e ∖ {[x]} ∪ fv_value u)
        ); simpl; intros; auto; repeat var_dec_solver; set_solver.
Qed.

Lemma fv_of_subst_value_closed: forall x (u e: value),
    fv_value u ≡ ∅ ->
    fv_value ({x := u }v e) = (fv_value e ∖ {[x]}).
Proof.
  intros x u.
  apply (value_mutual_rec
           (fun (e: value) =>
              fv_value u ≡ ∅ -> fv_value ({x := u }v e) = fv_value e ∖ {[x]})
           (fun (e: tm) =>
              fv_value u ≡ ∅ -> fv_tm ({x := u }t e) = fv_tm e ∖ {[x]})
        ); simpl; intros; auto; repeat var_dec_solver; set_solver.
Qed.

Lemma fv_of_subst_tm_closed: forall x (u: value) e,
    fv_value u ≡ ∅ -> fv_tm ({x := u }t e) = (fv_tm e ∖ {[x]}).
Proof.
  intros x u.
  apply (tm_mutual_rec
           (fun (e: value) =>
              fv_value u ≡ ∅ -> fv_value ({x := u }v e) = fv_value e ∖ {[x]})
           (fun (e: tm) =>
              fv_value u ≡ ∅ -> fv_tm ({x := u }t e) = fv_tm e ∖ {[x]})
        ); simpl; intros; auto; repeat var_dec_solver; set_solver.
Qed.

Lemma tm_msubst_then_subst: forall Γ env,
    instantiation Γ env ->
    forall (z: atom) (u: value) e, z ∉ ctxdom env -> z # u ->
    tm_msubst env ({z := u }t e) = ({z := value_msubst env u }t (tm_msubst env e)).
Proof.
  intros Γ env Hi. unfold closed_value.
  induction Hi; simpl; intros; mydestr; auto.
  - setoid_rewrite subst_subst_tm; try basic_typing_solver.
    rewrite IHHi; try fast_set_solver. simpl.
    apply basic_typing_contains_fv_value in H.
    pose (fv_of_subst_value x v u). set_solver.
Qed.

Lemma value_msubst_then_subst: forall Γ env,
    instantiation Γ env ->
    forall (z: atom) (u: value) e, z ∉ ctxdom env -> z # u ->
    value_msubst env ({z := u }v e) = ({z := value_msubst env u }v (value_msubst env e)).
Proof.
  intros Γ env Hi. unfold closed_value.
  induction Hi; simpl; intros; mydestr; auto.
  - setoid_rewrite subst_subst_value; try basic_typing_solver.
    rewrite IHHi; try fast_set_solver.
    apply basic_typing_contains_fv_value in H.
    pose (fv_of_subst_value x v u). set_solver.
Qed.

Lemma tm_msubst_then_open: forall Γ env,
    instantiation Γ env ->
    forall (u: value) e, ctxdom env ∩ fv_value u = ∅ ->
                    (tm_msubst env e) ^t^ u = (tm_msubst env (e ^t^ u)).
Proof.
  intros Γ env Hi.
  induction Hi; simpl; intros; mydestr; auto.
  - setoid_rewrite subst_open_tm; try basic_typing_solver.
    rewrite (subst_fresh_value u).
    rewrite IHHi; auto; set_solver.
    set_solver.
Qed.

Lemma value_msubst_then_open: forall Γ env,
    instantiation Γ env ->
    forall (u: value) e, ctxdom env ∩ fv_value u = ∅ ->
                    (value_msubst env e) ^v^ u = (value_msubst env (e ^v^ u)).
Proof.
  intros Γ env Hi.
  induction Hi; simpl; intros; mydestr; auto.
  - setoid_rewrite subst_open_value; try basic_typing_solver.
    rewrite (subst_fresh_value u).
    rewrite IHHi; auto; set_solver.
    set_solver.
Qed.

Lemma tm_msubst_then_open_closed: forall Γ env,
    instantiation Γ env ->
    forall (u: value) e, fv_value u ≡ ∅ ->
                    (tm_msubst env e) ^t^ u = (tm_msubst env (e ^t^ u)).
Proof.
  intros. eapply tm_msubst_then_open; eauto. set_solver.
Qed.

Lemma value_msubst_then_open_closed: forall Γ env,
    instantiation Γ env ->
    forall (u: value) e, fv_value u ≡ ∅ ->
                    (value_msubst env e) ^v^ u = (value_msubst env (e ^v^ u)).
Proof.
   intros. eapply value_msubst_then_open; eauto. set_solver.
Qed.

Ltac basic_typing_solver5 :=
  repeat (simpl; (basic_typing_solver4 ||
                    match goal with
                    | [H: _ ⊢t (tvalue _) ⋮t _  |- _ ] => invclear H; eauto
                    | [H: ?Γ ⊢t ?e ⋮t _, H': ?z ∉ ctxdom ?Γ  |- ?z ∉ fv_tm ?e ] =>
                        apply basic_typing_contains_fv_tm in H; simpl in H; fast_set_solver
                    | [H: ?Γ ⊢t ?e ⋮v _, H': ?z ∉ ctxdom ?Γ  |- ?z ∉ fv_value ?e ] =>
                        apply basic_typing_contains_fv_value in H; fast_set_solver
                    | [H: ?Γ ⊢t tlete ?u _ ⋮t _ |- ?Γ ⊢t ?u ⋮t _ ] => invclear H; eauto
                    | [H: ?Γ ⊢t tlete (tvalue ?u) _ ⋮t _ |- ?Γ ⊢t ?u ⋮v _ ] => invclear H; eauto
                    end)).

Lemma instantiation_implies_tm_msubst_closed:
  forall Γ env, instantiation Γ env -> ∀ e T, Γ ⊢t e ⋮t T -> closed_tm (tm_msubst env e).
Proof.
  intros Γ env Hi. induction Hi; simpl; intros; basic_typing_solver5.
  assert (c ⊢t ({x := v }t e0) ⋮t T0) by basic_typing_solver5; eauto.
Qed.

Lemma instantiation_implies_value_msubst_closed:
  forall Γ env, instantiation Γ env -> ∀ (e: value) T, Γ ⊢t e ⋮v T -> closed_value (value_msubst env e).
Proof.
  intros Γ env Hi. induction Hi; simpl; intros; basic_typing_solver5.
  assert (c ⊢t ({x := v }v e0) ⋮v T0) by basic_typing_solver5; eauto.
Qed.

Global Hint Unfold closed_value: core.
Global Hint Unfold closed_tm: core.

Ltac isolver :=
  instantiation_regular_solver || (basic_typing_solver5; lc_solver2).

Lemma tlete_reduce_to_subst: forall (u: value) z e,
    lc u -> lc e ->
    tlete u (z \t\ e) ↪* {z := u }t e.
Proof.
  intros.
  apply multistep_trans with (y := (z \t\ e) ^t^ u).
  apply multistep_R. econstructor; isolver.
  setoid_rewrite subst_as_close_open_tm; eauto.
  apply subst_lc_tm; auto.
  setoid_rewrite subst_as_close_open_tm; eauto.
  eapply multistep_refl. apply subst_lc_tm; auto; isolver.
Qed.

Lemma termR_let_one_step_from_basic_type: forall Γ (u: value) (z: atom) e T,
    lc e -> z ∉ ctxdom Γ ->
    Γ ⊢t (tlete u (z \t\ e)) ⋮t T ->
    (tlete u (z \t\ e)) <-<{ Γ; T} ({z := u }t e).
Proof.
  intros. constructor; auto.
  - eapply multi_preservation; eauto.
    apply tlete_reduce_to_subst; isolver.
  - unfold termRraw. intros. msubst_simpl.
    rewrite lete_step_spec in H3; simpl; mydestr.
    rewrite value_reduce_to_value_implies_same in H4; mydestr; subst.
    invclear H1.
    assert (closed_value (value_msubst env u)) by
    (eapply instantiation_implies_value_msubst_closed; eauto; basic_typing_solver5).
    assert (z ∉ ctxdom env) by instantiation_regular_solver.
    erewrite tm_msubst_then_subst; eauto; try basic_typing_solver5.
    erewrite tm_msubst_then_open_closed in H5; eauto; try basic_typing_solver5.
    rewrite subst_as_close_open_tm in H5; eauto; try basic_typing_solver5.
    erewrite tm_msubst_then_subst in H5; eauto; try basic_typing_solver5.
    eapply value_msubst_closed in H1. rewrite H1 in H5; auto.
    set_solver.
Qed.

Lemma instantiation_implies_value_msubst_lc:
      ∀ Γ Γv, instantiation Γ Γv -> forall (e: value), lc e -> lc (value_msubst Γv e).
Proof.
  intros Γ Γv Hi. induction Hi; simpl; intros; auto.
  - apply IHHi. apply subst_lc_value; basic_typing_solver3.
Qed.

Lemma termR_let_one_step_from_basic_type': forall Γ (u: value) (z: atom) e T,
    lc e -> z ∉ ctxdom Γ ->
    Γ ⊢t (tlete u (z \t\ e)) ⋮t T ->
    ({z := u }t e) <-<{ Γ; T} (tlete u (z \t\ e)).
Proof.
  intros. constructor; auto.
  - eapply multi_preservation; eauto.
    apply tlete_reduce_to_subst; isolver.
  - unfold termRraw. intros. msubst_simpl.
    rewrite lete_step_spec. split.
    + eapply msubst_preserves_body_tm; eauto. lc_solver.
      setoid_rewrite subst_as_close_open_tm; eauto.
      apply subst_lc_tm; auto.
    + invclear H1.
      exists (value_msubst env u). split.
      { rewrite value_reduce_to_value_implies_same; split; auto.
        eapply instantiation_implies_value_msubst_lc; eauto. basic_typing_solver5. }
      assert (z ∉ ctxdom env) by instantiation_regular_solver.
      assert (closed_value (value_msubst env u)) by
        (eapply instantiation_implies_value_msubst_closed; eauto; basic_typing_solver5).
      erewrite tm_msubst_then_subst in H3; eauto; try basic_typing_solver5.
      erewrite tm_msubst_then_open_closed; eauto; try basic_typing_solver5.
      rewrite subst_as_close_open_tm; eauto; try basic_typing_solver5.
      erewrite tm_msubst_then_subst; eauto; try basic_typing_solver5.
      rewrite value_msubst_closed; auto.
      set_solver.
Qed.

Lemma let_store_tyable: forall Γ u Tu z e T,
         Γ ⊢t u ⋮t Tu -> (Γ ++ [(z, Tu)]) ⊢t e ⋮t T -> Γ ⊢t tlete u (z \t\ e) ⋮t T.
Proof.
  intros. auto_exists_L; intros.
  rewrite subst_as_close_open_tm; eauto; try basic_typing_solver5.
  apply basic_has_type_renaming; try basic_typing_solver5.
Qed.

Lemma rRctx_backward_over: forall Γ st z b n d ϕ (e: tm) τ,
    not_overbasety τ ->
    {st}⟦ τ ⟧{ Γ ++ [(z, {v: b | n | d | ϕ})] } e <->
      (closed_rty 0 (ctxdom (Γ ++ [(z, {v: b | n | d | ϕ})]) ∪ dom aset st) τ /\
         ok_dctx (dom aset st) (Γ ++ [(z, {v: b | n | d | ϕ})]) /\
         (⌊Γ ++ [(z, {v: b | n | d | ϕ})]⌋*) ⊢t e ⋮t ⌊τ⌋ /\
            (forall (u: value), {st}⟦ {v: b | n | d | ϕ} ⟧{ Γ } u -> {st}⟦ { z := u }r τ ⟧{ Γ } ({ z := u }t e))).
Proof.
  induction Γ; split; simpl; intros; denotation_simp.
  - RD_simp. split; denotation_simp. split; auto. split; auto.
    intros. RD_simp. apply H13 in H1. RD_simp.
    rewrite denotation_st_update_iff_subst in H0; mydestr; auto; refinement_solver3.
    eapply termR_perserve_rR; eauto; refinement_solver3. ctx_erase_simp4.
    apply termR_let_one_step_from_basic_type; auto.
    basic_typing_solver. refinement_solver3. refinement_solver. denotation_simp.
  - constructor; try refinement_solver. intros.
    assert (({st}⟦{v:b|n|d|ϕ}⟧{ [] }) c_x) as HH by (constructor; auto).
    apply H3 in HH. RD_simp.
    rewrite denotation_st_update_iff_subst; refinement_solver3.
    split; refinement_solver3. denotation_simp.
    eapply termR_perserve_rR; eauto; denotation_simp; refinement_solver3.
    apply termR_let_one_step_from_basic_type'; basic_typing_solver5.
    eapply let_store_tyable; eauto.
  - invclear H0; denotation_simp.
    + do 3 (split; auto). intros. invclear H0. apply ctxrR_cons_over; denotation_simp; auto.
      admit. admit.
      intros.
      assert (({<[a:=c_x]> st}⟦{v:b|0|d|ϕ}⟧{Γ}) (tlete c_x (a \v\ u))) by auto.
      assert ({<[a:=c_x]> st}⟦τ⟧{ Γ ++ [(z, {v:b|0|d|ϕ})] } (tlete c_x (a \t\ e))) by auto.
      rewrite IHΓ in H3; mydestr.
      assert (({<[a:=c_x]> st}⟦{v:b|0|d|ϕ}⟧{Γ}) ({a := c_x}v u)).
      eapply termR_perserve_ctxrR; eauto.
      assert (({st}⟦{v:b|0|d|ϕ}⟧{Γ}) (tlete c_x (a \v\ u))).
      denotation_st_update_iff_subst

      apply H10 in H0.
    + do 3 (split; auto). intros.
  - destruct (classic (not_overbasety r)).
    + constructor; try refinement_solver3. admit.
    + auto_ty_exfalso. apply ctxrR_cons_over; try refinement_solver3.
      intros. rewrite IHΓ. split. admit. split. admit. split. admit.
      intros. admit.
Admitted.

Lemma rRctx_backward_under: forall Γ z τ_z (e: tm) τ,
    ~ not_overbasety τ_z ->
    ⟦ τ ⟧{ Γ ++ [(z, τ_z)] } e <->
      (∃ (u_hat: tm), ⟦ τ_z ⟧{ Γ } u_hat /\
                        (forall (u: tm), ⟦ τ_z ⟧{ Γ } u ->
                                    (∀ (v_u: value), u_hat ↪* v_u -> ⟦ { z:= v_u }r τ ⟧{ Γ } (tlete u (e ^t^ z)))
                        )).
Admitted.

(* Lemma no_halting_term_is_inhabitant: forall τ (st: state) n bst e, *)
(*     not_overbasety τ -> *)
(*     closed_rty n (dom aset st) τ -> *)
(*     (∀ (v: value), ~ e ↪* v) -> [] ⊢t e ⋮t ⌊ τ ⌋ -> {n;bst;st}⟦τ⟧ e. *)
(* Proof. *)
(*   induction τ; simpl; intros. *)
(*   - invclear H. *)
(*   - constructor; auto. constructor; auto. intros. *)
(*   intros. *)

Inductive ctxrR_halt: state -> listctx rty -> rty -> tm -> Prop :=
| ctxrR_halt_nil: forall st τ e, { st }⟦ τ ⟧ e -> ctxrR_halt st [] τ e
| ctxrR_halt_cons_over: forall st (x: atom) B n d ϕ Γ τ (e: tm),
    closed_rty 0 ({[x]} ∪ ctxdom Γ ∪ (dom _ st)) τ ->
    ok_dctx (dom _ st) ((x, {v: B | n | d | ϕ}) :: Γ) ->
    ((x, TBase B) :: (⌊Γ⌋*)) ⊢t e ⋮t ⌊τ⌋ ->
     (forall (c_x: constant), {st}⟦ {v: B | n | d | ϕ} ⟧ c_x ->
                         ctxrR_halt  (<[ x := c_x ]> st) Γ τ (tlete c_x ({ 0 <t~ x} e))) ->
     ctxrR_halt st ((x, {v: B | n | d | ϕ}) :: Γ) τ e
| ctxrR_halt_cons_under: forall st (x: atom) τ_x τ Γ e,
    not_overbasety τ_x ->
    closed_rty 0 ({[x]} ∪ ctxdom Γ ∪ (dom _ st)) τ ->
    ok_dctx (dom _ st) ((x, τ_x) :: Γ) ->
    ((x, ⌊τ_x⌋ ) :: (⌊Γ⌋*)) ⊢t e ⋮t ⌊τ⌋ ->
     (exists e_x_hat, {st}⟦ τ_x ⟧ e_x_hat /\
                   (∃ (v_x_hat: value), e_x_hat ↪* v_x_hat) /\
                   (forall e_x, {st}⟦ τ_x ⟧ e_x ->
                           (∀ (v_x: value), e_x_hat ↪* v_x ->
                                            ctxrR_halt ({ x ↦ v_x } st) Γ τ (tlete e_x ({ 0 <t~ x} e))))) ->
     ctxrR_halt  st ((x, τ_x) :: Γ) τ e.

Inductive halt_ctx: state -> listctx rty -> Prop :=
| halt_ctx_nil: ∀ st, halt_ctx st []
| halt_ctx_cons_ubase: forall st Γ x b n d ϕ,
    halt_ctx st Γ ->
    ~ ({st}⟦ [v: b | n | d | ϕ ] ⟧{ Γ } terr) ->
    halt_ctx st (Γ ++ [(x, [v: b | n | d | ϕ ])])
| halt_ctx_cons: forall st Γ x τ,
    not_underbasety τ ->
    halt_ctx st Γ ->
    halt_ctx st (Γ ++ [(x, τ)]).

Inductive halt_ctx_rev: state -> listctx rty -> Prop :=
| halt_ctx_rev_nil: ∀ st, halt_ctx_rev st []
| halt_ctx_rev_cons_obase: forall st Γ x (b: base_ty) n d (ϕ: refinement),
    (forall (c: constant), [] ⊢t c ⋮v b -> ϕ b∅ ∅ c -> halt_ctx_rev ({ x ↦ c } st) Γ) ->
    halt_ctx_rev st ((x, {v: b | n | d | ϕ }) :: Γ)
| halt_ctx_rev_cons_ubase: forall st Γ x (b: base_ty) n d (ϕ: refinement),
    (exists (c: constant), ϕ b∅ ∅ c) ->
    (forall (c: constant), [] ⊢t c ⋮v b -> halt_ctx_rev ({ x ↦ c } st) Γ) ->
    halt_ctx_rev st ((x, [v: b | n | d | ϕ ]) :: Γ)
| halt_ctx_rev_cons_arr: forall st Γ x τ,
    is_arr τ -> halt_ctx_rev st Γ -> halt_ctx_rev st ((x, τ) :: Γ).

Lemma halt_ctx_pre_destruct: forall Γ x b n d ϕ st,
    halt_ctx st ((x, [v: b | n | d | ϕ ]) :: Γ) <->
      (exists (c: constant), ϕ b∅ ∅ c) /\ (forall (c: constant), [] ⊢t c ⋮v b -> halt_ctx ({ x ↦ c } st) Γ).
Proof.
  apply (rev_ind (fun Γ => forall x b n d ϕ st,
                      halt_ctx st ((x, [v: b | n | d | ϕ ]) :: Γ) <->
                        (exists (c: constant), ϕ b∅ ∅ c) /\ (forall (c: constant), [] ⊢t c ⋮v b -> halt_ctx ({ x ↦ c } st) Γ)));
  repeat split; simpl; intros.
  - invclear H. admit. admit.
  - constructor.
  - mydestr. rewrite <- app_nil_l. constructor. constructor. admit.
  - invclear H0.
    + rewrite app_comm_cons in H1. apply app_inj_tail in H1. mydestr. invclear H1.
      apply H in H2. mydestr. exists x. auto.
    + rewrite app_comm_cons in H1. apply app_inj_tail in H1. mydestr. invclear H1.
      apply H in H4. mydestr. exists x. auto.
  - admit.
  - mydestr. rewrite app_comm_cons. destruct (classic (not_underbasety r)).
    + apply halt_ctx_cons; auto. rewrite H. split. exists x; auto. intros. apply H1 in H3. simpl. invclear H3.

Lemma halt_ctx_implies_halt_ctx_rev: forall Γ, halt_ctx Γ -> halt_ctx_rev ∅ Γ.
Proof.
  induction Γ; intros.
  - constructor.
  - mydestr. invclear H.

Lemma halt_ctx_plus_ctxrR_implies_ctxrR_halt: forall Γ τ e, halt_ctx Γ -> ⟦ τ ⟧{ Γ } e -> ctxrR_halt ∅ Γ τ e.
Proof.
  induction Γ; intros.
  - invclear H0. constructor; auto.
  - mydestr. invclear H.

Lemma random_inhabitant_in_any_under: ∀ (τ: rty) (bst : bstate) n st,
    not_overbasety τ ->
    closed_rty n (dom _ st) τ ->
    ({n;bst;st}⟦τ⟧) (random_inhabitant τ).
Proof.
  induction τ; intros; invclear H; mydestr.
  - destruct B.
    + split; simpl; auto. admit. split; auto. intros. admit.
    + split; simpl; auto. apply mk_bool_gen_typable; auto. split; auto. intros. destruct c; invclear H.
      apply mk_bool_gen_reduce_to_all_bool.
  - denotation_simp. constructor; auto. admit. constructor; auto.
    intros.
    apply (termR_perserve_rR _ (random_inhabitant τ)); auto; try refinement_solver1. admit.
    apply IHτ; refinement_solver1.
  - denotation_simp. constructor; auto. admit. constructor; auto.
    intros.
    assert (exists (v_x: value), e_x ↪* v_x). admit. destruct H1.
    apply (termR_perserve_rR _ (random_inhabitant τ2)); auto; try refinement_solver1. admit.
    apply IHτ2; refinement_solver1.
Admitted.

Lemma denotation_weaken_empty: forall Γ (e: tm) τ (st: state),
    not_overbasety τ ->
    valid_rty τ ->
    ok_dctx (dom _ st) Γ -> { st }⟦ τ ⟧ e -> {st}⟦ τ ⟧{ Γ } e.
Proof.
  induction Γ; intros; denotation_simp; mydestr.
  constructor; auto.
  destruct (classic (not_overbasety r)).
  - constructor; auto; try refinement_solver1.
    exists (random_inhabitant r); invclear H1;
    assert (({0;b∅;st}⟦r⟧) (random_inhabitant r)) by
      (intros; apply random_inhabitant_in_any_under; auto);
    split; try (apply random_inhabitant_in_any_under; auto); intros.
    + assert ([] ⊢t v_x ⋮v ⌊r⌋) as Hv_x by refinement_solver1.
      destruct v_x; mydestr; simpl; try auto_ty_exfalso.
      apply (termR_perserve_ctxrR _ _ e); auto. admit.
      apply IHΓ; auto. denotation_simp. intros. rewrite rR_shadow_update_st; auto; refinement_solver1.
      apply rR_regular1 in H2; mydestr. refinement_solver1.
    + apply (termR_perserve_ctxrR _ _ e); auto. admit.
      apply IHΓ; auto. denotation_simp. admit. admit.
  - refinement_solver1. constructor; auto; try refinement_solver1.
    intros. apply (termR_perserve_ctxrR _ _ e); auto. admit.
    invclear H1; apply IHΓ; auto; try auto_ty_exfalso; try ok_dctx_solver_slow.
    intros. rewrite rR_shadow_update_st; auto; refinement_solver1.
    apply rR_regular1 in H2; mydestr. refinement_solver1.
Admitted.

Lemma denotation_weaken: forall Γ1 Γ3 (e: tm) τ (st: state) Γ2,
    ctxrR_wf (dom _ st) (Γ1 ++ Γ2 ++ Γ3) τ -> {st}⟦ τ ⟧{ Γ1 ++ Γ3 } e -> {st}⟦ τ ⟧{ Γ1 ++ Γ2 ++ Γ3 } e.
Proof.
  intros. generalize dependent Γ2.
  induction H0; intros; listctx_set_simpl.
  - specialize (H b∅). invclear H; mydestr. denotation_simp.
  induction Γ1; intros; listctx_set_simpl.
  - admit.
  - 

Lemma err_exists_denotation_excluded: forall Γ b n (d: aset) (ϕ: refinement),
    closed_rty 0 (dom aset (∅: state)) [v:b|n|d|ϕ] ->
    ~ (⟦ [v: b | n | d | ϕ ] ⟧{ Γ } terr) -> (∃ (v: value), ⟦ {v: b | n | d | ϕ } ⟧{ Γ } v).
Proof.
  induction Γ; intros.
  - destruct (classic (∃ v : value, (⟦{v:b|n|d|ϕ}⟧{ [] }) v)); auto; exfalso; apply H0; clear H0.
    rewrite forall_iff_not_exists1 in H1.
    constructor. intros.
    do 2 constructor; auto. rewrite terr_reduction_iff_false_phi.
    intros. intros Hf.
    specialize (H1 c). apply H1.
    constructor; auto. intros. constructor; auto. constructor; auto.
    rewrite closed_rty_under_iff_over; auto.
    exists c. constructor; auto. constructor; auto.
    rewrite bound_in_refinement_0. apply Hf. refinement_solver1.
  - mydestr. refinement_simp1.
    destruct (classic (∃ v : value, (⟦{v:b|n|d|ϕ}⟧{ a :: Γ }) v)); auto; exfalso; apply H0; clear H0.
    rewrite forall_iff_not_exists1 in H1.
    mydestr.
    constructor.


    repeat match goal with
           | [H: closed_rty _ _ _ |- bound_in_refinement _ _ ] => destruct H; mydestr
           | [H: valid_rty _ |- bound_in_refinement _ _ ] => invclear H
           | [H: wf_r _ _ ?ϕ |- bound_in_refinement _ ?ϕ ] =>  destruct H
           end.
    refinement_simp1; auto.
    repeat match goal with
    | [H: lc_rty_idx 0 [v:_|?n|_|_] |- _ ] =>
        match n with
        | 0 => fail 1
        | _ =>  assert (n = 0) by (apply lc_rty_idx_under_0_is_0 in H; auto); subst
        end
    end.
    repeat match goal with
           | [H: closed_rty _ _ _ |- bound_in_refinement _ _ ] => destruct H; mydestr
           | [H: valid_rty _ |- bound_in_refinement _ _ ] => invclear H
           | [H: wf_r _ _ ?ϕ |- bound_in_refinement _ ?ϕ ] =>  destruct H
           end.
    assert (n = 0) by (apply lc_rty_idx_under_0_is_0 in H2; auto); subst.
    invclear H2.
    destruct H5. 
    invclear H.
 mydestr. invclear H. invclear H6. unfold bound_in_refinement in H5.
    assert (closed_rty 0 ∅ [v:b|n|d|ϕ]). setoid_rewrite <- H2; auto.

    setoid_rewrite H2 in H.

    admit. auto.
    intros. apply multistep_R.

    constructor.
    exfalso. apply H. constructor; intros. rewrite terr_inhabitant_iff_false_phi.


  intros. destruct (classic (∃ v : value, (⟦[v:b|n|d|ϕ]⟧{Γ}) v)); auto.
  exfalso. apply H. rewrite terr_inhabitant_iff_false_phi.

  intro Hf. mydestr. invclear Hf.
    + invclear H. denotation_simp.
      match goal with
      | [H: ∀ c, [] ⊢t (vconst c) ⋮v _ → _ _ ∅ c → terr ↪* (tvalue (vconst c)) |- _ ] => idtac H
      end.
      rewrite terr_inhabitant_implies_false_phi in H4.
      apply H4 in H7.
      repeat match goal with
      | [H: _ ⊢t ?v ⋮v (TBase _) |- _ ] =>
          match v with
          | vconst _ => fail 1
          | _ => set H7 as Htmp; apply empty_basic_typing_base_const_exists in Htmp; mydestr; subst
          end
      end.
      set H7 as Htmp; apply empty_basic_typing_base_const_exists in Htmp; mydestr; subst.
      match goal with
      | [H: _ ⊢t (tvalue ?v) ⋮t _ |- _ ] => invclear H
      end.

      apply empty_basic_typing_base_const_exists in H.

       specialize (H1 b∅).
      invclear H0. invclear H1. mydestr.
Admitted.

(* Denotation: *)

Fixpoint rR (n: nat) (bst: bstate) (st: state) (τ: rty) (e: tm) : Prop :=
  [] ⊢t e ⋮t ⌊ τ ⌋ /\ closed_rty n (dom _ st) τ /\
    match τ with
    | {v: B | _ | _ | ϕ} => exists (c: constant), [] ⊢t c ⋮v B /\ ϕ bst st c /\ e = c
    | [v: B | _ | _ | ϕ] => forall (c: constant), [] ⊢t c ⋮v B -> ϕ bst st c -> e ↪* c
    | -:{v: B | _ | _ | ϕ } ⤑ τ =>
        forall (c_x: constant), [] ⊢t c_x ⋮v B -> ϕ bst st c_x -> rR (S n) (<b[↦ c_x ]> bst) st τ (mk_app e c_x)
    | τ1 ⤑ τ2 => forall (e_x: tm), rR n bst st τ1 e_x -> rR n bst st τ2 (mk_app e e_x)
    end.

Notation " '{' n ';' bst ';' st '}⟦' τ '⟧' " :=
  (rR n bst st τ) (at level 20, format "{ n ; bst ; st }⟦ τ ⟧", bst constr, st constr, τ constr).
Notation " '{' st '}⟦' τ '⟧' " := (fun e => forall bst, rR 0 bst st τ e) (at level 20, format "{ st }⟦ τ ⟧", st constr, τ constr).
Notation " '⟦' τ '⟧' " := (fun e => forall bst, rR 0 bst ∅ τ e) (at level 20, format "⟦ τ ⟧", τ constr).

(* regular of the denation *)

Lemma rR_regular1:
  forall τ n bst st e, { n; bst; st }⟦ τ ⟧ e -> closed_rty n (dom _ st) τ /\ [] ⊢t e ⋮t ⌊ τ ⌋.
Proof.
  induction τ; intros; invclear H; mydestr; subst; split; intros; auto.
Qed.

Lemma rR_regular2:
  forall τ st e, { st }⟦ τ ⟧ e -> closed_rty 0 (dom _ st) τ /\ [] ⊢t e ⋮t ⌊ τ ⌋.
Proof.
  intros. specialize (H b∅). eapply rR_regular1; eauto.
Qed.

Lemma rR_regular3:
  forall τ e, ⟦ τ ⟧ e -> (closed_rty 0 ∅ τ) /\ [] ⊢t e ⋮t ⌊ τ ⌋.
Proof.
  intros. eapply rR_regular2 in H. mydestr; split; auto.
  intros. apply H. my_set_solver.
Qed.

Inductive ctxrR_wf: aset -> listctx rty -> rty -> Prop :=
| ctxrR_wf_nil: forall d τ, closed_rty 0 d τ -> ctxrR_wf d [] τ
| ctxrR_wf_cons_over: forall d (x: atom) τ_x Γ τ,
    ok_dctx d ((x, τ_x) :: Γ) ->
    closed_rty 0 d τ_x ->
    ctxrR_wf ({[x]} ∪ d) Γ τ ->
    ctxrR_wf d ((x, τ_x) :: Γ) τ.

Inductive ctxrR: state -> listctx rty -> rty -> tm -> Prop :=
| ctxrR_nil: forall st τ e, { st }⟦ τ ⟧ e -> ctxrR st [] τ e
| ctxrR_cons_over: forall st (x: atom) B d ϕ Γ τ (e: tm),
    ctxrR_wf (dom _ st) ((x, {v: B | 0 | d | ϕ}) :: Γ) τ ->
    ((x, TBase B) :: (⌊Γ⌋*)) ⊢t e ⋮t ⌊τ⌋ ->
     (forall (c_x: constant), {st}⟦ {v: B | 0 | d | ϕ} ⟧ c_x ->
                         ctxrR (<[ x := c_x ]> st) Γ τ (tlete c_x ({ 0 <t~ x} e))) ->
     ctxrR st ((x, {v: B | 0 | d | ϕ}) :: Γ) τ e
| ctxrR_cons_under: forall st (x: atom) τ_x τ Γ e,
        ctxrR_wf (dom _ st) ((x, τ_x) :: Γ) τ ->
        ((x, ⌊τ_x⌋ ) :: (⌊Γ⌋*)) ⊢t e ⋮t ⌊τ⌋ ->
         not_overbasety τ_x ->
         (exists e_x_hat, {st}⟦ τ_x ⟧ e_x_hat /\
                       (forall e_x, {st}⟦ τ_x ⟧ e_x ->
                               (∀ (v_x: value), e_x_hat ↪* v_x ->
                                                ctxrR ({ x ↦ v_x } st) Γ τ (tlete e_x ({ 0 <t~ x} e))))) ->
         ctxrR st ((x, τ_x) :: Γ) τ e.

Notation " '{' st '}⟦' τ '⟧{' Γ '}' " := (ctxrR st Γ τ) (at level 20, format "{ st }⟦ τ ⟧{ Γ }", st constr, τ constr, Γ constr).
Notation " '⟦' τ '⟧{' Γ '}' " := (ctxrR ∅ Γ τ) (at level 20, format "⟦ τ ⟧{ Γ }", τ constr, Γ constr).

Lemma ctxrR_implies_ctxrR_wf:
  forall Γ τ st e, { st }⟦ τ ⟧{ Γ } e -> ctxrR_wf (dom _ st) Γ τ.
Proof.
  induction Γ; simpl; intros; invclear H; simpl; auto.
  - apply rR_regular2 in H0; mydestr. constructor; auto.
Qed.

Lemma ctxrR_wf_regular:
  forall Γ τ d, ctxrR_wf d Γ τ -> (ok_dctx d Γ) /\ cl_dctx d Γ /\ closed_rty 0 (ctxdom Γ ∪ d) τ.
Proof.
  induction Γ; simpl; intros; invclear H; simpl.
  - split. repeat constructor; auto; fast_set_solver.
    split. constructor.
    closed_rty_solver.
  - apply IHΓ in H6; mydestr2.
    split; auto.
    split. constructor; listctx_set_simpl. closed_rty_solver.
Qed.

Lemma ctxrR_regular0:
  forall Γ τ st e, { st }⟦ τ ⟧{ Γ } e ->
              (ok_dctx (dom _ st) Γ) /\ cl_dctx (dom _ st) Γ /\ closed_rty 0 (ctxdom Γ ∪ (dom _ st)) τ.
Proof.
  intros. apply ctxrR_wf_regular. eapply ctxrR_implies_ctxrR_wf; eauto.
Qed.

Lemma ctxrR_regular1:
  forall Γ τ st e, { st }⟦ τ ⟧{ Γ } e -> ⌊ Γ ⌋* ⊢t e ⋮t ⌊ τ ⌋.
Proof.
  induction Γ; simpl; intros; invclear H; simpl; auto.
  - apply rR_regular2 in H0; mydestr; auto.
Qed.

Lemma ctxrR_regular:
  forall Γ τ st e, { st }⟦ τ ⟧{ Γ } e ->
              ⌊ Γ ⌋* ⊢t e ⋮t ⌊ τ ⌋ /\
                (ok_dctx (dom _ st) Γ) /\
                cl_dctx (dom _ st) Γ /\
                closed_rty 0 (ctxdom Γ ∪ (dom _ st)) τ.
Proof.
  intros. split.
  - eapply ctxrR_regular1; eauto.
  - eapply ctxrR_regular0; eauto.
Qed.

(* Lemma ctxrR_regular1: *)
(*   forall Γ τ st e, { st }⟦ τ ⟧{ Γ } e -> closed_rty 0 (ctxdom Γ ∪ dom _ st) τ /\ *)
(*                 ⌊ Γ ⌋* ⊢t e ⋮t ⌊ τ ⌋ /\ (ok_dctx (dom _ st) Γ). *)
(* Proof. *)
(*   induction Γ; simpl; intros; invclear H; simpl. *)
(*   - apply rR_regular2 in H0; mydestr; split; intros; my_simplify_map_eq. *)
(*     closed_rty_solver. *)
(*     split; auto. constructor; auto. fast_set_solver. *)
(*   - split; auto. closed_rty_solver. *)
(*   - mydestr. split; auto. apply rR_regular2 in H; mydestr. closed_rty_solver. *)
(* Qed. *)

Inductive ctxrR2: bstate -> state -> listctx rty -> rty -> rty -> Prop :=
| ctxrR2_nil: forall bst st τ1 τ2,
    closed_rty 0 (dom _ st) τ1 -> closed_rty 0 (dom _ st) τ2 ->
    (forall e, { st }⟦ τ1 ⟧ e -> { st }⟦ τ2 ⟧ e) ->
    ctxrR2 bst st [] τ1 τ2
| ctxrR2_cons_over: forall bst st (x: atom) B d ϕ Γ τ1 τ2,
    ctxrR_wf (dom _ st) ((x, {v: B | 0 | d | ϕ}) :: Γ) τ1 ->
    ctxrR_wf (dom _ st) ((x, {v: B | 0 | d | ϕ}) :: Γ) τ2 ->
    (forall (c_x: constant), {st}⟦ {v: B | 0 | d | ϕ} ⟧ c_x -> ctxrR2 bst (<[ x := c_x ]> st) Γ τ1 τ2) ->
    ctxrR2 bst st ((x, {v: B | 0 | d | ϕ}) :: Γ) τ1 τ2
| ctxrR2_cons_under: forall bst st (x: atom) τ_x τ1 τ2 Γ,
    ctxrR_wf (dom _ st) ((x, τ_x) :: Γ) τ1 ->
    ctxrR_wf (dom _ st) ((x, τ_x) :: Γ) τ2 ->
    not_overbasety τ_x ->
    (exists e_x_hat, {st}⟦ τ_x ⟧ e_x_hat /\
                  (forall e_x, {st}⟦ τ_x ⟧ e_x ->
                          (∀ (v_x: value), e_x_hat ↪* v_x -> ctxrR2 bst ({ x ↦ v_x } st) Γ τ1 τ2))) ->
    ctxrR2 bst st ((x, τ_x) :: Γ) τ1 τ2.

Notation " '{' st '}⟦' τ1 '⟧⊆⟦' τ2 '⟧{' Γ '}' " := (forall bst, ctxrR2 bst st Γ τ1 τ2) (at level 20, format "{ st }⟦ τ1 ⟧⊆⟦ τ2 ⟧{ Γ }", st constr, τ1 constr, τ2 constr, Γ constr).
Notation " '⟦' τ1 '⟧⊆⟦' τ2 '⟧{' Γ '}' " := (forall bst, ctxrR2 bst ∅ Γ τ1 τ2) (at level 20, format "⟦ τ1 ⟧⊆⟦ τ2 ⟧{ Γ }", τ1 constr, τ2 constr, Γ constr).
