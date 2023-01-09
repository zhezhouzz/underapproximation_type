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

Inductive wf_ctx: listctx rty -> Prop :=
| wf_ctx_nil: wf_ctx []
| wf_ctx_cons_ubase: forall Γ x b n d ϕ,
    wf_ctx Γ ->
    ~ (⟦ [v: b | n | d | ϕ ] ⟧{ Γ } terr) ->
    ok_dctx ∅ (Γ ++ [(x, [v: b | n | d | ϕ])]) ->
    wf_ctx (Γ ++ [(x, [v: b | n | d | ϕ ])])
| wf_ctx_cons: forall Γ x τ,
    wf_ctx Γ ->
    not_underbasety τ ->
    ok_dctx ∅ (Γ ++ [(x, τ)]) ->
    wf_ctx (Γ ++ [(x, τ)]).

Definition wf (Γ: listctx rty) (τ: rty) :=
  wf_ctx Γ /\ closed_rty 0 (ctxdom Γ) τ.

Notation " Γ '⊢WF' τ " := (wf Γ τ) (at level 20, τ constr, Γ constr).

Fixpoint random_inhabitant (τ: rty) :=
  match τ with
  | {v: _ | _ | _ | _ } => terr
  | [v: TNat | _ | _ | _ ] => nat-gen
  | [v: TBool | _ | _ | _ ] => bool-gen
  | -:{v: T1 | _ | _ | _ } ⤑ τ => vlam T1 (random_inhabitant τ)
  | τ1 ⤑ τ2 => vlam (rty_erase τ1) (random_inhabitant τ2)
  end.

Lemma vlam_tyable_dummy: forall Γ e Tx T,
  Γ ⊢t e ⋮t T -> Γ ⊢t vlam Tx e ⋮v Tx ⤍ T.
Proof.
  intros. auto_exists_L; intros.
  rewrite open_rec_lc_tm; basic_typing_solver.
Qed.

Lemma vlam_implies_open_tyable: forall Γ e1 v2 Tx T,
  Γ ⊢t v2 ⋮v Tx -> Γ ⊢t vlam Tx e1 ⋮v Tx ⤍ T -> Γ ⊢t e1 ^t^ v2 ⋮t T.
Proof.
  intros. invclear H0. auto_pose_fv x. repeat specialize_with x.
  eapply basic_typing_subst_tm_pre in H3; eauto.
  rewrite subst_open_tm in H3; basic_typing_solver.
  simpl in H3. var_dec_solver.
  rewrite subst_fresh_tm in H3; basic_typing_solver.
Qed.

Ltac basic_typing_solver6 :=
  repeat (basic_typing_solver ||
     (match goal with
            | [H: ?Γ ⊢t vlam ?Tx ?e1 ⋮v ?Tx ⤍ ?T |- ?Γ ⊢t ?e1 ^t^ ?v2 ⋮t ?T] =>
                apply vlam_implies_open_tyable with (Tx := Tx); eauto
             | [ |- _ ⊢t tvalue _ ⋮t _ ⤍ _ ] => constructor; auto
             | [ |- _ ⊢t vlam _ _ ⋮v _ ⤍ _ ] => apply vlam_tyable_dummy; eauto
             end)).

Lemma random_inhabitant_tyable: ∀ (τ: rty) Γ,
    ok Γ -> Γ ⊢t (random_inhabitant τ) ⋮t ⌊τ⌋.
Proof.
  induction τ; simpl; intros; auto.
  - destruct B; auto.
  - basic_typing_solver6.
  - basic_typing_solver6.
Qed.

Global Hint Resolve random_inhabitant_tyable: core.

Lemma random_inhabitant_lc: forall τ, lc (random_inhabitant τ).
Proof.
  intros.
  assert ([] ⊢t (random_inhabitant τ) ⋮t ⌊τ⌋);
  basic_typing_solver6.
Qed.

Global Hint Resolve random_inhabitant_lc: core.

Ltac refinement_solver4 :=
  repeat (match goal with
          | [H: closed_rty _ _ _ |- lc_rty_idx _ _] => invclear H; mydestr; auto
          | [H: closed_rty _ _ _ |- context [rty_fv _]] => invclear H; mydestr; auto
          end || refinement_solver3).

Lemma value_reduce_to_value_implies_same: forall (v1 v2: value), v1 ↪* v2 <-> v1 = v2 /\ lc v2.
Proof.
  split; intros.
  - invclear H; auto. invclear H0.
  - mydestr; subst. apply multistep_refl; auto.
Qed.

Ltac lc_solver3 :=
  lc_solver ||
  match goal with
  | [|- lc (tvalue (value_msubst _ _))] =>
      eapply instantiation_implies_value_msubst_lc; eauto;
      basic_typing_solver
  end.

Ltac op_solver1 :=
  op_simpl;
  auto;
  repeat match goal with
    | [H: _ ⊢t ?e ⋮t _ |- lc ?e ] => apply basic_typing_regular_tm in H; destruct H; auto
    | [H: _ ↪* ?e |- lc ?e] => apply multi_step_regular in H; mydestr; auto
    | [H: ?Γ ⊢t ?e ⋮t ?T |- (?Γ ++ _) ⊢t ?e ⋮t _] => apply basic_typing_weaken_tm_pre; eauto
    | [H: ?Γ ⊢t _ ⋮t _ |- ok _ ] => apply basic_typing_regular_tm in H; destruct H
    end; listctx_set_solver.

Ltac reduction_simpl1 :=
  repeat (simpl; msubst_simpl ||
            match goal with
            | [H: (tvalue _) ↪* (tvalue _) |- _ ] =>
                rewrite value_reduce_to_value_implies_same in H; mydestr; subst
            | [|- (tvalue _) ↪* (tvalue _)] =>
                rewrite value_reduce_to_value_implies_same; split; eauto
            | [H: context [?e ^v^ _] |- _ ] =>
               assert (lc e) as Htmp by (auto; lc_solver3);
               rewrite (open_rec_lc_value _ e) in H; auto;
               try clear Htmp
           | [|- context [?e ^v^ _] ] =>
               assert (lc e) as Htmp by (auto; lc_solver3);
               rewrite (open_rec_lc_value _ e); auto;
               try clear Htmp
           | [H: context [?e ^t^ _] |- _ ] =>
               assert (lc e) as Htmp by (auto; lc_solver3);
               rewrite (open_rec_lc_tm _ e) in H; auto;
               try clear Htmp
           | [|- context [?e ^t^ _] ] =>
               assert (lc e) as Htmp by (auto; lc_solver3);
               rewrite (open_rec_lc_tm _ e); auto;
               try clear Htmp
            end || auto_reduction_exfalso).

Lemma body_vbvar_0: body (vbvar 0).
Proof.
  unfold body. exists ∅. intros. constructor.
Qed.

Global Hint Resolve body_vbvar_0: core.

Lemma lc_tletapp: forall (v x: value), lc v -> lc x -> lc (tletapp v x (vbvar 0)).
Proof.
  intros. auto_exists_L; simpl; intros; auto.
Qed.

Global Hint Resolve lc_tletapp: core.

Lemma body_tletapp_0: forall (v: value), lc v -> body (tletapp v (vbvar 0) (vbvar 0)).
Proof.
  intros. auto_exists_L; intros. simpl. reduction_simpl1.
Qed.

Global Hint Resolve body_tletapp_0: core.

Global Hint Resolve mk_app_body: core.

Ltac reduction_solver1 :=
  reduction_simpl1; eauto;
  repeat (
      (match goal with
       | [|- lc (tvalue (value_msubst _ _))] =>
           eapply instantiation_implies_value_msubst_lc; eauto;
           basic_typing_solver6
       | [|- lc (tm_msubst _ _)] =>
           eapply instantiation_implies_msubst_lc; eauto;
           basic_typing_solver6
       end) || op_solver1 || basic_typing_solver6).

Lemma tm_open_then_msubst: forall Γ env,
    instantiation Γ env ->
    (forall e v, tm_msubst env (e ^t^ v) = (tm_msubst env e ^t^ value_msubst env v)).
Proof.
  intros Γ env Hi; induction Hi; simpl; intros; auto.
  rewrite subst_open_tm; basic_typing_solver6.
Qed.

Lemma value_open_then_msubst: forall Γ env,
    instantiation Γ env ->
    (forall e v, value_msubst env (e ^v^ v) = (value_msubst env e ^v^ value_msubst env v)).
Proof.
  intros Γ env Hi; induction Hi; simpl; intros; auto.
  rewrite subst_open_value; basic_typing_solver6.
Qed.

Lemma mk_app_reduce_to_open:
  ∀ (Γ : listctx ty) (Tx T : ty) e1 (v2 : value),
    Γ ⊢t v2 ⋮v Tx -> Γ ⊢t (vlam Tx e1) ⋮v (Tx ⤍ T) ->
    (mk_app (vlam Tx e1) v2) <-<{ Γ; T} (e1 ^t^ v2).
Proof.
  intros. constructor.
  - eapply mk_app_typable; eauto.
  - basic_typing_solver6.
  - unfold termRraw. intros. unfold mk_app in H2. reduction_simpl1.
    rewrite lete_step_spec in H2; simpl; mydestr. simpl in H4.
    rewrite lete_step_spec in H4; simpl; mydestr. simpl in H6.
    reduction_simpl1.
    rewrite letapp_step_spec in H6; mydestr.
    destruct H9; mydestr; invclear H9.
    rewrite lete_step_spec in H10; simpl; mydestr. simpl in H11.
    reduction_simpl1.
    erewrite tm_open_then_msubst; eauto.
Qed.

Lemma body_tm_open_1_same: forall e v, body e -> {1 ~t> v} e = e.
Proof.
  intros. invclear H. auto_pose_fv y. specialize_with y.
  apply fact1_tm with (j := 0) (v:=y); try lia.
  rewrite open_rec_lc_tm; auto.
Qed.

Lemma body_value_open_1_same: forall (e: value) v, body e -> {1 ~v> v} e = e.
Proof.
  intros. invclear H. auto_pose_fv y. specialize_with y.
  apply fact1_value with (j := 0) (v:=y); try lia.
  rewrite open_rec_lc_value; auto.
Qed.

Lemma mk_app_reduce_to_let':
  ∀ (Γ : listctx ty) (Tx T : ty) e1 e2,
    Γ ⊢t e2 ⋮t Tx -> Γ ⊢t (vlam Tx e1) ⋮v (Tx ⤍ T) ->
    (tlete e2 e1) <-<{ Γ; T} (mk_app (vlam Tx e1) e2).
Proof.
  intros. constructor; basic_typing_solver6.
  - invclear H0. auto_exists_L; simpl; intros.
  - eapply mk_app_typable; eauto.
  - unfold termRraw. intros. unfold mk_app. reduction_simpl1.
    assert (lc (tm_msubst env e2)) by reduction_solver1.
    assert (lc (value_msubst env (vlam Tx e1))) by
      (eapply instantiation_implies_value_msubst_lc; eauto; basic_typing_solver6).
    rewrite lete_step_spec. split; auto.
    eexists; split; reduction_simpl1.
    rewrite lete_step_spec. split; auto.
    rewrite lete_step_spec in H2; mydestr; subst.
    eexists; split; eauto.
    simpl. rewrite body_tm_open_1_same; reduction_solver1.
    rewrite letapp_step_spec. repeat split; reduction_solver1.
    left. do 2 eexists. split; auto.
    rewrite lete_step_spec. split; auto.
    eexists; split; reduction_solver1.
Qed.

Lemma mk_app_reduce_to_open':
  ∀ (Γ : listctx ty) (Tx T : ty) e1 (v2 : value),
    Γ ⊢t v2 ⋮v Tx -> Γ ⊢t (vlam Tx e1) ⋮v (Tx ⤍ T) ->
    (e1 ^t^ v2) <-<{ Γ; T} (mk_app (vlam Tx e1) v2).
Proof.
  intros. constructor; basic_typing_solver6.
  - eapply mk_app_typable; eauto.
  - unfold termRraw. intros. unfold mk_app. reduction_simpl1.
    assert (lc (value_msubst env v2)) by reduction_solver1.
    assert (lc (value_msubst env (vlam Tx e1))) by
      (eapply instantiation_implies_value_msubst_lc; eauto; basic_typing_solver6).
    rewrite lete_step_spec. split; auto.
    eexists; split; reduction_simpl1.
    rewrite lete_step_spec. split; auto.
    eexists; split; reduction_simpl1.
    simpl. rewrite body_tm_open_1_same; reduction_solver1.
    rewrite letapp_step_spec. repeat split; reduction_solver1.
    left. do 2 eexists. split; auto.
    rewrite lete_step_spec. split; auto.
    eexists; split; reduction_solver1.
    erewrite tm_open_then_msubst in H2; eauto.
Qed.

Ltac neg_apply H := pbc; apply H; clear H; neg_simpl.

Lemma inhabitant_in_arr_rty_halt: forall τ,
    valid_rty τ ->
    (forall n bst st, (∀ e, ({n;bst;st}⟦τ⟧) e -> (exists v, e ↪* v)) \/ (({n;bst;st}⟦τ⟧) terr)).
Proof.
  intros τ Hv. induction Hv; intros.
  - left. intros. invclear H0; mydestr; subst. eexists; eauto.
  - destruct (classic (({n0;bst;st}⟦[v:B|n|d|ϕ]⟧) terr)); auto. left; intros.
    neg_apply H0. invclear H1; mydestr. do 2 (split; auto). intros.
    apply H3 in H5; auto. exfalso; auto. eapply H2; eauto.
  - destruct (classic (({n0;bst;st}⟦-:{v: B | n | d | ϕ}⤑ τ⟧) terr)); auto. left; intros.
    neg_apply H1. invclear H2; mydestr. do 2 (split; auto). intros.
    apply H4 in H6; auto. eapply termR_perserve_rR; eauto. admit.
    (* specialize (IHHv (S n0) (<b[↦c_x]> bst) st). destruct IHHv. *)
    (* + apply H7 in H6. admit. *)
    (* + eapply termR_perserve_rR; eauto. admit. *)
  - destruct (classic ( ({n0;bst;st}⟦(-:{v: B | n | d | ϕ}⤑ τ1) ⤑ τ2⟧) terr)); auto. left; intros.
    neg_apply H0. invclear H1; mydestr. do 2 (split; auto). intros.
    apply H3 in H4; auto.
    eapply termR_perserve_rR; eauto. admit.
  - destruct (classic ( {n;bst;st}⟦(τ11 ⤑ τ12) ⤑ τ2⟧ terr )); auto. left; intros.
    neg_apply H0. invclear H1; mydestr. do 2 (split; auto). intros.
    apply H3 in H4; auto. admit.
Admitted.

Lemma random_inhabitant_in_any_under: ∀ (τ: rty) (bst : bstate) n st,
    not_overbasety τ ->
    closed_rty n (dom _ st) τ ->
    ({n;bst;st}⟦τ⟧) (random_inhabitant τ).
Proof.
  induction τ; intros; invclear H; mydestr.
  - destruct B.
    + do 2 (split; simpl; auto).
      intros. destruct c; invclear H.
      apply mk_nat_gen_reduce_to_all_nat.
    + do 2 (split; simpl; auto).
      intros. destruct c; invclear H.
      apply mk_bool_gen_reduce_to_all_bool.
  - denotation_simp. repeat split; refinement_solver4.
    + basic_typing_solver6.
    + intros.
      apply termR_perserve_rR with (e:= (random_inhabitant τ)); refinement_solver4.
      assert
        ((random_inhabitant τ ^t^ c_x) <-<{ []; ⌊τ⌋} (mk_app (vlam B (random_inhabitant τ)) c_x)). apply mk_app_reduce_to_open'; auto; basic_typing_solver6.
      reduction_simpl1.
      apply IHτ; refinement_solver4.
  - denotation_simp. repeat split; refinement_solver4.
    + basic_typing_solver6.
    + intros.
      (* assert (closed_rty n (dom aset st) τ1) by refinement_solver4. *)
      assert (closed_rty n (dom aset st) τ2) by refinement_solver4.
      (* assert (not_overbasety τ1) by (invclear H0; invclear H3; simpl; auto). *)
      assert (not_overbasety τ2) by (invclear H0; invclear H2; simpl; auto).
      (* apply IHτ1 with (bst:=bst) in H1; auto. *)
      apply IHτ2 with (bst:=bst) in H1; auto.
      apply termR_perserve_rR with (e:= tlete e_x (random_inhabitant τ2)); refinement_solver4.
      apply mk_app_reduce_to_let'; auto; refinement_solver4; basic_typing_solver6.
      apply termR_perserve_rR with (e:= random_inhabitant τ2); refinement_solver4.
      admit.
Admitted.

Lemma rRctx_last_underbase_not_err_implies_drop: forall Γ st τ x b n d ϕ e,
    not_overbasety τ ->
    ¬ ({st}⟦[v:b|n|d|ϕ]⟧{Γ} terr) ->
    ok_dctx (dom _ st) (Γ ++ [(x, [v:b|n|d|ϕ])]) ->
    (* closed_rty 0 (ctxdom Γ ∪ (dom _ st)) τ -> *)
    {st}⟦τ⟧{Γ} e -> {st}⟦τ⟧{Γ ++ [(x, [v:b|n|d|ϕ])] } e.
Proof.
  induction Γ; simpl; intros; auto.
  - constructor.
    + refinement_solver.
    + denotation_simp; refinement_solver.
    + RD_simp; denotation_simp; refinement_solver.
    + RD_simp; denotation_simp; refinement_solver.
    + exists (random_inhabitant [v:b|n|d|ϕ]). split. admit.
      intros. RD_simp. admit.
  - invclear H2.
    + constructor; auto; refinement_solver. admit. admit.
      intros. apply H11 in H2.
      apply IHΓ; auto. admit. admit.
    + mydestr. constructor; auto; refinement_solver. admit. admit.
      exists x1. split; auto. intros.
      eapply H3 in H4; eauto.
      apply IHΓ; auto. admit. admit.
Admitted.

Lemma rRctx_last_not_underbase_implies_drop: forall Γ st τ x r e,
    not_overbasety τ ->
    not_underbasety r ->
    ok_dctx (dom _ st) (Γ ++ [(x, r)]) ->
    {st}⟦τ⟧{Γ} e -> {st}⟦τ⟧{Γ ++ [(x, r)] } e.
Proof.
  induction Γ; simpl; intros; auto.
  - destruct (classic (not_overbasety r)).
    + constructor; auto.
      { denotation_simp; refinement_solver. }
      { RD_simp; denotation_simp; refinement_solver. }
      exists (random_inhabitant r). split. admit.
      intros. RD_simp. admit.
    + refinement_simp1. RD_simp. constructor; auto.
      { denotation_simp; refinement_solver. }
      { RD_simp; denotation_simp; refinement_solver. }
      intros. constructor. admit.
  - invclear H2.
    + constructor; auto; refinement_solver. admit. admit.
      intros. apply H11 in H2.
      apply IHΓ; auto. admit.
    + mydestr. constructor; auto; refinement_solver. admit. admit.
      exists x1. split; auto. intros.
      eapply H3 in H4; eauto.
      apply IHΓ; auto. admit.
Admitted.

Lemma rRctx_pre_weakening: forall Γ2 Γ1 τ,
    not_overbasety τ ->
    (Γ1 ++ Γ2) ⊢WF τ -> (forall e, ⟦τ⟧{Γ1} e -> ⟦τ⟧{Γ1 ++ Γ2} e).
Proof.
  apply (rev_ind (fun Γ2 => forall Γ1 τ,
                      not_overbasety τ ->
                      (Γ1 ++ Γ2) ⊢WF τ -> (forall e, ⟦τ⟧{Γ1} e -> ⟦τ⟧{Γ1 ++ Γ2} e)));
    intros; denotation_simp; auto.
  - invclear H1. invclear H3; listctx_set_simpl. admit. admit.
Admitted.

Lemma rRctx_weakening_empty: forall Γ τ, Γ ⊢WF τ -> (forall e, ⟦τ⟧ e -> ⟦τ⟧{Γ} e).
Admitted.

(* Lemma rRctx_backward_over: forall Γ z b n d ϕ (e: tm) τ, *)
(*     not_overbasety τ -> *)
(*     (forall st, {st}⟦ τ ⟧{ Γ ++ [(z, {v: b | n | d | ϕ})] } e) <-> *)
(*       (forall st, *)
(*           (closed_rty 0 (ctxdom (Γ ++ [(z, {v: b | n | d | ϕ})]) ∪ dom aset st) τ /\ *)
(*              ok_dctx (dom aset st) (Γ ++ [(z, {v: b | n | d | ϕ})]) /\ *)
(*              (⌊Γ ++ [(z, {v: b | n | d | ϕ})]⌋ ⊢t e ⋮t ⌊τ⌋ /\ *)
(*                 (forall (u: constant), {st}⟦ {v: b | n | d | ϕ} ⟧{ Γ } u -> {st}⟦ { z := u }r τ ⟧{ Γ } ({ z := u }t e)))). *)
(* Proof. *)
(*   induction Γ; split; simpl; intros; denotation_simp. *)
(*   - specialize (H0 st). RD_simp. split; denotation_simp. split; auto. split; auto. *)
(*     intros. RD_simp. apply H13 in H1. RD_simp. *)
(*     rewrite denotation_st_update_iff_subst in H0; mydestr; auto; refinement_solver3. *)
(*   - specialize (H0 st); mydestr. constructor; try refinement_solver. intros. *)
(*     assert (({st}⟦{v:b|n|d|ϕ}⟧{ [] }) c_x) as HH by (constructor; auto). *)
(*     apply H3 in HH. RD_simp. *)
(*     rewrite denotation_st_update_iff_subst; refinement_solver3. *)
(*     split; refinement_solver3. *)
(*   - specialize (H0 st). *)
(*     assert (ok_dctx (dom aset st) ((a, r) :: Γ ++ [(z, {v:b|n|d|ϕ})])) as Hok by *)
(*     (apply ctxrR_regular1 in H0; mydestr; auto). *)
(*     invclear H0; denotation_simp. *)
(*     + do 3 (split; auto). intros. invclear H0. *)
(*       { apply ctxrR_cons_over; denotation_simp; auto. admit. admit. *)
(*         intros. *)
(*         assert (({<[a:=c_x]> st}⟦{v:b|0|d|ϕ}⟧{Γ}) ({a := c_x}v u)) by auto. *)
(*         assert ({<[a:=c_x]> st}⟦τ⟧{ Γ ++ [(z, {v:b|0|d|ϕ})] } ({a := c_x}t e)) by auto. *)
(*         rewrite IHΓ in H3; mydestr; auto. *)
(*         apply H8 in H1. lc_simpl. rewrite subst_commute_tm; simpl; auto; refinement_solver. *)
(*         admit. fast_set_solver. *)
(*       } *)
(*       { mydestr. constructor; denotation_simp; auto. } *)
(*     + do 3 (split; auto). intros. *)
(*       invclear H2. *)
(*       { apply ctxrR_cons_over; denotation_simp; auto. } *)
(*       { mydestr. constructor; denotation_simp; auto. admit. admit. *)
(*         exists x. *)
(*         (* assert (({0;b∅;st}⟦r⟧) (random_inhabitant r)) as Hzz. admit. *) *)
(*         split; auto. intros. eapply H1 in H6; eauto. *)
(*         rewrite IHΓ in H6; mydestr; auto. *)
(*         assert (({({a↦v_x}) st}⟦{v:b|0|d|ϕ}⟧{Γ}) u) as Hu. admit. *)
(*         apply H16 in Hu. simpl in Hu. admit. *)
(*       } *)
(*   - destruct (classic (not_overbasety r)). *)
(*     + constructor; try refinement_solver3. admit. *)
(*       (* exists (random_inhabitant r). split. admit. intros. *) *)
(*       (* rewrite IHΓ; auto. split. admit. split. admit. split. admit. *) *)
(*       (* intros. *) *)
(*       (* assert (({st}⟦{v:b|n|d|ϕ}⟧{(a, r) :: Γ}) u) as Hu. *) *)
(*       (* { constructor; auto. admit. admit. admit. admit. } *) *)
(*       (* apply H3 in Hu. invclear Hu. denotation_simp. mydestr. *) *)
(*     + refinement_simp1. apply ctxrR_cons_over; denotation_simp; auto. *)
(*       intros. *)
(*       rewrite IHΓ; auto. split. admit. split. admit. split. admit. *)
(*       intros. *)
(*       assert (({st}⟦{v:b|n|d|ϕ}⟧{(a, {v:x|x0|x1|x2}) :: Γ}) u) as Hu. *)
(*       { constructor. admit. admit. admit. admit. } *)

(*       auto_ty_exfalso. apply ctxrR_cons_over; try refinement_solver3. *)
(*       intros. rewrite IHΓ. split. admit. split. admit. split. admit. *)
(*       intros. admit. *)
(* Admitted. *)

Lemma rRctx_backward_over_forward: forall Γ st z b n d ϕ (e: tm) τ,
    not_overbasety τ ->
    {st}⟦ τ ⟧{ Γ ++ [(z, {v: b | n | d | ϕ})] } e ->
      (closed_rty 0 (ctxdom (Γ ++ [(z, {v: b | n | d | ϕ})]) ∪ dom aset st) τ /\
         ok_dctx (dom aset st) (Γ ++ [(z, {v: b | n | d | ϕ})]) /\
         (⌊Γ ++ [(z, {v: b | n | d | ϕ})]⌋*) ⊢t e ⋮t ⌊τ⌋ /\
            (forall (u: constant), {st}⟦ {v: b | n | d | ϕ} ⟧{ Γ } u -> {st}⟦ { z := u }r τ ⟧{ Γ } ({ z := u }t e))).
Proof.
  induction Γ; simpl; intros; denotation_simp.
  - RD_simp. split; denotation_simp. split; auto. split; auto.
    intros. RD_simp. apply H13 in H1. RD_simp.
    rewrite denotation_st_update_iff_subst in H0; mydestr; auto; refinement_solver3.
  - assert (ok_dctx (dom aset st) ((a, r) :: Γ ++ [(z, {v:b|n|d|ϕ})])) as Hok by
    (apply ctxrR_regular1 in H0; mydestr; auto).
    invclear H0; denotation_simp.
    + do 3 (split; auto). intros. invclear H0.
      { apply ctxrR_cons_over; denotation_simp; auto. admit. admit.
        intros.
        assert (({<[a:=c_x]> st}⟦{v:b|0|d|ϕ}⟧{Γ}) ({a := c_x}v u)) by auto.
        assert ({<[a:=c_x]> st}⟦τ⟧{ Γ ++ [(z, {v:b|0|d|ϕ})] } ({a := c_x}t e)) by auto.
        apply IHΓ in H3; mydestr; auto.
        apply H8 in H1. lc_simpl. rewrite subst_commute_tm; simpl; auto; refinement_solver.
        admit. fast_set_solver.
      }
      { mydestr. constructor; denotation_simp; auto. }
    + do 3 (split; auto). intros.
      invclear H2.
      { apply ctxrR_cons_over; denotation_simp; auto. }
      { mydestr. constructor; denotation_simp; auto. admit. admit.
        exists x.
        (* assert (({0;b∅;st}⟦r⟧) (random_inhabitant r)) as Hzz. admit. *)
        split; auto. intros. eapply H1 in H6; eauto.
        apply IHΓ in H6; mydestr; auto.
        assert (({({a↦v_x}) st}⟦{v:b|0|d|ϕ}⟧{Γ}) u) as Hu. admit.
        apply H16 in Hu. simpl in Hu. admit.
      }
Admitted.

(* Lemma rRctx_backward_over: forall Γ st z b n d ϕ (e: tm) τ, *)
(*     not_overbasety τ -> *)
(*     {st}⟦ τ ⟧{ Γ ++ [(z, {v: b | n | d | ϕ})] } e <-> *)
(*       (closed_rty 0 (ctxdom (Γ ++ [(z, {v: b | n | d | ϕ})]) ∪ dom aset st) τ /\ *)
(*          ok_dctx (dom aset st) (Γ ++ [(z, {v: b | n | d | ϕ})]) /\ *)
(*          (⌊Γ ++ [(z, {v: b | n | d | ϕ})]⌋ ⊢t e ⋮t ⌊τ⌋ /\ *)
(*             (forall (u: constant), {st}⟦ {v: b | n | d | ϕ} ⟧{ Γ } u -> {st}⟦ { z := u }r τ ⟧{ Γ } ({ z := u }t e))). *)
(* Proof. *)
(*   induction Γ; split; simpl; intros; denotation_simp. *)
(*   - RD_simp. split; denotation_simp. split; auto. split; auto. *)
(*     intros. RD_simp. apply H13 in H1. RD_simp. *)
(*     rewrite denotation_st_update_iff_subst in H0; mydestr; auto; refinement_solver3. *)
(*   - constructor; try refinement_solver. intros. *)
(*     assert (({st}⟦{v:b|n|d|ϕ}⟧{ [] }) c_x) as HH by (constructor; auto). *)
(*     apply H3 in HH. RD_simp. *)
(*     rewrite denotation_st_update_iff_subst; refinement_solver3. *)
(*     split; refinement_solver3. *)
(*   - assert (ok_dctx (dom aset st) ((a, r) :: Γ ++ [(z, {v:b|n|d|ϕ})])) as Hok by *)
(*     (apply ctxrR_regular1 in H0; mydestr; auto). *)
(*     invclear H0; denotation_simp. *)
(*     + do 3 (split; auto). intros. invclear H0. *)
(*       { apply ctxrR_cons_over; denotation_simp; auto. admit. admit. *)
(*         intros. *)
(*         assert (({<[a:=c_x]> st}⟦{v:b|0|d|ϕ}⟧{Γ}) ({a := c_x}v u)) by auto. *)
(*         assert ({<[a:=c_x]> st}⟦τ⟧{ Γ ++ [(z, {v:b|0|d|ϕ})] } ({a := c_x}t e)) by auto. *)
(*         rewrite IHΓ in H3; mydestr; auto. *)
(*         apply H8 in H1. lc_simpl. rewrite subst_commute_tm; simpl; auto; refinement_solver. *)
(*         admit. fast_set_solver. *)
(*       } *)
(*       { mydestr. constructor; denotation_simp; auto. } *)
(*     + do 3 (split; auto). intros. *)
(*       invclear H2. *)
(*       { apply ctxrR_cons_over; denotation_simp; auto. } *)
(*       { mydestr. constructor; denotation_simp; auto. admit. admit. *)
(*         exists x. *)
(*         (* assert (({0;b∅;st}⟦r⟧) (random_inhabitant r)) as Hzz. admit. *) *)
(*         split; auto. intros. eapply H1 in H6; eauto. *)
(*         rewrite IHΓ in H6; mydestr; auto. *)
(*         assert (({({a↦v_x}) st}⟦{v:b|0|d|ϕ}⟧{Γ}) u) as Hu. admit. *)
(*         apply H16 in Hu. simpl in Hu. admit. *)
(*       } *)
(*   - destruct (classic (not_overbasety r)). *)
(*     + constructor; try refinement_solver3. admit. *)
(*       (* exists (random_inhabitant r). split. admit. intros. *) *)
(*       (* rewrite IHΓ; auto. split. admit. split. admit. split. admit. *) *)
(*       (* intros. *) *)
(*       (* assert (({st}⟦{v:b|n|d|ϕ}⟧{(a, r) :: Γ}) u) as Hu. *) *)
(*       (* { constructor; auto. admit. admit. admit. admit. } *) *)
(*       (* apply H3 in Hu. invclear Hu. denotation_simp. mydestr. *) *)
(*     + refinement_simp1. apply ctxrR_cons_over; denotation_simp; auto. *)
(*       intros. *)
(*       rewrite IHΓ; auto. split. admit. split. admit. split. admit. *)
(*       intros. *)
(*       assert (({st}⟦{v:b|n|d|ϕ}⟧{(a, {v:x|x0|x1|x2}) :: Γ}) u) as Hu. *)
(*       { constructor. admit. admit. admit. admit. } *)
(*       (* auto_ty_exfalso. apply ctxrR_cons_over; try refinement_solver3. *) *)
(*       (* intros. rewrite IHΓ. split. admit. split. admit. split. admit. *) *)
(*       (* intros. admit. *) *)
(* Admitted. *)


(* Lemma rRctx_backward_over: forall Γ st z b n d ϕ (e: tm) τ, *)
(*     not_overbasety τ -> *)
(*     {st}⟦ τ ⟧{ Γ ++ [(z, {v: b | n | d | ϕ})] } e <-> *)
(*       (closed_rty 0 (ctxdom (Γ ++ [(z, {v: b | n | d | ϕ})]) ∪ dom aset st) τ /\ *)
(*          ok_dctx (dom aset st) (Γ ++ [(z, {v: b | n | d | ϕ})]) /\ *)
(*          (⌊Γ ++ [(z, {v: b | n | d | ϕ})]⌋ ⊢t e ⋮t ⌊τ⌋ /\ *)
(*             (forall (u: value), {st}⟦ {v: b | n | d | ϕ} ⟧{ Γ } u -> {st}⟦ { z := u }r τ ⟧{ Γ } ({ z := u }t e))). *)
(* Proof. *)
(*   induction Γ; split; simpl; intros; denotation_simp. *)
(*   - RD_simp. split; denotation_simp. split; auto. split; auto. *)
(*     intros. RD_simp. apply H13 in H1. RD_simp. *)
(*     rewrite denotation_st_update_iff_subst in H0; mydestr; auto; refinement_solver3. *)
(*     (* eapply termR_perserve_rR; eauto; refinement_solver3. ctx_erase_simp4. *) *)
(*     (* apply termR_let_one_step_from_basic_type; auto. *) *)
(*     (* basic_typing_solver. refinement_solver3. refinement_solver. denotation_simp. *) *)
(*   - constructor; try refinement_solver. intros. *)
(*     assert (({st}⟦{v:b|n|d|ϕ}⟧{ [] }) c_x) as HH by (constructor; auto). *)
(*     apply H3 in HH. RD_simp. *)
(*     rewrite denotation_st_update_iff_subst; refinement_solver3. *)
(*     split; refinement_solver3. *)
(*     (* denotation_simp. *) *)
(*     (* eapply termR_perserve_rR; eauto; denotation_simp; refinement_solver3. *) *)
(*     (* apply termR_let_one_step_from_basic_type'; basic_typing_solver5. *) *)
(*     (* eapply let_store_tyable; eauto. *) *)
(*   - invclear H0; denotation_simp. *)
(*     + do 3 (split; auto). intros. invclear H0. *)
(*       { apply ctxrR_cons_over; denotation_simp; auto. admit. admit. *)
(*         intros. *)
(*         assert (({<[a:=c_x]> st}⟦{v:b|0|d|ϕ}⟧{Γ}) ({a := c_x}v u)) by auto. *)
(*         assert ({<[a:=c_x]> st}⟦τ⟧{ Γ ++ [(z, {v:b|0|d|ϕ})] } ({a := c_x}t e)) by auto. *)
(*         rewrite IHΓ in H3; mydestr; auto. *)
(*         apply H8 in H1. admit. *)
(*       } *)
(*       { admit. } *)
(*     + admit. *)
(*   - destruct (classic (not_overbasety r)). *)
(*     + constructor; try refinement_solver3. admit. *)
(*     + auto_ty_exfalso. apply ctxrR_cons_over; try refinement_solver3. *)
(*       intros. rewrite IHΓ. split. admit. split. admit. split. admit. *)
(*       intros. admit. *)
(* Admitted. *)

Lemma rRctx_backward_under_forward: forall Γ st z τ_z (e: tm) τ,
    not_overbasety τ ->
    ~ not_overbasety τ_z ->
    {st}⟦ τ ⟧{ Γ ++ [(z, τ_z)] } e ->
      (∃ (u_hat: tm), {st}⟦ τ_z ⟧{ Γ } u_hat /\
                        (forall (u: tm), {st}⟦ τ_z ⟧{ Γ } u ->
                                    (∀ (v_u: value), u_hat ↪* v_u -> {st}⟦ { z:= v_u }r τ ⟧{ Γ } (tlete u (e ^t^ z)))
                        )).
Admitted.

(* Lemma random_inhabitant_in_any_under: ∀ (τ: rty) (bst : bstate) n st, *)
(*     not_overbasety τ -> *)
(*     closed_rty n (dom _ st) τ -> *)
(*     ({n;bst;st}⟦τ⟧) (random_inhabitant τ). *)
(* Proof. *)
(*   induction τ; intros; invclear H; mydestr. *)
(*   - destruct B. *)
(*     + split; simpl; auto. admit. split; auto. intros. admit. *)
(*     + split; simpl; auto. apply mk_bool_gen_typable; auto. split; auto. intros. destruct c; invclear H. *)
(*       apply mk_bool_gen_reduce_to_all_bool. *)
(*   - denotation_simp. constructor; auto. admit. constructor; auto. *)
(*     intros. *)
(*     apply (termR_perserve_rR _ (random_inhabitant τ)); auto; try refinement_solver1. admit. *)
(*     apply IHτ; refinement_solver1. *)
(*   - denotation_simp. constructor; auto. admit. constructor; auto. *)
(*     intros. *)
(*     assert (exists (v_x: value), e_x ↪* v_x). admit. destruct H1. *)
(*     apply (termR_perserve_rR _ (random_inhabitant τ2)); auto; try refinement_solver1. admit. *)
(*     apply IHτ2; refinement_solver1. *)
(* Admitted. *)

(* Lemma denotation_weaken_empty: forall Γ (e: tm) τ (st: state), *)
(*     not_overbasety τ -> *)
(*     valid_rty τ -> *)
(*     ok_dctx (dom _ st) Γ -> { st }⟦ τ ⟧ e -> {st}⟦ τ ⟧{ Γ } e. *)
(* Proof. *)
(*   induction Γ; intros; denotation_simp; mydestr. *)
(*   constructor; auto. *)
(*   destruct (classic (not_overbasety r)). *)
(*   - constructor; auto; try refinement_solver1. *)
(*     exists (random_inhabitant r); invclear H1; *)
(*     assert (({0;b∅;st}⟦r⟧) (random_inhabitant r)) by *)
(*       (intros; apply random_inhabitant_in_any_under; auto); *)
(*     split; try (apply random_inhabitant_in_any_under; auto); intros. *)
(*     + assert ([] ⊢t v_x ⋮v ⌊r⌋) as Hv_x by refinement_solver1. *)
(*       destruct v_x; mydestr; simpl; try auto_ty_exfalso. *)
(*       apply (termR_perserve_ctxrR _ _ e); auto. admit. *)
(*       apply IHΓ; auto. denotation_simp. intros. rewrite rR_shadow_update_st; auto; refinement_solver1. *)
(*       apply rR_regular1 in H2; mydestr. refinement_solver1. *)
(*     + apply (termR_perserve_ctxrR _ _ e); auto. admit. *)
(*       apply IHΓ; auto. denotation_simp. admit. admit. *)
(*   - refinement_solver1. constructor; auto; try refinement_solver1. *)
(*     intros. apply (termR_perserve_ctxrR _ _ e); auto. admit. *)
(*     invclear H1; apply IHΓ; auto; try auto_ty_exfalso; try ok_dctx_solver_slow. *)
(*     intros. rewrite rR_shadow_update_st; auto; refinement_solver1. *)
(*     apply rR_regular1 in H2; mydestr. refinement_solver1. *)
(* Admitted. *)

(* Lemma denotation_weaken: forall Γ1 Γ3 (e: tm) τ (st: state) Γ2, *)
(*     ctxrR_wf (dom _ st) (Γ1 ++ Γ2 ++ Γ3) τ -> {st}⟦ τ ⟧{ Γ1 ++ Γ3 } e -> {st}⟦ τ ⟧{ Γ1 ++ Γ2 ++ Γ3 } e. *)
(* Proof. *)
(*   intros. generalize dependent Γ2. *)
(*   induction H0; intros; listctx_set_simpl. *)
(*   - specialize (H b∅). invclear H; mydestr. denotation_simp. *)
(*   induction Γ1; intros; listctx_set_simpl. *)
(*   - admit. *)
(*   -  *)

(* Lemma err_exists_denotation_excluded: forall Γ b n (d: aset) (ϕ: refinement), *)
(*     closed_rty 0 (dom aset (∅: state)) [v:b|n|d|ϕ] -> *)
(*     ~ (⟦ [v: b | n | d | ϕ ] ⟧{ Γ } terr) -> (∃ (v: value), ⟦ {v: b | n | d | ϕ } ⟧{ Γ } v). *)
(* Proof. *)
(*   induction Γ; intros. *)
(*   - destruct (classic (∃ v : value, (⟦{v:b|n|d|ϕ}⟧{ [] }) v)); auto; exfalso; apply H0; clear H0. *)
(*     rewrite forall_iff_not_exists1 in H1. *)
(*     constructor. intros. *)
(*     do 2 constructor; auto. rewrite terr_reduction_iff_false_phi. *)
(*     intros. intros Hf. *)
(*     specialize (H1 c). apply H1. *)
(*     constructor; auto. intros. constructor; auto. constructor; auto. *)
(*     rewrite closed_rty_under_iff_over; auto. *)
(*     exists c. constructor; auto. constructor; auto. *)
(*     rewrite bound_in_refinement_0. apply Hf. refinement_solver1. *)
(*   - mydestr. refinement_simp1. *)
(*     destruct (classic (∃ v : value, (⟦{v:b|n|d|ϕ}⟧{ a :: Γ }) v)); auto; exfalso; apply H0; clear H0. *)
(*     rewrite forall_iff_not_exists1 in H1. *)
(*     mydestr. *)
(*     constructor. *)


(*     repeat match goal with *)
(*            | [H: closed_rty _ _ _ |- bound_in_refinement _ _ ] => destruct H; mydestr *)
(*            | [H: valid_rty _ |- bound_in_refinement _ _ ] => invclear H *)
(*            | [H: wf_r _ _ ?ϕ |- bound_in_refinement _ ?ϕ ] =>  destruct H *)
(*            end. *)
(*     refinement_simp1; auto. *)
(*     repeat match goal with *)
(*     | [H: lc_rty_idx 0 [v:_|?n|_|_] |- _ ] => *)
(*         match n with *)
(*         | 0 => fail 1 *)
(*         | _ =>  assert (n = 0) by (apply lc_rty_idx_under_0_is_0 in H; auto); subst *)
(*         end *)
(*     end. *)
(*     repeat match goal with *)
(*            | [H: closed_rty _ _ _ |- bound_in_refinement _ _ ] => destruct H; mydestr *)
(*            | [H: valid_rty _ |- bound_in_refinement _ _ ] => invclear H *)
(*            | [H: wf_r _ _ ?ϕ |- bound_in_refinement _ ?ϕ ] =>  destruct H *)
(*            end. *)
(*     assert (n = 0) by (apply lc_rty_idx_under_0_is_0 in H2; auto); subst. *)
(*     invclear H2. *)
(*     destruct H5.  *)
(*     invclear H. *)
(*  mydestr. invclear H. invclear H6. unfold bound_in_refinement in H5. *)
(*     assert (closed_rty 0 ∅ [v:b|n|d|ϕ]). setoid_rewrite <- H2; auto. *)

(*     setoid_rewrite H2 in H. *)

(*     admit. auto. *)
(*     intros. apply multistep_R. *)

(*     constructor. *)
(*     exfalso. apply H. constructor; intros. rewrite terr_inhabitant_iff_false_phi. *)


(*   intros. destruct (classic (∃ v : value, (⟦[v:b|n|d|ϕ]⟧{Γ}) v)); auto. *)
(*   exfalso. apply H. rewrite terr_inhabitant_iff_false_phi. *)

(*   intro Hf. mydestr. invclear Hf. *)
(*     + invclear H. denotation_simp. *)
(*       match goal with *)
(*       | [H: ∀ c, [] ⊢t (vconst c) ⋮v _ → _ _ ∅ c → terr ↪* (tvalue (vconst c)) |- _ ] => idtac H *)
(*       end. *)
(*       rewrite terr_inhabitant_implies_false_phi in H4. *)
(*       apply H4 in H7. *)
(*       repeat match goal with *)
(*       | [H: _ ⊢t ?v ⋮v (TBase _) |- _ ] => *)
(*           match v with *)
(*           | vconst _ => fail 1 *)
(*           | _ => set H7 as Htmp; apply empty_basic_typing_base_const_exists in Htmp; mydestr; subst *)
(*           end *)
(*       end. *)
(*       set H7 as Htmp; apply empty_basic_typing_base_const_exists in Htmp; mydestr; subst. *)
(*       match goal with *)
(*       | [H: _ ⊢t (tvalue ?v) ⋮t _ |- _ ] => invclear H *)
(*       end. *)

(*       apply empty_basic_typing_base_const_exists in H. *)

(*        specialize (H1 b∅). *)
(*       invclear H0. invclear H1. mydestr. *)
(* Admitted. *)
