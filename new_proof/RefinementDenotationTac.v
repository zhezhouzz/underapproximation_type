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
From CT Require Import TermOrdering.
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
Import TermOrdering.

Lemma refinement_exclude_const: forall b (ϕ: refinement),
    (∀ c: constant, [] ⊢t c ⋮v b → ~ ϕ b∅ ∅ c) \/ (exists c: constant, [] ⊢t c ⋮v b /\ ϕ b∅ ∅ c).
Proof.
  intros.
  destruct (classic (exists c: constant, [] ⊢t c ⋮v b /\ ϕ b∅ ∅ c)); auto.
  - left. intros. intro Hf. apply H. exists c. split; auto.
Qed.

Lemma terr_reduction_iff_false_phi: forall bst st b (ϕ: refinement),
    (∀ c : constant, [] ⊢t c ⋮v b → ϕ bst st c → terr ↪* c) <-> (forall c: constant, [] ⊢t c ⋮v b → ~ ϕ bst st c).
Proof.
  split; intros.
  - intros Hf. apply H in Hf; auto. auto_reduction_exfalso.
  - apply H in H0. auto_reduction_exfalso.
Qed.

Lemma terr_inhabitant_iff_false_phi: forall n1 n2 bst st (d: aset) b (ϕ: refinement),
    {n1; bst; st}⟦ [v: b | n2 | d | ϕ ] ⟧ terr <->
      (closed_rty n1 (dom aset st) [v:b|n2|d|ϕ] /\ forall c: constant, [] ⊢t c ⋮v b → ~ ϕ bst st c).
Proof.
  split; intros.
  - invclear H. mydestr; subst. rewrite terr_reduction_iff_false_phi in H1; auto.
  - mydestr. constructor; auto. split; auto. rewrite terr_reduction_iff_false_phi; auto.
Qed.

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

Definition base_type_max_term (b: base_ty) :=
  match b with
  | TNat => nat-gen
  | TBool => bool-gen
  end.

Ltac denotation_simp0 :=
  my_simplify_map_eq3;
  ctx_erase_simp;
  repeat match goal with
    | [H: forall bst, ({0;bst; _ }⟦ _ ⟧) _ |- _ ] => specialize (H b∅); invclear H; mydestr
    | [H: _ ⊢t (tvalue ?v) ⋮t _ |- _ ] => invclear H
    | [H: _ ⊢t _ ⋮t ⌊ _ ⌋ |- _ ] => progress simpl in H
    | [H: _ ⊢t _ ⋮v ⌊ _ ⌋ |- _ ] => progress simpl in H
    | [H: ({_}⟦?τ⟧{_}) _  |- closed_rty _ _ ?τ] => apply ctxrR_regular in H; mydestr
    | [H: ({_}⟦_⟧{?Γ}) _  |- ok_dctx _ ?Γ] => apply ctxrR_regular in H; mydestr; my_simplify_map_eq
    | [H: _ ⊢t ?v ⋮v (TBase _) |- _ ] =>
        match v with
        | vconst _ => fail 1
        | _ => set H as Htmp; apply empty_basic_typing_base_const_exists in Htmp; mydestr; subst
        end
    | [H: ∀ c, [] ⊢t (vconst c) ⋮v _ → _ _ ∅ c → terr ↪* (tvalue (vconst c)) |- _ ] =>
        rewrite terr_reduction_iff_false_phi in H
    end.

Lemma base_type_max_term_typable: forall b, [] ⊢t base_type_max_term b ⋮t b.
Proof.
  destruct b; simpl; eauto.
Qed.

Global Hint Resolve base_type_max_term_typable: core.

Lemma base_type_max_term_spec: forall (b: base_ty) (c: constant), [] ⊢t c ⋮v b -> base_type_max_term b ↪* c.
Proof.
  destruct b; simpl; intros; eauto; destruct c; invclear H; subst; eauto.
Qed.

Global Hint Resolve base_type_max_term_spec: core.

Lemma base_type_reduce_to_constant: forall e (b: base_ty) (v: value), [] ⊢t e ⋮t b -> e ↪* v -> (∃ c, v = vconst c).
Proof.
  intros.
  eapply multi_preservation in H; eauto. denotation_simp0. eapply empty_basic_typing_base_const_exists; eauto.
Qed.

Lemma base_type_max_term_reduce_to_constant: forall b (v: value), base_type_max_term b ↪* v -> (∃ c, v = vconst c).
Proof.
  intros. assert ([] ⊢t base_type_max_term b ⋮t b); eauto.
  eapply multi_preservation in H; eauto. denotation_simp0. eapply empty_basic_typing_base_const_exists; eauto.
Qed.

Global Hint Resolve base_type_max_term_reduce_to_constant: core.

Lemma state_insert_closed_value:
  forall (v: value) a (st: state), closed_value v -> (exists (c: constant), v = c) \/ ({a↦v} st = st).
Proof.
  unfold closed_value. intros. destruct v; simpl in H.
  - left. eauto.
  - set_solver.
  - right; eauto.
  - right; eauto.
  - right; eauto.
Qed.

Lemma state_insert_closed_value_arr: forall (r: rty) (v : value) (a : atom) (st : state),
    is_arr r -> [] ⊢t v ⋮v ⌊ r ⌋ -> ({a↦v}) st = st.
Proof.
  intros.
  assert (closed_value v); basic_typing_solver.
  eapply state_insert_closed_value in H1; destruct H1; eauto.
  mydestr; subst. destruct r; invclear H0. invclear H. invclear H.
Qed.

Lemma ctx_rm_app: forall Γ1 Γ2, ⦑Γ1 ++ Γ2⦒ = ⦑Γ1⦒ ++ ⦑Γ2⦒.
Proof.
  induction Γ1; simpl; intros; mydestr; auto.
  dec_solver2. rewrite IHΓ1; auto.
Qed.

Ltac denotation_simp1 :=
  repeat (denotation_simp0;
          mydestr;
          repeat match goal with
            | [H: ¬ is_arr _ |- _ ] => apply not_is_arr in H; destruct H; mydestr; subst
            | [H: ¬ not_underbasety _ |- _ ] => apply not_not_underbasety in H; mydestr; subst
            | [H: ¬ ({_}⟦ _ ⟧{ [] }) _ |- _ ] => rewrite not_ctxrR_empty_iff_not_rR in H
            | [ |- ¬ ({_}⟦ _ ⟧{ [] }) _ ] => rewrite not_ctxrR_empty_iff_not_rR
            (* | [H: {_}⟦ _ ⟧{ [] } _ |- _ ] => invclear H *)
            | [H: closed_rty 0 _ [v:_|?n|_|_] |- _ ] =>
                match n with
                | 0 => fail 1
                | _ => assert (n = 0) by (apply closed_rty_0_underbase in H; auto); subst
                end
            | [H: closed_rty 0 _ {v:_|?n|_|_} |- _ ] =>
                match n with
                | 0 => fail 1
                | _ => assert (n = 0) by (apply closed_rty_0_overbase in H; auto); subst
                end
            | [H: ?e ↪* (tvalue ?v), H': [] ⊢t ?e ⋮t (TBase _)  |- _ ] =>
                match v with
                | vconst _ => fail 1
                | _ => assert (exists c, v = vconst c) as Htmp by (eapply base_type_reduce_to_constant in H; eauto);
                      destruct Htmp; subst
                end
            | [H: base_type_max_term _ ↪* (tvalue ?v) |- _ ] =>
                match v with
                | vconst _ => fail 1
                | _ => assert (exists c, v = vconst c) as Htmp by (eapply base_type_max_term_reduce_to_constant in H; eauto);
                      destruct Htmp; subst
                end
            | [H: context [⦑(_, _) :: _⦒] |- _ ] => simpl in H; try dec_solver2
            | [|- context [⦑(_, _) :: _⦒] ] => simpl; try dec_solver2
            | [H: context [⦑ ?a ++ ?b ⦒] |- _ ] => setoid_rewrite (ctx_rm_app a b) in H
            | [|- context [⦑ ?a ++ ?b ⦒] ] => setoid_rewrite (ctx_rm_app a b)
            | [H: context [ctxdom ((_, _) :: _)] |- _] => simpl in H
            | [|- context [ctxdom ((_, _) :: _)]] => simpl
            | [H: is_arr ?r, H': [] ⊢t ?v ⋮v ⌊ ?r ⌋ |- context [{ ?a ↦ ?v} _] ] =>
                rewrite (state_insert_closed_value_arr r v a); auto
            | [H: is_arr ?r, H': [] ⊢t ?v ⋮v ⌊ ?r ⌋, H'': context [{ ?a ↦ ?v} _] |- _ ] =>
                rewrite (state_insert_closed_value_arr r v a) in H''; auto
            | [H: tvalue ?a = tvalue ?b |- _ ] => invclear H
            | [H: vconst ?a = vconst ?b |- _ ] => invclear H
            | [H: ty_of_const ?a = ty_of_const ?b |- _ ] => invclear H
            | [H: context [⌊(_, _) :: _⌋*] |- _ ] => simpl in H
            | [|- context [⌊(_, _) :: _⌋*]] => simpl
            end).

Lemma valid_rty_under_iff_over: forall n b (d2: aset) (ϕ: refinement),
    valid_rty {v:b|n|d2|ϕ} <-> valid_rty [v:b|n|d2|ϕ].
Proof.
  split; intros; invclear H; constructor; auto.
Qed.

Global Hint Rewrite valid_rty_under_iff_over: core.

Lemma closed_rty_under_iff_over: forall n1 n2 b (d1 d2: aset) (ϕ: refinement),
    closed_rty n1 d1 {v:b|n2|d2|ϕ} <-> closed_rty n1 d1 [v:b|n2|d2|ϕ].
Proof.
  split; intros; invclear H; constructor; auto.
  - rewrite <- valid_rty_under_iff_over; auto.
  - invclear H1; constructor; auto.
  - rewrite valid_rty_under_iff_over; auto.
  - invclear H1; constructor; auto.
Qed.

Lemma crxrR_perservation: forall st r e e', ({st}⟦r⟧) e -> e ↪* e' -> [] ⊢t e' ⋮t ⌊r⌋.
Proof.
  intros. apply rR_regular1 in H; mydestr. eapply multi_preservation in H0; eauto.
Qed.

Ltac refinement_solver0 :=
  repeat (match goal with
          | [H: {_;_;_}⟦_⟧ ?e |- lc ?e] => apply rR_regular1 in H; mydestr; basic_typing_solver2
          | [H: ({0;b∅;_}⟦?r⟧) ?x, H': ?x ↪* ?y |- [] ⊢t ?y ⋮t ⌊?r⌋ ] =>
              apply (crxrR_perservation _ r x y) in H; auto
          | [H: ({0;b∅;_}⟦?r⟧) ?x, H': ?x ↪* (tvalue ?y) |- [] ⊢t ?y ⋮v ⌊?r⌋ ] =>
              (apply (crxrR_perservation _ r x y) in H; auto); (invclear H; auto)
          | [H: closed_rty _ _ _ |- bound_in_refinement _ _ ] => destruct H; mydestr
          | [H: valid_rty _ |- bound_in_refinement _ _ ] => invclear H
          | [H: valid_rty ?τ |- wf_r _ _ ?ϕ ] =>
              match τ with
              | context [ ϕ ] => invclear H; eauto
              end
          | [H: closed_rty _ _ ?τ |- wf_r _ _ ?ϕ ] =>
              match τ with
              | context [ ϕ ] => invclear H; mydestr; eauto
              end
          | [H: wf_r _ _ ?ϕ |- bound_in_refinement _ ?ϕ ] => destruct H
          | [H: ({_;_;_}⟦?τ⟧) _ |- closed_rty _ _ ?τ ] =>
              apply rR_regular1 in H; mydestr; eauto; try closed_rty_solver
          | [H: ({_;_;_}⟦?τ⟧) _ |- _ ⊢t _ ⋮t _ ] =>
              apply rR_regular1 in H; mydestr; eauto; try basic_typing_solver
          | [ |- closed_rty _ _ _ ] => closed_rty_solver
          (* | [H: closed_rty _ _ (-:{v: _ | _ | _ | _}⤑ _) |- closed_rty _ _ _] => *)
          (*     invclear H; mydestr *)
          (* | [H: closed_rty _ _ (_ ⤑ _) |- closed_rty _ _ _] => *)
          (*     invclear H; mydestr *)
          | [H: ok_dctx _ _ |- ok _ ] => ok_dctx_solver
          | [ |- ok ((_, _) :: _)] => rewrite ok_pre_destruct; split; ctx_erase_simp
          | [H: closed_rty _ _ ?τ |- _ ∉ rty_fv ?τ ] => invclear H; mydestr; try fast_set_solver
          | [H: closed_rty _ _ (-:{v: _ | _ | _ | _}⤑ ?τ) |- ?ret ] =>
              match ret with
              | not_overbasety ?τ => invclear H; mydestr; auto
              | valid_rty ?τ => invclear H; mydestr; auto
              end
          | [H: valid_rty (-:{v: _ | _ | _ | _}⤑ ?τ) |- ?ret ] =>
              match ret with
              | not_overbasety ?τ => invclear H; auto
              | valid_rty ?τ => invclear H; auto
              end
          | [H: closed_rty _ _ (?τ1 ⤑ ?τ2) |- ?ret ] =>
              match ret with
              | not_overbasety ?τ1 => invclear H; mydestr; auto
              | valid_rty ?τ1 => invclear H; mydestr; auto
              | not_overbasety ?τ2 => invclear H; mydestr; auto
              | valid_rty ?τ2 => invclear H; mydestr; auto
              end
          | [H: valid_rty (?τ1 ⤑ ?τ2) |- ?ret ] =>
              match ret with
              | not_overbasety ?τ1 => invclear H; auto
              | valid_rty ?τ1 => invclear H; auto
              | not_overbasety ?τ2 => invclear H; auto
              | valid_rty ?τ2 => invclear H; auto
              end
          end || refinement_simp1 || eauto || listctx_set_solver || fast_set_solver).

Ltac refinement_solver2 :=
  repeat (
      try auto_reduction_exfalso;
      match goal with
      | [H: {?st}⟦ ?τ ⟧{ [] } ?e |- {?st}⟦ ?τ ⟧ ?e ] => invclear H
      | [H: closed_rty _ _ ?τ |- valid_rty ?τ] => destruct H; auto
      | [ |- _ ⊢t terr ⋮t _ ] => constructor; auto
      | [H: ok_dctx ?d ((_, ?τ) :: _) |- closed_rty 0 ?d ?τ ] => invclear H; auto
      | [H: ok_dctx _ _ |- ok _ ] => apply ok_dctx_regular2 in H; mydestr
      | [H: ok (_ :: ?Γ) |- ok ⌊?Γ⌋* ] => rewrite ok_pre_destruct in H; mydestr; ctx_erase_simp
      | [ |- context [ ⌊ _ ⌋* ] ] => ctx_erase_simp; fast_set_solver
      | [H: ?e ↪* (tvalue ?v), H': [] ⊢t ?e ⋮t ?T |- [] ⊢t ?v ⋮v ?T ] =>
          eapply multi_preservation_value in H'; eauto
      | [H: [] ⊢t ?e ⋮t ?T |- _ ⊢t ?e ⋮t _ ] => apply basic_typing_weaken_tm_empty; eauto
      | [H: lc_rty_idx _ _ |- lc_rty_idx _ _ ] => invclear H; auto
      | [H: wf_r _ _ ?ϕ |- not_fv_in_refinement _ ?ϕ ] => invclear H; auto
      end || refinement_solver0).

Global Hint Constructors lc_rty_idx: core.

Ltac amap_dec_solver :=
  try auto_exfalso;
  match goal with
  | [H: Some ?a = Some ?b |- _] => inversion H; subst; clear H; simpl; auto
  | [H: ?a <> ?a |- _ ] => exfalso; lia
  | [ |- Some _ = None ] => exfalso; lia
  | [ |- None = Some _ ] => exfalso; lia
  | [H: context [ decide (?a ∈ ?d) ] |- _ ] => destruct (decide (a ∈ d)); subst; my_simplify_map_eq; eauto
  | [|- context [ decide (?a ∈ ?d) ] ] => destruct (decide (a ∈ d)); subst; my_simplify_map_eq; eauto
  | _ => progress (intros; simpl; eauto; var_dec_solver2)
  end.

Lemma wf_r_subst_excluded_forward_c: forall n d ϕ z (c: constant),
    wf_r n d ϕ -> wf_r n (refinement_set_subst z c d) (refinement_subst z c d ϕ).
Proof.
  intros. invclear H. unfold refinement_subst. unfold refinement_set_subst.
  amap_dec_solver; constructor; auto.
  - unfold not_fv_in_refinement. intros. apply H0. intros.
    unfold state_subst. amap_dec_solver.
    destruct (atom_dec x z); subst; my_simplify_map_eq3.
  - unfold bound_in_refinement in H1. unfold bound_in_refinement. intros. amap_dec_solver.
  - unfold not_fv_in_refinement. intros. apply H0. intros.
    unfold state_subst. var_dec_solver2.
  - unfold bound_in_refinement in H1. unfold bound_in_refinement. intros. amap_dec_solver.
Qed.

Lemma wf_r_subst_excluded_forward: forall n d ϕ z (c: value),
    wf_r n d ϕ -> wf_r n (refinement_set_subst z c d) (refinement_subst z c d ϕ).
Proof.
  intros. invclear H. unfold refinement_subst. unfold refinement_set_subst.
  amap_dec_solver; constructor; auto.
  - destruct c; auto; unfold not_fv_in_refinement; intros; apply H0; intros;
      unfold state_subst; amap_dec_solver.
    + destruct (atom_dec x z); subst; my_simplify_map_eq3.
    + unfold state_subst_var. destruct (decide (z = atom)).
      { assert (m !! atom = m' !! atom) by fast_set_solver. rewrite <- H3. dec_solver2.
        destruct (m!!atom);
          destruct (atom_dec x atom); subst; my_simplify_map_eq3.
      }
      { assert (m !! atom = m' !! atom) by fast_set_solver. rewrite <- H3. dec_solver2.
        destruct (m!!atom); my_simplify_map_eq3;
          destruct (atom_dec z x); subst; my_simplify_map_eq3. }
  - unfold bound_in_refinement in H1. unfold bound_in_refinement. intros. amap_dec_solver.
  - unfold not_fv_in_refinement. intros. apply H0. intros.
    unfold state_subst. var_dec_solver2.
  - unfold bound_in_refinement in H1. unfold bound_in_refinement. intros. amap_dec_solver.
Qed.

Lemma not_overbasety_subst_excluded_forward: forall τ z c,
    not_overbasety τ -> not_overbasety (({z:=c}r) τ).
Proof.
  induction τ; intros; auto.
Qed.

Lemma valid_rty_subst_excluded_forward: forall τ (z: atom) (c: value),
    valid_rty τ -> valid_rty (({z:=c}r) τ).
Proof.
  induction τ; intros.
  - constructor. invclear H. apply wf_r_subst_excluded_forward; auto.
  - constructor. invclear H. apply wf_r_subst_excluded_forward; auto.
  - invclear H. simpl. constructor; eauto.
    + apply wf_r_subst_excluded_forward; auto.
    + apply not_overbasety_subst_excluded_forward; auto.
  - simpl. invclear H. simpl. constructor; eauto.
    + apply not_overbasety_subst_excluded_forward; auto.
    + simpl. constructor; eauto. apply not_overbasety_subst_excluded_forward; auto.
Qed.

Lemma lc_rty_idx_subst_excluded_forward: forall τ (z: atom) (c: value) n1,
    lc_rty_idx n1 τ -> lc_rty_idx n1 (({z:=c}r) τ).
Proof.
  induction τ; intros; invclear H; constructor; auto.
Qed.

Lemma rty_fv_subst_excluded_forward: forall τ (z: atom) (c: constant) d1,
    rty_fv τ ⊆ {[z]} ∪ d1 -> rty_fv (({z:=c}r) τ) ⊆ d1.
Proof.
  induction τ; simpl; intros; unfold refinement_set_subst.
  - amap_dec_solver; set_solver.
  - amap_dec_solver; set_solver.
  - amap_dec_solver; set_solver.
  - set_solver.
Qed.

Lemma closed_rty_subst_excluded_forward: forall τ (z: atom) (c: constant) n1 (d1: aset),
    z ∉ d1 -> closed_rty n1 ({[z]} ∪ d1) τ -> closed_rty n1 d1 ({z := c}r τ).
Proof.
  induction τ; intros; repeat split; invclear H0; mydestr;
    try match goal with
      | [|- valid_rty _] => eapply valid_rty_subst_excluded_forward; eauto
      | [|- lc_rty_idx _ _] => eapply lc_rty_idx_subst_excluded_forward; eauto
      | [|- rty_fv _ ⊆ _] => eapply rty_fv_subst_excluded_forward; eauto
      end.
Qed.

Ltac refinement_solver3 :=
  repeat (match goal with
          | [H: closed_rty _ _ _ |- wf_r _ _ _ ] => invclear H; mydestr; eauto
          | [H: closed_rty _ _ ?τ |- valid_rty ?τ] => invclear H; mydestr; eauto
          | [H: valid_rty _ |- wf_r _ _ _ ] => invclear H; mydestr; eauto
          | [H: ok_dctx _ ((?z, _) :: _) |- ?z ∉ _ ] => invclear H; auto
          | [H: not_overbasety ?τ |- not_overbasety (({ _ := _}r) ?τ)] =>
              eapply not_overbasety_subst_excluded_forward; eauto
          | [|- valid_rty (({ _ := _ }r) _)] => eapply valid_rty_subst_excluded_forward; eauto
          | [|- lc_rty_idx _ (({ _ := _ }r) _)] => eapply lc_rty_idx_subst_excluded_forward; eauto
          | [|- rty_fv (({ _ := _ }r) _) ⊆ _] => eapply rty_fv_subst_excluded_forward; eauto
          end || refinement_solver2).

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

Ltac refinement_solver4 :=
  repeat (match goal with
          | [H: ({_;_;_}⟦_⟧ _) |- _ ⊢t ?v ⋮v _ ] => apply rR_regular1 in H; mydestr
          | [H: closed_rty _ _ _ |- lc_rty_idx _ _] => invclear H; mydestr; auto
          | [H: closed_rty _ _ _ |- context [rty_fv _]] => invclear H; mydestr; auto
          end || refinement_solver3).

Ltac refinement_solver5 :=
  (match goal with
   | [H: closed_rty _ _ (-:{v: _ | _ | _ | _}⤑ _) |- closed_rty _ _ _] =>
       invclear H; mydestr; refinement_solver4
   | [H: closed_rty _ _ (_ ⤑ _) |- closed_rty _ _ _] =>
       invclear H; mydestr; refinement_solver4
   (* | [H: closed_rty _ _ (_ ⤑ ?t) |- closed_rty _ _ ?t] => *)
   (*     invclear H; mydestr; refinement_solver4 *)
   | [H: ({_;_;_}⟦ ?τ ⟧) _ |- valid_rty ?τ] => apply rR_regular1 in H; mydestr; refinement_solver4
   | [H: ({_;_;_}⟦?τ⟧) _ |- _ ∉ rty_fv ?τ ] => apply rR_regular1 in H; mydestr; refinement_solver4
   | [H: ({_}⟦?τ⟧{ _ }) _ |- closed_rty _ _ ?τ] =>
       apply ctxrR_regular in H; mydestr; refinement_solver4
   end || refinement_solver4).

Lemma tyable_implies_closed_value: forall v_x T, [] ⊢t v_x ⋮v T -> closed_value v_x.
Proof.
  unfold closed_value. intros. apply basic_typing_contains_fv_value in H.
  set_solver.
Qed.

Global Hint Resolve tyable_implies_closed_value: core.

Lemma tyable_implies_closed_tm: forall v_x T, [] ⊢t v_x ⋮t T -> closed_tm v_x.
Proof.
  unfold closed_tm. intros. apply basic_typing_contains_fv_tm in H.
  set_solver.
Qed.

Global Hint Resolve tyable_implies_closed_tm: core.

Ltac refinement_solver6 :=
  match goal with
  | [H: is_arr ?τ_x |- ok_dctx _ ((_, ?τ_x) :: _)] =>
      apply ok_dctx_cons_arr; refinement_solver5
  | [|- ok_dctx _ ((_, {v:_|_|_|_}) :: _)] => constructor; refinement_solver5
  | [|- ok_dctx _ ((_, [v:_|_|_|_]) :: _)] => constructor; refinement_solver5
  | [H: [] ⊢t ?v ⋮v _ |- closed_value ?v] => eapply tyable_implies_closed_value in H; eauto
  | [H: [] ⊢t ?v ⋮t _ |- closed_tm ?v] => eapply tyable_implies_closed_tm in H; eauto
  | [H: ({_}⟦ ?τ ⟧{ _ }) terr |- valid_rty ?τ ] => apply ctxrR_regular in H; mydestr; refinement_solver5
  end || refinement_solver5.

Ltac refinement_solver7 :=
  listctx_set_simpl4;
  match goal with
  | [H: {_;_;_}⟦_⟧ ?e |- _ ∉ fv_tm ?e] =>
      apply rR_regular1 in H; mydestr; basic_typing_solver
  end || refinement_solver6.

Ltac denotation_simp2 :=
  denotation_simp1;
  repeat match goal with
    | [H: ok [] |- _ ] => clear H
    | [H: [] ⊢t ?v ⋮v (TBase _) |- _ ] => invclear H; denotation_simp1
    | [H: [v: _ |_|_|_] = [v:_|_|_|_] |- _ ] => invclear H
    end.

Ltac denotation_simp := denotation_simp2.

Ltac refinement_solver :=
  (refinement_solver7; (try denotation_simp; refinement_solver7)).

Lemma not_in_aset_implies_same_insert_refinement: forall (d: aset) (ϕ: refinement),
    not_fv_in_refinement d ϕ ->
    forall (x: atom), x ∉ d -> forall (m: state), forall bst v_x (v: constant), ϕ bst ({x  ↦ v_x} m) v <-> ϕ bst m v.
Proof.
  intros. apply H. intros.
  destruct v_x; simpl; auto; var_dec_solver2;
    assert (x <> x0) by fast_set_solver; my_simplify_map_eq3.
  destruct (m!!atom); my_simplify_map_eq3.
Qed.

Lemma refinement_shadow_update_st: forall bst v_x n ϕ (x: atom) d st (v: constant),
    x ∉ d -> wf_r n d ϕ -> ϕ bst ({ x ↦ v_x } st) v <-> ϕ bst st v.
Proof.
  intros.
  eapply not_in_aset_implies_same_insert_refinement; eauto. refinement_solver.
Qed.

Ltac rR_shadow_update_st_tac :=
  repeat match goal with
    | [H: valid_rty {v: _ |_ |?d|?ϕ} |- ?ϕ _ ({ _ ↦ _ } _) _ ] =>
        rewrite refinement_shadow_update_st with (d:=d); auto; refinement_solver
    | [H: valid_rty [v: _ |_ |?d|?ϕ] |- ?ϕ _ ({ _ ↦ _ } _) _ ] =>
        rewrite refinement_shadow_update_st with (d:=d); auto; refinement_solver
    | [H: valid_rty {v: _ |_ |?d|?ϕ}, H': ?ϕ _ ({ _ ↦ _ } _) _ |- ?ϕ _ _ _ ] =>
        rewrite refinement_shadow_update_st with (d:=d) in H'; auto; refinement_solver
    | [H: valid_rty [v: _ |_ |?d|?ϕ], H': ?ϕ _ ({ _ ↦ _ } _) _ |- ?ϕ _ _ _ ] =>
        rewrite refinement_shadow_update_st with (d:=d) in H'; auto; refinement_solver
    | [H: closed_rty _ _ ?τ, H': ?ϕ ?bst _ ?c_x |- ?ϕ ?bst _ ?c_x] =>
        match τ with
        | context [ϕ] => destruct H; mydestr
        end
    end.

Lemma rR_shadow_update_st: forall τ n st,
    closed_rty n (dom aset st) τ ->
    (forall bst (a: atom) (c: value) e, a ∉ (rty_fv τ) ->
                                   ({n; bst; { a ↦ c } st}⟦τ⟧) e <-> ({n; bst; st}⟦τ⟧) e
    ).
Proof.
  induction τ; split; simpl; intros; mydestr; subst;
    split; auto; split; try closed_rty_solver.
  - exists x. repeat split; auto. rR_shadow_update_st_tac.
  - exists x. repeat split; auto. rR_shadow_update_st_tac.
  - intros. apply H3; auto. rR_shadow_update_st_tac.
  - intros. apply H3; auto. rR_shadow_update_st_tac.
  - intros. refinement_simp1. apply H3 in H4. apply IHτ in H4; auto; refinement_solver5.
    rewrite refinement_shadow_update_st with (d:=d); auto; refinement_solver.
  - intros. refinement_simp1. apply H3 in H4. apply IHτ; refinement_solver5.
    rewrite <- refinement_shadow_update_st with (d:=d); auto; refinement_solver.
  - intros. refinement_simp1.
    rewrite <- IHτ1 with (a:= a) (c:=c) in H4; auto; refinement_solver.
    apply H3 in H4. rewrite IHτ2 in H4; refinement_solver5.
  - intros. refinement_simp1.
    rewrite IHτ1 in H4 by refinement_solver.
    apply H3 in H4; rewrite IHτ2; auto; refinement_solver5.
Qed.

Lemma rR_shadow_update_st_c: forall τ n st,
    closed_rty n (dom aset st) τ ->
    (forall bst (a: atom) (c: constant) e, a ∉ (rty_fv τ) ->
                                      ({n; bst; <[a:=c]> st}⟦τ⟧) e <-> ({n; bst; st}⟦τ⟧) e
    ).
Proof.
  intros. eapply rR_shadow_update_st with (c:=c) in H; eauto.
Qed.

Lemma lc_rR_shadow_update_st: forall τ st (a: atom) c e,
    closed_rty 0 (dom aset st) τ -> a ∉ (rty_fv τ) ->
    ({ { a ↦ c } st}⟦τ⟧) e <-> ({st}⟦τ⟧) e.
Proof.
  split; intros.
  - rewrite rR_shadow_update_st in H1; eauto.
  - rewrite rR_shadow_update_st; eauto.
Qed.

Lemma lc_rR_shadow_update_st_c: forall τ st (a: atom) c e,
    closed_rty 0 (dom aset st) τ -> a ∉ (rty_fv τ) ->
    ({ <[a:=c]> st}⟦τ⟧) e <-> ({st}⟦τ⟧) e.
Proof.
  intros. eapply lc_rR_shadow_update_st with (c:=c) in H; eauto.
Qed.

Lemma subst_insert_lookup_neq_Some {A:Type}: forall (a b: atom) (c0 c1: A) (st: amap A),
    a <> b -> st !! a = Some c1 -> <[b:=c0]> st !! a = Some c1.
Proof.
  intros. setoid_rewrite lookup_insert_Some; auto.
Qed.

Lemma subst_insert_lookup_neq_None {A:Type}: forall (a b: atom) (c0: A) (st: amap A),
    a <> b -> st !! a = None -> <[b:=c0]> st !! a = None.
Proof.
  intros. setoid_rewrite lookup_insert_None; auto.
Qed.

Ltac amap_solver1 Hsta :=
  repeat match goal with
    | [|- <[?a:=_]> (<[?b:=_]> ?st) = <[?b:=_]> (<[?a:=_]> ?st)] =>
        setoid_rewrite (insert_commute st a b); auto; try fast_set_solver
    | [H: ?st !! ?a = None |- context [<[?b:=?c0]> ?st !! ?a ] ] =>
        setoid_rewrite (subst_insert_lookup_neq_None a b c0); auto; try fast_set_solver
    | [H: ?st !! ?a = Some ?c1 |- context [<[?b:=?c0]> ?st !! ?a ] ] =>
        setoid_rewrite (subst_insert_lookup_neq_Some a b c0 c1); auto; try fast_set_solver
    | [|- context [<[?b:=?c0]> ?st !! ?a ] ] =>
        destruct (st !! a) eqn: Hsta
    end.

Lemma subst_st_insert_commute:forall (v_x: value) (a b: atom) (c: constant) st,
    fv_value v_x ∩ {[a; b]} = ∅ ->
    a <> b -> ({a↦v_x}) (<[b:=c]> st) = (<[b:=c]>) ({a↦v_x} st).
Proof.
  destruct v_x; intros; simpl; auto.
  - amap_solver1 Hsta.
  - simpl. my_simplify_map_eq.
    amap_solver1 Hsta1. amap_solver1 Hsta2. set_solver. set_solver.
Qed.

Lemma insert_constant_in_st: forall (v_x: constant) (a: atom) (st: state),
    a ∉ (dom aset st) -> (dom aset (({a↦v_x}) st)) ≡ ({[a]} ∪ dom aset st).
Proof.
  intros; simpl; auto. my_simplify_map_eq2.
Qed.

Lemma insert_vfvar_in_st: forall (v_x: atom) (a: atom) (st: state),
    v_x ∈ dom aset st -> a ∉ (dom aset st) ->
    (dom aset (({a↦v_x}) st)) ≡ ({[a]} ∪ dom aset st).
Proof.
  intros; simpl; auto.
  - simpl in H. my_simplify_map_eq2.
Qed.

Ltac ok_dctx_solver_slow :=
  match goal with
  | [H1: ok_dctx ?d1 ?τ, H2: ok_dctx ?d2 ?τ |- ok_dctx ?d3 ?τ ] =>
      apply (ok_dctx_cap τ d1 d2 d3); auto; simpl; refinement_set_simp; set_solver
  | [H: ok_dctx ?d1 ?τ |- ok_dctx ?d2 ?τ ] =>
      apply (ok_dctx_trans τ d1 d2); simpl; auto; refinement_set_simp; set_solver
  end.

Ltac auto_ty_exfalso1 :=
  match goal with
  | [H: [] ⊢t (vfvar _) ⋮v _ |- _ ] => invclear H; listctx_set_simpl
  | [H: [] ⊢t (vbvar _) ⋮v _ |- _ ] => invclear H
  | [H: is_arr {v:_|_|_|_} |- _ ] => invclear H
  | [H: is_arr [v:_|_|_|_] |- _ ] => invclear H
  | [H: ¬ is_arr ?r, H': [] ⊢t vlam _ _ ⋮v ⌊?r⌋ |- _ ] => (destruct r; invclear H'; exfalso; auto)
  | [H: ¬ is_arr ?r, H': [] ⊢t vfix _ _ ⋮v ⌊?r⌋ |- _ ] => (destruct r; invclear H'; exfalso; auto)
  | [H: is_arr ?r, H': [] ⊢t (vconst _) ⋮v ⌊?r⌋ |- _ ] => (destruct r; invclear H'; invclear H)
  | [H: not_overbasety {v: _ |_|_|_} |- _ ] => invclear H
  end.

Ltac auto_ty_exfalso2 :=
  repeat (match goal with
          | [H: ¬ not_overbasety ?τ |- _ ] =>
              match τ with
              | {v:_|_|_|_} => clear H
              | [v:_|_|_|_] => exfalso; apply H; simpl; auto
              | (-:{v:_|_|_|_}⤑ _) => exfalso; apply H; simpl; auto
              | (_ ⤑ _) => exfalso; apply H; simpl; auto
              | _ => destruct τ
              end
          end || auto_ty_exfalso1).

Lemma auto_ty_base: forall τ_x,
    not_overbasety τ_x -> ¬ is_arr τ_x -> (exists b n d ϕ, τ_x = [v:b|n|d|ϕ]).
Proof.
  intros. destruct τ_x; invclear H.
  - eauto.
  - neg_apply H0; simpl; auto.
  - neg_apply H0; simpl; auto.
Qed.

Ltac auto_ty_exfalso3 :=
  match goal with
  | [H: not_overbasety ?τ_x, H': ¬ is_arr ?τ_x |- _ ] =>
      apply auto_ty_base in H; auto; mydestr; subst; try clear H'
  end || auto_ty_exfalso2.

Lemma is_arr_is_not_constant: forall r (x:constant), is_arr r -> ~ ([] ⊢t x ⋮v ⌊r⌋).
Proof.
  intros. destruct r; simpl; auto.
  - intro Hz. invclear Hz.
  - intro Hz. invclear Hz.
Qed.

Ltac auto_ty_exfalso4 :=
  match goal with
  | [H: ?P, H': ~ ?P |- _ ] => apply H' in H; invclear H
  | [H: is_arr [v:_|_|_|_] |- _ ] =>invclear H
  | [H: is_arr {v:_|_|_|_} |- _ ] =>invclear H
  | [H: {v:_|_|_|_} = [v:_|_|_|_] |- _] => invclear H
  | [H: [v:_|_|_|_] = {v:_|_|_|_} |- _] => invclear H
  | [H: {v:_|_|_|_} = {v:_|_|_|_} |- _] => invclear H
  | [H: [v:_|_|_|_] = [v:_|_|_|_] |- _] => invclear H
  | [H: is_arr ?r, H': [] ⊢t (vconst _) ⋮v ⌊ ?r ⌋ |- _ ] => eapply is_arr_is_not_constant in H; apply H in H'; invclear H'
  end || auto_ty_exfalso3.

Ltac auto_ty_exfalso := auto_ty_exfalso4.

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

Lemma denotation_st_update_iff_subst:
  forall τ (z: atom) (c: constant) n bst (st: state) e,
    z ∉ (dom aset st) ->
    ({n;bst;<[z:=c]> st}⟦τ⟧ e) <-> (closed_rty n ({[z]} ∪ dom aset st) τ /\ {n;bst;st}⟦({z:=c}r) τ⟧ e).
Proof.
  induction τ; split; simpl; intros; mydestr; subst.
  - do 2 (split; denotation_simp; auto). split; auto.
    + eapply closed_rty_subst_excluded_forward in H1; eauto.
    + unfold refinement_subst. unfold state_subst. amap_dec_solver.
      repeat eexists; eauto. rewrite closed_rty_over_implies_state_dummy_insert in H3; eauto.
  - split; auto. split; auto.
    + denotation_simp.
    + repeat eexists; eauto.
      unfold refinement_subst in H4. unfold refinement_subst in H2.
      unfold refinement_set_subst in H2.
      unfold state_subst in H2. unfold state_subst in H4.
      amap_dec_solver.
      rewrite closed_rty_over_implies_state_dummy_insert; eauto.
  - split; auto; denotation_simp. split; auto. split; auto.
    + eapply closed_rty_subst_excluded_forward in H1; eauto.
    + unfold refinement_subst. unfold state_subst. amap_dec_solver.
      intros. apply H2; auto. rewrite closed_rty_under_implies_state_dummy_insert; eauto.
  - split; auto. split; auto.
    + denotation_simp.
    + repeat eexists; eauto. intros. apply H3; auto.
      unfold refinement_subst. unfold refinement_subst in H2. unfold refinement_set_subst in H2. unfold state_subst. unfold state_subst in H2.
      amap_dec_solver.
      rewrite closed_rty_under_implies_state_dummy_insert in H5; eauto.
  - split; auto; denotation_simp. split; auto; ctx_erase_simp4. split; auto.
    + eapply closed_rty_subst_excluded_forward in H1; eauto.
    + intros. unfold refinement_subst in H4.
      assert (ϕ bst (<[z:=c]> st) c_x).
      { unfold state_subst in H4.
        unfold state_subst in H2.
        amap_dec_solver; auto. rewrite wf_r_implies_state_dummy_insert; eauto. refinement_solver3. }
      apply H2 in H5; auto. rewrite IHτ in H5; mydestr; auto.
  - split; auto; ctx_erase_simp4. split; auto.
    + denotation_simp.
    + intros. rewrite IHτ; auto. split; refinement_solver.
      apply H3; auto.
      unfold refinement_subst. unfold state_subst. unfold state_subst in H3. amap_dec_solver.
      invclear H2. invclear H4.
      rewrite wf_r_implies_state_dummy_insert in H5; eauto. refinement_solver3.
  - split; auto; denotation_simp. split; auto; ctx_erase_simp4. split; auto.
    + eapply closed_rty_subst_excluded_forward in H1; eauto.
    + intros.
      assert (({n;bst;<[z:=c]> st}⟦τ1⟧) v_x) as HH. rewrite IHτ1; auto. split; refinement_solver4.
      apply H2 in HH. rewrite IHτ2 in HH; auto. mydestr; auto.
  - split; auto; ctx_erase_simp4. split; auto; denotation_simp.
    + intros. rewrite IHτ2; auto. split; refinement_solver. apply H3. rewrite IHτ1 in H4; auto. mydestr; auto.
Qed.

Lemma state_insert_insert_neq: forall (a b: atom) (c_a: constant) (v_b: value) (st: state),
    a <> b -> a ∉ fv_value v_b ->
    {a ↦ c_a} ({b ↦ v_b} st) = {b ↦ v_b} ({a ↦ c_a} st).
Proof.
  intros. destruct v_b; simpl; auto.
  - setoid_rewrite insert_commute at 1; auto.
  - assert (a <> atom) by fast_set_solver. my_simplify_map_eq3.
    destruct (st !! atom) eqn:Hst; my_simplify_map_eq3.
    setoid_rewrite insert_commute at 1; auto.
Qed.

Lemma state_insert_insert_neq_better: forall (a b: atom) (c_a: constant) (v_b: value) (st: state),
    a <> b -> v_b <> a ->
    {a ↦ c_a} ({b ↦ v_b} st) = {b ↦ v_b} ({a ↦ c_a} st).
Proof.
  intros. destruct v_b; auto.
  - apply state_insert_insert_neq; auto. set_solver.
  - apply state_insert_insert_neq; auto. set_solver.
Qed.

Ltac ctxrR_shadow_update_st_tac_c :=
  match goal with
  | [H: context [{0;b∅;<[?a0 := ?c]> ?st}⟦?τ⟧ _],
        H': ({0;b∅;?st}⟦?τ⟧) _ |- ?ret ] =>
      (* idtac H' *)
      (rewrite <- lc_rR_shadow_update_st with (a:=a0) (c:=c) in H'
       ;eauto; refinement_solver3);
      match ret with
      | context [({_↦?v_x}) _] => eapply H with (v_x := v_x) in H'; eauto
      | _ => apply H in H'
      end
  end.

Lemma ctxrR_shadow_update_st_c: forall Γ st τ,
    closed_rty 0 (ctxdom ⦑ Γ ⦒ ∪ dom aset st) τ ->
    ok_dctx (dom aset st) Γ ->
    (forall (a: atom) c e, a ∉ (ctxdom Γ ∪ dom aset st) ->
                      ({<[a:=c]> st}⟦τ⟧{Γ}) e <-> ({st}⟦τ⟧{Γ}) e
    ).
Proof.
  induction Γ; split; simpl; intros; mydestr; subst.
  - invclear H2. constructor; auto; intros.
    rewrite <- rR_shadow_update_st_c; eauto; refinement_solver.
  - invclear H2. constructor; auto; intros.
    rewrite rR_shadow_update_st_c; eauto; refinement_solver.
  - invclear H0; invclear H2; mydestr; try auto_ty_exfalso4.
    + constructor; auto. constructor; auto.
      intros.
      ctxrR_shadow_update_st_tac_c.
      setoid_rewrite insert_commute in H0; try fast_set_solver.
      rewrite IHΓ in H0; auto; denotation_simp.
      refinement_solver.
    + constructor; auto. denotation_simp. constructor; auto.
      exists x. split. intros. rewrite <- rR_shadow_update_st_c; eauto. refinement_solver.
      intros; mydestr.
      ctxrR_shadow_update_st_tac_c.
      assert ([] ⊢t v_x ⋮v x0) by refinement_solver. denotation_simp2.
      rewrite subst_st_insert_commute in H3; try fast_set_solver.
      rewrite IHΓ in H3; auto; denotation_simp.
      refinement_solver.
    + apply ctxrR_cons_under_arr; auto. denotation_simp. apply ok_dctx_cons_arr; auto.
      exists x. split. intros. rewrite <- rR_shadow_update_st_c; eauto. refinement_solver.
      intros; mydestr.
      ctxrR_shadow_update_st_tac_c.
      assert ([] ⊢t v_x ⋮v ⌊r⌋) by refinement_solver. denotation_simp2.
      rewrite IHΓ in H3; auto; refinement_solver.
  - invclear H0; invclear H2; mydestr; try auto_ty_exfalso4.
    + constructor; auto; denotation_simp. refinement_solver. ok_dctx_solver_slow.
      intros. specialize (H16 c_x).
      assert ({0;b∅; st}⟦{v:B|0|d|ϕ}⟧ c_x) by
        (rewrite <- lc_rR_shadow_update_st_c with (a:=a0) (c:=c); eauto; refinement_solver).
      apply H16 in H3.
      setoid_rewrite insert_commute; refinement_solver.
      rewrite IHΓ; auto; denotation_simp; refinement_solver.
    + invclear H7; mydestr.
      constructor; auto. denotation_simp. refinement_solver. ok_dctx_solver_slow.
      exists x. split. rewrite rR_shadow_update_st_c; eauto; refinement_solver.
      intros; mydestr.
      rewrite lc_rR_shadow_update_st_c in H7; auto; try refinement_solver3.
      eapply H2 with (v_x := v_x) in H7; eauto.
      assert ([] ⊢t v_x ⋮v x0) by refinement_solver. denotation_simp.
      setoid_rewrite insert_commute; denotation_simp.
      rewrite IHΓ; auto; denotation_simp; refinement_solver.
    + invclear H7; mydestr.
      apply ctxrR_cons_under_arr; auto. refinement_solver. ok_dctx_solver_slow.
      exists x. split. intros. rewrite rR_shadow_update_st_c; eauto; refinement_solver.
      intros; mydestr.
      rewrite lc_rR_shadow_update_st_c in H7; auto; try refinement_solver.
      eapply H2 with (v_x := v_x) in H7; eauto.
      assert ([] ⊢t v_x ⋮v ⌊r⌋) by refinement_solver. denotation_simp.
      rewrite IHΓ; auto; denotation_simp; refinement_solver.
Qed.

Lemma ctxrR_shadow_update_st: forall Γ st τ,
    closed_rty 0 (ctxdom ⦑ Γ ⦒ ∪ dom aset st) τ ->
    ok_dctx (dom aset st) Γ ->
    (forall (a: atom) c e, a ∉ (ctxdom Γ ∪ dom aset st) -> closed_value c ->
                      ({ {a↦c} st}⟦τ⟧{Γ}) e <-> ({st}⟦τ⟧{Γ}) e
    ).
Proof.
  intros. apply state_insert_closed_value with (a:=a) (v:=c) (st:=st) in H2; eauto.
  destruct H2; mydestr; subst; auto; try (rewrite H2; auto).
  apply ctxrR_shadow_update_st_c; auto.
Qed.

Lemma termR_perserve_rR: forall τ (e e': tm),
    not_overbasety τ ->
    valid_rty τ ->
    e <-<{[]; ⌊ τ ⌋ } e' -> (forall n bst st, {n;bst;st}⟦τ⟧ e -> {n;bst;st}⟦τ⟧ e').
Proof.
  induction τ; simpl; intros; invclear H0; mydestr; subst.
  - invclear H.
  - repeat (split; try termR_solver).
  - repeat (split; try termR_solver).
    intros. apply IHτ with (e := (mk_app e c_x)); eauto; termR_solver.
  - repeat (split; try termR_solver).
    intros. apply IHτ2 with (e := (mk_app e v_x)); eauto. termR_solver; refinement_solver.
  - repeat (split; try termR_solver).
    intros. apply IHτ2 with (e := (mk_app e v_x)); eauto. termR_solver; refinement_solver.
Qed.

Ltac denotation_simp3 :=
  repeat match goal with
    | [H: ({_}⟦[v:?b|_|_|_]⟧) ?ee, H': ?ee ↪* (tvalue ?v), H'': [] ⊢t ?v ⋮v (TBase ?b) |- _ ] => fail 1
    | [H: ({_}⟦[v:?b|_|_|_]⟧) ?ee, H': ?ee ↪* (tvalue ?v) |- _ ] => assert ([] ⊢t v ⋮v b) by refinement_solver
    | [H: ({_;_;_}⟦[v:?b|_|_|_]⟧) ?ee, H'': [] ⊢t ?ee ⋮t (TBase ?b) |- _ ] => fail 1
    | [H: ({_;_;_}⟦[v:?b|_|_|_]⟧) ?ee |- _ ] => assert ([] ⊢t ee ⋮t b) by refinement_solver
    end;
  denotation_simp2.

Lemma termR_perserve_rR_over: forall b n1 d ϕ (e e': value),
    valid_rty {v:b|n1|d|ϕ} ->
    e' <-<{[]; b } e ->
    (forall n bst st, {n;bst;st}⟦{v:b|n1|d|ϕ}⟧ e -> {n;bst;st}⟦{v:b|n1|d|ϕ}⟧ e').
Proof.
  intros. inversion H1; mydestr; subst.
  repeat (split; try termR_solver). denotation_simp3; subst.
  exists x. do 2 (split; auto). rewrite termR_value_iff_same in H0; mydestr; denotation_simp3.
Qed.

Lemma termR_perserve_ctxrR: forall Γ τ (e e': tm),
    not_overbasety τ ->
    valid_rty τ ->
    e <-<{ ⌊ Γ ⌋* ;  ⌊ τ ⌋ } e' -> (forall st, {st}⟦τ⟧{Γ} e -> {st}⟦τ⟧{Γ} e').
Proof.
  induction Γ; intros; invclear H2.
  - constructor. intros. eapply termR_perserve_rR; eauto.
  - constructor; auto. termR_solver.
    intros. apply IHΓ with (e:= ({x := c_x }t e)); eauto. denotation_simp.
    apply termR_tm_subst with (Tx := B); eauto. refinement_solver. denotation_simp3.
    termR_solver.
  - constructor; auto. termR_solver.
    destruct H13 as (e_x_hat & He_x_hat & HH). exists e_x_hat. split; auto.
    intros. apply IHΓ with (e:= (tlete e_x (x \t\ e))); eauto.
    apply termR_elete with (Tx := ⌊ τ_x ⌋ ); auto. constructor; auto; refinement_solver.
    simpl in H1. termR_solver.
  - apply ctxrR_cons_under_arr; auto. termR_solver.
    destruct H12 as (e_x_hat & He_x_hat & HH). exists e_x_hat. split; auto.
    intros. apply IHΓ with (e:= (tlete e_x (x \t\ e))); eauto.
    apply termR_elete with (Tx := ⌊ τ_x ⌋ ); auto. constructor; auto; refinement_solver.
    simpl in H1. termR_solver.
Qed.

(* terr *)

Lemma terr_is_not_inhabitant_of_overbase: forall τ n bst st,
    {n; bst; st}⟦τ⟧ terr -> not_overbasety τ.
Proof.
  destruct τ; intros; invclear H; mydestr; simpl; auto.
  invclear H3.
Qed.

Lemma ctxrR_err_overbase_forall_forward: forall Γ st τ x b n d ϕ,
    ¬ ({st}⟦τ⟧{(x, {v:b|n|d|ϕ}) :: Γ}) terr ->
    ok_dctx (dom aset st) ((x, {v:b|n|d|ϕ}) :: Γ) ->
    closed_rty 0 ({[x]} ∪ ctxdom ⦑Γ⦒ ∪ dom aset st) τ ->
    not_overbasety τ ->
    (exists (c: constant), {st}⟦{v:b|n|d|ϕ}⟧ c /\ ¬ ({<[x:=c]> st}⟦τ⟧{Γ}) terr).
Proof.
  intros. pbc. apply H. clear H. neg_simpl.
  constructor; auto; refinement_solver2.
  (* intros. *)
  (* assert (({<[x:=c_x]> st}⟦τ⟧{Γ}) terr); auto. *)
  (* apply termR_perserve_ctxrR with (e:= terr); auto; refinement_solver2. *)
  (* apply tyable_implies_terr_termR. *)
  (* econstructor; eauto; refinement_solver2; mydestr. *)
  (* apply basic_typing_weaken_value_empty; eauto. refinement_solver2. *)
  (* auto_exists_L_intros. do 2 constructor; refinement_solver2. *)
Qed.

Lemma ctxrR_err_overbase_forall_backward: forall Γ st τ x b n d ϕ,
    ok_dctx (dom aset st) ((x, {v:b|n|d|ϕ}) :: Γ) ->
    closed_rty 0 ({[x]} ∪ ctxdom ⦑Γ⦒ ∪ dom aset st) τ ->
    not_overbasety τ ->
    (exists (c: constant), {st}⟦{v:b|n|d|ϕ}⟧ c /\ ¬ ({<[x:=c]> st}⟦τ⟧{Γ}) terr) ->
    ¬ ({st}⟦τ⟧{(x, {v:b|n|d|ϕ}) :: Γ}) terr.
Proof.
  intros. mydestr. intro Hf. apply H3. clear H3.
  invclear Hf; try invclear H6.
  apply termR_perserve_ctxrR with (e:= (tlete x0 (x \t\ terr))); auto; refinement_solver2.
  eapply tyable_implies_terr_termR_terr; refinement_solver2.
  (* assert (({<[x:=x0]> st}⟦τ⟧{Γ}) terr); auto. constructor. *)
  apply termR_perserve_ctxrR with (e:= terr); auto; refinement_solver2.
  apply tyable_implies_terr_termR.
  apply let_store_typable with (Tu:=b); eauto.
  refinement_solver; basic_typing_solver.
  rewrite basic_has_type_head_to_tail_tm in H14; auto.
Qed.

Lemma ctxrR_err_overbase_forall: forall Γ st τ x b n d ϕ,
    ok_dctx (dom aset st) ((x, {v:b|n|d|ϕ}) :: Γ) ->
    closed_rty 0 ({[x]} ∪ ctxdom ⦑Γ⦒ ∪ dom aset st) τ ->
    not_overbasety τ ->
    ¬ ({st}⟦τ⟧{(x, {v:b|n|d|ϕ}) :: Γ}) terr <->
      (exists (c: constant), {st}⟦{v:b|n|d|ϕ}⟧ c /\ ¬ ({<[x:=c]> st}⟦τ⟧{Γ}) terr).
Proof.
  split; intros.
  - eapply ctxrR_err_overbase_forall_forward; eauto.
  - eapply ctxrR_err_overbase_forall_backward; eauto.
Qed.

Lemma ctxrR_err_underbase_forall_forward: forall Γ st τ x b n d ϕ,
    ¬ ({st}⟦τ⟧{(x, [v:b|n|d|ϕ]) :: Γ}) terr ->
    ok_dctx (dom aset st) ((x, [v:b|n|d|ϕ]) :: Γ) ->
    closed_rty 0 ({[x]} ∪ ctxdom ⦑Γ⦒ ∪ dom aset st) τ ->
    not_overbasety τ ->
    (forall e_x_hat, {st}⟦[v:b|n|d|ϕ]⟧ e_x_hat ->
                (exists (c: constant), [] ⊢t c ⋮v b /\ e_x_hat ↪* c /\ ¬ ({<[x:=c]> st}⟦τ⟧{Γ}) terr)
    ).
Proof.
  intros. pbc. apply H. clear H. neg_simpl.
  constructor; auto; refinement_solver2.
  exists (e_x_hat). split; auto. intros. denotation_simp1.
  apply termR_perserve_ctxrR with (e:= terr); auto; refinement_solver2.
  - apply tyable_implies_terr_termR. econstructor; eauto; refinement_solver2; mydestr.
    auto_exists_L_intros. do 2 constructor; refinement_solver2.
  - denotation_simp3.
Qed.

Lemma ctxrR_err_underbase_forall_backward: forall Γ st τ x b n d ϕ,
    ok_dctx (dom aset st) ((x, [v:b|n|d|ϕ]) :: Γ) ->
    closed_rty 0 ({[x]} ∪ ctxdom ⦑Γ⦒ ∪ dom aset st) τ ->
    not_overbasety τ ->
    (forall e_x_hat, {st}⟦[v:b|n|d|ϕ]⟧ e_x_hat ->
                (exists (c: constant), [] ⊢t c ⋮v b /\ e_x_hat ↪* c /\ ¬ ({<[x:=c]> st}⟦τ⟧{Γ}) terr)
    ) ->
    ¬ ({st}⟦τ⟧{(x, [v:b|n|d|ϕ]) :: Γ}) terr.
Proof.
  intros. intro Hf. invclear Hf; mydestr; denotation_simp3.
  assert (∃ c : constant, [] ⊢t c ⋮v x1 ∧ x0 ↪* c ∧ ¬ ({<[x:=c]> st}⟦τ⟧{Γ}) terr); auto.
  mydestr. apply H11. clear H11. eapply H4 in H3; eauto.
  apply termR_perserve_ctxrR with (e:= (tlete x0 (x \t\ terr))); auto; refinement_solver2; simpl.
  eapply tyable_implies_terr_termR_terr; refinement_solver2.
Qed.

Lemma ctxrR_err_underbase_forall: forall Γ st τ x b n d ϕ,
    ok_dctx (dom aset st) ((x, [v:b|n|d|ϕ]) :: Γ) ->
    closed_rty 0 ({[x]} ∪ ctxdom ⦑Γ⦒ ∪ dom aset st) τ ->
    not_overbasety τ ->
    ¬ ({st}⟦τ⟧{(x, [v:b|n|d|ϕ]) :: Γ}) terr <->
      (forall e_x_hat, {st}⟦[v:b|n|d|ϕ]⟧ e_x_hat ->
                  (exists (c: constant), [] ⊢t c ⋮v b /\ e_x_hat ↪* c /\ ¬ ({<[x:=c]> st}⟦τ⟧{Γ}) terr)
      ).
Proof.
  split; intros.
  - eapply ctxrR_err_underbase_forall_forward; eauto.
  - eapply ctxrR_err_underbase_forall_backward; eauto.
Qed.

Ltac reduction_solver2 :=
  RD_simp; repeat (reduction_solver1 || refinement_solver5).

Lemma rR_inhabitant_err_or_halt: forall τ,
    valid_rty τ ->
    (forall n bst st, (∀ e, ({n;bst;st}⟦τ⟧) e -> (exists (v: value), e ↪* v)) \/ (({n;bst;st}⟦τ⟧) terr)).
Proof.
  intros τ Hv. induction Hv; intros.
  - left. intros. invclear H0; mydestr; subst. eexists; eauto.
  - destruct (classic (({n0;bst;st}⟦[v:B|n|d|ϕ]⟧) terr)); auto. left; intros.
    neg_apply H0. invclear H1; mydestr. do 2 (split; auto). intros.
    apply H3 in H5; auto. exfalso_apply H2.
  - destruct (classic (({n0;bst;st}⟦-:{v: B | n | d | ϕ}⤑ τ⟧) terr)); auto. left; intros.
    neg_apply H1. invclear H2; mydestr. do 2 (split; auto). intros.
    apply H4 in H6; auto. eapply termR_perserve_rR; eauto.
    eapply mk_app_perserve_termR; eauto.
    apply stuck_tm_termR_terr; auto.
  - destruct (classic ( ({n0;bst;st}⟦(-:{v: B | n | d | ϕ}⤑ τ1) ⤑ τ2⟧) terr)); auto. left; intros.
    neg_apply H0. invclear H1; mydestr. do 2 (split; auto). intros.
    assert ([] ⊢t v_x ⋮v B ⤍ ⌊τ1⌋) by refinement_solver.
    apply H3 in H4; auto.
    eapply termR_perserve_rR; eauto.
    eapply mk_app_perserve_termR; eauto.
    apply stuck_tm_termR_terr; auto.
  - destruct (classic ( {n;bst;st}⟦(τ11 ⤑ τ12) ⤑ τ2⟧ terr )); auto. left; intros.
    neg_apply H0. invclear H1; mydestr. do 2 (split; auto). intros.
    assert ([] ⊢t v_x ⋮v ⌊τ11 ⤑ τ12⌋) by refinement_solver.
    apply H3 in H4; auto.
    eapply termR_perserve_rR; eauto.
    eapply mk_app_perserve_termR; eauto.
    apply stuck_tm_termR_terr; auto.
Qed.

Lemma not_rR_inhabitant_err_implies_halt: forall τ n bst st,
    valid_rty τ -> ~ ({n;bst;st}⟦τ⟧) terr -> ∀ e, ({n;bst;st}⟦τ⟧) e -> (exists (v: value), e ↪* v).
Proof.
  intros.
  eapply rR_inhabitant_err_or_halt in H. destruct H; eauto.
  exfalso_apply H.
Qed.

Lemma not_empty_ctxrR_inhabitant_err_implies_halt: forall τ st,
    valid_rty τ -> ~ ({st}⟦τ⟧{ [] }) terr -> ∀ e, ({st}⟦τ⟧) e -> (exists (v: value), e ↪* v).
Proof.
  intros.
  eapply rR_inhabitant_err_or_halt in H. destruct H; eauto.
  exfalso_apply H0. RD_simp; auto.
Qed.

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
      apply termR_perserve_rR with (e:= tlete v_x (random_inhabitant τ2)); refinement_solver4.
      apply mk_app_reduce_to_let'; auto; refinement_solver4; basic_typing_solver6.
      apply termR_perserve_rR with (e:= random_inhabitant τ2); refinement_solver4.
      eapply let_value_in_lc_termR_drop; eauto; reduction_solver2.
Qed.

Global Hint Resolve terr_is_not_inhabitant_of_overbase: core.

Lemma ctx_trans_terr_inhabitant: forall Γ st τ x τ_x,
    not_overbasety τ ->
    ok_dctx (dom aset st) ((x, τ_x) :: Γ) ->
    ¬ ({st}⟦ τ ⟧{(x, τ_x) :: Γ}) terr -> ¬ ({st}⟦ τ ⟧{ Γ }) terr.
Proof.
  intros. neg_apply H1; RD_simp; denotation_simp1.
  destruct (classic (not_overbasety τ_x)); invclear H0; auto_ty_exfalso; denotation_simp.
  - constructor; refinement_solver6.
    exists (random_inhabitant ([v:x0|0|x2|x3])).
    assert (({0;b∅;st}⟦ ([v:x0|0|x2|x3]) ⟧) (random_inhabitant ([v:x0|0|x2|x3]))) by
      (apply random_inhabitant_in_any_under; refinement_solver5).
    split; auto. intros.
    assert ([] ⊢t (random_inhabitant ([v:x0|0|x2|x3])) ⋮t ⌊ ([v:x0|0|x2|x3]) ⌋); auto.
    assert ([] ⊢t v_x ⋮v x0); reduction_solver2.
    assert (({st}⟦τ⟧{Γ}) (tlete e_x (x \t\ terr))).
    apply termR_perserve_ctxrR with (e:= terr); refinement_solver6.
    apply tyable_implies_terr_termR. reduction_solver2.
    eapply ty_tlete_dummy; refinement_solver6.
    setoid_rewrite ctxrR_shadow_update_st; auto; refinement_solver.
  - apply ctxrR_cons_under_arr; auto; refinement_solver.
    exists (random_inhabitant τ_x).
    assert (({0;b∅;st}⟦ τ_x ⟧) (random_inhabitant τ_x)) by
      (apply random_inhabitant_in_any_under; refinement_solver5).
    split; auto. intros. assert ([] ⊢t v_x ⋮v ⌊ τ_x ⌋ ) by reduction_solver2.
    denotation_simp3.
    apply termR_perserve_ctxrR with (e:= terr); refinement_solver6.
    apply tyable_implies_terr_termR. reduction_solver2.
    assert (ok ⌊Γ⌋* ) by refinement_solver.
    eapply ty_tlete_dummy; refinement_solver6. basic_typing_solver.
  - invclear H0; auto_ty_exfalso3. constructor; refinement_solver6.
    intros. reduction_simpl1.
    setoid_rewrite ctxrR_shadow_update_st_c; auto; refinement_solver.
Qed.

(* term meet *)

Lemma rR_tmeet_when_both: forall n bst st b n1 d ϕ e1 e2,
    ({n;bst;st}⟦ [v:b|n1|d|ϕ] ⟧) e1 -> ({n;bst;st}⟦ [v:b|n1|d|ϕ] ⟧) e2 ->
    ({n;bst;st}⟦ [v:b|n1|d|ϕ] ⟧) (e1 ⊗{ b } e2).
Proof.
  intros. invclear H; mydestr. constructor; auto. apply tmeet_typable; refinement_solver.
  constructor; auto.
  intros. rewrite reduction_tmeet_iff_both; refinement_solver.
Qed.

Lemma rR_tmeet3_when_all: forall n bst st b n1 d ϕ e1 e2 e3,
    ({n;bst;st}⟦ [v:b|n1|d|ϕ]  ⟧) e1 -> ({n;bst;st}⟦ [v:b|n1|d|ϕ]  ⟧) e2 -> ({n;bst;st}⟦ [v:b|n1|d|ϕ]  ⟧) e3 ->
    ({n;bst;st}⟦ [v:b|n1|d|ϕ]  ⟧) ((e1 ⊗{ b } e2) ⊗{ b } e3).
Proof.
  intros. apply rR_tmeet_when_both; auto. apply rR_tmeet_when_both; auto.
Qed.

Lemma rR_tmeet4_when_all: forall n bst st b n1 d ϕ e1 e2 e3 e4,
    ({n;bst;st}⟦ [v:b|n1|d|ϕ] ⟧) e1 ->
    ({n;bst;st}⟦ [v:b|n1|d|ϕ] ⟧) e2 -> ({n;bst;st}⟦ [v:b|n1|d|ϕ] ⟧) e3 -> ({n;bst;st}⟦ [v:b|n1|d|ϕ] ⟧) e4 ->
    ({n;bst;st}⟦ [v:b|n1|d|ϕ] ⟧) (((e1 ⊗{ b } e2) ⊗{ b } e3) ⊗{ b } e4).
Proof.
  intros. apply rR_tmeet_when_both; auto. apply rR_tmeet3_when_all; auto.
Qed.

Lemma reduction_tmeet3_iff_all: forall (T: base_ty) e1 e2 e3,
    [] ⊢t e1 ⋮t T -> [] ⊢t e2 ⋮t T -> [] ⊢t e3 ⋮t T ->
    (forall (v: value), ((e1 ⊗{ T } e2) ⊗{ T } e3) ↪* v <-> e1 ↪* v /\ e2 ↪* v /\ e3 ↪* v).
Proof.
  intros. rewrite reduction_tmeet_iff_both; refinement_solver.
  rewrite reduction_tmeet_iff_both; refinement_solver.
  apply tmeet_typable; refinement_solver.
Qed.

Lemma reduction_tmeet4_iff_all: forall (T: base_ty) e1 e2 e3 e4,
    [] ⊢t e1 ⋮t T -> [] ⊢t e2 ⋮t T -> [] ⊢t e3 ⋮t T -> [] ⊢t e4 ⋮t T ->
    (forall (v: value),
        (((e1 ⊗{ T } e2) ⊗{ T } e3) ⊗{ T } e4) ↪* v <-> e1 ↪* v /\ e2 ↪* v /\ e3 ↪* v /\ e4 ↪* v).
Proof.
  intros. rewrite reduction_tmeet_iff_both; refinement_solver.
  rewrite reduction_tmeet3_iff_all; refinement_solver.
  apply tmeet_typable; refinement_solver.
  apply tmeet_typable; refinement_solver.
Qed.
