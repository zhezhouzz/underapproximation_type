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

Ltac my_simplify_map_eq2 :=
  repeat (match goal with
          | [|- context [ <[?a:=_]> (<[?a:=_]> ?st) ]] =>
              setoid_rewrite insert_insert
          | [H: context [ <[?a:=_]> (<[?a:=_]> ?st) ] |- _ ] =>
              setoid_rewrite insert_insert in H
          | [|- context [<[?b:=?c0]> ?st !! ?b ] ] => setoid_rewrite lookup_insert
          | [H: context [<[?b:=?c0]> ?st !! ?b ] |- _ ] => setoid_rewrite lookup_insert in H
          | [H: ?st !! ?x = Some _ |- context [?st !! ?x]] => setoid_rewrite H
          | [H: ?x ∈ dom _ ?st |- context [?st !! ?x]] =>
              assert (exists c, st !! x = Some c) as Htmp by
                (destruct (st !! x) eqn: HH; eauto;
                 setoid_rewrite <- not_elem_of_dom in HH; set_solver); mydestr
          | [H': ?a ∉ dom _ ?st |- context [?st !! ?a]] =>
              assert (st !! a = None) as Htmp by (apply not_elem_of_dom; fast_set_solver);
              rewrite Htmp; try clear Htmp
          | [H': ?a ∉ dom _ ?st, H: context [?st !! ?a] |- _ ] =>
              assert (st !! a = None) as Htmp by (apply not_elem_of_dom; fast_set_solver);
              rewrite Htmp in H; try clear Htmp
          end || my_simplify_map_eq1).

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
  my_simplify_map_eq2;
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
   end).

Ltac denotation_simp := denotation_simp1.


Lemma valid_rty_under_iff_over: forall n b (d2: aset) (ϕ: refinement),
    valid_rty {v:b|n|d2|ϕ} <-> valid_rty [v:b|n|d2|ϕ].
Proof.
  split; intros; invclear H; constructor; auto.
Qed.

Lemma closed_rty_under_iff_over: forall n1 n2 b (d1 d2: aset) (ϕ: refinement),
    closed_rty n1 d1 {v:b|n2|d2|ϕ} <-> closed_rty n1 d1 [v:b|n2|d2|ϕ].
Proof.
  split; unfold closed_rty; intros.
  - invclear H; mydestr. constructor; auto.
    rewrite <- valid_rty_under_iff_over; auto.
    constructor; auto. invclear H. constructor; auto.
  - invclear H; mydestr. constructor; auto.
    rewrite valid_rty_under_iff_over; auto.
    constructor; auto. invclear H. constructor; auto.
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
      | [H: ok_dctx _ _ |- ok _ ] => apply ok_dctx_regular1 in H; mydestr
      | [H: ok (_ :: ?Γ) |- ok ⌊?Γ⌋* ] => rewrite ok_pre_destruct in H; mydestr; ctx_erase_simp
      | [ |- context [ ⌊ _ ⌋* ] ] => ctx_erase_simp; fast_set_solver
      | [H: ?e ↪* (tvalue ?v), H': [] ⊢t ?e ⋮t ?T |- [] ⊢t ?v ⋮v ?T ] =>
          eapply multi_preservation_value in H'; eauto
      | [H: [] ⊢t ?e ⋮t ?T |- _ ⊢t ?e ⋮t _ ] => apply basic_typing_weaken_tm_empty; eauto
      | [H: lc_rty_idx _ _ |- lc_rty_idx _ _ ] => invclear H; auto
      end || refinement_solver0).

Ltac refinement_solver := refinement_solver2.


Lemma refinement_shadow_update_st: forall bst c n ϕ (x: atom) d st (v: constant),
    x ∉ d -> wf_r n d ϕ -> ϕ bst (<[x:=c]> st) v <-> ϕ bst st v.
Proof.
  intros.
  destruct H0. apply H0; auto.
Qed.

Ltac rR_shadow_update_st_tac :=
  repeat match goal with
    | [H: valid_rty {v: _ |_ |?d|?ϕ} |- ?ϕ _ (<[_:=_]> _) _ ] =>
        rewrite refinement_shadow_update_st with (d:=d); auto; refinement_solver
    | [H: valid_rty [v: _ |_ |?d|?ϕ] |- ?ϕ _ (<[_:=_]> _) _ ] =>
        rewrite refinement_shadow_update_st with (d:=d); auto; refinement_solver
    | [H: valid_rty {v: _ |_ |?d|?ϕ}, H': ?ϕ _ (<[_:=_]> _) _ |- ?ϕ _ _ _ ] =>
        rewrite refinement_shadow_update_st with (d:=d) in H'; auto; refinement_solver
    | [H: valid_rty [v: _ |_ |?d|?ϕ], H': ?ϕ _ (<[_:=_]> _) _ |- ?ϕ _ _ _ ] =>
        rewrite refinement_shadow_update_st with (d:=d) in H'; auto; refinement_solver
    | [H: closed_rty _ _ ?τ, H': ?ϕ ?bst _ ?c_x |- ?ϕ ?bst _ ?c_x] =>
        match τ with
        | context [ϕ] => destruct H; mydestr
        end
    end.

Lemma rR_shadow_update_st: forall τ n st,
    closed_rty n (dom aset st) τ ->
    (forall bst (a: atom) c e, a ∉ (rty_fv τ) ->
                      ({n; bst; <[a:=c]> st}⟦τ⟧) e <-> ({n; bst; st}⟦τ⟧) e
    ).
Proof.
  induction τ; split; simpl; intros; mydestr; subst;
    split; auto; split; try closed_rty_solver.
  - exists x. repeat split; auto. rR_shadow_update_st_tac.
  - exists x. repeat split; auto. rR_shadow_update_st_tac.
  - intros. apply H3; auto. rR_shadow_update_st_tac.
  - intros. apply H3; auto. rR_shadow_update_st_tac.
  - intros. refinement_simp1. apply H3 in H4. apply IHτ in H4; refinement_solver.
    rewrite refinement_shadow_update_st with (d:=d); auto; refinement_solver.
  - intros. refinement_simp1. apply H3 in H4. apply IHτ; refinement_solver.
    rewrite <- refinement_shadow_update_st with (d:=d); auto; refinement_solver.
  - intros. refinement_simp1.
    rewrite <- IHτ1 with (a:= a) (c:=c) in H4; auto; refinement_solver.
    apply H3 in H4. rewrite IHτ2 in H4; by refinement_solver.
  - intros. refinement_simp1.
    rewrite IHτ1 in H4 by refinement_solver.
    apply H3 in H4; rewrite IHτ2; auto; refinement_solver.
Qed.

Lemma lc_rR_shadow_update_st: forall τ st (a: atom) c e,
    closed_rty 0 (dom aset st) τ -> a ∉ (rty_fv τ) ->
    ({<[a:=c]> st}⟦τ⟧) e <-> ({st}⟦τ⟧) e.
Proof.
  split; intros.
  - rewrite rR_shadow_update_st in H1; eauto.
  - rewrite rR_shadow_update_st; eauto.
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

Ltac ctxrR_shadow_update_st_tac :=
  match goal with
  | [H: context [{0;b∅;<[?a0:=?c]> ?st}⟦?τ⟧ _],
        H': ({0;b∅;?st}⟦?τ⟧) _ |- ?ret ] =>
      (* idtac H' *)
      (rewrite <- lc_rR_shadow_update_st with (a:=a0) (c:=c) in H'
       ;eauto; refinement_solver);
      match ret with
      | context [({_↦?v_x}) _] => eapply H with (v_x := v_x) in H'; eauto
      | _ => apply H in H'
      end
  end.

Ltac auto_ty_exfalso :=
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

Lemma ctxrR_shadow_update_st: forall Γ st τ,
    closed_rty 0 (ctxdom Γ ∪ dom aset st) τ ->
    ok_dctx (dom aset st) Γ ->
    (forall (a: atom) c e, a ∉ (ctxdom Γ ∪ dom aset st) ->
                        ({<[a:=c]> st}⟦τ⟧{Γ}) e <-> ({st}⟦τ⟧{Γ}) e
    ).
Proof.
  induction Γ; split; simpl; intros; mydestr; subst.
  - invclear H2. constructor; auto; intros.
    rewrite <- rR_shadow_update_st; eauto; refinement_solver.
  - invclear H2. constructor; auto; intros.
    rewrite rR_shadow_update_st; eauto; refinement_solver.
  - invclear H0; invclear H2; mydestr; try auto_ty_exfalso.
    + constructor; auto. constructor; auto.
      intros.
      ctxrR_shadow_update_st_tac.
      setoid_rewrite insert_commute in H0; try fast_set_solver.
      rewrite IHΓ in H0; auto; denotation_simp.
      refinement_solver.
    + constructor; auto. constructor; auto.
      exists x. split. intros. rewrite <- rR_shadow_update_st; eauto. refinement_solver.
      intros; mydestr.
      ctxrR_shadow_update_st_tac.
      assert ([] ⊢t v_x ⋮v ⌊r⌋) by refinement_solver.
      simpl in H1. destruct v_x; try auto_ty_exfalso.
      { rewrite subst_st_insert_commute in H3; try fast_set_solver.
        rewrite IHΓ in H3; auto; denotation_simp.
        refinement_solver. }
      (* { denotation_simp; rewrite IHΓ in H3; auto; denotation_simp. refinement_solver. ok_dctx_solver_slow. } *)
      (* { denotation_simp; rewrite IHΓ in H3; auto; denotation_simp. refinement_solver. ok_dctx_solver_slow. } *)
    + constructor; auto. apply ok_dctx_cons_arr; auto.
      exists x. split. intros. rewrite <- rR_shadow_update_st; eauto. refinement_solver.
      intros; mydestr.
      ctxrR_shadow_update_st_tac.
      assert ([] ⊢t v_x ⋮v ⌊r⌋) by refinement_solver.
      simpl in H1. destruct v_x; try auto_ty_exfalso;
        denotation_simp; rewrite IHΓ in H3; auto; denotation_simp; refinement_solver.
   - invclear H0; invclear H2; mydestr; try auto_ty_exfalso.
    + constructor; auto; denotation_simp. refinement_solver. ok_dctx_solver_slow.
      intros. specialize (H16 c_x).
      assert ({0;b∅; st}⟦{v:B|0|d|ϕ}⟧ c_x) by
      (rewrite <- lc_rR_shadow_update_st with (a:=a0) (c:=c); eauto; refinement_solver).
      apply H16 in H2.
      setoid_rewrite insert_commute; refinement_solver.
      rewrite IHΓ; auto; denotation_simp; refinement_solver.
    + invclear H7; mydestr.
      constructor; auto. refinement_solver. ok_dctx_solver_slow.
      exists x. split. rewrite rR_shadow_update_st; eauto; refinement_solver.
      intros; mydestr.
      rewrite lc_rR_shadow_update_st in H12; auto; try refinement_solver.
      eapply H2 with (v_x := v_x) in H12; eauto.
      assert ([] ⊢t v_x ⋮v ⌊r⌋) by refinement_solver.
      simpl in H1. destruct v_x; try auto_ty_exfalso.
      { rewrite subst_st_insert_commute; denotation_simp.
        rewrite IHΓ; auto; denotation_simp; refinement_solver. }
    + invclear H7; mydestr.
      constructor; auto. refinement_solver. ok_dctx_solver_slow.
      exists x. split. intros. rewrite rR_shadow_update_st; eauto; refinement_solver.
      intros; mydestr.
      rewrite lc_rR_shadow_update_st in H12; auto; try refinement_solver.
      eapply H2 with (v_x := v_x) in H12; eauto.
      assert ([] ⊢t v_x ⋮v ⌊r⌋) by refinement_solver.
      simpl in H1. destruct v_x; try auto_ty_exfalso;
        denotation_simp; rewrite IHΓ; auto; denotation_simp; refinement_solver.
Qed.

(* Lemma denotation_lc_lete: forall Γ (e_x e: tm) τ (st: state), *)
(*   {st}⟦τ⟧{Γ} e_x -> {st}⟦τ⟧{Γ} e -> ({st}⟦τ⟧{Γ}) (tlete e_x e). *)
(* Proof. *)
(*   induction Γ; intros; denotation_simp; invclear H; invclear H0. *)
(*   - constructor. admit. *)
(*   - constructor; auto. admit. *)
(*     intros. apply IHΓ; auto. admit. simpl. apply IHΓ; auto. *)
(*     + apply IHΓ; auto. admit. simpl. *)


(*       rewrite ctxrR_shadow_update_st; auto. *)
(*       apply H. *)
(*     apply H16 in H. apply IHΓ. *)
(*     + rewrite ctxrR_shadow_update_st; auto. *)
(*     simpl. *)

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
    intros. apply IHτ2 with (e := (mk_app e e_x)); eauto. termR_solver; refinement_solver.
  - repeat (split; try termR_solver).
    intros. apply IHτ2 with (e := (mk_app e e_x)); eauto. termR_solver; refinement_solver.
Qed.

Lemma termR_perserve_ctxrR: forall Γ τ (e e': tm),
    not_overbasety τ ->
    valid_rty τ ->
    e <-<{ ⌊ Γ ⌋* ;  ⌊ τ ⌋ } e' -> (forall st, {st}⟦τ⟧{Γ} e -> {st}⟦τ⟧{Γ} e').
Proof.
  induction Γ; intros; invclear H2.
  - constructor. intros. eapply termR_perserve_rR; eauto.
  - constructor; auto. termR_solver.
    intros. apply IHΓ with (e:= (tlete c_x (x \t\ e))); eauto.
    apply termR_elete with (Tx := B); auto. constructor; auto; refinement_solver; basic_typing_solver2.
    simpl in H1. termR_solver.
  - constructor; auto. termR_solver.
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
    closed_rty 0 ({[x]} ∪ ctxdom Γ ∪ dom aset st) τ ->
    not_overbasety τ ->
    (exists (c: constant), {st}⟦{v:b|n|d|ϕ}⟧ c /\ ¬ ({<[x:=c]> st}⟦τ⟧{Γ}) terr).
Proof.
  intros. pbc. apply H. clear H. neg_simpl.
  constructor; auto; refinement_solver2.
  intros.
  assert (({<[x:=c_x]> st}⟦τ⟧{Γ}) terr); auto.
  apply termR_perserve_ctxrR with (e:= terr); auto; refinement_solver2.
  apply tyable_implies_terr_termR.
  econstructor; eauto; refinement_solver2; mydestr.
  apply basic_typing_weaken_value_empty; eauto. refinement_solver2.
  auto_exists_L_intros. do 2 constructor; refinement_solver2.
Qed.

Lemma ctxrR_err_overbase_forall_backward: forall Γ st τ x b n d ϕ,
    ok_dctx (dom aset st) ((x, {v:b|n|d|ϕ}) :: Γ) ->
    closed_rty 0 ({[x]} ∪ ctxdom Γ ∪ dom aset st) τ ->
    not_overbasety τ ->
    (exists (c: constant), {st}⟦{v:b|n|d|ϕ}⟧ c /\ ¬ ({<[x:=c]> st}⟦τ⟧{Γ}) terr) ->
    ¬ ({st}⟦τ⟧{(x, {v:b|n|d|ϕ}) :: Γ}) terr.
Proof.
  intros. mydestr. intro Hf. apply H3. clear H3.
  invclear Hf; try invclear H6.
  apply termR_perserve_ctxrR with (e:= (tlete x0 (x \t\ terr))); auto; refinement_solver2.
  eapply tyable_implies_terr_termR_terr; refinement_solver2.
  apply basic_typing_weaken_value_empty; eauto. refinement_solver2.
Qed.

Lemma ctxrR_err_overbase_forall: forall Γ st τ x b n d ϕ,
    ok_dctx (dom aset st) ((x, {v:b|n|d|ϕ}) :: Γ) ->
    closed_rty 0 ({[x]} ∪ ctxdom Γ ∪ dom aset st) τ ->
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
    closed_rty 0 ({[x]} ∪ ctxdom Γ ∪ dom aset st) τ ->
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
  - invclear H0; denotation_simp; apply H4; auto; refinement_solver2.
Qed.

Lemma ctxrR_err_underbase_forall_backward: forall Γ st τ x b n d ϕ,
    ok_dctx (dom aset st) ((x, [v:b|n|d|ϕ]) :: Γ) ->
    closed_rty 0 ({[x]} ∪ ctxdom Γ ∪ dom aset st) τ ->
    not_overbasety τ ->
    (forall e_x_hat, {st}⟦[v:b|n|d|ϕ]⟧ e_x_hat ->
                (exists (c: constant), [] ⊢t c ⋮v b /\ e_x_hat ↪* c /\ ¬ ({<[x:=c]> st}⟦τ⟧{Γ}) terr)
    ) ->
    ¬ ({st}⟦τ⟧{(x, [v:b|n|d|ϕ]) :: Γ}) terr.
Proof.
  intros. intro Hf. invclear Hf; mydestr.
  assert ([] ⊢t x0 ⋮t b) by refinement_solver2.
  assert (∃ c : constant, [] ⊢t c ⋮v b ∧ x0 ↪* c ∧ ¬ ({<[x:=c]> st}⟦τ⟧{Γ}) terr); auto.
  mydestr. apply H11. clear H11. eapply H4 in H3; eauto.
  apply termR_perserve_ctxrR with (e:= (tlete x0 (x \t\ terr))); auto; refinement_solver2; simpl.
  eapply tyable_implies_terr_termR_terr; refinement_solver2.
Qed.

Lemma ctxrR_err_underbase_forall: forall Γ st τ x b n d ϕ,
    ok_dctx (dom aset st) ((x, [v:b|n|d|ϕ]) :: Γ) ->
    closed_rty 0 ({[x]} ∪ ctxdom Γ ∪ dom aset st) τ ->
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
