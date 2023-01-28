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

Inductive wf_ctxrR: state -> listctx rty -> Prop :=
| wf_ctxrR_nil: forall st, wf_ctxrR st []
| wf_ctxrR_cons_over: forall st (x: atom) B n d ϕ Γ,
    ok_dctx (dom _ st) ((x, {v: B | n | d | ϕ}) :: Γ) ->
    (* (exists (c_wf: constant), {st}⟦ {v: B | n | d | ϕ} ⟧ c_wf) -> *)
    (forall (c_x: constant), {st}⟦ {v: B | n | d | ϕ} ⟧ c_x ->
                        wf_ctxrR (<[ x := c_x ]> st) Γ) ->
    wf_ctxrR st ((x, {v: B | n | d | ϕ}) :: Γ)
| wf_ctxrR_cons_under_base: forall st (x: atom) B n d ϕ Γ,
    ok_dctx (dom _ st) ((x, [v: B | n | d | ϕ] ) :: Γ) ->
    (forall e_wf, {st}⟦ [v: B | n | d | ϕ] ⟧ e_wf -> (exists (v_wf: value), e_wf ↪* v_wf)) ->
    (exists e_x_hat, {st}⟦ [v: B | n | d | ϕ] ⟧ e_x_hat /\
                  (forall e_x, {st}⟦ [v: B | n | d | ϕ] ⟧ e_x ->
                          (∀ (v_x: value), e_x_hat ↪* v_x ->
                                           wf_ctxrR ({ x ↦ v_x } st) Γ ))) ->
    wf_ctxrR st ((x, [v: B | n | d | ϕ]) :: Γ)
| wf_ctxrR_cons_under_arr: forall st (x: atom) τ_x Γ,
    is_arr τ_x ->
    ok_dctx (dom _ st) ((x, τ_x) :: Γ) ->
    (forall e_wf, {st}⟦ τ_x ⟧ e_wf -> (exists (v_wf: value), e_wf ↪* v_wf)) ->
    wf_ctxrR st Γ ->
    wf_ctxrR st ((x, τ_x) :: Γ).

Ltac auto_meet_exists HE :=
  match goal with
     | [H1: ({0;b∅;?st}⟦[v:?b|?n|?d|?ϕ]⟧) ?x1,
           H2: ({0;b∅;?st}⟦[v:?b|?n|?d|?ϕ]⟧) ?x2,
             H3: ({0;b∅;?st}⟦[v:?b|?n|?d|?ϕ]⟧) ?x3,
               H4: ({0;b∅;?st}⟦[v:?b|?n|?d|?ϕ]⟧) ?x4,
                 H5: ({0;b∅;?st}⟦[v:?b|?n|?d|?ϕ]⟧) ?x5
        |- ∃ e, {0;b∅;?st}⟦[v:?b|?n|?d|?ϕ]⟧ e /\ _ ] =>
         exists ((((x1 ⊗{ b } x2) ⊗{ b } x3) ⊗{ b } x4) ⊗{ b } x5)
         ; assert ( {0;b∅;st}⟦[v:b|n|d|ϕ]⟧ ((((x1 ⊗{ b } x2) ⊗{ b } x3) ⊗{ b } x4) ⊗{ b } x5))
           as HE by (eapply rR_tmeet5_when_all; auto); split; auto; intros
  | [H1: ({0;b∅;?st}⟦[v:?b|?n|?d|?ϕ]⟧) ?x1,
        H2: ({0;b∅;?st}⟦[v:?b|?n|?d|?ϕ]⟧) ?x2,
          H3: ({0;b∅;?st}⟦[v:?b|?n|?d|?ϕ]⟧) ?x3,
            H4: ({0;b∅;?st}⟦[v:?b|?n|?d|?ϕ]⟧) ?x4 |- ∃ e, {0;b∅;?st}⟦[v:?b|?n|?d|?ϕ]⟧ e /\ _ ] =>
      exists (((x1 ⊗{ b } x2) ⊗{ b } x3) ⊗{ b } x4) ;
      assert ( {0;b∅;st}⟦[v:b|n|d|ϕ]⟧ (((x1 ⊗{ b } x2) ⊗{ b } x3) ⊗{ b } x4)) as HE by (eapply rR_tmeet4_when_all; auto); split; auto; intros
  | [H1: ({0;b∅;?st}⟦[v:?b|?n|?d|?ϕ]⟧) ?x1,
        H2: ({0;b∅;?st}⟦[v:?b|?n|?d|?ϕ]⟧) ?x2,
          H3: ({0;b∅;?st}⟦[v:?b|?n|?d|?ϕ]⟧) ?x3 |- ∃ e, {0;b∅;?st}⟦[v:?b|?n|?d|?ϕ]⟧ e /\ _ ] =>
      exists ((x1 ⊗{ b } x2) ⊗{ b } x3) ; assert ( {0;b∅;st}⟦[v:b|n|d|ϕ]⟧ ((x1 ⊗{ b } x2) ⊗{ b } x3)) as HE by (eapply rR_tmeet3_when_all; auto); split; auto; intros
  | [H1: ({0;b∅;?st}⟦[v:?b|?n|?d|?ϕ]⟧) ?x1,
        H2: ({0;b∅;?st}⟦[v:?b|?n|?d|?ϕ]⟧) ?x2 |- ∃ e, {0;b∅;?st}⟦[v:?b|?n|?d|?ϕ]⟧ e /\ _ ] =>
      exists (x1 ⊗{ b } x2); assert ( {0;b∅;st}⟦[v:b|n|d|ϕ]⟧ (x1 ⊗{ b } x2)) as HE by (eapply rR_tmeet_when_both; eauto); split; auto; intros
  | [H1: ({0;b∅;?st}⟦[v:?b|?n|?d|?ϕ]⟧) ?x1 |- ∃ e, {0;b∅;?st}⟦[v:?b|?n|?d|?ϕ]⟧ e /\ _ ] =>
      exists x1; split; eauto; intros
  | [|- ∃ e, {0;b∅;?st}⟦?r⟧ e /\ _ ] =>
      assert ({0;b∅;st}⟦r⟧ (random_inhabitant r)) by (apply random_inhabitant_in_any_under; refinement_solver7);
      eexists; split; eauto; intros
  end.

Ltac auto_meet_reduce :=
  match goal with
  | [H: ((((?x1 ⊗{ ?b } ?x2) ⊗{ ?b } ?x3) ⊗{ ?b } ?x4) ⊗{ ?b } ?x5) ↪* ?v |- _ ] =>
      rewrite reduction_tmeet5_iff_all in H; refinement_solver7; mydestr; eauto
  | [H: (((?x1 ⊗{ ?b } ?x2) ⊗{ ?b } ?x3) ⊗{ ?b } ?x4)↪* ?v |- _ ] =>
      rewrite reduction_tmeet4_iff_all in H; refinement_solver7; mydestr; eauto
  | [H: ((?x1 ⊗{ ?b } ?x2) ⊗{ ?b } ?x3) ↪* ?v |- _ ] =>
      rewrite reduction_tmeet3_iff_all in H; refinement_solver7; mydestr; eauto
  | [H: (?x1 ⊗{ ?b} ?x2) ↪* ?v |- _ ] =>
      rewrite reduction_tmeet_iff_both in H; refinement_solver7; mydestr; eauto
  end.

Ltac auto_under_specialize e :=
  repeat (match goal with
          | [He: ?P (tvalue e), H: forall e_x, ?P e_x -> _ |- _ ] => specialize (H _ He)
          | [He: ?P e, H: forall e_x, ?P e_x -> _ |- _ ] => specialize (H _ He)
          | [He: ?P (tvalue e), H: forall e_x, _ (tvalue e_x) -> _ |- _ ] => specialize (H _ He)
          | [He: ?P (tvalue (vconst e)), H: forall c_x, _ (tvalue (vconst c_x)) -> _ |- _ ] => specialize (H _ He)
          end; mydestr).

Ltac auto_under_reduction_specialize :=
  repeat match goal with
    | [H: forall v, ?e ↪* (tvalue _) → _, H': ?e ↪* (tvalue _) |- _ ] => specialize (H  _ H')
    (* | [H: forall v, ?e ↪* (tvalue _) → _, H': ?e ↪* (tvalue _) |- _ ] => specialize (H  _ H') *)
    end.

Ltac auto_under e :=
  auto_under_specialize e; auto_under_reduction_specialize.

Lemma is_arr_implies_random_inhabitant_is_value: forall τ_x, is_arr τ_x -> (exists (v: value), random_inhabitant τ_x = v).
Proof.
  induction τ_x; simpl; intro; invclear H; eauto.
Qed.

Lemma mk_eq_constant_is_not_overbasety: forall c, not_overbasety (mk_eq_constant c).
Proof.
  intros. destruct c; simpl; auto.
Qed.

Global Hint Resolve mk_eq_constant_is_not_overbasety: core.

Lemma mk_eq_var_is_not_overbasety: forall b x, not_overbasety (mk_eq_var b x).
Proof.
  intros. destruct b; simpl; auto.
Qed.

Global Hint Resolve mk_eq_var_is_not_overbasety: core.

Lemma is_arr_implies_not_overbasety: forall τ, is_arr τ -> not_overbasety τ.
Proof.
  intros. destruct τ; simpl; auto; invclear H; auto.
Qed.

Global Hint Resolve is_arr_implies_not_overbasety: core.

Lemma wf_ctxrR_iff_wf_ctxrR_not_terr: forall (Γ: listctx rty) st, wf_ctxrR st Γ <-> wf_ctxrR_not_terr st Γ.
Proof.
  induction Γ; split; intros.
  - constructor; auto.
  - constructor; auto.
  - invclear H.
    + constructor; auto. intros. rewrite <- IHΓ; auto.
    + mydestr. constructor; auto. simpl; auto. intro Hf. apply H4 in Hf. mydestr. reduction_simpl1.
      mydestr. exists x0. split; auto.
      intros. rewrite <- IHΓ; eauto.
    + constructor; auto.
      intro Hf. apply H5 in Hf. mydestr. reduction_simpl1.
      mydestr.
      assert ({0;b∅;st}⟦τ_x⟧ (random_inhabitant τ_x)).
      apply random_inhabitant_in_any_under; refinement_solver7.
      eexists; split; eauto. intros. rewrite <- IHΓ; eauto. erewrite state_insert_closed_value_arr; eauto.
      refinement_solver.
  - invclear H. constructor; auto. intros. rewrite IHΓ; auto.
    destruct (decide (is_arr τ_x)); auto_ty_exfalso.
    + mydestr.
      assert (∀ e_wf : tm, ({0;b∅;st}⟦τ_x⟧) e_wf → ∃ v_wf : value, e_wf ↪* v_wf) as Hv.
      intros. eapply not_rR_inhabitant_err_implies_halt in H5; eauto; refinement_solver.
      constructor; auto.
      specialize (Hv _ H). mydestr. specialize (H0 _ H _ H1). rewrite <- IHΓ in H0.
      erewrite state_insert_closed_value_arr in H0; eauto. refinement_solver.
    + constructor; auto.
      intros. eapply not_rR_inhabitant_err_implies_halt in H5; eauto; refinement_solver.
      mydestr. exists x0. split; auto.
      intros. rewrite IHΓ; eauto.
Qed.

Global Hint Constructors ok_dctx: core.

Lemma rR_implies_no_free: forall st e τ a, ({0;b∅;st}⟦τ⟧) e -> a ∉ fv_tm e.
Proof.
  intros. refinement_solver7. basic_typing_solver7. set_solver.
Qed.

Global Hint Resolve rR_implies_no_free: core.

Lemma ctxrR_tlete_drop_halt_lhs: forall st τ τ_x a e_x e,
    not_overbasety τ ->
   {0;b∅;st}⟦τ⟧ e -> {0;b∅;st}⟦τ_x⟧ e_x ->
   (∃ v : value, e_x ↪* v) ->
   ({0;b∅;st}⟦τ⟧) (tlete e_x (a \t\ e)).
Proof.
  intros.
  eapply termR_perserve_rR; eauto; refinement_solver5.
  eapply termR_tlete_drop_halt_lhs'; refinement_solver7.
Qed.

Global Hint Resolve ctxrR_tlete_drop_halt_lhs: core.

(* Lemma rR_random_inhabitant: forall st r, {0;b∅;st}⟦r⟧ (random_inhabitant r). *)
(* Proof. *)
(*   intros. apply random_inhabitant_in_any_under; refinement_solver7. *)

Lemma rR_implies_reduction_no_free: forall st r e (v: value),
({0;b∅;st}⟦r⟧) e -> e ↪* v -> closed_value v.
Proof.
  intros.
  assert ([] ⊢t v ⋮v ⌊ r ⌋);eauto. refinement_solver.
Qed.

Global Hint Resolve rR_implies_reduction_no_free: core.

Lemma rRctx_pre_weakening_empty: forall Γ st τ,
    not_overbasety τ ->
    wf_ctxrR st Γ ->
    (forall e, {st}⟦τ⟧ e -> {st}⟦τ⟧{Γ} e).
Proof.
  induction Γ; intros; RD_simp; denotation_simp.
  invclear H0.
  - invclear H5; auto_ty_exfalso. constructor; refinement_solver7.
    intros. apply IHΓ; auto.
    rewrite subst_fresh_tm; eauto.
    rewrite rR_shadow_update_st_c; refinement_solver.
  - constructor; refinement_solver7. mydestr. auto_meet_exists Hmeet.
    apply IHΓ; eauto.
    rewrite rR_shadow_update_st; refinement_solver.
  - apply ctxrR_cons_under_arr; refinement_solver7.
     auto_meet_exists Hmeet. invclear H7; auto_ty_exfalso.
     rewrite ctxrR_shadow_update_st; refinement_solver.
Qed.

Global Hint Resolve is_arr_implies_not_overbasety: core.

Lemma is_arr_simpl_rewrite: forall r a st (P: state -> tm -> Prop),
    is_arr r ->
    closed_rty 0 (dom aset st) r ->
    (∀ e_wf : tm, ({0;b∅;st}⟦r⟧) e_wf → ∃ v_wf : value, e_wf ↪* v_wf) ->
    (∃ e_x_hat : tm, ({0;b∅;st}⟦r⟧) e_x_hat
                     ∧ (∀ e_x : tm, ({0;b∅;st}⟦r⟧) e_x → ∀ v_x : value, e_x_hat ↪* v_x → P ({a↦v_x} st) e_x)) <->
      (∀ e_x : tm, ({0;b∅;st}⟦r⟧) e_x → P st e_x).
Proof.
  split; intros.
  - mydestr. assert (∃ v_x : value, x ↪* v_x); auto. mydestr.
    eapply H4 in H3; eauto.
    assert ([] ⊢t x0 ⋮v ⌊r⌋). refinement_solver7.
    assert (closed_value x0) as Hclosedv. basic_typing_solver.
    apply state_insert_closed_value with (a:=a) (st:=st) in Hclosedv.
    destruct Hclosedv; mydestr; subst.
    + invclear H6. destruct r; destruct x1; inversion H; invclear H9.
    + rewrite <- H7; auto.
  - exists (random_inhabitant r).
    assert ({0;b∅;st}⟦r⟧ (random_inhabitant r)).
    apply random_inhabitant_in_any_under; refinement_solver7.
    split; auto. intros.
    assert ([] ⊢t v_x ⋮v ⌊r⌋) by refinement_solver7.
    assert (closed_value v_x) as Hclosedv. basic_typing_solver.
    apply state_insert_closed_value with (a:=a) (st:=st) in Hclosedv.
    destruct Hclosedv; mydestr; subst.
    + invclear H6. destruct r; destruct x; inversion H; invclear H9.
    + rewrite H7; auto.
Qed.

Lemma is_arr_simpl_rewrite_value: forall r a st (P: state -> tm -> Prop),
    is_arr r ->
    closed_rty 0 (dom aset st) r ->
    (∀ e_wf : tm, ({0;b∅;st}⟦r⟧) e_wf → ∃ v_wf : value, e_wf ↪* v_wf) ->
    (∃ e_x_hat : tm, ({0;b∅;st}⟦r⟧) e_x_hat
                     ∧ (∀ e_x : value, ({0;b∅;st}⟦r⟧) e_x → ∀ v_x : value, e_x_hat ↪* v_x → P ({a↦v_x} st) e_x)) <->
      (∀ e_x : value, ({0;b∅;st}⟦r⟧) e_x → P st e_x).
Proof.
  split; intros.
  - mydestr. assert (∃ v_x : value, x ↪* v_x); auto. mydestr.
    eapply H4 in H3; eauto.
    assert ([] ⊢t x0 ⋮v ⌊r⌋). refinement_solver7.
    assert (closed_value x0) as Hclosedv. basic_typing_solver.
    apply state_insert_closed_value with (a:=a) (st:=st) in Hclosedv.
    destruct Hclosedv; mydestr; subst.
    + invclear H6. destruct r; destruct x1; inversion H; invclear H9.
    + rewrite <- H7; auto.
  - exists (random_inhabitant r).
    assert ({0;b∅;st}⟦r⟧ (random_inhabitant r)).
    apply random_inhabitant_in_any_under; refinement_solver7.
    split; auto. intros.
    assert ([] ⊢t v_x ⋮v ⌊r⌋) by refinement_solver7.
    assert (closed_value v_x) as Hclosedv. basic_typing_solver.
    apply state_insert_closed_value with (a:=a) (st:=st) in Hclosedv.
    destruct Hclosedv; mydestr; subst.
    + invclear H6. destruct r; destruct x; inversion H; invclear H9.
    + rewrite H7; auto.
Qed.

Lemma is_arr_simpl_rewrite_wf_ctxrR: forall r a st Γ,
    is_arr r ->
    closed_rty 0 (dom aset st) r ->
    (∀ e_wf : tm, ({0;b∅;st}⟦r⟧) e_wf → ∃ v_wf : value, e_wf ↪* v_wf) ->
    (∃ e_x_hat : tm, ({0;b∅;st}⟦r⟧) e_x_hat
                     ∧ (∀ e_x : tm, ({0;b∅;st}⟦r⟧) e_x → ∀ v_x : value, e_x_hat ↪* v_x →
                                                                        wf_ctxrR ({a↦v_x} st) Γ)) <->
      (∀ e_x : tm, ({0;b∅;st}⟦r⟧) e_x → wf_ctxrR st Γ).
Proof.
  intros.
  eapply is_arr_simpl_rewrite with (P:= fun st _ => wf_ctxrR st Γ) in H1; eauto.
Qed.

Lemma is_arr_simpl_rewrite_ctxrR: forall r a st τ Γ e,
    is_arr r ->
    closed_rty 0 (dom aset st) r ->
    (∀ e_wf : tm, ({0;b∅;st}⟦r⟧) e_wf → ∃ v_wf : value, e_wf ↪* v_wf) ->
    (∃ e_x_hat : tm, ({0;b∅;st}⟦r⟧) e_x_hat
                     ∧ (∀ e_x : tm, ({0;b∅;st}⟦r⟧) e_x →
                                    ∀ v_x : value, e_x_hat ↪* v_x →
                                                   ({({a↦v_x}) st}⟦τ⟧{Γ}) (tlete e_x e))) <->
      (∀ e_x : tm, ({0;b∅;st}⟦r⟧) e_x → ({st}⟦τ⟧{Γ}) (tlete e_x e)).
Proof.
  intros.
  eapply is_arr_simpl_rewrite with (P:= fun st e_x => ({st}⟦τ⟧{Γ}) (tlete e_x e)) in H1; eauto.
Qed.

Lemma is_arr_simpl_rewrite_wf_ctxrR_value: forall r a st Γ,
    is_arr r ->
    closed_rty 0 (dom aset st) r ->
    (∀ e_wf : tm, ({0;b∅;st}⟦r⟧) e_wf → ∃ v_wf : value, e_wf ↪* v_wf) ->
    (∃ e_x_hat : tm, ({0;b∅;st}⟦r⟧) e_x_hat
                     ∧ (∀ e_x : value, ({0;b∅;st}⟦r⟧) e_x → ∀ v_x : value, e_x_hat ↪* v_x →
                                                                        wf_ctxrR ({a↦v_x} st) Γ)) <->
      (∀ e_x : value, ({0;b∅;st}⟦r⟧) e_x → wf_ctxrR st Γ).
Proof.
  intros.
  eapply is_arr_simpl_rewrite_value with (P:= fun st _ => wf_ctxrR st Γ) in H1; eauto.
Qed.

Lemma is_arr_simpl_rewrite_ctxrR_value: forall r a st τ Γ e,
    is_arr r ->
    closed_rty 0 (dom aset st) r ->
    (∀ e_wf : tm, ({0;b∅;st}⟦r⟧) e_wf → ∃ v_wf : value, e_wf ↪* v_wf) ->
    (∃ e_x_hat : tm, ({0;b∅;st}⟦r⟧) e_x_hat
                     ∧ (∀ e_x : value, ({0;b∅;st}⟦r⟧) e_x →
                                    ∀ v_x : value, e_x_hat ↪* v_x →
                                                   ({({a↦v_x}) st}⟦τ⟧{Γ}) (tlete e_x e))) <->
      (∀ e_x : value, ({0;b∅;st}⟦r⟧) e_x → ({st}⟦τ⟧{Γ}) (tlete e_x e)).
Proof.
  intros.
  eapply is_arr_simpl_rewrite_value with (P:= fun st e_x => ({st}⟦τ⟧{Γ}) (tlete e_x e)) in H1; eauto.
Qed.

Ltac is_arr_simp :=
  repeat match goal with
    | [H: ∃ e_x_hat, ({0;b∅;?st}⟦?r⟧) e_x_hat /\
                       (∀ e_x, ({0;b∅;?st}⟦?r⟧) e_x -> ∀ v_x, e_x_hat ↪* (tvalue v_x) -> wf_ctxrR _ _) |- _] =>
        rewrite is_arr_simpl_rewrite_wf_ctxrR in H; auto; refinement_solver7
    | [H: ∃ e_x_hat, ({0;b∅;?st}⟦?r⟧) e_x_hat /\
                       (∀ e_x, ({0;b∅;?st}⟦?r⟧) e_x -> ∀ v_x, e_x_hat ↪* (tvalue v_x) -> {({?a↦v_x}) ?st}⟦_⟧{_} _) |- _] =>
        rewrite is_arr_simpl_rewrite_ctxrR in H; auto; refinement_solver7
    | [|- ∃ e_x_hat, ({0;b∅;?st}⟦?r⟧) e_x_hat /\
                      (∀ e_x, ({0;b∅;?st}⟦?r⟧) e_x -> ∀ v_x, e_x_hat ↪* (tvalue v_x) -> {({?a↦v_x}) ?st}⟦_⟧{_} _)] =>
        rewrite is_arr_simpl_rewrite_ctxrR; auto; refinement_solver7

    | [H: ∃ e_x_hat, ({0;b∅;?st}⟦?r⟧) e_x_hat /\
                       (∀ e_x, ({0;b∅;?st}⟦?r⟧) (tvalue e_x) -> ∀ v_x, e_x_hat ↪* (tvalue v_x) -> wf_ctxrR _ _) |- _] =>
        rewrite is_arr_simpl_rewrite_wf_ctxrR_value in H; auto; refinement_solver7
    | [H: ∃ e_x_hat, ({0;b∅;?st}⟦?r⟧) e_x_hat /\
                       (∀ e_x, ({0;b∅;?st}⟦?r⟧) (tvalue e_x) -> ∀ v_x, e_x_hat ↪* (tvalue v_x) -> {({?a↦v_x}) ?st}⟦_⟧{_} _) |- _] =>
        rewrite is_arr_simpl_rewrite_ctxrR_value in H; auto; refinement_solver7
    | [|- ∃ e_x_hat, ({0;b∅;?st}⟦?r⟧) e_x_hat /\
                      (∀ e_x, ({0;b∅;?st}⟦?r⟧) (tvalue e_x) -> ∀ v_x, e_x_hat ↪* (tvalue v_x) -> {({?a↦v_x}) ?st}⟦_⟧{_} _)] =>
        rewrite is_arr_simpl_rewrite_ctxrR_value; auto; refinement_solver7
    end.

Lemma under_not_overbasety: forall x1 x2 x3 x4, not_overbasety [v:x1|x2|x3|x4].
Proof.
  simpl; auto.
Qed.

Global Hint Resolve under_not_overbasety: core.

Lemma rRctx_pre_weakening: forall Γ1 Γ2 st τ,
    not_overbasety τ ->
    wf_ctxrR st (Γ1 ++ Γ2) ->
    (forall e, {st}⟦τ⟧{Γ1} e -> {st}⟦τ⟧{Γ1 ++ Γ2} e).
Proof.
  induction Γ1; intros; RD_simp; denotation_simp.
  - apply rRctx_pre_weakening_empty; auto.
  - invclear H1; invclear H0; auto_ty_exfalso.
    + constructor; refinement_solver. basic_typing_solver7. ctx_erase_simp5.
    + denotation_simp. invclear H9; try auto_ty_exfalso.
      constructor; auto. refinement_solver. ctx_erase_simp5. basic_typing_solver7. ctx_erase_simp5.
      auto_meet_exists Hmeet. auto_meet_reduce.
    + invclear H9; auto_ty_exfalso4. apply ctxrR_cons_under_arr; auto.
      refinement_solver.
      ctx_erase_simp. basic_typing_solver7. listctx_set_simpl. rewrite ok_pre_destruct. split; refinement_solver7.
      is_arr_simp.
Qed.

Lemma rR_implies_reduction_no_free_unfold
     : ∀ (st : state) (r : rty) (e : tm) (v : value), ({0;b∅;st}⟦r⟧) e → e ↪* v → fv_value v ≡ ∅.
Proof.
  intros. eapply rR_implies_reduction_no_free in H; eauto.
Qed.

Global Hint Resolve rR_implies_reduction_no_free_unfold: core.

Lemma rR_implies_no_free_value_unfold
     : ∀ (st : state) (r : rty) (v : value), ({0;b∅;st}⟦r⟧) v → fv_value v ≡ ∅.
Proof.
  intros.
  assert ([] ⊢t v ⋮v ⌊ r ⌋); refinement_solver. basic_typing_solver.
Qed.

Global Hint Resolve rR_implies_no_free_value_unfold: core.

Lemma rR_implies_no_free_value
     : ∀ (st : state) (r : rty) (v : value), ({0;b∅;st}⟦r⟧) v → closed_value v.
Proof.
  intros. unfold closed_value. eapply rR_implies_no_free_value_unfold; eauto.
Qed.

Global Hint Resolve rR_implies_no_free_value: core.

Ltac refinement_solver8 :=
  match goal with
  | [H: ok_dctx _ ((?a, ?b) :: ?c ++ _) |- ok_dctx _ ((?a, ?b) :: ?c) ] =>
      rewrite app_comm_cons in H;
      rewrite ok_dctx_post_destruct in H; mydestr; auto
  | [H: {0;b∅;?st}⟦?r⟧ (tvalue ?v_x) |- _ ∉ fv_value ?v_x ] =>
      assert ([] ⊢t v_x ⋮t ⌊ r ⌋ ) by refinement_solver7; basic_typing_solver
  end || refinement_solver7.

Ltac dsimp1 :=
  match goal with
  | [H: ({0;b∅;_}⟦[v:?b|_|_|_]⟧) ?e, H': ?e ↪* (tvalue ?v) |- _ ] =>
      assert ([] ⊢t v ⋮v b) by refinement_solver; denotation_simp3
  end.

Fixpoint ty_inhabitant (t: ty): value :=
  match t with
  | TBase TNat => (vconst 1)
  | TBase TBool => (vconst true)
  | T1 ⤍ T2 => vlam T1 (ty_inhabitant T2)
  end.

Lemma closed_ty_inhabitant: forall T, fv_value (ty_inhabitant T) ≡ ∅.
Proof.
  induction T; simpl; intros; auto.
  - destruct b; constructor; auto.
Qed.

Global Hint Resolve closed_ty_inhabitant: core.

Lemma lc_ty_inhabitant: forall T, lc (ty_inhabitant T).
Proof.
  induction T; simpl; intros; auto.
  - destruct b; constructor; auto.
  - apply lc_vlam with (L := ∅). intros.
    rewrite open_rec_lc_tm; auto.
Qed.

Global Hint Resolve lc_ty_inhabitant: core.

Lemma ty_inhabitant_typable: forall Γ T, ok Γ -> Γ ⊢t (ty_inhabitant T) ⋮v T.
Proof.
  induction T; simpl; intros; auto.
  - destruct b; constructor; auto.
  - auto_exists_L; intros. rewrite open_rec_lc_tm; auto.
    apply basic_typing_weaken_tm_pre; auto.
Qed.

Global Hint Resolve ty_inhabitant_typable: core.

Lemma cons_basic_strengthen_tm: forall x Tx Γ1 Γ2 e T,
    x ∉ fv_tm e ->
    (Γ1 ++ (x, Tx) :: Γ2) ⊢t e ⋮t T ->
    (Γ1 ++ Γ2) ⊢t e ⋮t T.
Proof.
  intros. apply basic_typing_subst_tm with (u:= ty_inhabitant Tx) in H0; auto.
  - rewrite subst_fresh_tm in H0; auto.
  - apply ty_inhabitant_typable. basic_typing_solver.
Qed.

Lemma cons_basic_strengthen_value: forall x Tx Γ1 Γ2 e T,
    x ∉ fv_value e ->
    (Γ1 ++ (x, Tx) :: Γ2) ⊢t e ⋮v T ->
    (Γ1 ++ Γ2) ⊢t e ⋮v T.
Proof.
  intros. apply basic_typing_subst_value with (u:= ty_inhabitant Tx) in H0; auto.
  - rewrite subst_fresh_value in H0; auto.
  - apply ty_inhabitant_typable. basic_typing_solver.
Qed.

Lemma cons_basic_strengthen2_tm: forall Γ2 Γ1 e T,
    ctxdom Γ2 ∩ fv_tm e ≡ ∅ ->
    (Γ1 ++ Γ2) ⊢t e ⋮t T ->
    Γ1 ⊢t e ⋮t T.
Proof.
  induction Γ2; simpl; intros; auto; mydestr.
  - listctx_set_simpl.
  - apply cons_basic_strengthen_tm in H0; auto. apply IHΓ2; auto. set_solver. set_solver.
Qed.

Lemma cons_basic_strengthen2_value: forall Γ2 Γ1 e T,
    ctxdom Γ2 ∩ fv_value e ≡ ∅ ->
    (Γ1 ++ Γ2) ⊢t e ⋮v T ->
    Γ1 ⊢t e ⋮v T.
Proof.
  induction Γ2; simpl; intros; auto; mydestr.
  - listctx_set_simpl.
  - apply cons_basic_strengthen_value in H0; auto. apply IHΓ2; auto. set_solver. set_solver.
Qed.

Lemma cons_basic_strengthen_pre_cons_tm: forall x Tx Γ1 Γ2 e T,
    ctxdom Γ2 ∩ fv_tm e ≡ ∅ ->
    ((x, Tx) :: Γ1 ++ Γ2) ⊢t e ⋮t T ->
    ((x, Tx) :: Γ1) ⊢t e ⋮t T.
Proof.
  intros. rewrite app_comm_cons in H0. apply cons_basic_strengthen2_tm in H0; auto.
Qed.

Lemma cons_basic_strengthen_pre_cons_value: forall x Tx Γ1 Γ2 e T,
    ctxdom Γ2 ∩ fv_value e ≡ ∅ ->
    ((x, Tx) :: Γ1 ++ Γ2) ⊢t e ⋮v T ->
    ((x, Tx) :: Γ1) ⊢t e ⋮v T.
Proof.
  intros. rewrite app_comm_cons in H0. apply cons_basic_strengthen2_value in H0; auto.
Qed.

Lemma cons_basic_strengthen1_pre_cons_tm: forall x Tx y Ty Γ1 e T,
    y ∉ fv_tm e ->
    ((x, Tx) :: Γ1 ++ [(y, Ty)]) ⊢t e ⋮t T ->
    ((x, Tx) :: Γ1) ⊢t e ⋮t T.
Proof.
  intros. eapply cons_basic_strengthen_pre_cons_tm; eauto. set_solver.
Qed.

Lemma cons_basic_strengthen1_pre_cons_value: forall x Tx y Ty Γ1 e T,
    y ∉ fv_value e ->
    ((x, Tx) :: Γ1 ++ [(y, Ty)]) ⊢t e ⋮v T ->
    ((x, Tx) :: Γ1) ⊢t e ⋮v T.
Proof.
  intros. eapply cons_basic_strengthen_pre_cons_value; eauto. set_solver.
Qed.

Lemma close_rm_fv_tm: forall x e k, x ∉ fv_tm ({k <t~ x} e).
Proof.
  intros x e.
  apply (tm_mutual_rec
           (fun (e: value) => forall k, x ∉ fv_value ({k <v~ x} e))
           (fun (e: tm) => forall k, x ∉ fv_tm ({k <t~ x} e))
        ); simpl; intros; auto; repeat var_dec_solver; set_solver.
Qed.

Lemma close_rm_fv_value: forall x e k, x ∉ fv_value ({k <v~ x} e).
Proof.
  intros x e.
  apply (value_mutual_rec
           (fun (e: value) => forall k, x ∉ fv_value ({k <v~ x} e))
           (fun (e: tm) => forall k, x ∉ fv_tm ({k <t~ x} e))
        ); simpl; intros; auto; repeat var_dec_solver; set_solver.
Qed.

Lemma close_then_subst_same_tm: forall x v_x e,
  ({x := v_x }t (x \t\ e)) = (x \t\ e).
Proof.
  intros. rewrite subst_fresh_tm; auto. apply close_rm_fv_tm.
Qed.

Lemma close_then_subst_same_value: forall x v_x e,
  ({x := v_x }v (x \v\ e)) = (x \v\ e).
Proof.
  intros. rewrite subst_fresh_value; auto. apply close_rm_fv_value.
Qed.

Ltac lc_simpl3 :=
  simpl;
  repeat (match goal with
          | [H: ({0;b∅;_}⟦ _ ⟧) ?e_x, H': context [ {?x := _ }t ?e_x] |- _ ] =>
              assert (x ∉ fv_tm e_x) as Htmp by (eauto; refinement_solver);
              rewrite (subst_fresh_tm e_x x) in H'; auto; try clear Htmp
          | [H: ({0;b∅;_}⟦ _ ⟧) ?e_x |- context [ {?x := _ }t ?e_x] ] =>
              assert (x ∉ fv_tm e_x) as Htmp by (eauto; refinement_solver);
              rewrite (subst_fresh_tm e_x x); auto; try clear Htmp
          | [H: ({0;b∅;_}⟦ _ ⟧) (tvalue ?e_x), H': context [ {?x := _ }v ?e_x] |- _ ] =>
              assert (x ∉ fv_tm e_x) as Htmp by (eauto; refinement_solver);
              rewrite (subst_fresh_value e_x x) in H'; auto; try clear Htmp
          | [H: ({0;b∅;_}⟦ _ ⟧) (tvalue ?e_x) |- context [ {?x := _ }v ?e_x] ] =>
              assert (x ∉ fv_tm e_x) as Htmp by (eauto; refinement_solver);
              rewrite (subst_fresh_value e_x x); auto; try clear Htmp
          | [|- context [ {?x := _ }t (?x \t\ _) ] ] => rewrite close_then_subst_same_tm
          | [H: context [ {?x := _ }t (?x \t\ _) ] |- _ ] => rewrite close_then_subst_same_tm in H
          | [|- context [ {?x := _ }v (?x \v\ _) ] ] => rewrite close_then_subst_same_value
          | [H: context [ {?x := _ }v (?x \v\ _) ] |- _ ] => rewrite close_then_subst_same_value in H
          end).

Lemma tlete_apply_typable_aux: forall Γ (x: atom) e_x e Tx T,
    Γ ⊢t e_x ⋮t Tx -> (Γ ++ [(x, Tx)]) ⊢t e ⋮t T -> Γ ⊢t tlete e_x (x \t\ e) ⋮t T.
Proof.
  intros. auto_exists_L; intros. rewrite subst_as_close_open_tm; basic_typing_solver.
  apply basic_has_type_renaming; auto.
  fast_set_solver. basic_typing_solver.
Qed.

Lemma tlete_apply_typable_aux2: forall Γ (x: atom) e_x e Tx T,
    Γ ⊢t e_x ⋮t Tx -> (Γ ++ [(x, Tx)]) ⊢t e ⋮t T -> (Γ ++ [(x, Tx)]) ⊢t tlete e_x (x \t\ e) ⋮t T.
Proof.
  intros. assert (ok (Γ ++ [(x, Tx)])) by basic_typing_solver.
  eapply tlete_apply_typable_aux in H0; eauto. basic_typing_solver.
Qed.

Lemma tlete_apply_typable_aux3: forall (x: atom) e_x e Tx T,
    [] ⊢t e_x ⋮t Tx -> [(x, Tx)] ⊢t e ⋮t T -> [(x, Tx)] ⊢t tlete e_x (x \t\ e) ⋮t T.
Proof.
  intros. assert (([] ++ [(x, Tx)]) ⊢t tlete e_x (x \t\ e) ⋮t T).
  eapply tlete_apply_typable_aux2; auto. listctx_set_simpl.
Qed.

Lemma tlete_apply_typable_aux4: forall a Ta Γ (x: atom) e_x e Tx T,
    ((a, Ta):: Γ) ⊢t e_x ⋮t Tx -> ((a, Ta) :: Γ ++ [(x, Tx)]) ⊢t e ⋮t T ->
    ((a, Ta):: Γ ++ [(x, Tx)]) ⊢t tlete e_x (x \t\ e) ⋮t T.
Proof.
  intros. rewrite app_comm_cons in H0. rewrite app_comm_cons.
  eapply tlete_apply_typable_aux2; auto.
Qed.

Ltac ctx_rm_simp1 :=
  repeat
    match goal with
    | [H: context [⦑(_, _) :: _⦒] |- _ ] => simpl in H; try dec_solver2
    | [|- context [⦑(_, _) :: _⦒] ] => simpl; try dec_solver2
    | [H: context [⦑ ?a ++ ?b ⦒] |- _ ] => setoid_rewrite (ctx_rm_app a b) in H
    | [|- context [⦑ ?a ++ ?b ⦒] ] => setoid_rewrite (ctx_rm_app a b)
    | [H: context [ctxdom ((_, _) :: _)] |- _] => simpl in H
    | [|- context [ctxdom ((_, _) :: _)]] => simpl
    | [H: context [ctxdom (_ ++ _)] |- _] =>  setoid_rewrite ctxdom_app_union in H
    | [|- context [ctxdom (_ ++ _)]] =>  setoid_rewrite ctxdom_app_union
    end.

Lemma tyable_implies_fresh_tm: forall Γ e T x,
    Γ ⊢t e ⋮t T -> x ∉ (ctxdom Γ) -> x ∉ fv_tm e.
Proof.
  intros.
  apply basic_typing_contains_fv_tm in H. set_solver.
Qed.

Lemma tyable_implies_fresh_value: forall Γ e T x,
    Γ ⊢t e ⋮v T -> x ∉ (ctxdom Γ) -> x ∉ fv_value e.
Proof.
  intros.
  apply basic_typing_contains_fv_value in H. set_solver.
Qed.

Lemma rty_open_perserve_erase: forall τ x k, ⌊{k ~r> x} τ ⌋ = ⌊τ⌋.
Proof.
  induction τ; simpl; intros; auto.
  - rewrite IHτ; auto.
  - rewrite IHτ1; auto. rewrite IHτ2; auto.
Qed.

Lemma open_subst_same_rset: forall x (v: constant) d, x ∉ d -> refinement_set_subst x v ({[x]} ∪ d) = d.
Proof.
  intros. unfold refinement_set_subst. dec_solver2. set_solver.
Qed.

Lemma open_subst_same_r: forall x (v: constant) d k (ϕ: refinement),
    x ∉ d -> not_fv_in_refinement d ϕ ->
      refinement_subst x v ({[x]} ∪ d) (refinement_open k x ϕ) = refinement_open k v ϕ.
Proof.
  intros. unfold refinement_subst. unfold refinement_open. unfold state_subst.
  apply prop_ex3. intros.
  dec_solver2. my_simplify_map_eq3.
  apply H0. intros. my_simplify_map_eq3.
Qed.

Lemma open_subst_same_rty: forall x (v: constant) τ k,
    valid_rty τ ->
    x # (rty_fv τ) -> {x := v }r ({k ~r> x} τ) = {k ~r> v} τ.
Proof.
  intros x v. induction τ; simpl; intros.
  - rewrite open_subst_same_rset; auto. setoid_rewrite open_subst_same_r; auto.
    invclear H. invclear H2; auto.
  - rewrite open_subst_same_rset; auto. setoid_rewrite open_subst_same_r; auto.
    invclear H. invclear H2; auto.
  - rewrite open_subst_same_rset; auto; try fast_set_solver.
    setoid_rewrite open_subst_same_r; auto; try fast_set_solver.
    rewrite IHτ; auto; try fast_set_solver.
    refinement_solver. invclear H. invclear H4; auto.
  - rewrite IHτ1; auto; refinement_solver.
    rewrite IHτ2; auto; refinement_solver.
Qed.

Lemma opened_term_tyable_renaming: forall Γ x Tx e T,
    x ∉ fv_tm e ->
    (Γ ++ [(x, Tx)]) ⊢t e ^t^ x ⋮t T ->
    Γ ⊢t vlam Tx e ⋮v Tx ⤍ T.
Proof.
  intros. auto_exists_L; simpl; intros.
  apply basic_has_type_renaming with (x0:=x0) in H0.
  rewrite open_subst_same_tm in H0; auto.
  fast_set_solver. basic_typing_solver.
Qed.

Lemma rty_open_perserve_not_overbasety: forall τ k v, not_overbasety ({k ~r> v} τ) <-> not_overbasety τ.
Proof.
  induction τ; simpl; intros; auto.
Qed.

Lemma valid_rty_open: forall B n d ϕ (c: constant),
    valid_rty {v:B|S n|d|ϕ} -> valid_rty {v:B|n|d|refinement_open n c ϕ}.
Proof.
  intros. invclear H. invclear H1. constructor; auto. constructor; auto.
  - unfold not_fv_in_refinement in H. unfold not_fv_in_refinement. intros. apply H; auto.
  - unfold bound_in_refinement. unfold bound_in_refinement in H0.
    intros. unfold refinement_open. apply H0.
    unfold bst_eq. intros. unfold bstate_insert. dec_solver1. apply H1. lia.
Qed.

Lemma bst_eq_insert_S: forall m bst bst' c, bst_eq m bst bst' -> bst_eq (S m) (<b[m:=c]> bst) (<b[m:=c]> bst').
Proof.
  unfold bst_eq; intros.
  unfold bstate_insert. repeat var_dec_solver. apply H. lia.
Qed.

Lemma bst_eq_insert_same: forall m n bst bst' c, bst_eq m bst bst' -> bst_eq m (<b[n:=c]> bst) (<b[n:=c]> bst').
Proof.
  unfold bst_eq; intros.
  unfold bstate_insert. repeat var_dec_solver.
Qed.

Lemma bound_in_refinement_btrans: forall n m c ϕ,
    n ≤ S m ->
    bound_in_refinement n ϕ ->
    bound_in_refinement (n `min` m) (λ (bst : bstate) (st0 : state) (v : constant), ϕ (<b[m:=c]> bst) st0 v).
Proof.
  unfold bound_in_refinement; intros.
  destruct (decide (n = S m)); subst.
  - rewrite min_r in H1; try lia. apply H0. apply bst_eq_insert_S; auto.
  - rewrite min_l in H1; try lia. apply H0. apply bst_eq_insert_same; auto.
Qed.

Lemma wf_r_btrans: forall n m c d ϕ,
    n ≤ S m -> wf_r n d ϕ ->
    wf_r (n `min` m) d (λ (bst : bstate) (st0 : state) (v : constant), ϕ (<b[m:=c]> bst) st0 v).
Proof.
  intros. invclear H0. constructor; auto.
  - unfold not_fv_in_refinement. intros. apply H1; auto.
  - apply bound_in_refinement_btrans; auto.
Qed.

Global Hint Resolve wf_r_btrans: core.

Lemma not_overbasety_open_trans: forall τ m c,
    not_overbasety τ -> not_overbasety ({m ~r> c} τ).
Proof.
  induction τ; simpl; intros; auto.
Qed.

Global Hint Resolve not_overbasety_open_trans: core.

Lemma valid_rty_btrans: forall τ m (c: constant),
  lc_rty_idx (S m) τ -> valid_rty τ -> valid_rty ({m ~r> c} τ).
Proof.
  induction τ; simpl; unfold refinement_open; intros.
  - invclear H0; invclear H; constructor; eauto.
  - invclear H0; invclear H; constructor; eauto.
  - invclear H0; invclear H; constructor; eauto.
  - invclear H0; invclear H; constructor; auto; fold rty_open; eauto.
Qed.

Global Hint Resolve valid_rty_btrans: core.

Lemma lc_rty_idx_rty_btrans: forall τ m (c: constant),
  lc_rty_idx (S m) τ -> lc_rty_idx m ({m ~r> c} τ).
Proof.
  induction τ; simpl; intros; invclear H; constructor; auto; lia.
Qed.

Global Hint Resolve lc_rty_idx_rty_btrans: core.

Lemma rty_fv_open_c_eq: forall τ m (c: constant),
    rty_fv ({m ~r> c} τ) ≡ rty_fv τ.
Proof.
  induction τ; simpl; unfold refinement_set_open; intros; auto.
  - rewrite IHτ; auto.
  - rewrite IHτ1; auto. rewrite IHτ2; auto.
Qed.

Lemma closed_rty_open_trans: forall τ m d (c: constant),
    closed_rty (S m) d τ ->
    closed_rty m d ({m ~r> c} τ).
Proof.
  induction τ; intros; invclear H; constructor; auto.
  - rewrite rty_fv_open_c_eq; auto.
  - rewrite rty_fv_open_c_eq; auto.
Qed.

Global Hint Resolve closed_rty_open_trans: core.

Lemma closed_rty_implies_weak_bound_in_refinement_over: forall m L b n d ϕ,
   closed_rty m L {v:b|n|d|ϕ} -> bound_in_refinement m ϕ.
Proof.
  intros. invclear H. invclear H0. invclear H3. invclear H1.
  unfold bound_in_refinement in H0. unfold bound_in_refinement. intros. apply H0.
  eapply bst_eq_trans; eauto.
Qed.

Lemma closed_rty_implies_weak_bound_in_refinement_under: forall m L b n d ϕ,
   closed_rty m L [v:b|n|d|ϕ] -> bound_in_refinement m ϕ.
Proof.
  intros. invclear H. invclear H0. invclear H3. invclear H1.
  unfold bound_in_refinement in H0. unfold bound_in_refinement. intros. apply H0.
  eapply bst_eq_trans; eauto.
Qed.

Lemma closed_rty_destruct_oarr: forall τ m L b n d ϕ,
  closed_rty m L (-:{v: b | n | d | ϕ}⤑ τ) <->
    (closed_rty m L {v: b | n | d | ϕ} /\ closed_rty (S m) L τ /\ not_overbasety τ).
Proof.
  repeat split; intros; refinement_solver.
  - constructor; auto.
  - invclear H. invclear H0. constructor; auto. invclear H2; auto.
Qed.

Lemma closed_rty_destruct_arrarr: forall m L τ1 τ2,
  closed_rty m L (τ1 ⤑ τ2) <->
    (closed_rty m L τ1 /\ closed_rty m L τ2 /\ is_arr τ1 /\ not_overbasety τ2).
Proof.
  repeat split; intros; refinement_solver.
  - invclear H; invclear H0; simpl; auto.
  - destruct τ1; auto_ty_exfalso.
    + constructor; auto; refinement_solver.
    + constructor; auto; refinement_solver.
Qed.

From Coq Require Import Logic.FunctionalExtensionality.

Lemma bstate_insert_push: forall m (c c_x: constant) bst, (<b[S m:=c]> (<b[↦c_x]> bst)) = (<b[↦c_x]> (<b[m:=c]> bst)).
Proof.
  unfold bstate_push; unfold bstate_insert; intros.
  apply functional_extensionality. intros.
  destruct x; repeat dec_solver2. invclear e. exfalso. apply n; auto.
Qed.

Lemma bst_eq_push: forall m (c_x: constant) bst bst',
    bst_eq m bst bst' ->
    bst_eq (S m) (<b[↦c_x]> bst) (<b[↦c_x]> bst').
Proof.
  unfold bst_eq; unfold bstate_push; intros.
  destruct i; auto. apply H. lia.
Qed.

Ltac inv_rd_simpl0 :=
  progress repeat
    (ctx_erase_simp;
     listctx_set_simpl;
     ctx_rm_simp1;
     my_simplify_map_eq3;
     basic_typing_simpl1;
     repeat match goal with
       | [H: context [ ⌊{_ ~r> _} _ ⌋ ] |- _ ] => rewrite rty_open_perserve_erase in H
       | [|- context [ ⌊{_ ~r> _} _ ⌋ ]] => rewrite rty_open_perserve_erase
       | [H: closed_rty _ _ ?τ |- _ ∉ rty_fv ?τ ] => invclear H
       end).

Lemma rR_open_trans': forall τ m bst bst' st (c: constant) e,
    bst_eq (S m) (<b[m:=c]> bst) bst' ->
    (closed_rty (S m) (dom aset st) τ /\ ({m;bst;st}⟦{m ~r> c} τ⟧) e) <-> ({S m;bst';st}⟦τ⟧) e.
Proof.
  induction τ; split; intros; mydestr.
  - invclear H1; mydestr. do 2 (split; auto).
    eexists; split; eauto. split; auto. subst.
    apply closed_rty_implies_weak_bound_in_refinement_over in H0.
    eapply H0 in H. rewrite <- H; eauto.
  - invclear H0; mydestr; subst. do 3 (split; auto).
    + apply closed_rty_open_trans with (c:=c) in H0; auto.
    + eexists; split; eauto. split; auto.
      apply closed_rty_implies_weak_bound_in_refinement_over in H0.
      eapply H0 in H. rewrite <- H in H3; eauto.
  - invclear H1; mydestr. do 2 (split; auto).
    intros. apply H3; auto.
    apply closed_rty_implies_weak_bound_in_refinement_under in H0.
    eapply H0 in H. rewrite <- H in H5; eauto.
  - invclear H0; mydestr. do 3 (split; auto).
    + apply closed_rty_open_trans with (c:=c) in H0; auto.
    + intros. apply H2; auto.
      apply closed_rty_implies_weak_bound_in_refinement_under in H0.
      eapply H0 in H. rewrite <- H; eauto.
  - invclear H1; mydestr. simpl in H2. simpl in H1. inv_rd_simpl0. do 2 (split; auto).
    eexists; split; eauto.
    intros. apply IHτ with (c:=c) (bst:= (<b[↦c_x]> bst)); eauto.
    + rewrite bstate_insert_push. apply bst_eq_push; auto.
    + rewrite closed_rty_destruct_oarr in H0; mydestr. split; auto.
      apply H4; auto.
      apply closed_rty_implies_weak_bound_in_refinement_over in H0.
      eapply H0 in H. rewrite <- H in H6; eauto.
  - invclear H0; mydestr. inv_rd_simpl0. do 3 (split; auto).
    apply closed_rty_open_trans with (c:=c) in H0; auto.
    eexists; split; eauto.
    intros. rewrite closed_rty_destruct_oarr in H0; mydestr.
    assert (ϕ bst' st c_x) as Hz.
    { apply closed_rty_implies_weak_bound_in_refinement_over in H0.
      eapply H0 in H. rewrite <- H; eauto. }
    apply H3 in Hz; auto. rewrite <- IHτ in Hz; mydestr; eauto.
    rewrite bstate_insert_push. apply bst_eq_push; auto.
  - invclear H1; mydestr. simpl in H2. inv_rd_simpl0. do 2 (split; auto).
    eexists; split; eauto.
    intros. rewrite closed_rty_destruct_arrarr in H0; mydestr.
    eapply IHτ2; eauto. split; auto. apply H4. rewrite <- IHτ1 in H5; mydestr; eauto.
  - invclear H0; mydestr. simpl in H1. inv_rd_simpl0. do 3 (split; auto).
    apply closed_rty_open_trans with (c:=c) in H0; auto.
    eexists; split; eauto.
    intros. rewrite closed_rty_destruct_arrarr in H0; mydestr.
    assert (({S m;bst';st}⟦τ1⟧) v_x) as Hz by (rewrite <- IHτ1; eauto).
    apply H3 in Hz. rewrite <- IHτ2 in Hz; mydestr; eauto.
Qed.

Lemma rR_open_trans_empty: forall τ st (c: constant) e,
    closed_rty 1 (dom aset st) τ ->
    ({0;b∅;st}⟦τ ^r^ c⟧) e -> {1;<b[↦c]> b∅;st}⟦τ⟧ e.
Proof.
  intros. rewrite <- rR_open_trans'; eauto.
  unfold bst_eq; intros. destruct i; auto.
Qed.

Lemma ctxfind_get_last: forall a B Γ x τ_x,
    ok ((a, B) :: ⌊Γ ++ [(x, τ_x)]⌋*) ->
    ctxfind ((a, B) :: ⌊Γ ++ [(x, τ_x)]⌋*) x = Some ⌊τ_x⌋.
Proof.
  intros.
  assert (x <> a). ctx_erase_simp.  basic_typing_solver8.
  repeat dec_solver2. ctx_erase_simp. apply ctxfind_last_eq. ctx_erase_simp.
Qed.

Lemma var_last_typable: forall a B Γ x τ_x,
    ok ((a, B) :: ⌊Γ ++ [(x, τ_x)]⌋*) -> ((a, B) :: ⌊Γ ++ [(x, τ_x)]⌋*) ⊢t x ⋮t ⌊τ_x⌋.
Proof.
  intros. constructor; auto. constructor; auto. apply ctxfind_get_last; auto.
Qed.

Lemma closed_rty_mk_eq_var: forall b x, closed_rty 0 ({[x]}) (mk_eq_var b x).
Proof.
  intros. constructor; eauto.
  - do 2 (constructor; auto).
    + unfold not_fv_in_refinement. intros. rewrite H; fast_set_solver.
    + unfold bound_in_refinement. auto.
  - constructor; auto.
Qed.

Lemma reduction_implies_termR: forall e e' T, e ↪* e' -> [] ⊢t e ⋮t T -> e' <-<{ []; T} e.
Proof.
  intros. constructor; auto.
  - eapply multi_preservation; eauto.
  - unfold termRraw. intros Γv. intros. invclear H1. simpl. simpl in H2.
    eapply multistep_trans; eauto.
Qed.
