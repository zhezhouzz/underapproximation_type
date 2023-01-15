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
| ctxrR_cons_over: forall st (x: atom) B n d ϕ Γ,
    ok_dctx (dom _ st) ((x, {v: B | n | d | ϕ}) :: Γ) ->
    (* (exists (c_wf: constant), {st}⟦ {v: B | n | d | ϕ} ⟧ c_wf) -> *)
    (forall (c_x: constant), {st}⟦ {v: B | n | d | ϕ} ⟧ c_x ->
                        wf_ctxrR (<[ x := c_x ]> st) Γ) ->
    wf_ctxrR st ((x, {v: B | n | d | ϕ}) :: Γ)
| ctxrR_cons_under: forall st (x: atom) τ_x Γ,
    not_overbasety τ_x ->
    ok_dctx (dom _ st) ((x, τ_x) :: Γ) ->
    (forall e_wf, {st}⟦ τ_x ⟧ e_wf -> (exists (v_wf: value), e_wf ↪* v_wf)) ->
    (exists e_x_hat, {st}⟦ τ_x ⟧ e_x_hat /\
                  (forall e_x, {st}⟦ τ_x ⟧ e_x ->
                          (∀ (v_x: value), e_x_hat ↪* v_x ->
                                           wf_ctxrR ({ x ↦ v_x } st) Γ ))) ->
     wf_ctxrR st ((x, τ_x) :: Γ).

Lemma wf_ctxrR_iff_wf_ctxrR_not_terr: forall (Γ: listctx rty) st, wf_ctxrR st Γ <-> wf_ctxrR_not_terr st Γ.
Proof.
  induction Γ; split; intros.
  - constructor; auto.
  - constructor; auto.
  - invclear H.
    + constructor; auto. intros. rewrite <- IHΓ; auto.
    + constructor; auto.
      assert (valid_rty τ_x) as Hz by refinement_solver.
      intro Hf. apply H5 in Hf. mydestr. reduction_simpl1.
      mydestr. exists x0. split; auto.
      intros. rewrite <- IHΓ; eauto.
  - invclear H.
    + constructor; auto. intros. rewrite IHΓ; auto.
    + constructor; auto.
      intros. eapply not_rR_inhabitant_err_implies_halt in H5; eauto; refinement_solver.
      mydestr. exists x0. split; auto.
      intros. rewrite IHΓ; eauto.
Qed.

Global Hint Constructors ok_dctx: core.

Lemma rRctx_pre_weakening_empty: forall Γ st τ,
    not_overbasety τ ->
    wf_ctxrR st Γ ->
    (forall e, {st}⟦τ⟧ e -> {st}⟦τ⟧{Γ} e).
Proof.
  induction Γ; intros; RD_simp; denotation_simp.
  invclear H0.
  - invclear H5; auto_ty_exfalso. constructor; refinement_solver7.
    intros. apply IHΓ; auto.
    rewrite subst_fresh_tm.
    rewrite rR_shadow_update_st_c; refinement_solver.
    refinement_solver7. basic_typing_solver7. set_solver.
  - destruct (decide (is_arr r)).
    + apply ctxrR_cons_under_arr; refinement_solver7. mydestr.
      exists x; split; auto. intros. apply IHΓ; eauto.
      rewrite rR_shadow_update_st; refinement_solver.
      eapply termR_perserve_rR; eauto; refinement_solver5.
      eapply termR_tlete_drop_halt_lhs'; refinement_solver7.
    + constructor; refinement_solver7.
      mydestr.
      exists x; split; auto. intros. apply IHΓ; eauto.
      rewrite rR_shadow_update_st; refinement_solver.
      eapply termR_perserve_rR; eauto; refinement_solver5.
      eapply termR_tlete_drop_halt_lhs'; refinement_solver7.
Qed.

Lemma is_arr_implies_not_overbasety: forall r, is_arr r -> not_overbasety r.
Proof.
  intros. destruct r; invclear H; simpl; auto.
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

Ltac auto_meet_exists HE :=
  match goal with
  | [H1: ({0;b∅;?st}⟦[v:?b|?n|?d|?ϕ]⟧) ?x1,
        H2: ({0;b∅;?st}⟦[v:?b|?n|?d|?ϕ]⟧) ?x2,
          H3: ({0;b∅;?st}⟦[v:?b|?n|?d|?ϕ]⟧) ?x3 |- ∃ e, {0;b∅;?st}⟦[v:?b|?n|?d|?ϕ]⟧ e /\ _ ] =>
      exists ((x1 ⊗{ b } x2) ⊗{ b } x3) ; assert ( {0;b∅;st}⟦[v:b|n|d|ϕ]⟧ ((x1 ⊗{ b } x2) ⊗{ b } x3)) as HE by (eapply rR_tmeet3_when_all; auto); split; auto; intros
  | [H1: ({0;b∅;?st}⟦[v:?b|?n|?d|?ϕ]⟧) ?x1,
        H2: ({0;b∅;?st}⟦[v:?b|?n|?d|?ϕ]⟧) ?x2 |- ∃ e, {0;b∅;?st}⟦[v:?b|?n|?d|?ϕ]⟧ e /\ _ ] =>
      exists (x1 ⊗{ b } x2); assert ( {0;b∅;st}⟦[v:b|n|d|ϕ]⟧ (x1 ⊗{ b } x2)) as HE by (eapply rR_tmeet_when_both; eauto); split; auto; intros
  end.

Ltac auto_meet_reduce :=
  match goal with
  | [H: ((?x1 ⊗{ ?b } ?x2) ⊗{ ?b } ?x3) ↪* ?v |- _ ] =>
      rewrite reduction_tmeet3_iff_all in H; refinement_solver7; mydestr; eauto
  | [H: (?x1 ⊗{ ?b} ?x2) ↪* ?v |- _ ] =>
      rewrite reduction_tmeet_iff_both in H; refinement_solver7; mydestr; eauto
  end.

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

Lemma rRctx_pre_weakening: forall Γ1 Γ2 st τ,
    not_overbasety τ ->
    wf_ctxrR st (Γ1 ++ Γ2) ->
    (forall e, {st}⟦τ⟧{Γ1} e -> {st}⟦τ⟧{Γ1 ++ Γ2} e).
Proof.
  induction Γ1; intros; RD_simp; denotation_simp.
  - apply rRctx_pre_weakening_empty; auto.
  - invclear H1; invclear H0; try auto_ty_exfalso2.
    + constructor; refinement_solver. basic_typing_solver7. ctx_erase_simp5.
    + denotation_simp. invclear H9; try auto_ty_exfalso4.
      constructor; auto. refinement_solver. ctx_erase_simp5. basic_typing_solver7. ctx_erase_simp5.
      auto_meet_exists Hmeet2. auto_meet_reduce.
    + invclear H9; try auto_ty_exfalso4. apply ctxrR_cons_under_arr; auto.
      denotation_simp. refinement_solver.
      ctx_erase_simp. basic_typing_solver7. refinement_solver7.
      is_arr_simp.
Qed.

(* Ltac auto_under_specialize He := *)
(*   repeat match goal with *)
(*     | [H: forall e_x, _ e_x -> _ |- _ ] => specialize (H _ He) *)
(*     | [H: forall c_x, _ (tvalue (vconst c_x)) -> _ |- _ ] => specialize (H _ He) *)
(*     end. *)

Ltac auto_under_specialize e :=
  repeat match goal with
    | [He: ?P e, H: forall e_x, ?P e_x -> _ |- _ ] => specialize (H _ He)
    | [He: ?P (tvalue e), H: forall e_x, _ (tvalue e_x) -> _ |- _ ] => specialize (H _ He)
    | [He: ?P (tvalue (vconst e)), H: forall c_x, _ (tvalue (vconst c_x)) -> _ |- _ ] => specialize (H _ He)
    end.

Ltac auto_under_reduction_specialize :=
  repeat match goal with
      | [H: forall v, ?e ↪* (tvalue _) → _, H': ?e ↪* (tvalue _) |- _ ] => specialize (H  _ H')
      end.

Ltac auto_under e :=
  auto_under_specialize e; mydestr; auto_under_reduction_specialize.

Lemma wf_implies_ctxrR_drop_last: forall Γ st x τ_x e τ,
    wf_ctxrR st (Γ ++ [(x, τ_x)]) ->
    closed_rty 0 (ctxdom ⦑ Γ ⦒ ∪ dom aset st) τ ->
    x ∉ rty_fv τ -> x ∉ fv_tm e -> not_overbasety τ_x ->
    {st}⟦τ⟧{Γ ++ [(x, τ_x)] } e ->
    {st}⟦τ⟧{Γ} e.
Proof.
  induction Γ; intros.
  - invclear H; invclear H4; try auto_ty_exfalso2.
    + auto_ty_exfalso5. auto_under x0. RD_simp.
      rewrite rR_shadow_update_st in H9; auto. admit. refinement_solver.
    + is_arr_simp. RD_simp.
      assert ({0;b∅;st}⟦τ_x⟧ (random_inhabitant τ_x)) as Hrandom.
      { apply random_inhabitant_in_any_under; refinement_solver7. }
      auto_under Hrandom.
      admit.
  - mydestr. invclear H; invclear H4; try auto_ty_exfalso2.
    + constructor; auto. admit. admit. intros. auto_under c_x.
      apply IHΓ in H18; auto. denotation_simp. refinement_solver. admit.
    + auto_ty_exfalso5. constructor; auto. admit. admit.
      auto_meet_exists Hmeet2. auto_meet_reduce. auto_under e_x.
      apply IHΓ in H4; auto. admit. admit.
Admitted.

(*     wf_ctxrR st (Γ ++ [(x, τ_x)]) -> *)
(*     closed_rty 0 (ctxdom ⦑ Γ ⦒ ∪ dom aset st) τ -> *)
(*     x ∉ rty_fv τ -> x ∉ fv_tm e -> not_overbasety τ_x -> not_overbasety τ -> *)
(*     {st}⟦τ⟧{Γ ++ [(x, τ_x)] } e -> *)
(*     {st}⟦τ⟧{Γ} e. *)

Definition ctxrR_appliable (st: state) (Γ : listctx rty) (e_x: tm) (τ_x: rty) :=
  (forall x τ e, wf_ctxrR st (Γ ++ [(x, τ_x)]) ->
            {st}⟦τ_x⟧{Γ} e_x ->
            {st}⟦τ⟧{Γ ++ [(x, τ_x)] } e -> {st}⟦τ⟧{Γ ++ [(x, τ_x)] } (tlete e_x (x \t\ e))).

Lemma wf_implies_ctxrR_tlete: forall Γ st (e_x: value) b n d ϕ,
    ctxrR_appliable st Γ e_x [v:b|n|d|ϕ].
Proof.
  unfold ctxrR_appliable; induction Γ; intros; mydestr; RD_simp; denotation_simp.
  - invclear H; invclear H1; try auto_ty_exfalso; auto. invclear H1.
    constructor; auto. admit.
    auto_meet_exists Hmeet3. auto_meet_reduce. auto_under e_x0. RD_simp. simpl. admit.
  - invclear H; invclear H0; invclear H1; try auto_ty_exfalso2; auto.
    + constructor; auto. admit. intros. auto_under c_x.
      eapply IHΓ in H18; eauto. fold value_subst in H18. simpl. admit.
    + auto_ty_exfalso5. constructor; auto. admit.
      auto_meet_exists Hmeet3. auto_meet_reduce. auto_under e_x0.
      eapply IHΓ in H0; eauto.
Admitted.

Lemma wf_implies_ctxrR_tlete_arr: forall Γ st (e_x: value) τ,
    is_arr τ -> ctxrR_appliable st Γ e_x τ.
Proof.
  unfold ctxrR_appliable; induction Γ; intros; mydestr; RD_simp; denotation_simp.
  - invclear H0; invclear H2; try auto_ty_exfalso; auto.
    apply ctxrR_cons_under_arr; auto. admit.
    is_arr_simp. intros. auto_under e_x0. RD_simp. admit.
  - invclear H0; invclear H1; invclear H2; try auto_ty_exfalso2; auto.
    + constructor; auto. admit. intros. auto_under c_x.
      eapply IHΓ in H19; eauto. fold value_subst in H19.
      admit.
    + auto_ty_exfalso5. constructor; auto. admit.
      auto_meet_exists Hmeet3. auto_meet_reduce. auto_under e_x0.
      apply IHΓ with (e_x:=vlam x1 (mk_app (tlete e_x0 (a \t\ e_x)) 0)) in H1; eauto. admit. admit. admit.
    + auto_ty_exfalso.
    + auto_ty_exfalso.
    + apply ctxrR_cons_under_arr; auto. admit.
      is_arr_simp. intros. auto_under e_x0.
      apply IHΓ with (e_x:=e_x) in H21; eauto.
      admit. admit. admit.
Admitted.

Definition ctxrR_appliable (st: state) (Γ : listctx rty) (e_x: tm) (τ_x: rty) :=
  (forall x τ e, wf_ctxrR st (Γ ++ [(x, τ_x)]) ->
            {st}⟦τ_x⟧{Γ} e_x ->
            {st}⟦τ⟧{Γ ++ [(x, τ_x)] } e -> {st}⟦τ⟧{Γ ++ [(x, τ_x)] } (tlete e_x (x \t\ e))).

Lemma wf_implies_ctxrR_tlete: forall Γ st (e_x: value) τ,
    is_arr τ -> wf_ctxrR st Γ -> ctxrR_appliable st Γ e_x τ.
Proof.
  unfold ctxrR_appliable; induction Γ; intros; mydestr; RD_simp; denotation_simp.
  - invclear H1; invclear H3; try auto_ty_exfalso; auto.
    apply ctxrR_cons_under_arr; auto. admit.
    is_arr_simp. intros. auto_under e_x0. RD_simp. admit.
  - invclear H1; invclear H2; invclear H3; try auto_ty_exfalso2; auto.
    + constructor; auto. admit. intros. auto_under c_x.
      eapply IHΓ in H20; eauto. fold value_subst in H20.
      admit. admit.
    + auto_ty_exfalso5. constructor; auto. admit.
      auto_meet_exists Hmeet3. auto_meet_reduce. auto_under e_x0.
      apply IHΓ with (e_x:=e_x) in H2; eauto. admit. admit. admit.
    + auto_ty_exfalso.
    + auto_ty_exfalso.
    + apply ctxrR_cons_under_arr; auto. admit.
      is_arr_simp. intros. auto_under e_x0.
      apply IHΓ with (e_x:=e_x) in H21; eauto.
      admit. admit. admit.
Admitted.

(* Lemma wf_implies_ctxrR_drop_last: forall Γ st x τ_x e τ, *)
(*     wf_ctxrR st (Γ ++ [(x, τ_x)]) -> *)
(*     closed_rty 0 (ctxdom ⦑ Γ ⦒ ∪ dom aset st) τ -> *)
(*     x ∉ rty_fv τ -> x ∉ fv_tm e -> not_overbasety τ_x -> not_overbasety τ -> *)
(*     {st}⟦τ⟧{Γ ++ [(x, τ_x)] } e -> *)
(*     {st}⟦τ⟧{Γ} e. *)
(* Proof. *)
(*   induction Γ; intros. *)
(*   - invclear H; invclear H5; try auto_ty_exfalso. *)
(*     + specialize (H12 _ H). mydestr. specialize (H5 _ H _ H6). RD_simp. *)
(*       rewrite rR_shadow_update_st in H8; auto. admit. refinement_solver. *)
(*     + is_arr_simp. *)
(*       assert ({0;b∅;st}⟦τ_x⟧ (random_inhabitant τ_x)) as Hrandom. *)
(*       { apply random_inhabitant_in_any_under; refinement_solver7. } *)
(*       admit. *)
(*   - mydestr. invclear H; invclear H5; try auto_ty_exfalso2. *)
(*     + constructor; auto. admit. admit. intros. *)
(*       specialize (H11 _ H). specialize (H19 _ H). *)
(*       apply IHΓ in H19; auto. denotation_simp. refinement_solver. admit. *)
(*     + auto_ty_exfalso5. constructor; auto. admit. admit. *)
(*       auto_meet_exists Hmeet2. auto_meet_reduce. *)
(*       specialize (H8 _ H6 _ H10). specialize (H5 _ H6 _ H13). *)
(*       apply IHΓ  *)in H5; auto. admit. admit.
Admitted.


Lemma wf_implies_ctxrR_tlete: forall Γ st x τ_x e_x e τ,
    not_overbasety τ -> not_overbasety τ_x ->
    wf_ctxrR st (Γ ++ [(x, τ_x)]) ->
    {st}⟦mk_eq_var ty⟧{Γ ++ [(x, τ_x)] } e_x ->
    {st}⟦τ⟧{Γ ++ [(x, τ_x)] } e ->
    {st}⟦τ⟧{Γ ++ [(x, τ_x)] } (tlete e_x e).
Proof.
  induction Γ; intros; mydestr; RD_simp; denotation_simp.
  - invclear H3. invclear H0.
    + mydestr. invclear H1; try auto_ty_exfalso.
      denotation_simp. constructor; auto. admit.
      exists x0. split; auto. intros. eapply H3 in H6; eauto. RD_simp; auto. admit.
    + invclear H1. auto_ty_exfalso.
      apply ctxrR_cons_under_arr; auto. admit.
      is_arr_simp. intros. apply H14 in H1. RD_simp; auto. admit.
  - invclear H2; invclear H3; try auto_ty_exfalso4.
    + invclear H1; try auto_ty_exfalso4; ctx_erase_simp.
      constructor; auto. admit.
      intros.
      specialize (H13 _ H1). specialize (H19 _ H1). specialize (H14 _ H1).
      eapply IHΓ in H19; eauto.
    + denotation_simp. invclear H1. constructor; auto. denotation_simp. refinement_solver. admit.
      mydestr.
      exists ((x0 ⊗{ x6 } x1) ⊗{ x6 } x2).
      split. apply rR_tmeet3_when_all; auto.
      intros. rewrite reduction_tmeet3_iff_all in H13; refinement_solver7; mydestr.
      assert (wf_ctxrR (({a↦v_x}) st) (Γ ++ [(x, τ_x)])) as Hwf by eauto.
      assert ({({a↦v_x}) st}⟦τ_x⟧{Γ} (tlete e_x0 (a \t\ e_x))) as Hex by eauto.
      assert (({({a↦v_x}) st}⟦τ⟧{Γ ++ [(x, τ_x)] }) (tlete e_x0 (a \t\ e))) as He by eauto.
      assert ([] ⊢t v_x ⋮v x6). refinement_solver7. denotation_simp.
      eapply IHΓ in He; eauto.
      eapply termR_perserve_ctxrR; refinement_solver7.
      eapply termR_tlete_commute_tlete; eauto; refinement_solver.
      basic_typing_solver8.

      admit. denotation_simp. refinement_solver7.
    + apply ctxrR_cons_under_arr; auto. denotation_simp. eapply let_store_typable; eauto.
      is_arr_simp. intros.
      specialize (H23 _ H2). specialize (H19 _ H2). specialize (H13 _ H2).
      (* assert ([] ⊢t e_x ⋮t ⌊τ_x⌋) as Hxx by refinement_solver. *)
      eapply IHΓ in H23; eauto. eauto.
      assert (({st}⟦τ⟧{Γ}) (tlete e_x (x \t\ (tlete e_x0 (a \t\ e))))). {  eapply IHΓ in H23; eauto.
      eapply IHΓ in H23; eauto.
      eapply termR_perserve_ctxrR; refinement_solver7.
      eapply termR_tlete_commute_tlete; eauto; refinement_solver.
      basic_typing_solver8. admit.
      denotation_simp.
Admitted.



Lemma wf_implies_ctxrR_tlete: forall Γ st x τ_x e_x e τ,
    not_overbasety τ -> not_overbasety τ_x ->
    wf_ctxrR st (Γ ++ [(x, τ_x)]) ->
    {st}⟦τ_x⟧{Γ} e_x ->
    {st}⟦τ⟧{Γ ++ [(x, τ_x)] } e ->
    {st}⟦τ⟧{Γ ++ [(x, τ_x)] } (tlete e_x e).
Proof.
  induction Γ; intros; mydestr; RD_simp; denotation_simp.
  - invclear H3. invclear H0.
    + mydestr. invclear H1; try auto_ty_exfalso.
      denotation_simp. constructor; auto. admit.
      exists x0. split; auto. intros. eapply H3 in H6; eauto. RD_simp; auto. admit.
    + invclear H1. auto_ty_exfalso.
      apply ctxrR_cons_under_arr; auto. admit.
      is_arr_simp. intros. apply H14 in H1. RD_simp; auto. admit.
  - invclear H2; invclear H3; try auto_ty_exfalso4.
    + invclear H1; try auto_ty_exfalso4; ctx_erase_simp.
      constructor; auto. admit.
      intros.
      specialize (H13 _ H1). specialize (H19 _ H1). specialize (H14 _ H1).
      eapply IHΓ in H19; eauto.
    + denotation_simp. invclear H1. constructor; auto. denotation_simp. refinement_solver. admit.
      mydestr.
      exists ((x0 ⊗{ x6 } x1) ⊗{ x6 } x2).
      split. apply rR_tmeet3_when_all; auto.
      intros. rewrite reduction_tmeet3_iff_all in H13; refinement_solver7; mydestr.
      assert (wf_ctxrR (({a↦v_x}) st) (Γ ++ [(x, τ_x)])) as Hwf by eauto.
      assert ({({a↦v_x}) st}⟦τ_x⟧{Γ} (tlete e_x0 (a \t\ e_x))) as Hex by eauto.
      assert (({({a↦v_x}) st}⟦τ⟧{Γ ++ [(x, τ_x)] }) (tlete e_x0 (a \t\ e))) as He by eauto.
      assert ([] ⊢t v_x ⋮v x6). refinement_solver7. denotation_simp.
      eapply IHΓ in He; eauto.
      eapply termR_perserve_ctxrR; refinement_solver7.
      eapply termR_tlete_commute_tlete; eauto; refinement_solver.
      basic_typing_solver8.

      admit. denotation_simp. refinement_solver7.
    + apply ctxrR_cons_under_arr; auto. denotation_simp. eapply let_store_typable; eauto.
      is_arr_simp. intros.
      specialize (H23 _ H2). specialize (H19 _ H2). specialize (H13 _ H2).
      (* assert ([] ⊢t e_x ⋮t ⌊τ_x⌋) as Hxx by refinement_solver. *)
      eapply IHΓ in H23; eauto. eauto.
      assert (({st}⟦τ⟧{Γ}) (tlete e_x (x \t\ (tlete e_x0 (a \t\ e))))). {  eapply IHΓ in H23; eauto.
      eapply IHΓ in H23; eauto.
      eapply termR_perserve_ctxrR; refinement_solver7.
      eapply termR_tlete_commute_tlete; eauto; refinement_solver.
      basic_typing_solver8. admit.
      denotation_simp.
Admitted.

Lemma wf_implies_ctxrR_drop_last: forall Γ st x τ_x e_x e τ,
    not_overbasety τ -> not_overbasety τ_x ->
    closed_rty 0 (ctxdom ⦑Γ⦒ ∪ dom _ st) τ ->
    wf_ctxrR st (Γ ++ [(x, τ_x)]) ->
    x ∉ stale τ ->
    {st}⟦τ_x⟧{Γ} e_x ->
    {st}⟦τ⟧{Γ ++ [(x, τ_x)] } e ->
    {st}⟦τ⟧{Γ} (tlete e_x (x \t\ e)).
Proof.
  induction Γ; intros; mydestr; RD_simp; denotation_simp.
  - invclear H2; invclear H5; try auto_ty_exfalso4.
    + denotation_simp. apply H12 in H2; mydestr. eapply H4 in H6; eauto. RD_simp.
      rewrite rR_shadow_update_st in H10; auto.
      refinement_solver.
    + is_arr_simp. apply H19 in H6. RD_simp; auto.
  - invclear H2; invclear H4; invclear H5; try auto_ty_exfalso4; ctx_erase_simp.
    + invclear H9; auto_ty_exfalso. constructor; auto. eapply let_store_typable; eauto.
      intros.
      assert (wf_ctxrR (<[a:=c_x]> st) (Γ ++ [(x, τ_x)])) by auto.
      assert (({<[a:=c_x]> st}⟦τ_x⟧{Γ}) ({a := c_x }t e_x)) by auto.
      apply H22 in H2.
      eapply IHΓ in H2; eauto. simpl.
      rewrite subst_close_tm in H2; auto; refinement_solver7. refinement_solver.
    + constructor; auto. denotation_simp. eapply let_store_typable; eauto.
      mydestr; auto_ty_exfalso.
      exists ((x0 ⊗{ x3 } x1) ⊗{ x3 } x2).
      split. apply rR_tmeet3_when_all; auto.
      intros. rewrite reduction_tmeet3_iff_all in H15; refinement_solver7; mydestr.
      assert (wf_ctxrR (({a↦v_x}) st) (Γ ++ [(x, τ_x)])) as Hwf by eauto.
      assert ({({a↦v_x}) st}⟦τ_x⟧{Γ} (tlete e_x0 (a \t\ e_x))) as Hex by eauto.
      assert (({({a↦v_x}) st}⟦τ⟧{Γ ++ [(x, τ_x)] }) (tlete e_x0 (a \t\ e))) as He by eauto.
      assert ([] ⊢t v_x ⋮v x3). refinement_solver7. denotation_simp.
      eapply IHΓ in He; eauto.
      eapply termR_perserve_ctxrR; refinement_solver7.
      eapply termR_tlete_commute_tlete; eauto; refinement_solver.
      basic_typing_solver8.

      admit. denotation_simp. refinement_solver7.
    + apply ctxrR_cons_under_arr; auto. denotation_simp. eapply let_store_typable; eauto.
      is_arr_simp. intros.
      specialize (H23 _ H2). specialize (H19 _ H2). specialize (H13 _ H2).
      (* assert ([] ⊢t e_x ⋮t ⌊τ_x⌋) as Hxx by refinement_solver. *)
      eapply IHΓ in H23; eauto. eauto.
      assert (({st}⟦τ⟧{Γ}) (tlete e_x (x \t\ (tlete e_x0 (a \t\ e))))). {  eapply IHΓ in H23; eauto.
      eapply IHΓ in H23; eauto.
      eapply termR_perserve_ctxrR; refinement_solver7.
      eapply termR_tlete_commute_tlete; eauto; refinement_solver.
      basic_typing_solver8. admit.
      denotation_simp.
Admitted.
