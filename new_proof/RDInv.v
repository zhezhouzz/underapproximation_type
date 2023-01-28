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

Global Hint Resolve mk_eq_constant_is_not_overbasety: core.
Global Hint Resolve mk_eq_var_is_not_overbasety: core.
Global Hint Resolve is_arr_implies_not_overbasety: core.
Global Hint Constructors ok_dctx: core.
Global Hint Resolve rR_implies_no_free: core.
Global Hint Resolve ctxrR_tlete_drop_halt_lhs: core.
Global Hint Resolve rR_implies_reduction_no_free: core.
Global Hint Resolve is_arr_implies_not_overbasety: core.
Global Hint Resolve under_not_overbasety: core.

Lemma wf_ctxrR_implies_ok: forall Γ, wf_ctxrR ∅ Γ -> ok Γ.
Proof.
  induction Γ; intros; auto.
  invclear H; refinement_solver7.
Qed.

Global Hint Resolve wf_ctxrR_implies_ok: core.

Inductive inv_ctxrR: state -> listctx rty -> rty -> tm -> Prop :=
| inv_ctxrR_nil: forall st τ e, { st }⟦ τ ⟧ e -> inv_ctxrR st [] τ e
| inv_ctxrR_cons_over: forall st (x: atom) B n d ϕ Γ τ (e: tm),
    closed_rty 0 ({[x]} ∪ ctxdom ⦑Γ⦒ ∪ (dom _ st)) τ ->
    ok_dctx (dom _ st) ((x, {v: B | n | d | ϕ}) :: Γ) ->
    ((x, TBase B) :: (⌊Γ⌋*)) ⊢t e ⋮t ⌊τ⌋ ->
     (forall (c_x: constant), {st}⟦ {v: B | n | d | ϕ} ⟧ c_x ->
                         inv_ctxrR (<[ x := c_x ]> st) Γ τ ({x := c_x}t e)) ->
     inv_ctxrR st ((x, {v: B | n | d | ϕ}) :: Γ) τ e
| inv_ctxrR_cons_under_base: forall st (x: atom) b n d ϕ τ Γ e,
    closed_rty 0 ({[x]} ∪ ctxdom ⦑Γ⦒ ∪ (dom _ st)) τ ->
    ok_dctx (dom _ st) ((x, [v: b | n | d | ϕ]) :: Γ) ->
    ((x, TBase b ) :: (⌊Γ⌋*)) ⊢t e ⋮t ⌊τ⌋ ->
    (* (forall e_wf, {st}⟦ [v: b | n | d | ϕ] ⟧ e_wf -> (exists (v_wf: value), e_wf ↪* v_wf)) -> *)
     (exists e_x_hat, {st}⟦ [v: b | n | d | ϕ] ⟧ e_x_hat /\
                   (∀ (v_x: value), e_x_hat ↪* v_x ->
                                       inv_ctxrR ({ x ↦ v_x } st) Γ τ ({x := v_x}t e))) ->
     inv_ctxrR st ((x, [v: b | n | d | ϕ]) :: Γ) τ e
| inv_ctxrR_cons_under_arr: forall st (x: atom) τ_x τ Γ e,
    is_arr τ_x ->
    closed_rty 0 (ctxdom ⦑Γ⦒ ∪ (dom _ st)) τ ->
    ok_dctx (dom _ st) ((x, τ_x) :: Γ) ->
    ((x, ⌊τ_x⌋ ) :: (⌊Γ⌋*)) ⊢t e ⋮t ⌊τ⌋ ->
     (forall (v_x: value), {st}⟦ τ_x ⟧ v_x -> inv_ctxrR st Γ τ ({x := v_x}t e)) ->
     inv_ctxrR st ((x, τ_x) :: Γ) τ e.

Notation " '⅋{' st '}⟦' τ '⟧{' Γ '}' " := (inv_ctxrR st Γ τ) (at level 20, format "⅋{ st }⟦ τ ⟧{ Γ }", st constr, τ constr, Γ constr).

Ltac RD_simp2 :=
  repeat
    (listctx_set_simpl;
     match goal with
     | [H: (⅋{_}⟦ _ ⟧{ [] }) _ |- _ ] => invclear H
     | [|- (⅋{_}⟦ _ ⟧{ [] }) _ ] => constructor
     | [H: (⅋{_}⟦_⟧{ (_, _) :: _ }) _ |- _ ] => invclear H; try auto_ty_exfalso
     | [H: wf_ctxrR _ ((_,_) :: _ ) |- _ ] => invclear H; try auto_ty_exfalso
     end).

Ltac inv_simpl :=
  repeat match goal with
    | [H: ?x ∉ fv_tm ?e, H': context [({?x := _ }t ?e)] |- _ ] =>
        setoid_rewrite (subst_fresh_tm e x) in H'; auto
    | [H: ?x ∉ fv_tm ?e |- context [({?x := _ }t ?e)] ] =>
        setoid_rewrite (subst_fresh_tm e x); auto
    | [H: ?x ∉ rty_fv ?r, H': context [{0;b∅;({?x↦_}) _}⟦_⟧ _] |- _ ] =>
        rewrite rR_shadow_update_st in H'; refinement_solver
    | [H: ?x ∉ rty_fv ?r |- context [{0;b∅;({?x↦_}) _}⟦_⟧ _] ] =>
        rewrite rR_shadow_update_st; refinement_solver
    end.

Definition riv (τ: rty): value :=
  match τ with
  | {v: _ | _ | _ | _ } => 0
  | [v: _ | _ | _ | _ ] => 0
  | -:{v: T1 | _ | _ | _ } ⤑ τ => vlam T1 (random_inhabitant τ)
  | τ1 ⤑ τ2 => vlam (rty_erase τ1) (random_inhabitant τ2)
  end.

Lemma riv_in_any_under: ∀ (τ : rty) (bst : bstate) (n : nat) (st : state),
    is_arr τ → closed_rty n (dom aset st) τ → ({n;bst;st}⟦τ⟧) (riv τ).
Proof.
  intros. destruct τ; auto_ty_exfalso.
  - apply random_inhabitant_in_any_under; refinement_solver7.
  - apply random_inhabitant_in_any_under; refinement_solver7.
Qed.

Ltac lc_solver_r :=
  match goal with
  | [H: ?x ∉ fv_tm ?e |- ?x ∉ fv_tm ({_ := _ }t ?e)] =>
      rewrite fv_of_subst_tm_closed; refinement_solver
  end.

Lemma inv_implies_ctxrR_drop_last: forall Γ st x τ_x e τ,
    wf_ctxrR st (Γ ++ [(x, τ_x)]) ->
    closed_rty 0 (ctxdom ⦑ Γ ⦒ ∪ dom aset st) τ ->
    x ∉ rty_fv τ -> x ∉ fv_tm e -> not_overbasety τ_x ->
    ⅋{st}⟦τ⟧{Γ ++ [(x, τ_x)] } e ->
    ⅋{st}⟦τ⟧{Γ} e.
Proof.
  induction Γ; intros.
  - RD_simp2; mydestr.
    + auto_under x1. RD_simp2. inv_simpl.
    + assert ({0;b∅;st}⟦ τ_x ⟧ (riv τ_x)) by (apply riv_in_any_under; refinement_solver7).
      assert ((riv τ_x) ↪* (riv τ_x)). admit.
      (* auto_under (riv τ_x). *)
      admit.
  - RD_simp2.
    + inversion H10; auto_ty_exfalso4; subst. constructor; auto. refinement_solver8.
      denotation_simp. eapply cons_basic_strengthen1_pre_cons_tm; eauto.
      intros. auto_under c_x.
      apply IHΓ in H14; auto. refinement_solver. lc_solver_r.
    + inversion H10; auto_ty_exfalso4; subst. mydestr. constructor; auto. refinement_solver8.
      denotation_simp. eapply cons_basic_strengthen1_pre_cons_tm; eauto.
      auto_meet_exists Hmeet. auto_meet_reduce. auto_under x0.
      apply IHΓ in H11; auto. dsimp1. refinement_solver.
      lc_solver_r.
    + inversion H11; auto_ty_exfalso4; subst. denotation_simp. constructor; auto. refinement_solver8.
      denotation_simp. eapply cons_basic_strengthen1_pre_cons_tm; eauto.
      intros. auto_under v_x.
      apply IHΓ in H15; auto. lc_solver_r.
Admitted.

Lemma wf_implies_ctxrR_tlete_is_arr: forall Γ st x τ_x (e_x: value) e τ,
    wf_ctxrR st (Γ ++ [(x, τ_x)]) ->
    not_overbasety τ -> is_arr τ_x ->
    ⅋{st}⟦τ_x⟧{Γ} e_x ->
    ⅋{st}⟦τ⟧{Γ ++ [(x, τ_x)] } e -> ⅋{st}⟦τ⟧{Γ ++ [(x, τ_x)] } (tlete e_x (x \t\ e)).
Proof.
  induction Γ; intros; RD_simp2.
  - constructor; auto. denotation_simp. eapply tlete_apply_typable_aux3; eauto; refinement_solver8.
    intros. auto_under e_x. RD_simp2.
    lc_simpl3. eapply termR_perserve_rR; eauto; refinement_solver.
    (* admit. *)
    apply termR_let_one_step_from_basic_type'. basic_typing_solver. fast_set_solver.
    eapply tlete_apply_typable_aux; eauto; refinement_solver.
  - constructor; auto. denotation_simp. eapply tlete_apply_typable_aux4; eauto; refinement_solver8.
    intros. auto_under c_x.
    eapply IHΓ in H13; eauto. fold value_subst in H13. lc_simpl3.
    rewrite <- subst_close_tm; auto. refinement_solver. denotation_simp. basic_typing_solver8.
  - constructor; auto. denotation_simp. eapply tlete_apply_typable_aux4; eauto; refinement_solver8.
    mydestr. auto_meet_exists Hmeet. auto_meet_reduce.
    auto_under ({a := v_x }t e_x). eapply IHΓ in H4; eauto. fold value_subst in H4. lc_simpl3.
    rewrite <- subst_close_tm; auto. dsimp1. denotation_simp. basic_typing_solver8.
  - constructor; auto. denotation_simp. eapply tlete_apply_typable_aux4; eauto; refinement_solver8.
    intros. auto_under v_x.
    eapply IHΓ in H18; eauto. fold value_subst in H18. lc_simpl3.
    rewrite <- subst_close_tm; auto. refinement_solver8.
    denotation_simp. basic_typing_solver8.
Qed.

Lemma inv_ctxrR_regular0:
  forall Γ τ st e, ⅋{ st }⟦ τ ⟧{ Γ } e ->
              closed_rty 0 (ctxdom ⦑Γ⦒ ∪ (dom _ st)) τ /\ ok_dctx (dom _ st) Γ.
Proof.
  induction Γ; simpl; intros; invclear H; simpl; auto; dec_solver2.
  - apply rR_regular2 in H0; mydestr. constructor; simpl; auto.
    + closed_rty_solver.
Qed.

Lemma inv_ctxrR_regular1:
  forall Γ τ st e, ⅋{ st }⟦ τ ⟧{ Γ } e ->
              (ok_dctx (dom _ st) Γ) /\
                ctx_closed_rty (dom _ st) Γ /\
                closed_rty 0 (ctxdom ⦑Γ⦒ ∪ (dom _ st)) τ.
Proof.
  intros. apply inv_ctxrR_regular0 in H. mydestr.
  do 2 (split; auto). apply ok_dctx_regular2 in H0; mydestr; auto.
Qed.

Lemma inv_ctxrR_regular2:
  forall Γ τ st e, ⅋{ st }⟦ τ ⟧{ Γ } e -> ⌊ Γ ⌋* ⊢t e ⋮t ⌊ τ ⌋.
Proof.
  induction Γ; simpl; intros; invclear H; simpl; auto.
  - apply rR_regular2 in H0; mydestr; auto.
Qed.

Lemma inv_ctxrR_regular:
  forall Γ τ st e, ⅋{ st }⟦ τ ⟧{ Γ } e ->
              ⌊ Γ ⌋* ⊢t e ⋮t ⌊ τ ⌋ /\
                (ok_dctx (dom _ st) Γ) /\
                ctx_closed_rty (dom _ st) Γ /\
                closed_rty 0 (ctxdom ⦑Γ⦒ ∪ (dom _ st)) τ.
Proof.
  intros. split.
  - eapply inv_ctxrR_regular2; eauto.
  - eapply inv_ctxrR_regular1; eauto.
Qed.

Lemma wf_ctxrR_regular1: ∀ (st: state) (Γ: listctx rty), wf_ctxrR st Γ → ok_dctx (dom _ st) Γ.
Proof.
  intros st Γ Hwf. induction Hwf; auto.
Qed.

Ltac ok_dctx_simpl1 :=
  repeat
    match goal with
    | [H: wf_ctxrR _ _ |- _ ≡ _ ] => apply wf_ctxrR_regular1 in H; mydestr
    | [H: wf_ctxrR _ _ |- _ ⊆ _ ] => apply wf_ctxrR_regular1 in H; mydestr
    | [H: wf_ctxrR _ _ |- _ ∉ _ ] => apply wf_ctxrR_regular1 in H; mydestr
    | [H: ok_dctx _ _ |- _ ≡ _ ] => apply ok_dctx_regular2 in H; mydestr
    | [H: ok_dctx _ _ |- _ ⊆ _ ] => apply ok_dctx_regular2 in H; mydestr
    | [H: ok_dctx _ _ |- _ ∉ _ ] => apply ok_dctx_regular2 in H; mydestr
    | [H: ok ((_, _) :: _) |- _ ≡ _ ] => rewrite ok_pre_destruct in H; mydestr
    | [H: ok ((_, _) :: _) |- _ ⊆ _ ] => rewrite ok_pre_destruct in H; mydestr
    | [H: ok ((_, _) :: _) |- _ ∉ _ ] => rewrite ok_pre_destruct in H; mydestr
    | [H: ok (_ ++ [(_, _)]) |- _ ≡ _ ] => rewrite ok_post_destruct in H; mydestr
    | [H: ok (_ ++ [(_, _)]) |- _ ⊆ _ ] => rewrite ok_post_destruct in H; mydestr
    | [H: ok (_ ++ [(_, _)]) |- _ ∉ _ ] => rewrite ok_post_destruct in H; mydestr
    end.

Ltac inv_rd_simpl1 :=
  progress repeat
    (ctx_erase_simp;
     listctx_set_simpl;
     ok_dctx_simpl1;
     ctx_rm_simp1;
     my_simplify_map_eq3;
     basic_typing_simpl1;
     repeat match goal with
       | [H: context [ ⌊{_ ~r> _} _ ⌋ ] |- _ ] => rewrite rty_open_perserve_erase in H
       | [|- context [ ⌊{_ ~r> _} _ ⌋ ]] => rewrite rty_open_perserve_erase
       | [H: ⅋{_}⟦_⟧{_} _ |- _ ∉ fv_tm _] => apply inv_ctxrR_regular2 in H; mydestr
       | [H: ⅋{_}⟦_⟧{_} _ |- _ ∉ fv_value _] => apply inv_ctxrR_regular2 in H; mydestr
       | [H: closed_rty _ _ ?τ |- _ ∉ rty_fv ?τ ] => invclear H
       | [H: ⅋{_}⟦_⟧{_} _ |- _ ⊢t _ ⋮t _ ] => apply inv_ctxrR_regular2 in H; mydestr
       | [H: ⅋{_}⟦_⟧{_} _ |- _ ⊢t _ ⋮v _] => apply inv_ctxrR_regular2 in H; mydestr
       end).

Ltac inv_rd_solver1 :=
  inv_rd_simpl1;
  match goal with
  | [H: ?Γ ⊢t ?e ⋮v ?T |- ?x ∉ fv_value ?e] => eapply tyable_implies_fresh_value in H; eauto
  | [H: ?Γ ⊢t ?e ⋮t ?T |- ?x ∉ fv_tm ?e] => eapply tyable_implies_fresh_tm in H; eauto
  end;
  inv_rd_simpl1.

(* Lemma ok_dctx_state_subst_implies_rty_subst_eq: forall Γ (a: atom) (L: aset) x b n d ϕ k (c_x: constant), *)
(*     ok_dctx ({[a]} ∪ L) [(x, {k ~r> c_x} [v:b|n|d|ϕ])] -> *)
(*     ok_dctx ({[a]} ∪ L) (Γ ++ [(x, ({k ~r> a} [v:b|n|d|ϕ]))]). *)
(* Proof. *)
(*   induction Γ; simpl; intros. *)
(*   - invclear H; auto_ty_exfalso. constructor; auto. *)
(*     invclear H4. constructor; auto. *)
(*     + invclear H. constructor; auto. invclear H4. constructor; auto. *)
(*       unfold not_fv_in_refinement; intros. unfold not_fv_in_refinement in H. *)
(*       unfold refinement_open. unfold refinement_open in H. *)
(*       destruct (m !! a); destruct (m' !! a). *)
(*       apply H, *)

Lemma state_subst_implies_r_susbt_eq: forall k (a: atom) ϕ bst st (c_x: constant) v,
    refinement_open k a ϕ bst (<[a:=c_x]> st) v <-> refinement_open k c_x ϕ bst (<[a:=c_x]> st) v.
Proof.
  unfold refinement_open; split; intros; my_simplify_map_eq3.
Qed.

Lemma state_subst_implies_rty_subst_eq: forall (a: atom) (c_x: constant) Γ st τ x k b n d ϕ e,
    (* closed_rty 0 (ctxdom Γ ∪ dom aset (<[a:=c_x]> st)) ({k ~r> a} [v:b|n|d|ϕ]) -> *)
    ok_dctx (dom aset (<[a:=c_x]> st)) (Γ ++ [(x, {k ~r> a} [v:b|n|d|ϕ])]) ->
    ok_dctx (dom aset (<[a:=c_x]> st)) (Γ ++ [(x, {k ~r> c_x} [v:b|n|d|ϕ])]) ->
    (⅋{<[a:=c_x]> st}⟦τ⟧{Γ ++ [(x, {k ~r> c_x} [v:b|n|d|ϕ])] } e) <->
    (⅋{<[a:=c_x]> st}⟦τ⟧{Γ ++ [(x, {k ~r> a} [v:b|n|d|ϕ])] } e).
Proof.
  induction Γ; split; intros; RD_simp2.
  - mydestr. simpl. constructor; auto.
    (* invclear H11; auto_ty_exfalso. constructor; auto. refinement_solver. *)
    exists x0. split; auto. invclear H1; mydestr. constructor; auto. split; auto. refinement_solver.
    intros. apply H4; auto. rewrite <- state_subst_implies_r_susbt_eq; auto.
  - mydestr. simpl. constructor; auto.
    (* invclear H11; auto_ty_exfalso. constructor; auto. refinement_solver. *)
    exists x0. split; auto. invclear H1; mydestr. constructor; auto. split; auto. refinement_solver.
    intros. apply H4; auto. rewrite state_subst_implies_r_susbt_eq; auto.
  - constructor; auto. inv_rd_simpl1. inv_rd_simpl1. intros. auto_under c_x0.
    assert (a0 <> a).
    { rewrite dom_insert_simp in H. apply ok_dctx_regular in H; mydestr. set_solver. }
    setoid_rewrite insert_commute; auto.
    eapply IHΓ; eauto.
    + repeat rewrite dom_insert_simp. invclear H; auto_ty_exfalso. rewrite dom_insert_simp in H15. ok_dctx_solver_slow.
    + repeat rewrite dom_insert_simp. invclear H7; auto_ty_exfalso. rewrite dom_insert_simp in H15. ok_dctx_solver_slow.
    + setoid_rewrite insert_commute; auto.
  - constructor; auto. inv_rd_simpl1. inv_rd_simpl1. intros. auto_under c_x0. mydestr.
    assert (a0 <> a).
    { rewrite dom_insert_simp in H. apply ok_dctx_regular in H; mydestr. set_solver. }
    exists x0. split; auto. intros. auto_under v_x. dsimp1.
    setoid_rewrite insert_commute; auto.
    eapply IHΓ; eauto.
    + repeat rewrite dom_insert_simp. invclear H; auto_ty_exfalso. ok_dctx_solver_slow.
    + repeat rewrite dom_insert_simp. invclear H7; auto_ty_exfalso. ok_dctx_solver_slow.
    + setoid_rewrite insert_commute; auto.
  - constructor; auto. inv_rd_simpl1. inv_rd_simpl1. intros. auto_under v_x.
    eapply IHΓ; eauto.
    + repeat rewrite dom_insert_simp. invclear H; auto_ty_exfalso. ok_dctx_solver_slow.
    + repeat rewrite dom_insert_simp. invclear H0; auto_ty_exfalso. ok_dctx_solver_slow.
  - constructor; auto. inv_rd_simpl1. inv_rd_simpl1. intros. auto_under c_x0.
    assert (a0 <> a).
    { rewrite dom_insert_simp in H. apply ok_dctx_regular in H; mydestr. set_solver. }
    setoid_rewrite insert_commute; auto.
    eapply IHΓ; eauto.
    + repeat rewrite dom_insert_simp. invclear H; auto_ty_exfalso. ok_dctx_solver_slow.
    + repeat rewrite dom_insert_simp. invclear H0; auto_ty_exfalso. ok_dctx_solver_slow.
    + setoid_rewrite insert_commute; auto.
  - constructor; auto. inv_rd_simpl1. inv_rd_simpl1. mydestr. exists x0. split; auto. intros.
    auto_under v_x. dsimp1.
    assert (a0 <> a).
    { apply ok_dctx_regular in H; mydestr. set_solver. }
    setoid_rewrite insert_commute; auto.
    eapply IHΓ; eauto.
    + repeat rewrite dom_insert_simp. invclear H; auto_ty_exfalso. ok_dctx_solver_slow.
    + repeat rewrite dom_insert_simp. invclear H0; auto_ty_exfalso. ok_dctx_solver_slow.
    + setoid_rewrite insert_commute; auto.
  - constructor; auto. inv_rd_simpl1. inv_rd_simpl1. intros.
    auto_under v_x.
    eapply IHΓ; eauto.
    + repeat rewrite dom_insert_simp. invclear H; auto_ty_exfalso. ok_dctx_solver_slow.
    + repeat rewrite dom_insert_simp. invclear H0; auto_ty_exfalso. ok_dctx_solver_slow.
Qed.

Lemma state_subst_implies_rty_subst_eq': forall (a: atom) (c_x: constant) Γ st τ x k b n d ϕ e,
    (ok_dctx (dom aset (<[a:=c_x]> st)) (Γ ++ [(x, {k ~r> a} [v:b|n|d|ϕ])]) /\
       (⅋{<[a:=c_x]> st}⟦τ⟧{Γ ++ [(x, {k ~r> c_x} [v:b|n|d|ϕ])] } e)) <->
    (ok_dctx (dom aset (<[a:=c_x]> st)) (Γ ++ [(x, {k ~r> c_x} [v:b|n|d|ϕ])]) /\
       (⅋{<[a:=c_x]> st}⟦τ⟧{Γ ++ [(x, {k ~r> a} [v:b|n|d|ϕ])] } e)).
Proof.
  split; intros; mydestr.
  - assert (ok_dctx (dom aset (<[a:=c_x]> st)) (Γ ++ [(x, {k ~r> c_x} [v:b|n|d|ϕ])])).
    apply inv_ctxrR_regular in H0; mydestr; auto.
    split; auto. rewrite <- state_subst_implies_rty_subst_eq; auto.
  - assert (ok_dctx (dom aset (<[a:=c_x]> st)) (Γ ++ [(x, {k ~r> a} [v:b|n|d|ϕ])])).
    apply inv_ctxrR_regular in H0; mydestr; auto.
    split; auto. rewrite state_subst_implies_rty_subst_eq; auto.
Qed.

Lemma state_subst_implies_rty_subst_eq2: forall (a v: atom) Γ st (c_x: constant) τ x k b n d ϕ e,
    (ok_dctx (dom aset (<[a:=c_x]> st)) (Γ ++ [(x, {k ~r> v} [v:b|n|d|ϕ])]) /\
        ⅋{<[a:=c_x]> st}⟦τ⟧{Γ ++ [(x, {k ~r> ({a := c_x}v v)} [v:b|n|d|ϕ])] } e) <->
    (ok_dctx (dom aset (<[a:=c_x]> st)) (Γ ++ [(x, {k ~r> ({a := c_x}v v)} [v:b|n|d|ϕ])]) /\
       ⅋{<[a:=c_x]> st}⟦τ⟧{Γ ++ [(x, {k ~r> v} [v:b|n|d|ϕ])] } e).
Proof.
  intros a v. destruct (decide (a = v)).
  - assert (forall c_x, ({a := c_x}v v) = c_x) as HJ by (simpl; intros; rewrite decide_True; auto).
    intros. rewrite HJ. subst. rewrite state_subst_implies_rty_subst_eq'; auto.
  - intros. rewrite subst_fresh_value; auto. fast_set_solver.
Qed.

Lemma lc_fresh_var_implies_body: forall e (x: atom),
  x # e -> lc (e ^t^ x) -> body e.
Proof.
  intros.
  apply (body_lc_after_close_tm x) in H0. rewrite close_open_var_tm in H0; auto.
Qed.

Global Hint Resolve lc_fresh_var_implies_body: core.

Lemma inv_rRctx_oarr: forall Γ st (τ: rty) x b n d ϕ τ e,
    x ∉ fv_tm e -> x ∉ rty_fv τ ->
    wf_ctxrR st (Γ ++ [(x, {v:b|n|d|ϕ})]) ->
    closed_rty 0 (ctxdom ⦑Γ⦒ ∪ dom _ st) (-:{v: b | n | d | ϕ}⤑ τ) ->
    ⅋{st}⟦τ ^r^ x⟧{Γ ++ [(x, {v:b|n|d|ϕ})] } (e ^t^ x) ->
    ⅋{st}⟦-:{v: b | n | d | ϕ}⤑ τ⟧{Γ} (vlam b e).
Proof.
  induction Γ; intros; RD_simp2; inv_rd_simpl1.
  - invclear H6; auto_ty_exfalso. do 2 constructor.
    + eapply opened_term_tyable_renaming; eauto.
    + apply (closed_rty_trans _ (ctxdom ⦑[]⦒ ∪ dom aset st)); eauto. fast_set_solver.
    + exists (vlam b e). split; auto.
      { apply multistep_refl. rewrite lc_abs_iff_body.
        eapply lc_fresh_var_implies_body; basic_typing_solver. }
      intros.
      assert (({0;b∅;st}⟦{v:b|n|d|ϕ}⟧) c_x). do 2 constructor; refinement_solver.
      auto_under c_x. RD_simp2. rewrite open_subst_same_tm in H6; auto.
      rewrite denotation_st_update_iff_subst in H6; mydestr; try fast_set_solver.
      rewrite open_subst_same_rty in H8; auto.
      assert (({0;b∅;st}⟦τ0 ^r^ c_x⟧) (mk_app (vlam b e) c_x)).
      eapply termR_perserve_rR; eauto. rewrite rty_open_perserve_not_overbasety. refinement_solver8.
      refinement_solver.
      apply mk_app_reduce_to_open'; refinement_solver8. inv_rd_simpl1.
      eapply opened_term_tyable_renaming; eauto.
      rewrite closed_rty_destruct_oarr in H2; mydestr.
      apply rR_open_trans_empty; auto. refinement_solver.
      refinement_solver8.
  - invclear H9; auto_ty_exfalso. constructor; auto.
    + refinement_solver8.
    + simpl. constructor; auto. eapply opened_term_tyable_renaming; eauto.
    + intros. auto_under c_x. rewrite subst_open_var_tm in H13; auto.
      eapply IHΓ in H13; eauto.
      pose (fv_of_subst_tm a c_x e). set_solver.
      apply (closed_rty_trans _ ({[a]} ∪ ctxdom ⦑Γ⦒ ∪ dom aset st)); eauto. fast_set_solver.
      refinement_solver.
  - invclear H8; auto_ty_exfalso. constructor; auto.
    + refinement_solver8.
    + simpl. constructor; auto. eapply opened_term_tyable_renaming; eauto.
    + mydestr. auto_meet_exists Hmeet. auto_meet_reduce. auto_under x0.
      rewrite subst_open_var_tm in H8; auto.
      eapply IHΓ in H8; eauto. dsimp1.
      pose (fv_of_subst_tm a x3 e). set_solver.
      apply (closed_rty_trans _ ({[a]} ∪ ctxdom ⦑Γ⦒ ∪ dom aset st)); eauto. dsimp1.
      refinement_solver. op_solver1.
  - invclear H10; auto_ty_exfalso. constructor; auto.
    + refinement_solver8.
    + simpl. constructor; auto. eapply opened_term_tyable_renaming; eauto.
    + intros. auto_under v_x.
      rewrite subst_open_var_tm in H14; auto.
      eapply IHΓ in H14; eauto.
      pose (fv_of_subst_tm a v_x e).
      assert (fv_value v_x ≡ ∅) by refinement_solver8. set_solver.
      refinement_solver. refinement_solver.
Qed.

Lemma inv_rRctx_arrarr: forall Γ st (τ: rty) x τ1 τ2 τ e,
    x ∉ fv_tm e -> x ∉ rty_fv τ ->
    wf_ctxrR st (Γ ++ [(x, τ1 ⤑ τ2)]) ->
    closed_rty 0 (ctxdom ⦑Γ⦒ ∪ dom _ st) ((τ1 ⤑ τ2) ⤑ τ) ->
    ⅋{st}⟦τ⟧{Γ ++ [(x, τ1 ⤑ τ2)] } (e ^t^ x) ->
    ⅋{st}⟦(τ1 ⤑ τ2)⤑ τ⟧{Γ} (vlam ⌊τ1 ⤑ τ2⌋ e).
Proof.
  induction Γ; intros; RD_simp2.
  - clear H11. invclear H10; auto_ty_exfalso. do 2 constructor.
    + eapply opened_term_tyable_renaming; eauto.
    + refinement_solver.
    + exists (vlam ⌊τ1 ⤑ τ2⌋ e). split; auto.
      { apply multistep_refl. rewrite lc_abs_iff_body.
        eapply lc_fresh_var_implies_body; basic_typing_solver. }
      intros. auto_under v_x. RD_simp2. rewrite open_subst_same_tm in H4; auto.
      rewrite closed_rty_destruct_arrarr in H2; mydestr.
      eapply termR_perserve_rR; eauto. refinement_solver8. simpl.
      apply mk_app_reduce_to_open'; refinement_solver8; inv_rd_simpl1.
      eapply opened_term_tyable_renaming; eauto.
  - invclear H9; auto_ty_exfalso. constructor; auto.
    + refinement_solver8.
    + inv_rd_simpl1. constructor; auto. eapply opened_term_tyable_renaming; eauto.
    + intros. auto_under c_x. rewrite subst_open_var_tm in H13; auto.
      eapply IHΓ in H13; eauto.
      pose (fv_of_subst_tm a c_x e). set_solver.
      apply (closed_rty_trans _ ({[a]} ∪ ctxdom ⦑Γ⦒ ∪ dom aset st)); eauto. fast_set_solver.
      refinement_solver.
  - invclear H8; auto_ty_exfalso. constructor; auto.
    + refinement_solver8.
    + inv_rd_simpl1. constructor; auto. eapply opened_term_tyable_renaming; eauto.
    + mydestr. auto_meet_exists Hmeet. auto_meet_reduce. auto_under x0.
      rewrite subst_open_var_tm in H8; auto.
      eapply IHΓ in H8; eauto. dsimp1.
      pose (fv_of_subst_tm a x3 e). set_solver.
      apply (closed_rty_trans _ ({[a]} ∪ ctxdom ⦑Γ⦒ ∪ dom aset st)); eauto. dsimp1.
      refinement_solver. op_solver1.
  - invclear H10; auto_ty_exfalso. constructor; auto.
    + inv_rd_simpl1.
    + refinement_solver8.
    + inv_rd_simpl1. constructor; auto. eapply opened_term_tyable_renaming; eauto.
    + inv_rd_simpl1. intros. auto_under v_x.
      rewrite subst_open_var_tm in H14; auto.
      eapply IHΓ in H14; eauto.
      pose (fv_of_subst_tm a v_x e).
      assert (fv_value v_x ≡ ∅) by refinement_solver8. set_solver.
      refinement_solver. refinement_solver.
Qed.

Lemma inv_rRctx_pre_weakening_empty: forall Γ st τ,
    not_overbasety τ ->
    wf_ctxrR st Γ ->
    (forall e, {st}⟦τ⟧ e -> ⅋{st}⟦τ⟧{Γ} e).
Proof.
  induction Γ; intros; RD_simp2; denotation_simp.
  - invclear H5; auto_ty_exfalso. constructor; refinement_solver7.
    intros. apply IHΓ; auto. lc_simpl3.
    rewrite rR_shadow_update_st_c; refinement_solver.
  - constructor; refinement_solver7. mydestr. auto_meet_exists Hmeet.
    apply IHΓ; eauto.
    rewrite rR_shadow_update_st; refinement_solver.
    auto_under x. lc_simpl3.
  - constructor; refinement_solver7. intros. lc_simpl3.
Qed.

Lemma inv_rRctx_pre_weakening: forall Γ1 Γ2 st τ,
    not_overbasety τ ->
    wf_ctxrR st (Γ1 ++ Γ2) ->
    (forall e, ⅋{st}⟦τ⟧{Γ1} e -> ⅋{st}⟦τ⟧{Γ1 ++ Γ2} e).
Proof.
  induction Γ1; intros; RD_simp2; denotation_simp.
  - apply inv_rRctx_pre_weakening_empty; auto.
  - constructor; refinement_solver. ctx_erase_simp5. basic_typing_solver8. ctx_erase_simp5.
  - constructor; auto. refinement_solver. ctx_erase_simp5. basic_typing_solver7. ctx_erase_simp5.
    auto_meet_exists Hmeet. auto_meet_reduce.
  - constructor; auto. refinement_solver. ctx_erase_simp5. basic_typing_solver7. ctx_erase_simp5.
Qed.

Lemma wf_ctxrR_implies_inv_var_is_arr_last: forall (Γ: listctx rty) st x τ_x,
    is_arr τ_x -> closed_rty 0 (ctxdom ⦑ Γ ⦒ ∪ dom _ st) τ_x ->
    wf_ctxrR st (Γ ++ [(x, τ_x)]) -> (⅋{st}⟦τ_x⟧{(Γ ++ [(x, τ_x)])}) x.
Proof.
  induction Γ; simpl; intros; mydestr.
  - RD_simp2. constructor; auto.
    + constructor; auto. constructor; auto. refinement_solver. simpl. var_dec_solver.
    + intros. RD_simp2. lc_simpl3. var_dec_solver.
  -  assert (x <> a).
     { apply wf_ctxrR_regular1 in H1. apply ok_dctx_regular in H1; mydestr. basic_typing_solver. }
     RD_simp2; simpl in H0.
    + constructor; auto. inv_rd_simpl1. refinement_solver.
      apply var_last_typable; refinement_solver.
      intros. auto_under c_x. lc_simpl3. var_dec_solver. apply IHΓ; auto. refinement_solver8.
    + mydestr.
      constructor; auto. refinement_solver. apply var_last_typable; refinement_solver.
      auto_meet_exists Hmeet. auto_under x0.
      lc_simpl3. var_dec_solver. apply IHΓ; auto. dsimp1. refinement_solver8.
    + constructor; auto. refinement_solver. apply var_last_typable; refinement_solver.
      intros. lc_simpl3. var_dec_solver. apply IHΓ; auto. dec_solver2.
Qed.

Lemma wf_ctxrR_implies_inv_var_over_last: forall (Γ: listctx rty) st x b n d ϕ,
    wf_ctxrR st (Γ ++ [(x, {v:b|n|d|ϕ})]) -> (⅋{st}⟦ mk_eq_var b x ⟧{(Γ ++ [(x, {v:b|n|d|ϕ})])}) x.
Proof.
  induction Γ; simpl; intros; mydestr.
  - RD_simp2. pose (closed_rty_mk_eq_var b x). constructor; auto.
    + refinement_solver.
    + constructor; auto. constructor; auto. refinement_solver. simpl. var_dec_solver.
    + intros. RD_simp2. lc_simpl3. var_dec_solver. invclear H; mydestr; subst.
      do 2 (split; auto). denotation_simp. refinement_solver.
      intros. my_simplify_map_eq3.
  - pose (closed_rty_mk_eq_var b x).
    assert (x <> a).
    { apply wf_ctxrR_regular1 in H. apply ok_dctx_regular in H; mydestr. basic_typing_solver. }
     RD_simp2; simpl in H0.
    + constructor; auto. inv_rd_simpl1. refinement_solver.
      simpl. apply var_last_typable; refinement_solver.
      intros. auto_under c_x. lc_simpl3. var_dec_solver.
    + mydestr.
      constructor; auto. refinement_solver. simpl. apply var_last_typable; refinement_solver.
      auto_meet_exists Hmeet. auto_under x0.
      lc_simpl3. var_dec_solver.
    + constructor; auto. refinement_solver. simpl. apply var_last_typable; refinement_solver.
      intros. lc_simpl3. var_dec_solver.
Qed.

Lemma wf_ctxrR_implies_inv_var_under_last: forall (Γ: listctx rty) st x b n d ϕ,
    wf_ctxrR st (Γ ++ [(x, [v:b|n|d|ϕ])]) -> (⅋{st}⟦ mk_eq_var b x ⟧{(Γ ++ [(x, [v:b|n|d|ϕ])])}) x.
Proof.
  induction Γ; simpl; intros; mydestr.
  - RD_simp2. mydestr. pose (closed_rty_mk_eq_var b x). constructor; auto.
    + refinement_solver.
    + constructor; auto. constructor; auto. refinement_solver. simpl. var_dec_solver.
    + auto_meet_exists Hmeet. auto_under x0. dsimp1. RD_simp2. var_dec_solver. simpl.
      do 2 (split; auto). denotation_simp. refinement_solver.
      intros. my_simplify_map_eq3.
  - pose (closed_rty_mk_eq_var b x).
    assert (x <> a).
    { apply wf_ctxrR_regular1 in H. apply ok_dctx_regular in H; mydestr. basic_typing_solver. }
     RD_simp2; simpl in H0.
    + constructor; auto. inv_rd_simpl1. refinement_solver.
      simpl. apply var_last_typable; refinement_solver.
      intros. auto_under c_x. lc_simpl3. var_dec_solver.
    + mydestr.
      constructor; auto. refinement_solver. simpl. apply var_last_typable; refinement_solver.
      auto_meet_exists Hmeet. auto_under x0.
      lc_simpl3. var_dec_solver.
    + constructor; auto. refinement_solver. simpl. apply var_last_typable; refinement_solver.
      intros. lc_simpl3. var_dec_solver.
Qed.

Lemma wf_implies_ctxrR_tlete_is_arr_drop: forall Γ st x τ_x (e_x: value) e τ,
    wf_ctxrR st (Γ ++ [(x, τ_x)]) ->
    closed_rty 0 (ctxdom ⦑Γ⦒ ∪ dom _ st) τ ->
    not_overbasety τ -> is_arr τ_x ->
    ⅋{st}⟦τ_x⟧{Γ} e_x ->
    ⅋{st}⟦τ⟧{Γ ++ [(x, τ_x)] } e -> ⅋{st}⟦τ⟧{Γ} (tlete e_x (x \t\ e)).
Proof.
  intros. eapply wf_implies_ctxrR_tlete_is_arr in H4; eauto.
  eapply inv_implies_ctxrR_drop_last; eauto.
  - inv_rd_simpl1. assert (x ∉ ctxdom ⦑Γ⦒); ctx_erase_simp. set_solver.
  - assert (x ∉ fv_value e_x). inv_rd_solver1.
    assert (x ∉ fv_tm (x \t\ e)) by apply close_rm_fv_tm.
    set_solver.
Qed.

Lemma wf_implies_ctxrR_tlete_ubase: forall Γ st x b n d ϕ e_x e τ,
    wf_ctxrR st (Γ ++ [(x, [v:b|n|d|ϕ] )]) ->
    not_overbasety τ ->
    ⅋{st}⟦ [v:b|n|d|ϕ] ⟧{Γ} e_x ->
    ⅋{st}⟦τ⟧{Γ ++ [(x, [v:b|n|d|ϕ] )] } e -> ⅋{st}⟦τ⟧{Γ ++ [(x, [v:b|n|d|ϕ] )] } (tlete e_x (x \t\ e)).
Proof.
  induction Γ; intros; RD_simp2.
  - constructor; auto. denotation_simp. eapply tlete_apply_typable_aux3; eauto; refinement_solver8.
    mydestr. auto_meet_exists Hmeet. auto_meet_reduce. auto_under e_x. RD_simp2.
    lc_simpl3. assert ([] ⊢t v_x ⋮v b) by refinement_solver.
    assert (({0;b∅;({x↦v_x}) st}⟦τ⟧) (tlete v_x (x \t\ e))).
    eapply termR_perserve_rR; eauto; refinement_solver.
    apply termR_let_one_step_from_basic_type'. basic_typing_solver. fast_set_solver.
    eapply tlete_apply_typable_aux; eauto; refinement_solver.
    eapply termR_perserve_rR; eauto; refinement_solver.
    eapply termR_elete; eauto.
    eapply reduction_implies_termR; eauto. refinement_solver8.
  - constructor; auto. denotation_simp. eapply tlete_apply_typable_aux4; eauto; refinement_solver8.
    intros. auto_under c_x.
    eapply IHΓ in H12; eauto. lc_simpl3.
    rewrite <- subst_close_tm; auto. refinement_solver.
    denotation_simp; basic_typing_solver8.
  - constructor; auto. denotation_simp. eapply tlete_apply_typable_aux4; eauto; refinement_solver8.
    mydestr. auto_meet_exists Hmeet. auto_meet_reduce.
    auto_under ({a := v_x }t e_x). eapply IHΓ in H3; eauto. lc_simpl3.
    rewrite <- subst_close_tm; auto. dsimp1.
    denotation_simp; basic_typing_solver8.
  - constructor; auto. denotation_simp. eapply tlete_apply_typable_aux4; eauto; refinement_solver8.
    intros. auto_under v_x.
    eapply IHΓ in H18; eauto. lc_simpl3.
    rewrite <- subst_close_tm; auto. refinement_solver8.
    denotation_simp; basic_typing_solver8.
Qed.

Lemma wf_implies_ctxrR_tlete_ubase_drop: forall Γ st x b n d ϕ e_x e τ,
    wf_ctxrR st (Γ ++ [(x, [v:b|n|d|ϕ] )]) ->
    closed_rty 0 (ctxdom ⦑Γ⦒ ∪ dom _ st) τ ->
    not_overbasety τ ->
    ⅋{st}⟦ [v:b|n|d|ϕ] ⟧{Γ} e_x ->
    ⅋{st}⟦τ⟧{Γ ++ [(x, [v:b|n|d|ϕ] )] } e -> ⅋{st}⟦τ⟧{Γ} (tlete e_x (x \t\ e)).
Proof.
  intros. eapply wf_implies_ctxrR_tlete_ubase in H3; eauto.
  eapply inv_implies_ctxrR_drop_last; eauto.
  - inv_rd_simpl1. assert (x ∉ ctxdom ⦑Γ⦒); ctx_erase_simp. set_solver.
  - assert (x ∉ fv_tm e_x). inv_rd_solver1.
    assert (x ∉ fv_tm (x \t\ e)) by apply close_rm_fv_tm.
    set_solver.
Qed.

Definition eval_op_under_bound_tm (op: biop) (a: nat) (b: nat): tm :=
  match op with
  | op_plus => a + b
  | op_eq => (Nat.eqb a b)
  | op_lt => (Nat.ltb a b)
  | op_rannat => nat-gen
  end.

Definition eval_op_under_bound_tm_all (op: biop) (a: value) (b: value): tm :=
  tletbiop op_eq a b (vbvar 0).

Lemma eval_op_under_bound_tm_reduction_sepc: forall (op: biop) (a: nat) (b: nat) (c: constant),
    eval_op op a b c ->
    eval_op_under_bound_tm op a b ↪* c.
Proof.
  destruct op; simpl; intros; invclear H; auto.
Qed.

Global Hint Resolve eval_op_under_bound_tm_reduction_sepc: core.

(* Lemma eval_op_under_bound_tm_all_reduction_sepc: forall (op: biop) (v1 v2: value) (c: constant), *)
(*     Γ ⊢t v1 ⋮v fst_ty_of_op op -> *)
(*     Γ ⊢t v2 ⋮v snd_ty_of_op op -> *)
(*     eval_op op a b c -> *)
(*     eval_op_under_bound_tm op a b ↪* c. *)
(* Proof. *)
(*   destruct op; simpl; intros; invclear H; auto. *)
(* Qed. *)

Lemma eval_op_under_bound_tm_reduction_sepc': forall (op: biop) (a: nat) (b: nat) (c: constant),
    eval_op_under_bound_tm op a b ↪* c ->
    eval_op op a b c.
Proof.
  destruct op; simpl; intros.
  - rewrite value_reduce_to_value_implies_same in H; mydestr; subst.
    invclear H. constructor.
  - rewrite value_reduce_to_value_implies_same in H; mydestr; subst.
    invclear H. constructor.
  - rewrite value_reduce_to_value_implies_same in H; mydestr; subst.
    invclear H. constructor.
  - assert ([] ⊢t nat-gen ⋮t TNat); auto.
    assert ([] ⊢t c ⋮t TNat). eapply multi_preservation; eauto.
    invclear H1. invclear H4. destruct c; invclear H3. constructor.
Qed.

Global Hint Resolve eval_op_under_bound_tm_reduction_sepc': core.

Lemma eval_op_under_bound_tm_tyable: forall (op: biop) (a: nat) (b: nat),
    [] ⊢t eval_op_under_bound_tm op a b ⋮t ret_ty_of_op op.
Proof.
  destruct op; simpl; intros; eauto.
  - do 2 constructor; auto.
  - do 2 constructor; auto.
  - do 2 constructor; auto.
Qed.

Global Hint Resolve eval_op_under_bound_tm_tyable: core.

Ltac lia_dec_solver :=
  repeat match goal with
    | [H: context [ decide (?a = ?b) ] |- _ ] =>
        (assert (a = b) as Htmp by lia; rewrite (decide_True _ _ Htmp) in H; try clear Htmp) ||
          (assert (a <> b) as Htmp by lia; rewrite (decide_False _ _ Htmp) in H; try clear Htmp)
    end.

Lemma rR_eval_op_under_bound_tm: forall st (op: biop) (v1: nat) (v2: nat),
    ({0;b∅;st}⟦[v:ret_ty_of_op op|0|∅|refinement_open 1 v2
                                        (refinement_open 0 v1
                                           (λ (bst : bstate) (_ : state) (v : constant),
                                             eval_op op (bst 0) (bst 1) v))]⟧)
      (eval_op_under_bound_tm op v1 v2).
Proof.
  unfold refinement_open; intros. constructor; auto; try inv_rd_simpl1.
  do 2 (constructor; auto).
  - do 2 (constructor; auto).
    + unfold not_fv_in_refinement; auto.
    + unfold bound_in_refinement. intros. unfold bstate_insert. dec_solver2.
  - fast_set_solver.
Qed.

Lemma rR_eval_op_under_bound_tm_min: forall st (op: biop) (v1: nat) (v2: nat) e,
    ({0;b∅;st}⟦[v:ret_ty_of_op op|0|∅|refinement_open 1 v2
                                        (refinement_open 0 v1
                                           (λ (bst : bstate) (_ : state) (v : constant),
                                             eval_op op (bst 0) (bst 1) v))]⟧) e ->
    (forall (v_x: value), eval_op_under_bound_tm op v1 v2 ↪* v_x -> e ↪* v_x).
Proof.
  unfold refinement_open. simpl; intros. invclear H; mydestr.
  unfold termRraw.
  assert ([] ⊢t eval_op_under_bound_tm op v1 v2 ⋮t ret_ty_of_op op); auto.
  denotation_simp.
  apply H2; refinement_solver.
  unfold bstate_insert. dec_solver2.
Qed.

Lemma fst_ty_of_op_implies_nat:
  forall m bst st op n d ϕ (v: value),
    ({m;bst;st}⟦[v:fst_ty_of_op op|n|d|ϕ]⟧) v -> (exists (n: nat), v = n).
Proof.
  intros. invclear H; mydestr. simpl in H0. denotation_simp.
  destruct op; destruct x; simpl in H3; invclear H3; eauto.
Qed.

Lemma tletbiop_typable: forall op (v1 v2: value) e (x: atom) T,
    x ∉ fv_tm e ->
    [(x, TBase (ret_ty_of_op op))] ⊢t (e ^t^ x) ⋮t T ->
    [] ⊢t v1 ⋮v (fst_ty_of_op op) ->
    [] ⊢t v2 ⋮v (snd_ty_of_op op) ->
    [] ⊢t tletbiop op v1 v2 e ⋮t T.
Proof.
  intros.
  econstructor; eauto. instantiate (1:= {[x]}). intros. listctx_set_simpl.
  assert ( ([] ++ [(x, TBase (ret_ty_of_op op))]) ⊢t e ^t^ x ⋮t T) as Hz; auto.
  apply basic_has_type_renaming with (x0:=x0) in Hz; try listctx_set_solver.
  rewrite open_subst_same_tm in Hz; auto.
Qed.

Lemma tletbiop_typable_ctx: forall Γ op (v1 v2: value) e (x: atom) T,
    x ∉ fv_tm e ->
    (Γ ++ [(x, TBase (ret_ty_of_op op))]) ⊢t (e ^t^ x) ⋮t T ->
    Γ ⊢t v1 ⋮v (fst_ty_of_op op) ->
    Γ ⊢t v2 ⋮v (snd_ty_of_op op) ->
    Γ ⊢t tletbiop op v1 v2 e ⋮t T.
Proof.
  intros. auto_exists_L; intros.
  apply basic_has_type_renaming with (x0:=x0) in H0; try basic_typing_solver.
  rewrite open_subst_same_tm in H0; auto.
Qed.

Lemma tletbiop_typable_ctx_dummy: forall Γ op (v1 v2: value) e (x: atom) T,
    x ∉ fv_tm e ->
    (Γ ++ [(x, TBase (ret_ty_of_op op))]) ⊢t (e ^t^ x) ⋮t T ->
    Γ ⊢t v1 ⋮v (fst_ty_of_op op) ->
    Γ ⊢t v2 ⋮v (snd_ty_of_op op) ->
    (Γ ++ [(x, TBase (ret_ty_of_op op))]) ⊢t tletbiop op v1 v2 e ⋮t T.
Proof.
  intros. assert (Γ ⊢t tletbiop op v1 v2 e ⋮t T).
  eapply tletbiop_typable_ctx; eauto. basic_typing_solver.
Qed.

Lemma tletbiop_typable_ctx_dummy_cons: forall a Ta Γ op (v1 v2: value) e (x: atom) T,
    x ∉ fv_tm e ->
    ((a, Ta) :: Γ ++ [(x, TBase (ret_ty_of_op op))]) ⊢t (e ^t^ x) ⋮t T ->
    ((a, Ta) :: Γ) ⊢t v1 ⋮v (fst_ty_of_op op) ->
    ((a, Ta) :: Γ) ⊢t v2 ⋮v (snd_ty_of_op op) ->
    ((a, Ta) :: Γ ++ [(x, TBase (ret_ty_of_op op))]) ⊢t tletbiop op v1 v2 e ⋮t T.
Proof.
  intros. assert (((a, Ta) :: Γ) ⊢t tletbiop op v1 v2 e ⋮t T).
  eapply tletbiop_typable_ctx; eauto. basic_typing_solver. listctx_set_simpl. basic_typing_solver.
Qed.

Ltac inv_rd_simpl2 :=
  repeat match goal with
    | [H: ⅋{_}⟦_⟧{_} _ |- _ ⊢t _ ⋮t _ ] => apply inv_ctxrR_regular2 in H; mydestr
    | [H: ⅋{_}⟦_⟧{_} _ |- _ ⊢t _ ⋮v _] => apply inv_ctxrR_regular2 in H; mydestr
    end.

Lemma letapp_eval_op_under_bound_tm_termR: forall op (v1 v2: nat) (v_x: value) (x:atom) e T,
    x ∉ fv_tm e ->
    eval_op_under_bound_tm op v1 v2 ↪* v_x ->
    [(x, TBase (ret_ty_of_op op))] ⊢t (e ^t^ x) ⋮t T ->
    [] ⊢t (e ^t^ v_x) ⋮t T ->
    (e ^t^ v_x) <-<{ []; T} (tletbiop op v1 v2 e).
Proof.
  intros. assert ([] ⊢t tletbiop op v1 v2 e ⋮t T) as Ht.
  { econstructor; eauto. destruct op; auto. instantiate (1:= {[x]}). intros. listctx_set_simpl.
    assert ( ([] ++ [(x, TBase (ret_ty_of_op op))]) ⊢t e ^t^ x ⋮t T) as Hz; auto.
    apply basic_has_type_renaming with (x0:=x0) in Hz; try listctx_set_solver.
    rewrite open_subst_same_tm in Hz; auto. }
  constructor; auto. unfold termRraw. intros. invclear H3. simpl in H4. simpl.
  assert ([] ⊢t (eval_op_under_bound_tm op v1 v2) ⋮t ret_ty_of_op op) as Htmp; auto.
  assert ([] ⊢t v_x ⋮t ret_ty_of_op op). eapply multi_preservation; eauto.
  invclear H3. invclear H7; listctx_set_simpl.
  rewrite letbiop_step_spec. do 6 eexists; eauto.
  assert (lc (tletbiop op v1 v2 e)) by basic_typing_solver.
  rewrite letbiop_lc_body in H5; mydestr; auto.
Qed.

Lemma snd_ty_of_op_implies_nat:
  forall m bst st op n d ϕ (v: value),
    ({m;bst;st}⟦[v:snd_ty_of_op op|n|d|ϕ]⟧) v -> (exists (n: nat), v = n).
Proof.
  intros. invclear H; mydestr. simpl in H0. denotation_simp.
  destruct op; destruct x; simpl in H3; invclear H3; eauto.
Qed.

Lemma rR_basety_implies_nat:
  forall m bst st b n d ϕ (v: value),
    ({m;bst;st}⟦[v:b|n|d|ϕ]⟧) v -> (exists (c: constant), v = c).
Proof.
  intros. invclear H; mydestr. simpl in H0. denotation_simp.
  destruct x; simpl in H; invclear H; eauto.
Qed.
