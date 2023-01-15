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

(* refine *)

(* Ltac refinement_solver7 := *)
(*   listctx_set_simpl4; *)
(*   match goal with *)
(*   | [H: {_;_;_}⟦_⟧ ?e |- _ ∉ fv_tm ?e] => *)
(*       apply rR_regular1 in H; mydestr; basic_typing_solver *)
(*   end || refinement_solver6. *)

(* Lemma reduction_tmeet_iff_both: forall (T: base_ty) e1 e2, *)
(*     [] ⊢t e1 ⋮t T -> [] ⊢t e2 ⋮t T -> (forall (v: value), (mk_term_meet T e1 e2) ↪* v <-> e1 ↪* v /\ e2 ↪* v). *)
(* Admitted. *)

(* Lemma tmeet_typable: forall (T: base_ty) e1 e2, *)
(*     [] ⊢t e1 ⋮t T -> [] ⊢t e2 ⋮t T -> [] ⊢t (mk_term_meet T e1 e2) ⋮t T. *)
(* Admitted. *)

(* Lemma wf_implies_ctxrR_drop_last: forall Γ st x τ_x e_x e τ, *)
(*     not_overbasety τ -> not_overbasety τ_x -> *)
(*     closed_rty 0 (ctxdom Γ ∪ dom _ st) τ -> *)
(*     wf_ctxrR st (Γ ++ [(x, τ_x)]) -> *)
(*     x ∉ stale τ -> *)
(*     {st}⟦τ_x⟧{Γ} e_x -> *)
(*     {st}⟦τ⟧{Γ ++ [(x, τ_x)] } e -> *)
(*     {st}⟦τ⟧{Γ} (tlete e_x (x \t\ e)). *)
(* Proof. *)
(*   induction Γ; intros; mydestr; RD_simp; denotation_simp. *)
(*   - invclear H2; invclear H5; auto_ty_exfalso. mydestr. *)
(*     apply H12 in H2; mydestr. *)
(*     eapply H4 in H6; eauto. RD_simp. *)
(*     rewrite rR_shadow_update_st in H13; auto. *)
(*     refinement_solver. *)
(*   - invclear H2; invclear H4; invclear H5; auto_ty_exfalso; mydestr; ctx_erase_simp. *)
(*     + invclear H9; auto_ty_exfalso. constructor; auto. eapply let_store_typable; eauto. *)
(*       intros. *)
(*       assert (wf_ctxrR (<[a:=c_x]> st) (Γ ++ [(x, τ_x)])) by auto. *)
(*       assert (({<[a:=c_x]> st}⟦τ_x⟧{Γ}) ({a := c_x }t e_x)) by auto. *)
(*       apply H22 in H2. *)
(*       (* assert (({a := c_x }t (e ^t^ x)) = ({a := c_x }t e) ^t^ x) as Htmp. admit. *) *)
(*       (* rewrite Htmp in H2. clear Htmp. *) *)
(*       eapply IHΓ in H2; eauto. simpl. *)
(*       rewrite subst_close_tm in H2; auto; refinement_solver7. refinement_solver. *)
(*     + constructor; auto. eapply let_store_typable; eauto. *)
(*       destruct (classic (is_arr r)). *)
(*       { exists (random_inhabitant r). *)
(*         assert ({0;b∅;st}⟦r⟧ (random_inhabitant r)) by *)
(*           (apply random_inhabitant_in_any_under; refinement_solver7). *)
(*         split; auto. intros e_x_final. intros. *)
(*         apply H4 in H21. *)
(*       } *)
(*       exists ((x0 ⊗{  ⌊r⌋ } x1) ⊗{ ⌊r⌋ } x2). *)
(*       split. apply rR_tmeet3_when_all; auto. *)
(*       intros. rewrite reduction_tmeet3_iff_all in H20; refinement_solver7; mydestr. *)
(*       assert (wf_ctxrR (({a↦v_x}) st) (Γ ++ [(x, τ_x)])) as Hwf by eauto. *)
(*       assert ({({a↦v_x}) st}⟦τ_x⟧{Γ} (tlete e_x0 (a \t\ e_x))) as Hex by eauto. *)
(*       assert (({({a↦v_x}) st}⟦τ⟧{Γ ++ [(x, τ_x)] }) (tlete e_x0 (a \t\ e))) as He by eauto. *)
(*       assert ([] ⊢t v_x ⋮v ⌊r⌋). refinement_solver7. *)
(*       assert (closed_value v_x) as Hclosedv. basic_typing_solver. *)
(*       apply state_insert_closed_value with (a:=a) (st:=st) in Hclosedv. *)
(*       destruct Hclosedv; mydestr; subst. *)
(*       { eapply IHΓ in He; eauto. admit. *)
(*         assert (dom aset (({a↦x3}) st) ≡ {[a]} ∪ dom aset st) *)
(*         by (eapply insert_constant_in_st; refinement_solver7). *)
(*         refinement_solver7. *)
(*       } *)
(*       { rename H25 into Hzz. *)
(*         rewrite Hzz in He. rewrite Hzz. rewrite Hzz in Hex. rewrite Hzz in Hwf. *)
(*         ctx_erase_simp. *)
(*         assert (ok ((a, ⌊r⌋) :: ⌊Γ⌋* ++ ⌊[(x, τ_x)]⌋) as Hok by basic_typing_solver. *)
(*         assert (a <> x). listctx_set_solver. *)
(*         eapply IHΓ in He; eauto. admit. *)
(*         apply closed_rty_cap with *)
(*           (d1:= (ctxdom Γ ∪ ({[x]} ∪ ∅) ∪ dom aset st)) *)
(*           (d2:= ({[a]} ∪ ctxdom Γ ∪ dom aset st)); eauto; refinement_solver7. } *)
(* Admitted. *)

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
    end.

Ltac auto_ty_exfalso4 :=
  auto_ty_exfalso3;
  match goal with
  | [H: is_arr [v:_|_|_|_] |- _ ] =>invclear H
  | [H: is_arr {v:_|_|_|_} |- _ ] =>invclear H
  | [H: {v:_|_|_|_} = [v:_|_|_|_]|- _] => invclear H
  end.

Lemma rRctx_pre_weakening: forall Γ1 Γ2 st τ,
    not_overbasety τ ->
    wf_ctxrR st (Γ1 ++ Γ2) ->
    (forall e, {st}⟦τ⟧{Γ1} e -> {st}⟦τ⟧{Γ1 ++ Γ2} e).
Proof.
  induction Γ1; intros; RD_simp; denotation_simp.
  - apply rRctx_pre_weakening_empty; auto.
  - invclear H1; invclear H0; try auto_ty_exfalso4.
    + constructor; refinement_solver. basic_typing_solver7. ctx_erase_simp5.
    + denotation_simp. invclear H9; auto_ty_exfalso.
      constructor; auto. refinement_solver. ctx_erase_simp5. basic_typing_solver7. ctx_erase_simp5.
      exists (x0 ⊗{ x1 } x). split. eapply rR_tmeet_when_both; auto. intros.
      rewrite reduction_tmeet_iff_both in H6; refinement_solver7; mydestr; eauto.
    + invclear H9; try auto_ty_exfalso4. apply ctxrR_cons_under_arr; auto.
      denotation_simp. refinement_solver.
      ctx_erase_simp. basic_typing_solver7. refinement_solver.
      is_arr_simp.
Qed.

Check ok_first_not_equal_hd.

Lemma basic_type_first_not_equal_hd_tm
     : ∀ (Γ : list (atom * ty)) (x y : atom) (a b : ty) e T, ((x, a) :: Γ ++ [(y, b)]) ⊢t e ⋮t T → x ≠ y.
Proof.
  intros. assert (ok ((x, a) :: Γ ++ [(y, b)])) by basic_typing_solver2.
  eapply ok_first_not_equal_hd; eauto.
Qed.

Lemma basic_type_first_not_equal_hd_value
     : ∀ (Γ : list (atom * ty)) (x y : atom) (a b : ty) e T, ((x, a) :: Γ ++ [(y, b)]) ⊢t e ⋮v T → x ≠ y.
Proof.
  intros. assert (ok ((x, a) :: Γ ++ [(y, b)])) by basic_typing_solver2.
  eapply ok_first_not_equal_hd; eauto.
Qed.

Ltac basic_typing_solver8 :=
  match goal with
  | [H: ((?x, _) :: _ ++ [(?y, _)]) ⊢t _ ⋮t _ |- ?x ≠ ?y] => apply basic_type_first_not_equal_hd_tm in H; auto
  | [H: ((?x, _) :: _ ++ [(?y, _)]) ⊢t _ ⋮v _ |- ?x ≠ ?y] => apply basic_type_first_not_equal_hd_value in H; auto
  end || basic_typing_solver7.

Lemma wf_implies_ctxrR_tlete: forall Γ st x b n d ϕ e_x e τ,
    not_overbasety τ -> not_overbasety τ_x ->
    wf_ctxrR st (Γ ++ [(x, τ_x)]) ->
    {st}⟦mk_eq_var ty⟧{Γ ++ [(x, τ_x)] } e_x ->
    {st}⟦τ⟧{Γ ++ [(x, τ_x)] } e ->
    {st}⟦τ⟧{Γ ++ [(x, τ_x)] } (tlete e_x e).
Proof.

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
