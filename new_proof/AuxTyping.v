From stdpp Require Import mapset.
From stdpp Require Import natmap.
From CT Require Import CoreLangProp.
From CT Require Import OperationalSemanticsProp.
From CT Require Import BasicTypingProp.
From CT Require Import SyntaxSugar.
From CT Require Import Refinement.
From CT Require Import RefinementDenotation.
From CT Require Import RefinementDenotationTac.
From CT Require Import RefinementDenotationProp.
From CT Require Import RDInv.
From CT Require Import RDInv2.

Import Atom.
Import CoreLang.
Import Tactics.
Import NamelessTactics.
Import ListCtx.
Import OperationalSemantics.
Import OperationalSemanticsProp.
Import BasicTyping.
Import SyntaxSugar.
Import TermOrdering.
Import Refinement.
Import RefinementTac.
Import RefinementDenotation.
Import RefinementDenotationTac.
Import RefinementDenotationProp.
Import RDInv.
Import RDInv2.

Definition wf (Γ: listctx rty) (τ: rty) :=
  wf_ctxrR_not_terr ∅ Γ /\ closed_rty 0 (ctxdom ⦑Γ⦒ ) τ /\ not_overbasety τ.

Notation " Γ '⊢WF' τ " := (wf Γ τ) (at level 20, τ constr, Γ constr).

Lemma well_formed_regular: forall Γ τ,
  Γ ⊢WF τ -> wf_ctxrR ∅ Γ /\ closed_rty 0 (ctxdom ⦑Γ⦒ ) τ /\ not_overbasety τ.
Proof.
  intros. destruct H. setoid_rewrite wf_ctxrR_iff_wf_ctxrR_not_terr.
  split; auto.
Qed.

Definition disjunction (Γ: listctx rty) (τ1 τ2 τ3: rty) :=
  forall e, (⅋{∅}⟦ τ1 ⟧{ Γ } e) /\ (⅋{∅}⟦ τ2 ⟧{ Γ } e) <-> (⅋{∅}⟦ τ3 ⟧{ Γ } e).

Notation " Γ '⊢' τ1 '⩔' τ2 '⩵' τ3 " := (disjunction Γ τ1 τ2 τ3) (at level 20, τ1 constr, τ2 constr, τ3 constr, Γ constr).

(* Subtyping *)

Inductive subtyping: listctx rty -> rty -> rty -> Prop :=
| subtyping_ubase: forall Γ b n d ϕ1 ϕ2,
    ⟦ [v: b | n | d | ϕ1 ] ⟧⊆⟦ [v: b | n | d | ϕ2 ] ⟧{ Γ } ->
    subtyping Γ [v: b | n | d | ϕ1 ] [v: b | n | d | ϕ2 ]
| subtyping_obase: forall Γ b n d ϕ1 ϕ2,
    ⟦ [v: b | n | d | ϕ2 ] ⟧⊆⟦ [v: b | n | d | ϕ1 ] ⟧{ Γ } ->
    subtyping Γ {v: b | n | d | ϕ1 } {v: b | n | d | ϕ2 }
| subtyping_oarr: forall Γ b n d ϕ1 ϕ2 τ1 τ2 (L: aset),
    subtyping Γ {v: b | n | d | ϕ2 } {v: b | n | d | ϕ1 } ->
    (forall x, x ∉ L -> subtyping (Γ ++ [(x, {v: b | n | d | ϕ2 })]) τ1 τ2) ->
    ⟦ [v: b | n | d | ϕ2 ] ⟧⊆⟦ [v: b | n | d | ϕ1 ] ⟧{ Γ } ->
    subtyping Γ (-:{v: b | n | d | ϕ1 }⤑ τ1) (-:{v: b | n | d | ϕ2 }⤑ τ2).

Notation " Γ '⊢' τ1 '⪡' τ2 " := (subtyping Γ τ1 τ2) (at level 20, τ1 constr, τ2 constr, Γ constr).

Lemma subtyping_implies_not_overbasety:
  forall Γ τ1 τ2, Γ ⊢ τ1 ⪡ τ2 -> not_overbasety τ1 <-> not_overbasety τ2.
Proof.
  intros Γ τ1 τ2 H; induction H; split; intros; auto.
Qed.

Definition subtyping_spec: forall Γ τ1 τ2, wf_ctxrR ∅ Γ -> Γ ⊢ τ1 ⪡ τ2 -> (forall e, {∅}⟦ τ1 ⟧{ Γ } e -> {∅}⟦ τ2 ⟧{ Γ } e).
Admitted.

Reserved Notation "Γ '⊢' τ '⋮t' T" (at level 20).
Reserved Notation "Γ '⊢' τ '⋮v' T"  (at level 20).

Inductive term_under_type_check : listctx rty -> tm -> rty -> Prop :=
| UT_Value: forall Γ v τ, Γ ⊢ v ⋮v τ -> Γ ⊢ v ⋮t τ
| UT_terr: forall Γ b, Γ ⊢WF (mk_under_bot b) -> Γ ⊢ terr ⋮t (mk_under_bot b)
| UT_Sub: forall Γ e (τ1 τ2:rty), Γ ⊢WF τ2 -> [] ⊢ e ⋮t τ1 -> [] ⊢ τ1 ⪡ τ2 ->  (Γ ⊢ e ⋮t τ2)
| UT_Eq: forall Γ e τ1 τ2, Γ ⊢WF τ2 -> Γ ⊢ e ⋮t τ1 -> Γ ⊢ τ1 ⪡ τ2 -> Γ ⊢ τ2 ⪡ τ1 -> Γ ⊢ e ⋮t τ2
| UT_Merge: forall Γ e (τ1 τ2 τ3: rty), Γ ⊢WF τ3 -> Γ ⊢ e ⋮t τ1 -> Γ ⊢ e ⋮t τ2 -> Γ ⊢ τ1 ⩔ τ2 ⩵ τ3 -> Γ ⊢ e ⋮t τ3
| UT_Lete_base: forall Γ e_x e τ b n d ϕ (L: aset),
    Γ ⊢WF τ -> Γ ⊢ e_x ⋮t [v:b|n|d|ϕ] ->
    (forall x, x ∉ L -> (Γ ++ [(x, [v:b|n|d|ϕ])]) ⊢ (e ^t^ x) ⋮t τ ) -> Γ ⊢ (tlete e_x e) ⋮t τ
| UT_Lete_arr: forall Γ (e_x: value) e τ τ_x (L: aset),
    is_arr τ_x ->
    Γ ⊢WF τ -> Γ ⊢ e_x ⋮t τ_x -> (forall x, x ∉ L -> (Γ ++ [(x, τ_x)]) ⊢ (e ^t^ x) ⋮t τ ) -> Γ ⊢ (tlete e_x e) ⋮t τ
(* operators only take nat type *)
| UT_LetOp: forall (Γ: listctx rty) op (v1 v2: value) e τ d1 d2 ϕ1 ϕ2 (L: aset),
    Γ ⊢WF τ ->
    Γ ⊢ v1 ⋮v [v: fst_ty_of_op op | 0 | d1 | ϕ1 ] ->
    Γ ⊢ v2 ⋮v [v: snd_ty_of_op op | 0 | d2 | ϕ2 ] ->
    (forall x, x ∉ L -> (Γ ++ [(x, ({1 ~r> v2} ({0 ~r> v1} (mk_op_ret op))))]) ⊢ (e ^t^ x) ⋮t τ) ->
    Γ ⊢ (tletbiop op v1 v2 e) ⋮t τ
| UT_LetAppIndepend: forall Γ (v1 v2: value) e τ τ2 τ_x (L: aset),
    Γ ⊢WF τ -> Γ ⊢ v1 ⋮v (τ2 ⤑ τ_x) -> Γ ⊢ v2 ⋮v τ2 -> (forall x, x ∉ L -> (Γ ++ [(x, τ_x)]) ⊢ (e ^t^ x) ⋮t τ) ->
    Γ ⊢ (tletapp v1 v2 e) ⋮t τ
(* the value should only be constant or variables *)
| UT_LetAppDepend: forall Γ (v1 v2: value) e τ b d ϕ τ_x (L: aset),
    Γ ⊢WF τ ->
    Γ ⊢ v1 ⋮v (-:{v: b | 0 | d | ϕ}⤑ τ_x) -> Γ ⊢ v2 ⋮v [v: b | 0 | d | ϕ] ->
    (forall x, x ∉ L -> (Γ ++ [(x, [v: b | 0 | d | ϕ])]) ⊢WF τ_x /\
                    (Γ ++ [(x, τ_x ^r^ v2)]) ⊢ (e ^t^ x) ⋮t τ ) ->
    Γ ⊢ (tletapp v1 v2 e) ⋮t τ
| UT_Matchb_true: forall Γ (v: value) e1 e2 τ,
    Γ ⊢WF τ -> Γ ⊢ v ⋮v (mk_eq_constant true) -> Γ ⊢ e1 ⋮t τ -> ⌊ Γ ⌋* ⊢t e2 ⋮t ⌊ τ ⌋ -> Γ ⊢ (tmatchb v e1 e2) ⋮t τ
| UT_Matchb_false: forall Γ (v: value) e1 e2 τ,
    Γ ⊢WF τ -> Γ ⊢ v ⋮v (mk_eq_constant false) -> Γ ⊢ e2 ⋮t τ -> ⌊ Γ ⌋* ⊢t e1 ⋮t ⌊ τ ⌋ -> Γ ⊢ (tmatchb v e1 e2) ⋮t τ
with value_under_type_check : listctx rty -> tm -> rty -> Prop :=
| UT_Contant: forall Γ (c: constant), Γ ⊢WF (mk_eq_constant c) -> Γ ⊢ c ⋮v (mk_eq_constant c)
| UT_VarOver: forall Γ b (x: atom),
    Γ ⊢WF (mk_eq_var b x) -> (exists n d ϕ, ctxfind Γ x = Some {v: b | n | d | ϕ}) -> Γ ⊢ x ⋮v (mk_eq_var b x)
| UT_VarBase: forall Γ b (x: atom),
    Γ ⊢WF (mk_eq_var b x) -> (exists n d ϕ, ctxfind Γ x = Some [v: b | n | d | ϕ]) -> Γ ⊢ x ⋮v (mk_eq_var b x)
| UT_VarArr: forall Γ x τ_x, Γ ⊢WF τ_x -> is_arr τ_x -> ctxfind Γ x = Some τ_x -> Γ ⊢ x ⋮v τ_x
| UT_LamDep: forall Γ b d ϕ e τ (L: aset),
    Γ ⊢WF (-:{v: b | 0 | d | ϕ}⤑ τ) -> (forall x, x ∉ L -> (Γ ++ [(x, {v: b | 0 | d | ϕ})]) ⊢ (e ^t^ x) ⋮t (τ ^r^ x)) ->
    Γ ⊢ (vlam b e) ⋮v (-:{v: b | 0 | d | ϕ}⤑ τ)
| UT_LamIndep: forall Γ τ1 τ2 e τ (L: aset),
    Γ ⊢WF ((τ1 ⤑ τ2) ⤑ τ) -> (forall x, x ∉ L -> (Γ ++ [(x, (τ1 ⤑ τ2))]) ⊢ (e ^t^ x) ⋮t τ) ->
    Γ ⊢ (vlam ⌊ τ1 ⤑ τ2 ⌋ e) ⋮v ((τ1 ⤑ τ2) ⤑ τ)
| UT_LamFix: forall Γ b d ϕ e τ,
    Γ ⊢WF (-:{v: b | 0 | d | ϕ}⤑ τ) ->
    Γ ⊢ (vlam b (vlam (b ⤍ ⌊ τ ⌋) e)) ⋮v (-:{v: b | 0 | d | ϕ}⤑ (-:{v: b | 1 | d | ≻≻ ϕ}⤑ τ) ⤑ τ) ->
    Γ ⊢ (vfix (b ⤍ ⌊ τ ⌋) (vlam b e)) ⋮v (-:{v: b | 0 | d | ϕ}⤑ τ)
where
"Γ '⊢' τ '⋮t' T" := (term_under_type_check Γ τ T) and "Γ '⊢' τ '⋮v' T" := (value_under_type_check Γ τ T).

Scheme value_under_type_check_rec := Induction for value_under_type_check Sort Prop
    with term_under_type_check_rec := Induction for term_under_type_check Sort Prop.

Ltac soundness_simp :=
  match goal with
  | [H: _ ⊢WF _ |- _ ] => apply well_formed_regular in H; mydestr
  end.

Lemma ok_dctx_weaken: forall Γ1 Γ2 d, ok_dctx d (Γ1 ++ Γ2) -> ok_dctx d Γ1.
Proof.
  induction Γ1; simpl; intros; mydestr; auto.
  invclear H.
  - constructor; eauto; listctx_set_simpl; try fast_set_solver.
  - apply ok_dctx_cons_arr; eauto; listctx_set_simpl; try fast_set_solver.
Qed.

Lemma wf_ctxrR_weaken: forall Γ1 Γ2 st, wf_ctxrR st (Γ1 ++ Γ2) -> wf_ctxrR st Γ1.
Proof.
  induction Γ1; simpl; intros; mydestr; auto. constructor.
  invclear H. invclear H3; auto_ty_exfalso.
  - constructor; eauto. apply ok_dctx_weaken with (Γ2:=Γ2). listctx_set_simpl.
  - mydestr. constructor; eauto. apply ok_dctx_weaken with (Γ2:=Γ2). listctx_set_simpl.
  - constructor; eauto. apply ok_dctx_weaken with (Γ2:=Γ2). listctx_set_simpl.
Qed.

Lemma wf_ctxrR_implies_inner_closed_rty: forall Γ1 x τ_x Γ2 st,
    wf_ctxrR st (Γ1 ++ (x, τ_x) :: Γ2) -> closed_rty 0 (ctxdom ⦑Γ1⦒ ∪ dom aset st) τ_x.
Proof.
  induction Γ1; simpl; intros; mydestr; auto.
  - invclear H; refinement_solver.
    + invclear H3; refinement_solver.
    + invclear H5; refinement_solver.
  - invclear H; refinement_solver.
    + apply ok_dctx_regular2 in H3; mydestr.
      assert ((a, {v:B|n|d|ϕ}) :: Γ1 ++ (x, τ_x) :: Γ2 = ((a, {v:B|n|d|ϕ}) :: Γ1) ++ [(x, τ_x)] ++ Γ2) as Hz by listctx_set_simpl.
      apply H in Hz. inv_rd_simpl0.
    + apply ok_dctx_regular2 in H4; mydestr.
      assert ((a, [v:B|n|d|ϕ]) :: Γ1 ++ (x, τ_x) :: Γ2 = ((a, [v:B|n|d|ϕ]) :: Γ1) ++ [(x, τ_x)] ++ Γ2) as Hz by listctx_set_simpl.
      apply H1 in Hz. inv_rd_simpl0.
    + apply ok_dctx_regular2 in H5; mydestr.
      assert ((a, r) :: Γ1 ++ (x, τ_x) :: Γ2 = ((a, r) :: Γ1) ++ [(x, τ_x)] ++ Γ2) as Hz by listctx_set_simpl.
      apply H in Hz. dec_solver2. inv_rd_simpl0.
Qed.

Lemma rty_judgement_implies_wf: forall (Γ: listctx rty) e (τ: rty), Γ ⊢ e ⋮t τ -> Γ ⊢WF τ.
Proof.
  apply (term_under_type_check_rec
           (fun Γ v τ _ => Γ ⊢WF τ)
           (fun Γ e τ _ => Γ ⊢WF τ)); intros Γ; intros; mydestr; auto.
Qed.

Theorem soundness: forall (Γ: listctx rty) (e: tm) (τ: rty), Γ ⊢ e ⋮t τ -> ⅋{∅}⟦ τ ⟧{ Γ } e.
Proof.
  apply (term_under_type_check_rec
           (fun Γ v τ _ => ⅋{∅}⟦ τ ⟧{ Γ } v)
           (fun Γ e τ _ => ⅋{∅}⟦ τ ⟧{ Γ } e)); intros Γ; intros; mydestr; try soundness_simp.
  (* constant *)
  - apply inv_rRctx_pre_weakening_empty; auto. admit.
  (* var base *)
  - apply ctxfind_some_spec in H; auto; mydestr; subst.
    rewrite app_assoc.
    apply inv_rRctx_pre_weakening; refinement_solver.
    apply wf_ctxrR_implies_inv_var_over_last.
    eapply wf_ctxrR_weaken; listctx_set_simpl; eauto.
  - apply ctxfind_some_spec in H; auto; mydestr; subst.
    rewrite app_assoc.
    apply inv_rRctx_pre_weakening; refinement_solver.
    apply wf_ctxrR_implies_inv_var_under_last.
    eapply wf_ctxrR_weaken; listctx_set_simpl; eauto.
  (* var arr *)
  - apply ctxfind_some_spec in e; auto; mydestr; subst.
    rewrite app_assoc.
    apply inv_rRctx_pre_weakening; refinement_solver.
    apply wf_ctxrR_implies_inv_var_is_arr_last; auto.
    eapply wf_ctxrR_implies_inner_closed_rty; eauto.
    eapply wf_ctxrR_weaken; listctx_set_simpl; eauto.
  (* lam *)
  - auto_pose_fv x. repeat specialize_with x.
    eapply inv_rRctx_oarr; eauto; refinement_solver2.
    apply rty_judgement_implies_wf in t; eauto. soundness_simp; auto.
  - auto_pose_fv x. repeat specialize_with x.
    eapply inv_rRctx_arrarr; eauto; refinement_solver2.
    apply rty_judgement_implies_wf in t. soundness_simp; auto.
  (* fix *)
  - admit.
  (* value *)
  - auto.
  (* err *)
  - admit.
  (* sub *)
  - apply subtyping_spec with (e := e) in s; RD_simp2; auto.
    + RD_simp. apply inv_rRctx_pre_weakening_empty; auto.
    + constructor.
    + constructor; auto.
  (* eq *)
  - admit.
  (* merge *)
  - match goal with
    | [H: _ ⊢ _ ⩔ _ ⩵ _ |- _ ] => unfold disjunction in H; erewrite <- H; split; auto
    end.
  (* lete *)
  - auto_pose_fv x. repeat specialize_with x. eapply wf_implies_ctxrR_tlete_ubase_drop in H0; eauto. setoid_rewrite close_open_var_tm in H0; fast_set_solver.
    apply rty_judgement_implies_wf in t0. soundness_simp; auto.
    refinement_solver.
  - auto_pose_fv x. repeat specialize_with x. eapply wf_implies_ctxrR_tlete_is_arr_drop in H0; eauto. setoid_rewrite close_open_var_tm in H0; fast_set_solver.
    apply rty_judgement_implies_wf in t0. soundness_simp; auto.
    refinement_solver.
  (* letop *)
  - auto_pose_fv x. repeat specialize_with x.
    apply inv_implies_ctxrR_drop_last with (x:=x) (τ_x:= ({1 ~r> v2} (mk_op_ret op ^r^ v1))); eauto; try fast_set_solver.
    + apply rty_judgement_implies_wf in t. soundness_simp; auto.
    + refinement_solver.
    + eapply inv_rRctx_tletbiop; eauto; try fast_set_solver.
      apply rty_judgement_implies_wf in t. soundness_simp; auto.
  - auto_pose_fv x. repeat specialize_with x. admit.
  - auto_pose_fv x. repeat specialize_with x. mydestr.
    admit.
  (* matchb true *)
  - admit. (* easy *)
  - admit. (* easy *)
Admitted.
