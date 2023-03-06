From stdpp Require Import mapset.
From stdpp Require Import natmap.
From CT Require Import CoreLangProp.
From CT Require Import OperationalSemanticsProp.
From CT Require Import BasicTypingProp.
From CT Require Import SyntaxSugar.
From CT Require Import RefinementType.
From CT Require Import RefinementTypeDenotation.
From CT Require Import RefinementTypeDenotationTac.
From CT Require Import RefinementTypeDenotationProp.
From CT Require Import InvDenotation.
From CT Require Import InvDenotationProp1.
From CT Require Import InvDenotationProp2.
From CT Require Import InvDenotationProp3.
From CT Require Import InvDenotationProp4.
From CT Require Import CtxDenotationProp.

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
Import RefinementType.
Import RefinementTypeTac.
Import RefinementTypeDenotation.
Import RefinementTypeDenotationTac.
Import RefinementTypeDenotationProp.
Import WFCtxDenotation.
Import WFCtxDenotationProp.
Import InvDenotation.
Import InvDenotationProp1.
Import InvDenotationProp2.
Import InvDenotationProp3.
Import InvDenotationProp4.
Import CtxDenotation.
Import CtxDenotationProp.

(** Well-formedness *)
Inductive wf_typing: listctx rty -> rty -> Prop :=
| wf_typing_under_base: forall Γ b n d ϕ, wf_ctxrR_not_terr ∅ Γ -> closed_rty 0 (ctxdom ⦑Γ⦒ ) [v: b | n | d | ϕ] -> wf_typing Γ [v: b | n | d | ϕ]
| wf_typing_oarr: forall Γ b n d ϕ τ (L: aset),
    (forall x, x ∉ L -> wf_typing (Γ ++ [(x, [v: b | n | d | ϕ])]) (τ ^r^ x)) ->
    closed_rty 0 (ctxdom ⦑Γ⦒ ) (-:{v: b | n | d | ϕ }⤑ τ) ->
    wf_typing Γ (-:{v: b | n | d | ϕ }⤑ τ)
| wf_typing_arrarr: forall Γ τ1 τ2,
    is_arr τ1 ->
    wf_typing Γ τ1 -> wf_typing Γ τ2 ->
    closed_rty 0 (ctxdom ⦑Γ⦒ ) (τ1 ⤑ τ2) ->
    wf_typing Γ (τ1 ⤑ τ2).

Notation " Γ '⊢WF' τ " := (wf_typing Γ τ) (at level 20, τ constr, Γ constr).

Lemma wf_typing_implies_wf_ctxrR_not_terr (Γ: listctx rty) (τ: rty): Γ ⊢WF τ -> wf_ctxrR_not_terr ∅ Γ.
Proof.
  intros. induction H; auto. specialize_L.
  rewrite <- wf_ctxrR_iff_wf_ctxrR_not_terr. rewrite <- wf_ctxrR_iff_wf_ctxrR_not_terr in H0.
  apply wf_ctxrR_weaken in H0; auto.
Qed.

Lemma wf_typing_implies_not_overbasety (Γ: listctx rty) (τ: rty): Γ ⊢WF τ -> not_overbasety τ.
Proof.
  intros. induction H; simpl; auto.
Qed.

Lemma wf_typing_implies_closed_rty (Γ: listctx rty) (τ: rty): Γ ⊢WF τ -> closed_rty 0 (ctxdom ⦑Γ⦒ ) τ.
Proof.
  intros. induction H; auto.
Qed.

Lemma well_formed_regular: forall Γ τ,
  Γ ⊢WF τ -> wf_ctxrR ∅ Γ /\ closed_rty 0 (ctxdom ⦑Γ⦒ ) τ /\ not_overbasety τ.
Proof.
    intros. split. eapply wf_typing_implies_wf_ctxrR_not_terr in H; eauto.
    rewrite wf_ctxrR_iff_wf_ctxrR_not_terr; auto.
    split. apply wf_typing_implies_closed_rty; auto. eapply  wf_typing_implies_not_overbasety; eauto.
Qed.

Lemma wf_typing_implies_last_wf_ctxrR (Γ: listctx rty) b n d ϕ (τ: rty):
  wf_typing Γ (-:{v: b | n | d | ϕ }⤑ τ) ->
  (exists (L: aset), forall x, x ∉ L -> wf_ctxrR ∅ (Γ ++ [(x, [v:b|n|d|ϕ])])).
Proof.
  intros. invclear H. exists L. intros. specialize_with x.
  apply well_formed_regular in H3. mydestr; auto.
Qed.

Definition disjunction (Γ: listctx rty) (τ1 τ2 τ3: rty) :=
  forall e, (⅋{∅}⟦ τ1 ⟧{ Γ } e) /\ (⅋{∅}⟦ τ2 ⟧{ Γ } e) <-> (⅋{∅}⟦ τ3 ⟧{ Γ } e).

Notation " Γ '⊢' τ1 '⩔' τ2 '⩵' τ3 " := (disjunction Γ τ1 τ2 τ3) (at level 20, τ1 constr, τ2 constr, τ3 constr, Γ constr).

(** Subtyping *)
Inductive subtyping: listctx rty -> rty -> rty -> Prop :=
| subtyping_ubase: forall Γ b n d ϕ1 ϕ2,
    {∅}⟦ [v: b | n | d | ϕ1 ] ⟧⊆⟦ [v: b | n | d | ϕ2 ] ⟧{ Γ } ->
    subtyping Γ [v: b | n | d | ϕ1 ] [v: b | n | d | ϕ2 ]
| subtyping_obase: forall Γ b n d ϕ1 ϕ2,
    {∅}⟦ [v: b | n | d | ϕ2 ] ⟧⊆⟦ [v: b | n | d | ϕ1 ] ⟧{ Γ } ->
    subtyping Γ {v: b | n | d | ϕ1 } {v: b | n | d | ϕ2 }
| subtyping_oarr: forall Γ b n d ϕ1 ϕ2 τ1 τ2 (L: aset),
    subtyping Γ {v: b | n | d | ϕ2 } {v: b | n | d | ϕ1 } ->
    (forall x, x ∉ L -> subtyping (Γ ++ [(x, {v: b | n | d | ϕ2 })]) (τ1 ^r^ x) (τ2 ^r^ x)) ->
    closed_rty 0 (ctxdom ⦑ Γ ⦒) (-:{v: b | n | d | ϕ1 }⤑ τ1) ->
    closed_rty 0 (ctxdom ⦑ Γ ⦒) (-:{v: b | n | d | ϕ2 }⤑ τ2) ->
    subtyping Γ (-:{v: b | n | d | ϕ1 }⤑ τ1) (-:{v: b | n | d | ϕ2 }⤑ τ2)
| subtyping_arrarr: forall Γ τ11 τ12 τ21 τ22,
    is_arr τ21 -> is_arr τ11 ->
    subtyping Γ τ21 τ11 ->
    subtyping Γ τ12 τ22 ->
    closed_rty 0 (ctxdom ⦑ Γ ⦒) (τ11 ⤑ τ12) ->
    closed_rty 0 (ctxdom ⦑ Γ ⦒) (τ21 ⤑ τ22) ->
    subtyping Γ (τ11 ⤑ τ12) (τ21 ⤑ τ22).

Notation " Γ '⊢' τ1 '⪡' τ2 " := (subtyping Γ τ1 τ2) (at level 20, τ1 constr, τ2 constr, Γ constr).

Lemma subtyping_implies_not_overbasety:
  forall Γ τ1 τ2, Γ ⊢ τ1 ⪡ τ2 -> not_overbasety τ1 <-> not_overbasety τ2.
Proof.
  intros Γ τ1 τ2 H; induction H; split; intros; auto.
Qed.

Definition subtyping_spec_raw: forall Γ τ1 τ2,
    Γ ⊢ τ1 ⪡ τ2 -> {∅}⟦ τ1 ⟧⊆⟦ τ2 ⟧{ Γ }.
Proof.
  intros Γ τ1 τ2 H. induction H; auto. rewrite <- ctxrR2_under_over; auto.
  - auto_pose_fv x. repeat specialize_with x.
    apply inv_ctxrR2_oarr with (x:=x); eauto. fast_set_solver. closed_rty_solver.
    closed_rty_solver.
  - auto_pose_fv x. repeat specialize_with x.
    apply inv_ctxrR2_arrarr with (x:=x); eauto. fast_set_solver. closed_rty_solver.
    closed_rty_solver.
Qed.

Definition subtyping_spec: forall Γ τ1 τ2, Γ ⊢ τ1 ⪡ τ2 -> (forall e, ⅋{∅}⟦ τ1 ⟧{ Γ } e -> ⅋{∅}⟦ τ2 ⟧{ Γ } e).
Proof.
  intros. apply subtyping_spec_raw in H.
  eapply ctxrR2_implies_inv_ctxrR in H; eauto.
Qed.

Reserved Notation "Γ '⊢' τ '⋮t' T" (at level 20).
Reserved Notation "Γ '⊢' τ '⋮v' T"  (at level 20).

(** Typing *)
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
(** operators only take nat type *)
| UT_LetOp: forall (Γ: listctx rty) op (v1 v2: value) e τ d1 d2 ϕ1 ϕ2 (L: aset),
    Γ ⊢WF τ ->
    Γ ⊢ v1 ⋮v [v: fst_ty_of_op op | 0 | d1 | ϕ1 ] ->
    Γ ⊢ v2 ⋮v [v: snd_ty_of_op op | 0 | d2 | ϕ2 ] ->
    (forall x, x ∉ L -> (Γ ++ [(x, ({1 ~r> v2} ({0 ~r> v1} (mk_op_ret op))))]) ⊢ (e ^t^ x) ⋮t τ) ->
    Γ ⊢ (tletbiop op v1 v2 e) ⋮t τ
| UT_LetAppIndepend: forall Γ (v1 v2: value) e τ τ2 τ_x (L: aset),
    Γ ⊢WF τ -> Γ ⊢ v1 ⋮v (τ2 ⤑ τ_x) -> Γ ⊢ v2 ⋮v τ2 -> (forall x, x ∉ L -> (Γ ++ [(x, τ_x)]) ⊢ (e ^t^ x) ⋮t τ) ->
    Γ ⊢ (tletapp v1 v2 e) ⋮t τ
(** the value should only be constant or variables *)
| UT_LetAppDepend: forall Γ (v1 v2: value) e τ b d ϕ τ_x (L: aset),
    Γ ⊢WF τ ->
    Γ ⊢ v1 ⋮v (-:{v: b | 0 | d | ϕ}⤑ τ_x) -> Γ ⊢ v2 ⋮v [v: b | 0 | d | ϕ] ->
    (forall x, x ∉ L -> (Γ ++ [(x, τ_x ^r^ v2)]) ⊢ (e ^t^ x) ⋮t τ ) ->
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

Lemma rty_judgement_implies_wf: forall (Γ: listctx rty) e (τ: rty), Γ ⊢ e ⋮t τ -> Γ ⊢WF τ.
Proof.
  apply (term_under_type_check_rec
           (fun Γ v τ _ => Γ ⊢WF τ)
           (fun Γ e τ _ => Γ ⊢WF τ)); intros Γ; intros; mydestr; auto.
Qed.

Lemma rty_judgement_implies_wf_value: forall (Γ: listctx rty) e (τ: rty), Γ ⊢ e ⋮v τ -> Γ ⊢WF τ.
Proof.
  apply (value_under_type_check_rec
           (fun Γ v τ _ => Γ ⊢WF τ)
           (fun Γ e τ _ => Γ ⊢WF τ)); intros Γ; intros; mydestr; auto.
Qed.
