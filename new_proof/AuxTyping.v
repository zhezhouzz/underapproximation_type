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
Import RefinementDenotation.
Import RefinementDenotationTac.
Import RefinementDenotationProp.

Lemma wf_ctxrR_implies_ok: forall Γ, wf_ctxrR ∅ Γ -> ok Γ.
Proof.
  induction Γ; intros; auto.
  invclear H; refinement_solver7.
Qed.

Global Hint Resolve wf_ctxrR_implies_ok: core.

Definition wf (Γ: listctx rty) (τ: rty) :=
  wf_ctxrR_not_terr ∅ Γ /\ closed_rty 0 (ctxdom Γ) τ /\ not_overbasety τ.

Notation " Γ '⊢WF' τ " := (wf Γ τ) (at level 20, τ constr, Γ constr).

Lemma well_formed_regular: forall Γ τ,
  Γ ⊢WF τ -> wf_ctxrR ∅ Γ /\ closed_rty 0 (ctxdom Γ) τ /\ not_overbasety τ.
Proof.
  intros. destruct H. setoid_rewrite wf_ctxrR_iff_wf_ctxrR_not_terr.
  split; auto.
Qed.

Definition disjunction (Γ: listctx rty) (τ1 τ2 τ3: rty) := forall e, (⟦ τ1 ⟧{ Γ } e) /\ (⟦ τ2 ⟧{ Γ } e) <-> (⟦ τ3 ⟧{ Γ } e).

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

Definition subtyping_spec: forall Γ τ1 τ2, wf_ctxrR ∅ Γ -> Γ ⊢ τ1 ⪡ τ2 -> (forall e, ⟦ τ1 ⟧{ Γ } e -> ⟦ τ2 ⟧{ Γ } e).
Admitted.

Reserved Notation "Γ '⊢' τ '⋮t' T" (at level 20).
Reserved Notation "Γ '⊢' τ '⋮v' T"  (at level 20).

Inductive term_under_type_check : listctx rty -> tm -> rty -> Prop :=
| UT_Value: forall Γ v τ, Γ ⊢ v ⋮v τ -> Γ ⊢ v ⋮t τ
| UT_terr: forall Γ b, Γ ⊢WF (mk_under_bot b) -> Γ ⊢ terr ⋮t (mk_under_bot b)
| UT_Sub: forall Γ e (τ1 τ2:rty), Γ ⊢WF τ2 -> [] ⊢ e ⋮t τ1 -> [] ⊢ τ1 ⪡ τ2 ->  (Γ ⊢ e ⋮t τ2)
| UT_Eq: forall Γ e τ1 τ2, Γ ⊢WF τ2 -> Γ ⊢ e ⋮t τ1 -> Γ ⊢ τ1 ⪡ τ2 -> Γ ⊢ τ2 ⪡ τ1 -> Γ ⊢ e ⋮t τ2
| UT_Merge: forall Γ e (τ1 τ2 τ3: rty), Γ ⊢WF τ3 -> Γ ⊢ e ⋮t τ1 -> Γ ⊢ e ⋮t τ2 -> Γ ⊢ τ1 ⩔ τ2 ⩵ τ3 -> Γ ⊢ e ⋮t τ3
| UT_Lete: forall Γ e_x e τ τ_x (L: aset),
    Γ ⊢WF τ -> Γ ⊢ e_x ⋮t τ_x -> (forall x, x ∉ L -> (Γ ++ [(x, τ_x)]) ⊢ (e ^t^ x) ⋮t τ ) -> Γ ⊢ (tlete e_x e) ⋮t τ
(* operators only take nat type *)
| UT_LetOp: forall (Γ: listctx rty) op (v1 v2: value) e τ d1 d2 ϕ1 ϕ2 (L: aset),
    Γ ⊢WF τ ->
    Γ ⊢ v1 ⋮v [v: fst_ty_of_op op | 0 | d1 | ϕ1 ] ->
    Γ ⊢ v2 ⋮v [v: snd_ty_of_op op | 0 | d2 | ϕ2 ] ->
    (forall x, x ∉ L -> (Γ ++ [(x, ((mk_op_ret op) ^r^ v1) ^r^ v2)]) ⊢ (e ^t^ x) ⋮t τ) ->
    Γ ⊢ (tletbiop op v1 v2 e) ⋮t τ
| UT_LetAppIndepend: forall Γ (v1 v2: value) e τ τ2 τ_x (L: aset),
    Γ ⊢WF τ -> Γ ⊢ v1 ⋮v (τ2 ⤑ τ_x) -> Γ ⊢ v2 ⋮v τ2 -> (forall x, x ∉ L -> (Γ ++ [(x, τ_x)]) ⊢ (e ^t^ x) ⋮t τ) ->
    Γ ⊢ (tletapp v1 v2 e) ⋮t τ
(* the value should only be constant or variables *)
| UT_LetAppDepend: forall Γ (v1 v2: value) e τ b d ϕ τ_x (L: aset),
    Γ ⊢WF τ -> Γ ⊢ v1 ⋮v (-:{v: b | 0 | d | ϕ}⤑ τ_x) -> Γ ⊢ v2 ⋮v [v: b | 0 | d | ϕ] ->
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

(* Lemma rRctx_backward_over: forall Γ z b n d ϕ (e: tm) τ, *)
(*     ⟦ τ ⟧{ Γ ++ [(z, {v: b | n | d | ϕ})] } e <-> *)
(*     (forall (u: value), ⟦ {v: b | n | d | ϕ} ⟧{ Γ } u -> ⟦ { z := u }r τ ⟧{ Γ } ({ z := u }t e)). *)
(* Admitted. *)

(* Lemma rRctx_backward_under: forall Γ z τ_z (e: tm) τ, *)
(*     ~ not_overbasety τ_z -> *)
(*     ⟦ τ ⟧{ Γ ++ [(z, τ_z)] } e <-> *)
(*       (∃ (u_hat: tm), ⟦ τ_z ⟧{ Γ } u_hat /\ *)
(*                         (forall (u: tm), ⟦ τ_z ⟧{ Γ } u -> *)
(*                                     (∀ (v_u: value), u_hat ↪* v_u -> ⟦ { z:= v_u }r τ ⟧{ Γ } (tlete u (e ^t^ z))) *)
(*                         )). *)
(* Admitted. *)

Ltac soundness_simp :=
  match goal with
  | [H: _ ⊢WF _ |- _ ] => apply well_formed_regular in H; mydestr
  end.

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

(* Theorem judge_implies_not_overbasety: *)
(*   forall (Γ: listctx rty) (e: tm) (τ: rty), Γ ⊢ e ⋮t τ -> not_overbasety τ. *)
(* Proof. *)
(*   apply (term_under_type_check_rec *)
(*            (fun Γ v τ _ => not_overbasety τ) *)
(*            (fun Γ e τ _ => not_overbasety τ)); intros Γ; intros; mydestr; simpl; auto; *)
(*     try match goal with *)
(*     | [H: _ ⊢ ?τ1 ⪡ ?τ2 |- not_overbasety ?τ2] => *)
(*         apply subtyping_implies_not_overbasety in H; rewrite <- H; auto *)
(*     | [H: _ ⊢ ?τ1 ⪡ ?τ2 |- not_overbasety ?τ1] => *)
(*         apply subtyping_implies_not_overbasety in H; rewrite H; auto *)
(*     end. *)
(*   - admit. *)
(*   - auto_pose_fv x; repeat specialize_with x; auto. *)
(*   - auto_pose_fv x; repeat specialize_with x; auto. *)
(*   - auto_pose_fv x; repeat specialize_with x; auto. *)
(*   - auto_pose_fv x; repeat specialize_with x; auto. *)
(* Admitted. *)

    (* rewrite rR_shadow_update_st in H11. admit. refinement_solver. *)
    (* setoid_rewrite close_open_var_tm in H11. admit. refinement_solver. *)




Theorem soundness: forall (Γ: listctx rty) (e: tm) (τ: rty), Γ ⊢ e ⋮t τ -> ⟦ τ ⟧{ Γ } e.
Proof.
  (* intros. assert (not_overbasety τ) by (eapply judge_implies_not_overbasety; eauto). *)
  (* generalize dependent τ. generalize dependent e. generalize dependent Γ. *)
  apply (term_under_type_check_rec
           (fun Γ v τ _ => ⟦ τ ⟧{ Γ } v)
           (fun Γ e τ _ => ⟦ τ ⟧{ Γ } e)); intros Γ; intros; mydestr; try soundness_simp.
  (* constant *)
  - apply rRctx_pre_weakening_empty; auto. admit.
  (* var base *)
  - apply ctxfind_some_spec in H; auto; mydestr; subst.
    rewrite app_assoc. apply rRctx_pre_weakening; refinement_solver.
    admit.
  - apply ctxfind_some_spec in H; auto; mydestr; subst.
    rewrite app_assoc. apply rRctx_pre_weakening; refinement_solver.
    admit.
  (* var arr *)
  - apply ctxfind_some_spec in e; auto; mydestr; subst.
    rewrite app_assoc. apply rRctx_pre_weakening; refinement_solver. admit.
  (* lam *)
  - auto_pose_fv x. repeat specialize_with x. admit.
  - auto_pose_fv x. repeat specialize_with x. admit.
  (* fix *)
  - admit.
  (* value *)
  - auto.
  (* err *)
  - admit.
  (* sub *)
  - apply subtyping_spec with (e := e) in s; RD_simp; auto.
    + apply rRctx_pre_weakening_empty; auto.
    + constructor.
  (* eq *)
  - eapply subtyping_spec; eauto.
  (* merge *)
  - match goal with
    | [H: _ ⊢ _ ⩔ _ ⩵ _ |- _ ] => unfold disjunction in H; erewrite <- H; split; auto
    end.
  (* lete *)
  - auto_pose_fv x. repeat specialize_with x. admit.
  (* letop *)
  - auto_pose_fv x. repeat specialize_with x. admit.
  - auto_pose_fv x. repeat specialize_with x. admit.
  - auto_pose_fv x. repeat specialize_with x. admit.
  (* matchb true *)
  - admit.
  - admit.
Admitted.
