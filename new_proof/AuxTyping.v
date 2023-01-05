From stdpp Require Import mapset.
From stdpp Require Import natmap.
From CT Require Import CoreLangProp.
From CT Require Import OperationalSemanticsProp.
From CT Require Import BasicTypingProp.
From CT Require Import SyntaxSugar.
From CT Require Import Refinement.
From CT Require Import RefinementDenotation.

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

(* Well Formedness *)

Inductive wf_ctx: listctx rty -> Prop :=
| wf_ctx_nil: wf_ctx []
| wf_ctx_cons_ubase: forall Γ x b n d ϕ,
    wf_ctx Γ ->
    x ∉ ctxdom Γ ->
    ~ (⟦ [v: b | n | d | ϕ ] ⟧{ Γ } terr) ->
    wf_ctx (Γ ++ [(x, [v: b | n | d | ϕ ])])
| wf_ctx_cons: forall Γ x τ,
    wf_ctx Γ ->
    x ∉ ctxdom Γ ->
    not_underbasety τ ->
    wf_ctx (Γ ++ [(x, τ)]).

Lemma err_exists_denotation_excluded: forall Γ b n d ϕ,
    (∃ e, ⟦ [v: b | n | d | ϕ ] ⟧{ Γ } e) <-> ~ (⟦ [v: b | n | d | ϕ ] ⟧{ Γ } terr).
Admitted.

Lemma wf_ctx_implies_ok: forall Γ, wf_ctx Γ -> ok Γ.
Admitted.

Lemma wf_ctx_implies_valid: forall Γ x τ,  wf_ctx Γ -> ctxfind Γ x = Some τ -> valid_rty τ.
Admitted.

Lemma wf_ctx_implies_lc_rty: forall Γ x τ,  wf_ctx Γ -> ctxfind Γ x = Some τ -> lc_rty τ.
Admitted.

Lemma wf_ctx_regular:
  ∀ Γ, wf_ctx Γ -> ok Γ /\ (forall x τ, ctxfind Γ x = Some τ -> valid_rty τ) /\ (forall x τ, ctxfind Γ x = Some τ -> lc_rty τ).
Admitted.

Lemma wf_ctx_implies_mid_closed: forall Γ1 x τ Γ2, wf_ctx (Γ1 ++ [(x, τ)] ++ Γ2) -> (rty_fv τ) ⊆ (ctxdom Γ1).
Admitted.

Lemma wf_ctx_implies_inhabitant_exists: forall Γ1 x τ Γ2, wf_ctx (Γ1 ++ [(x, τ)] ++ Γ2) -> (exists e, ⟦ τ ⟧{ Γ1 } e).
Admitted.

Definition wf (Γ: listctx rty) (τ: rty) := wf_ctx Γ /\ closed_rty 0 (ctxdom Γ) τ.

Notation " Γ '⊢WF' τ " := (wf Γ τ) (at level 20, τ constr, Γ constr).

(* Disjunction *)

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

Definition subtyping_spec: forall Γ τ1 τ2, Γ ⊢WF τ2 -> Γ ⊢ τ1 ⪡ τ2 -> (forall e, ⟦ τ1 ⟧{ Γ } e -> ⟦ τ2 ⟧{ Γ } e).
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
    (forall x, x ∉ L -> (Γ ++ [(x, ((mk_op_ret op) ^r^ v1) ^r^ v2)]) ⊢ e ⋮t τ) ->
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

Lemma rRctx_pre_weakening: forall Γ1 Γ2 τ, (Γ1 ++ Γ2) ⊢WF τ -> (forall e, ⟦τ⟧{Γ1} e -> ⟦τ⟧{Γ1 ++ Γ2} e).
Admitted.

Lemma rRctx_weakening_empty: forall Γ τ, Γ ⊢WF τ -> (forall e, ⟦τ⟧ e -> ⟦τ⟧{Γ} e).
Admitted.

Lemma rRctx_backward_over: forall Γ z b n d ϕ (e: tm) τ,
    ⟦ τ ⟧{ Γ ++ [(z, {v: b | n | d | ϕ})] } e <->
    (forall (u: value), ⟦ {v: b | n | d | ϕ} ⟧{ Γ } u -> ⟦ { z := u }r τ ⟧{ Γ } ({ z := u }t e)).
Admitted.

Lemma rRctx_backward_under: forall Γ z τ_z (e: tm) τ,
    ~ not_overbasety τ_z ->
    ⟦ τ ⟧{ Γ ++ [(z, τ_z)] } e <->
      (∃ (u_hat: tm), ⟦ τ_z ⟧{ Γ } u_hat /\
                        (forall (u: tm), ⟦ τ_z ⟧{ Γ } u ->
                                    (∀ (v_u: value), u_hat ↪* v_u -> ⟦ { z:= v_u }r τ ⟧{ Γ } (tlete u (e ^t^ z)))
                        )).
Admitted.

Theorem soundness: forall (Γ: listctx rty) (e: tm) (τ: rty), Γ ⊢ e ⋮t τ -> ⟦ τ ⟧{ Γ } e.
Proof.
  apply (term_under_type_check_rec
           (fun Γ v τ _ => ⟦ τ ⟧{ Γ } v)
           (fun Γ e τ _ => ⟦ τ ⟧{ Γ } e)); intros Γ; intros; mydestr.
  (* constant *)
  - apply rRctx_weakening_empty; auto. admit.
  (* var base *)
  - apply ctxfind_some_spec in H; auto; mydestr; subst.
    rewrite app_assoc. apply rRctx_pre_weakening; auto. admit. rewrite rRctx_backward_over.
    intros. apply rRctx_weakening_empty. admit. admit. admit.
  - apply ctxfind_some_spec in H; auto; mydestr; subst.
    rewrite app_assoc. apply rRctx_pre_weakening; auto. admit. rewrite rRctx_backward_under. admit. admit. admit.
  (* var arr *)
  - apply ctxfind_some_spec in e; auto; mydestr; subst.
    rewrite app_assoc. apply rRctx_pre_weakening; auto. admit. rewrite rRctx_backward_under. admit. admit. admit.
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
  - repeat match goal with
    | [H: ⟦ ?τ ⟧{ [] } ?e |- ⟦ ?τ ⟧{ ?Γ } ?e ] => invclear H; apply rRctx_weakening_empty; auto
    | [H: _ ⊢ _ ⪡ _ |- _ ?e ] => apply subtyping_spec with (e := e) in H
    end. admit. admit.
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
