From stdpp Require Import mapset.
From CT Require Import CoreLangClass.
From CT Require Import OperationalSemantics.
From CT Require Import BasicTypingClass.
From CT Require Import RefinementTypeClass.
From CT Require Import Denotation.
From CT Require Import DenotationCtx.
From CT Require Import InstantiationProp.

Import Atom.
Import CoreLang.
Import Tactics.
Import NamelessTactics.
Import ListCtx.
Import OperationalSemantics.
Import BasicTyping.
Import RefinementType.
Import Denotation.
Import DenotationCtx.
Import Instantiation.
Import Qualifier.
Import InstantiationProp.

(** This file formalizes the type system of λᴱ and the main metatheoretic
  results. *)

Inductive wf_rty: listctx rty -> rty -> Prop :=
| WfBaseOver: forall Γ b ϕ,
    closed_rty (ctxdom Γ) {: b | ϕ } -> wf_rty Γ {: b | ϕ }
| WfBaseUnder: forall Γ b ϕ,
    closed_rty (ctxdom Γ) [: b | ϕ ] -> wf_rty Γ [: b | ϕ ]
| WfArr: forall Γ ρ τ (L: aset),
    wf_rty Γ ρ ->
    (forall x, x ∉ L -> wf_rty (Γ ++ [(x, ρ)]) (τ ^^ x)) ->
    wf_rty Γ (ρ ⇨ τ).

Notation " Γ '⊢WF' τ " := (wf_rty Γ τ) (at level 20, τ constr, Γ constr).

(** Semantic subtyping *)
(* Instead of the syntactic subtyping relation presented in the paper, we use
semantic subtyping in the mechanization. *)

Definition subtyping (Γ: listctx rty) (τ1 τ2: rty) : Prop :=
  (* Assume [τ1] and [τ2] are valid [rty]s. *)
  ⌊ τ1 ⌋ = ⌊ τ2 ⌋ /\
    forall e, ⟪ Γ ⟫ (fun Γv => ⟦m{ Γv } τ1⟧ e) →
         ⟪ Γ ⟫ (fun Γv => ⟦m{ Γv } τ2⟧ e).

Notation " Γ '⊢' τ1 '<⋮' τ2 " := (subtyping Γ τ1 τ2) (at level 20, τ1 constr, τ2 constr, Γ constr).

Reserved Notation "Γ '⊢' e '⋮t' τ" (at level 20).
Reserved Notation "Γ '⊢' e '⋮v' τ"  (at level 20).

(** Disjunction *)
Definition disjunction (Γ: listctx rty) (τ1 τ2 τ3: rty) :=
  forall e, ⟪ Γ ⟫ (fun Γv => ⟦m{ Γv } τ1⟧ e /\ ⟦m{ Γv } τ2⟧ e <-> ⟦m{ Γv } τ3⟧ e).

Notation " Γ '⊢' τ1 '⩔' τ2 '⩵' τ3 " := (disjunction Γ τ1 τ2 τ3) (at level 20, τ1 constr, τ2 constr, τ3 constr, Γ constr).

Definition builtin_op_ret_relation (op: op) (v_in v_out: constant) :=
  match op with
  | op_plus_one =>
      (match v_in with
       | cnat n => v_out = (n + 1)
       | _ => False
      end)
  | op_minus_one =>
      (match v_in with
       | cnat n => v_out = (n - 1)
       | _ => False
      end)
  | op_eq_zero =>
      (match v_in with
       | 0 => v_out = false
       | _ => v_out = true
      end)
  | op_rannat =>
      (match v_out with
       | cnat _ => True
       | _ => False
      end)
  | op_ranbool =>
      (match v_out with
       | cbool _ => True
       | _ => False
      end)
  end.

Definition builtin_op_para_rty (op: op) :=
  match op with
  | op_plus_one => {:TNat | mk_q_under_top}
  | op_minus_one =>
      {:TNat | qual [# vbvar 0]
                 (fun v => match (v !!! 0) with
                        | cnat n => n > 0
                        | _ => False
                        end)%fin
      }
  | op_eq_zero => {:TNat | mk_q_under_top}
  | op_rannat => {:TNat | mk_q_under_top}
  | op_ranbool => {:TNat | mk_q_under_top}
  end.

Definition builtin_typing_relation (op: op) :=
  (builtin_op_para_rty op) ⇨
    [:(ret_ty_of_op op) | qual [# vbvar 0; vbvar 1]
               (fun v => builtin_op_ret_relation op (v !!! 0) (v !!! 1))%fin].

(** Typing rules for values and terms. Values always have refinement types, while
  terms always have Hoare automata types. *)
Inductive term_type_check : listctx rty -> tm -> rty -> Prop :=
| TValue : forall Γ (v: value) τ, Γ ⊢ v ⋮v τ -> Γ ⊢ v ⋮t τ
(* | TErr : forall Γ (b: base_ty), Γ ⊢WF (mk_bot b) -> Γ ⊢ terr ⋮t (mk_bot b) *)
| TSub: forall Γ e (τ1 τ2: rty),
    Γ ⊢WF τ2 ->
    Γ ⊢ e ⋮t τ1 ->
    Γ ⊢ τ1 <⋮ τ2 ->
    Γ ⊢ e ⋮t τ2
| TMerge: forall Γ e (τ1 τ2 τ3: rty), Γ ⊢WF τ3 -> Γ ⊢ e ⋮t τ1 -> Γ ⊢ e ⋮t τ2 -> Γ ⊢ τ1 ⩔ τ2 ⩵ τ3 -> Γ ⊢ e ⋮t τ3
| TLetE: forall Γ e_x e τ τ_x (L: aset),
    Γ ⊢WF τ -> Γ ⊢ e_x ⋮t τ_x ->
    (forall x, x ∉ L -> (Γ ++ [(x, τ_x)]) ⊢ (e ^^ x) ⋮t τ ) ->
    Γ ⊢ (tlete e_x e) ⋮t τ
| TLetAppBase: forall Γ (v1 v2: value) e τ b ϕ τ_x (L: aset),
    Γ ⊢WF τ ->
    Γ ⊢ v1 ⋮v ({:b|ϕ} ⇨ τ_x) ->
    Γ ⊢ v2 ⋮v [:b|ϕ] ->
    (forall x, x ∉ L -> (Γ ++ [(x, τ_x ^^ v2)]) ⊢ (e ^^ x) ⋮t τ) ->
    Γ ⊢ (tletapp v1 v2 e) ⋮t τ
| TLetAppArrArr: forall Γ (v1 v2: value) e τ τ2 τ_x (L: aset),
    Γ ⊢WF τ ->
    Γ ⊢ v1 ⋮v (τ2 ⇨ τ_x) ->
    Γ ⊢ v2 ⋮v τ2 ->
    (forall x, x ∉ L -> (Γ ++ [(x, τ_x)]) ⊢ (e ^^ x) ⋮t τ) ->
    Γ ⊢ (tletapp v1 v2 e) ⋮t τ
| TLetOp: forall Γ op (v: value) e τ τ_x ϕ (L: aset),
    Γ ⊢WF τ ->
    Γ ⊢ v ⋮v [:TNat | ϕ ] ->
    builtin_typing_relation op = ({:TNat | ϕ} ⇨ τ_x) ->
    (forall x, x ∉ L -> (Γ ++ [(x, τ_x ^^ v)]) ⊢ (e ^^ x) ⋮t τ) ->
    Γ ⊢ (tletop op v e) ⋮t τ
| TMatchb: forall Γ (v: value) e1 e2 ϕ τ (L : aset),
    Γ ⊢WF τ ->
    Γ ⊢ v ⋮v {:TBool | ϕ} ->
    (* We can also directly encode the path condition without mentioning [x]: *)
(*     {: TNat | (qual [# v] (fun V => V !!! 0 = (cbool true))%fin) & ϕ ^q^ v} *)
    (forall x, x ∉ L -> (Γ ++ [(x, {: TBool | b0:c=true & b0:v= v & ϕ})]) ⊢ e1 ⋮t τ) ->
    (forall x, x ∉ L -> (Γ ++ [(x, {: TBool | b0:c=false & b0:v= v & ϕ})]) ⊢ e2 ⋮t τ) ->
    Γ ⊢ (tmatchb v e1 e2) ⋮t τ
with value_type_check : listctx rty -> value -> rty -> Prop :=
| TConst: forall Γ (c: constant),
    Γ ⊢WF (mk_eq_constant c) ->
    Γ ⊢ c ⋮v (mk_eq_constant c)
| TBaseVarOver: forall Γ (x: atom) b ϕ,
    Γ ⊢WF (mk_eq_var b x) ->
    ctxfind Γ x = Some {: b | ϕ} ->
    Γ ⊢ x ⋮v (mk_eq_var b x)
| TBaseVarUnder: forall Γ (x: atom) b ϕ,
    Γ ⊢WF (mk_eq_var b x) ->
    ctxfind Γ x = Some [: b | ϕ] ->
    Γ ⊢ x ⋮v (mk_eq_var b x)
| TFuncVar: forall Γ (x: atom) ρ τ,
    Γ ⊢WF (ρ ⇨ τ) ->
    ctxfind Γ x = Some (ρ ⇨ τ) ->
    Γ ⊢ x ⋮v (ρ ⇨ τ)
| TFun: forall Γ Tx ρ e τ (L: aset),
    Γ ⊢WF (ρ ⇨ τ) ->
    (forall x, x ∉ L -> (Γ ++ [(x, ρ)]) ⊢ (e ^^ x) ⋮t (τ ^^ x)) ->
    Tx = ⌊ ρ ⌋ ->
    Γ ⊢ (vlam Tx e) ⋮v (ρ ⇨ τ)
(* | TFix: forall Γ (Tx : base_ty) ϕ e τ T (L: aset), *)
(*     Γ ⊢WF ({: Tx | ϕ} ⇨ τ) -> *)
(*     (* NOTE: should not open the whole type, because the first argument (bound *)
(*     variable 0) of the return type is not the fixed point function but [{: Tx | *)
(*     ϕ}]. The return type should be opened by [x]. *) *)
(*     (forall x, x ∉ L -> *)
(*           (Γ ++ [(x, {: Tx | ϕ})]) ⊢ (vlam (Tx ⤍ T) e) ^v^ x ⋮v (({: Tx | b0:x≺ x & ϕ} ⇨ τ) ⇨ (τ ^h^ x))) -> *)
(*     T = ⌊ τ ⌋ -> *)
(*     Γ ⊢ (vfix (Tx ⤍ T) (vlam (Tx ⤍ T) e)) ⋮v ({: Tx | ϕ} ⇨ τ) *)
where
"Γ '⊢' e '⋮t' τ" := (term_type_check Γ e τ) and "Γ '⊢' e '⋮v' ρ" := (value_type_check Γ e ρ).


Scheme value_term_type_check_ind := Minimality for value_type_check Sort Prop
    with term_value_type_check_ind := Minimality for term_type_check Sort Prop.
Combined Scheme value_term_type_check_mutind from
  value_term_type_check_ind, term_value_type_check_ind.

(** * Properties of the type system *)

Lemma subtyping_preserves_basic_typing Γ τ1 τ2 :
  Γ ⊢ τ1 <⋮ τ2 ->
  ⌊τ1⌋ = ⌊τ2⌋.
Proof.
  qauto.
Qed.

Lemma effop_typing_preserves_basic_typing op :
  ⌊builtin_typing_relation op⌋ = ty_of_op op.
Proof.
  inversion 0; subst; destruct op; simpl; eauto.
Qed.

Lemma value_typing_regular_wf : forall (Γ: listctx rty) (v: value) (ρ: rty),
    Γ ⊢ v ⋮v ρ -> wf_rty Γ ρ
with tm_typing_regular_wf : forall (Γ: listctx rty) (e: tm) (τ: rty),
    Γ ⊢ e ⋮t τ -> wf_rty Γ τ.
Proof.
  all: destruct 1; eauto.
Qed.

Lemma value_tm_typing_regular_basic_typing:
  (forall (Γ: listctx rty) (v: value) (ρ: rty),
    Γ ⊢ v ⋮v ρ -> ⌊ Γ ⌋* ⊢t v ⋮v ⌊ ρ ⌋) /\
  (forall (Γ: listctx rty) (e: tm) (τ: rty),
    Γ ⊢ e ⋮t τ -> ⌊ Γ ⌋* ⊢t e ⋮t ⌊ τ ⌋).
Proof.
Admitted.

Lemma value_typing_regular_basic_typing: forall (Γ: listctx rty) (v: value) (ρ: rty),
    Γ ⊢ v ⋮v ρ -> ⌊ Γ ⌋* ⊢t v ⋮v ⌊ ρ ⌋.
Proof.
  apply value_tm_typing_regular_basic_typing.
Qed.

Lemma tm_typing_regular_basic_typing: forall (Γ: listctx rty) (e: tm) (τ: rty),
    Γ ⊢ e ⋮t τ -> ⌊ Γ ⌋* ⊢t e ⋮t ⌊ τ ⌋.
Proof.
  apply value_tm_typing_regular_basic_typing.
Qed.

Lemma ctxRst_ctxfind Γ x ρ :
  ctxfind Γ x = Some ρ ->
  ctxRst Γ (fun Γv => exists (v : value), Γv !! x = Some v /\ ⟦ m{ Γv } ρ ⟧ v).
Proof.
Admitted.

Lemma wf_rty_closed Γ ρ : wf_rty Γ ρ -> closed_rty (ctxdom Γ) ρ.
Admitted.

Lemma denotation_application_lam Tx T ρ τ e :
  Tx ⤍ T = ⌊ ρ⇨τ ⌋ ->
  ∅ ⊢t vlam Tx e ⋮t Tx ⤍ T ->
  closed_rty ∅ (ρ⇨τ) ->
  (forall (v_x : value), ⟦ρ⟧ v_x -> ⟦τ ^^ v_x⟧ (e ^^ v_x)) ->
  ⟦ρ⇨τ⟧ (vlam Tx e).
Proof.
Admitted.

(** * Main metatheoretic results *)

(** Convert an event operator to a value:
  [op] is [fun x => leteffop y = op x in y] *)
Definition value_of_op op : value :=
  vlam TNat (tletop op (vbvar 0) (vbvar 0)).

(** Well-formed built-in operator typing context (Definition 4.7) *)
(* We simply treat the event operator as a value. This is equivalent to the
definition in the paper (if we expand the denotation of this value). *)
Definition well_formed_builtin_typing :=
  forall op, ⟦ builtin_typing_relation op ⟧ (value_of_op op).

Lemma msubst_value_of_op Γv op :
  m{Γv} (value_of_op op) = value_of_op op.
Proof.
  rewrite msubst_fresh_value. eauto.
  my_set_solver.
Qed.

Lemma value_of_op_regular_basic_typing op:
  ∅ ⊢t value_of_op op ⋮v ty_of_op op.
Proof.
  econstructor.
  simpl. instantiate (1:=∅). intros.
  econstructor. econstructor. simplify_map_eq. reflexivity. reflexivity.
  instantiate_atom_listctx.
  simpl. econstructor. econstructor. simplify_map_eq. reflexivity.
Qed.

(* At some point the proof is very slow without marking [msubst] opaque. *)
Opaque msubst.

(** Fundamental theorem for event operator typing *)
Lemma builtin_fundamental:
  forall (Γ: listctx rty) (op: op),
    ctxRst Γ (fun Γv =>
                ⟦ m{ Γv } (builtin_typing_relation op) ⟧ (value_of_op op)).
Proof.
Admitted.

Lemma wf_rty_implies_strengthen (Γ: listctx rty) (τ: rty) (p: env -> Prop) :
  wf_rty Γ τ -> (forall Γv, closed_env Γv -> p Γv) -> ⟪ Γ ⟫ (fun Γv => p Γv).
Admitted.

Lemma emptyctx_closed: closed_env ∅.
Proof.
  unfold closed_env. unfold map_Forall.
  intuition. inversion H.
Qed.

Global Hint Resolve emptyctx_closed: core.

Ltac msubst_simp :=
  match goal with
  | H: context [ m{ _ } (tlete _ _) ] |- _ => setoid_rewrite msubst_lete in H
  | |- context [ m{ _ } (tlete _ _) ] => setoid_rewrite msubst_lete
  | H: context [ m{ _ } (tletop _ _ _) ] |- _ => setoid_rewrite msubst_tletop in H
  | |- context [ m{ _ } (tletop _ _ _) ] => setoid_rewrite msubst_tletop
  | H: context [ m{ _ } (tletapp _ _ _) ] |- _ => setoid_rewrite msubst_tletapp in H
  | |- context [ m{ _ } (tletapp _ _ _) ] => setoid_rewrite msubst_tletapp
  | H: context [ m{ _ } (vfix _ _) ] |- _ => setoid_rewrite msubst_fix in H
  | |- context [ m{ _ } (vfix _ _) ] => setoid_rewrite msubst_fix
  | H: context [ m{ _ } (treturn _) ] |- _ => setoid_rewrite msubst_value in H
  | |- context [ m{ _ } (treturn _) ] => setoid_rewrite msubst_value
  | H: context [ m{ _ } (vlam _ _) ] |- _ => setoid_rewrite msubst_lam in H
  | |- context [ m{ _ } (vlam _ _) ] => setoid_rewrite msubst_lam
  | H: context [ m{ _ } (tmatchb _ _ _) ] |- _ => setoid_rewrite msubst_match in H
  | |- context [ m{ _ } (tmatchb _ _ _) ] => setoid_rewrite msubst_match
  | H: context [ m{ _ } (vbvar _) ] |- _ => setoid_rewrite msubst_bvar in H
  | |- context [ m{ _ } (vbvar _) ] => setoid_rewrite msubst_bvar
  | H: context [ m{ _ } (vfvar _) ] |- _ => setoid_rewrite msubst_fvar in H
  | |- context [ m{ _ } (vfvar _) ] => setoid_rewrite msubst_fvar
  | H: context [ m{ _ } (vconst _) ] |- _ => setoid_rewrite msubst_constant in H
  | |- context [ m{ _ } (vconst _) ] => setoid_rewrite msubst_constant
  | H: context [ m{ _ } _ ] |- _ => setoid_rewrite msubst_qualifier in H
  | |- context [ m{ _ } _ ] => setoid_rewrite msubst_qualifier
  | H: context [ m{ _ } _ ] |- _ => setoid_rewrite msubst_qualifier in H
  | |- context [ m{ _ } _ ] => setoid_rewrite msubst_qualifier
  | H: context [ m{ _ } (_ & _) ] |- _ => setoid_rewrite msubst_qualifier_and in H
  | |- context [ m{ _ } (_ & _) ] => setoid_rewrite msubst_qualifier_and
  | H: context [ m{ _ } [: _ | _ ] ] |- _ => setoid_rewrite msubst_baserty in H
  | |- context [ m{ _ } [: _ | _ ] ] => setoid_rewrite msubst_baserty
  | H: context [ m{ _ } {: _ | _ } ] |- _ => setoid_rewrite msubst_baserty_over in H
  | |- context [ m{ _ } {: _ | _ } ] => setoid_rewrite msubst_baserty_over
  | H: context [ m{ _ } (_ ⇨ _) ] |- _ => setoid_rewrite msubst_arrrty in H
  | |- context [ m{ _ } (_ ⇨ _) ] => setoid_rewrite msubst_arrrty
  | H: context [ m{ _ } (mk_top _) ] |- _ => setoid_rewrite msubst_mk_top in H
  | |- context [ m{ _ } (mk_top _) ] => setoid_rewrite msubst_mk_top
  | H: context [ m{ _ } (mk_eq_constant _) ] |- _ => setoid_rewrite msubst_mk_eq_constant in H
  | |- context [ m{ _ } (mk_eq_constant _) ] => setoid_rewrite msubst_mk_eq_constant
  | H: context [ m{ _ } (mk_eq_var _ ?x) ], H': _ !! ?x = Some ?v |- _ => setoid_rewrite msubst_mk_eq_var with (v:=v) in H
  | H': _ !! ?x = Some ?v |- context [ m{ _ } (mk_eq_var _ ?x) ] => setoid_rewrite msubst_mk_eq_var with (v:=v)
  end; eauto.

Lemma rtyctx_append_over:
  forall (Γ: listctx rty) (e: tm) (x: atom) (b: base_ty) ϕ (τ: rty),
  ⟪Γ ++ [(x, {:b|ϕ})]⟫ (fun Γv => ⟦m{Γv} τ⟧ (m{Γv} e)) ->
  ⟪Γ⟫ (fun Γv => forall (v: value),
           ⟦(m{Γv}) {:b|ϕ}⟧ v ->
           ⟦(m{<[x := v]> Γv}) τ⟧ (m{<[x := v]> Γv} e) ).
Proof.
  induction Γ; intros.
  - simpl in *. intros. apply H. msubst_simp.
  -  simpl in *.
    apply emptyctx_closed. eauto. unfold closed_env. unfold map_Forall. intros.inversion H1.  cbv. eauto.
    Check  msubst_baserty.
    setoid_rewrite msubst_baserty in H0.
    assert 

    unfold ctxRst. destruct ρ; simpl in *; intros; eauto.


    repeat msubst_simp.

(** Combined fundamental theorem for value typing (refinemnet types) and term
  typing (Hoare automata types) *)
Theorem fundamental_combined:
  (forall (Γ: listctx rty) (v: value) (ρ: rty),
    Γ ⊢ v ⋮v ρ -> ctxRst Γ (fun Γv => ⟦ m{Γv} ρ ⟧ (m{Γv} v))) /\
  (forall (Γ: listctx rty) (e: tm) (τ: rty),
    Γ ⊢ e ⋮t τ -> ctxRst Γ (fun Γv => ⟦ m{Γv} τ ⟧ (m{Γv} e))).
Proof.
  apply value_term_type_check_mutind.
  (* [TConst] *)
  - intros Γ c HWF. eapply wf_rty_implies_strengthen; eauto.
    intros Γv ClosedΓ. repeat msubst_simp. admit.
  (* [TBaseVarOver] *)
  - intros Γ x b ϕ HWF Hin. admit.
  (* [TBaseVarUnder] *)
  - intros Γ x b ϕ HWF Hin. admit.
  (* [TBaseVarArr] *)
  - admit.
  (* [TLam] *)
  - intros Γ Tx ρ e τ L HWF Hind HindD HTx. subst.

      

    assert (forall (Γv: env), ((m{Γv}) (vconst c)) = c).
    repeat msubst_simp.
    setoid_rewrite H.

    unfold
Admitted.

(** Fundamental theorem for value typing *)
Theorem fundamental_value:
  forall (Γ: listctx rty) (v: value) (ρ: rty),
    Γ ⊢ v ⋮v ρ -> ctxRst Γ (fun Γv => ⟦ m{Γv} ρ ⟧ (m{Γv} v)).
Proof.
  qauto using fundamental_combined.
Qed.

(** Fundamental theorem (Theorem 4.8) *)
Theorem fundamental:
  forall (Γ: listctx rty) (e: tm) (τ: rty),
    Γ ⊢ e ⋮t τ -> ctxRst Γ (fun Γv => ⟦ m{Γv} τ ⟧ (m{Γv} e)).
Proof.
  qauto using fundamental_combined.
Qed.

Transparent msubst.

(** A general type soundness theorem *)
Corollary soundness :
  forall (e: tm) b ϕ,
    [] ⊢ e ⋮t [:b|ϕ] ->
    (forall (v: value), denote_qualifier (ϕ ^^ v) -> e ↪* v).
Proof.
Admitted.

Print Assumptions soundness.
