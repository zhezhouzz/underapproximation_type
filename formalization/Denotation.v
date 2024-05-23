From stdpp Require Import mapset.
From stdpp Require Import natmap.
From Coq.Program Require Import Wf.
From CT Require Import CoreLangClass.
From CT Require Import OperationalSemantics.
From CT Require Import BasicTypingClass.
From CT Require Import RefinementTypeClass.
From CT Require Import Instantiation.

Import Atom.
Import CoreLang.
Import Tactics.
Import NamelessTactics.
Import ListCtx.
Import OperationalSemantics.
Import BasicTyping.
Import RefinementType.
Import Qualifier.
Import Instantiation.

(* This measure function is used to guarantee termination of the denotation.
Instead of addtion, we can also use [max] for the subterms. *)
Fixpoint rty_measure (ρ: rty) : nat :=
  match ρ with
  | {: _ | _} => 1
  | [: _ | _] => 1
  | ρ ⇨ τ => 1 + rty_measure ρ + rty_measure τ
  end.

Fixpoint rtyR (gas: nat) (ρ: rty) (e: tm) : Prop :=
  match gas with
  | 0 => False
  | S gas' =>
      ∅ ⊢t e ⋮ ⌊ ρ ⌋ /\ closed_rty ∅ ρ /\
        match ρ with
        | {: b | ϕ} => forall (v: value), e ↪* v -> denote_qualifier (ϕ ^^ v)
        | [: b | ϕ] => forall (v: value), denote_qualifier (ϕ ^^ v) -> e ↪* v
        | ρx ⇨ τ => forall (v_x: value), rtyR gas' ρx v_x -> rtyR gas' (τ ^^ v_x) (mk_app_e_v e v_x)
        end
  end.

Notation "'⟦' τ '⟧' " := (rtyR (rty_measure τ) τ) (at level 20, format "⟦ τ ⟧", τ constr).

Definition prenex := (env -> Prop) -> Prop.

Definition wf_judgement (Γ: listctx rty) (p: env -> Prop) := forall Γv, p Γv -> ctxdom Γ ≡ dom Γv /\ closed_env Γv.

Definition wf_prenex (Γ: listctx rty) (PN: prenex) := forall p, PN p -> wf_judgement Γ p.

Definition is_over (ρ: rty) :=
  match ρ with
  | [: _ | _] => False
  | _ => True
  end.

Inductive ctxRst: listctx rty -> prenex -> Prop :=
| ctxRst0: ctxRst [] (fun p => wf_judgement [] p /\ p ∅)
| ctxRst1: forall Γ PN (x: atom) b ϕ,
    ctxRst Γ PN ->
    (* [ok_ctx] implies [ρ] is closed and valid, meaning that it does not use
    any function variables. *)
    ok_ctx (Γ ++ [(x, [: b | ϕ])]) ->
    ctxRst (Γ ++ [(x, [: b | ϕ])])
      (fun p => wf_judgement (Γ ++ [(x, [: b | ϕ])]) p /\ PN (fun Γv => exists (v: value), ⟦ m{ Γv } {: b | ϕ} ⟧ v /\ p (<[ x := v ]> Γv)))
| ctxRst2: forall Γ PN (x: atom) ρ,
    ctxRst Γ PN ->
    is_over ρ ->
    ok_ctx (Γ ++ [(x, ρ)]) ->
    ctxRst (Γ ++ [(x, ρ)])
      (fun p => wf_judgement (Γ ++ [(x, ρ)]) p /\ PN (fun Γv => forall (v: value), ⟦ m{ Γv } ρ ⟧ v -> p (<[ x := v ]> Γv))).

(** * Properties of denotation *)

Lemma rtyR_typed_closed gas τ e :
  rtyR gas τ e -> ∅ ⊢t e ⋮ ⌊ τ ⌋ /\ closed_rty ∅ τ.
Proof.
  destruct gas; simpl; tauto.
Qed.

Lemma rtyR_closed gas ρ e :
  rtyR gas ρ e -> closed e.
Proof.
  intros H.
  apply rtyR_typed_closed in H.
  destruct H as (H&_).
  apply basic_typing_contains_fv_tm in H.
  my_set_solver.
Qed.

Lemma rtyR_lc gas ρ e :
  rtyR gas ρ e -> lc e.
Proof.
  intros H.
  apply rtyR_typed_closed in H.
  destruct H as (H&_).
  eauto using basic_typing_regular_tm.
Qed.

Lemma ctxRst_closed_env Γ PN : ctxRst Γ PN -> wf_prenex Γ PN.
Proof.
  unfold wf_prenex.
  induction 1; intros; intuition.
Qed.

Lemma ctxRst_ok_ctx Γ Γv :
  ctxRst Γ Γv -> ok_ctx Γ.
Proof.
  induction 1; eauto. econstructor.
Qed.

Lemma mk_over_top_closed_rty b : closed_rty ∅ ({:b | mk_q_under_top}).
Proof.
  econstructor. unshelve (repeat econstructor). exact ∅.
  my_set_solver.
Qed.

Lemma mk_under_top_closed_rty b : closed_rty ∅ ([:b | mk_q_under_top]).
Proof.
  econstructor. unshelve (repeat econstructor). exact ∅.
  my_set_solver.
Qed.

Lemma mk_over_top_denote_rty (b : base_ty) (v : value) :
  ∅ ⊢t v ⋮ b -> ⟦ ({:b | mk_q_under_top}) ⟧ v.
Proof.
  intros.
  split; [| split]; simpl; eauto using mk_over_top_closed_rty.
Qed.

Lemma mk_eq_constant_closed_rty c : closed_rty ∅ (mk_eq_constant c).
Proof.
  econstructor. unshelve (repeat econstructor). exact ∅.
  my_set_solver.
Qed.

Lemma mk_eq_constant_denote_rty c: ⟦ mk_eq_constant c ⟧ c.
Proof.
  simpl. split; [| split]; cbn; eauto using mk_eq_constant_closed_rty.
  intros.
  destruct v; auto; try inversion H. simpl in H. subst. repeat econstructor.
Qed.

Lemma closed_over_base_rty_qualifier_and B ϕ1 ϕ2 Γ:
  closed_rty Γ {: B | ϕ1 } ->
  closed_rty Γ {: B | ϕ2 } ->
  closed_rty Γ {: B | ϕ1 & ϕ2}.
Proof.
  intros [Hlc1 Hfv1] [Hlc2 Hfv2]. sinvert Hlc1. sinvert Hlc2.
  econstructor.
  econstructor. instantiate_atom_listctx.
  rewrite qualifier_and_open.
  eauto using lc_qualifier_and.
  simpl in *.
  rewrite qualifier_and_fv. my_set_solver.
Qed.

Lemma closed_under_base_rty_qualifier_and B ϕ1 ϕ2 Γ:
  closed_rty Γ [: B | ϕ1 ] ->
  closed_rty Γ [: B | ϕ2 ] ->
  closed_rty Γ [: B | ϕ1 & ϕ2].
Proof.
  intros [Hlc1 Hfv1] [Hlc2 Hfv2]. sinvert Hlc1. sinvert Hlc2.
  econstructor.
  econstructor. instantiate_atom_listctx.
  rewrite qualifier_and_open.
  eauto using lc_qualifier_and.
  simpl in *.
  rewrite qualifier_and_fv. my_set_solver.
Qed.

Lemma denote_over_base_rty_qualifier_and B ϕ1 ϕ2 ρ:
  ⟦ {: B | ϕ1 } ⟧ ρ ->
  ⟦ {: B | ϕ2 } ⟧ ρ ->
  ⟦ {: B | ϕ1 & ϕ2} ⟧ ρ.
Proof.
  intros (?&?&?) (?&?&?).
  split; [| split]; eauto using closed_over_base_rty_qualifier_and.
  intros.
  rewrite qualifier_and_open.
  rewrite denote_qualifier_and.
  qauto.
Qed.

(* Lemma denote_under_base_rty_qualifier_and B ϕ1 ϕ2 ρ: *)
(*   (⟦ [: B | ϕ1 ] ⟧ ρ \/ ⟦ [: B | ϕ2 ] ⟧ ρ) -> *)
(*   ⟦ [: B | ϕ1 & ϕ2] ⟧ ρ. *)
(* Proof. *)
(*   intros. destruct H; simpl in *. *)
(*   - split; [| split]; intuition; eauto using closed_under_base_rty_qualifier_and. *)
(*     eauto using closed_under_base_rty_qualifier_and. *)
(*     intros. *)
(*   rewrite qualifier_and_open. *)
(*   rewrite denote_qualifier_and. *)
(*   qauto. *)
(* Qed. *)

Lemma rty_measure_gt_0 ρ : rty_measure ρ > 0.
Proof.
  induction ρ; simpl; lia.
Qed.

Lemma rty_measure_S ρ : exists n, rty_measure ρ = S n.
Proof.
  destruct (Nat.lt_exists_pred 0 (rty_measure ρ)).
  pose proof (rty_measure_gt_0 ρ). lia.
  intuition eauto.
Qed.

Lemma open_preserves_rty_measure ρ: forall k t, rty_measure ρ = rty_measure ({k ~> t} ρ).
Proof.
  induction ρ; intros; simpl; eauto.
Qed.

Lemma subst_preserves_rty_measure ρ: forall x t, rty_measure ρ = rty_measure ({x:=t} ρ).
Proof.
  induction ρ; intros; simpl; eauto.
Qed.

(* The conclusion has to be strengthened to an equivalence to get around
termination checker. *)
Lemma rtyR_measure_irrelevant m n ρ e:
  rty_measure ρ <= n ->
  rty_measure ρ <= m ->
  rtyR n ρ e <-> rtyR m ρ e.
Proof.
  generalize dependent m. generalize dependent n.
  generalize dependent e.
  induction ρ; simpl; intros.
  - destruct m, n;
      try solve [ pose proof (rty_measure_gt_0 ρ); lia
              | pose proof (rty_measure_gt_0 τ); lia ]; split; intros; simpl; auto; inversion H; lia.
  - destruct m, n;
      try solve [ pose proof (rty_measure_gt_0 ρ); lia
                | pose proof (rty_measure_gt_0 τ); lia ]; split; intros; simpl; auto; inversion H; lia.
  - destruct m, n;
      try solve [ pose proof (rty_measure_gt_0 ρ); lia
                | pose proof (rty_measure_gt_0 τ); lia ]; split; intros; simpl; auto; try lia.
    inversion H1. intuition.
    rewrite <- open_preserves_rty_measure. lia.
    intros.
    intut

    inversion H.
    simpl.


  induction n; intros;
    try solve [ pose proof (rty_measure_gt_0 ρ); lia
              | pose proof (rty_measure_gt_0 τ); lia ].
  intuition. rewrite <- IHn.


  all: destruct m, n; intros;
    try solve [ pose proof (rty_measure_gt_0 ρ); lia
              | pose proof (rty_measure_gt_0 τ); lia ].
  simpl.
  - intuition.
    + destruct ρ; intros; simpl in *; eauto.
      rewrite <- rtyR_measure_irrelevant.
      auto_apply.
      rewrite rtyR_measure_irrelevant; eauto. lia. lia.
      rewrite <- open_preserves_rty_measure. lia.
      rewrite <- open_preserves_rty_measure. lia.
      rewrite <- rtyR_measure_irrelevant; eauto.
      rewrite <- open_preserves_rty_measure. lia.
      rewrite <- open_preserves_rty_measure. lia.
    + destruct ρ; intros; simpl in *; eauto.
      rewrite rtyR_measure_irrelevant.
      auto_apply.
      rewrite <- rtyR_measure_irrelevant; eauto. lia. lia.
      rewrite <- open_preserves_rty_measure. lia.
      rewrite <- open_preserves_rty_measure. lia.
      rewrite rtyR_measure_irrelevant; eauto.
      rewrite <- open_preserves_rty_measure. lia.
      rewrite <- open_preserves_rty_measure. lia.
  - intuition.
    + destruct τ; intros; simpl in *; eauto.
      specialize (H4 _ _ _ H3 H5). intuition.
      rewrite <- rtyR_measure_irrelevant; eauto. lia. lia.
      intuition.
      rewrite <- rtyR_measure_irrelevant; eauto. lia. lia.
      rewrite <- rtyR_measure_irrelevant; eauto. lia. lia.
    + destruct τ; intros; simpl in *; eauto.
      specialize (H4 _ _ _ H3 H5). intuition.
      rewrite rtyR_measure_irrelevant; eauto. lia. lia.
      intuition.
      rewrite rtyR_measure_irrelevant; eauto. lia. lia.
      rewrite rtyR_measure_irrelevant; eauto. lia. lia.
Qed.

Lemma rtyR_measure_irrelevant' n ρ e :
  rty_measure ρ <= n ->
  rtyR n ρ e <-> p⟦ ρ ⟧ e.
Proof.
  intros. rewrite rtyR_measure_irrelevant; eauto.
Qed.

Lemma rtyR_measure_irrelevant' n τ e :
  rty_measure τ <= n ->
  rtyR n τ e <-> ⟦ τ ⟧ e.
Proof.
  intros. rewrite rtyR_measure_irrelevant; eauto.
Qed.

Ltac rewrite_measure_irrelevant :=
  let t := (rewrite <- ?open_preserves_rty_measure,
                    <- ?open_preserves_rty_measure; lia) in
  match goal with
  | H : context [rtyR _ _ _] |- _ =>
      setoid_rewrite rtyR_measure_irrelevant' in H; [ | t .. ]
  | H : context [rtyR _ _ _] |- _ =>
      setoid_rewrite rtyR_measure_irrelevant' in H; [ | t .. ]
  | |- context [rtyR _ _ _] =>
      setoid_rewrite rtyR_measure_irrelevant'; [ | t .. ]
  | |- context [rtyR _ _ _] =>
      setoid_rewrite rtyR_measure_irrelevant'; [ | t .. ]
  end.

(* A machinery to simplify certain proofs *)
Definition tm_refine e e' :=
  (* Alternatively, we may require [∅ ⊢t e ⋮t ⌊τ⌋] in [rtyR_refine]. However, we
  would need [wf_rty] as a side-condition (or some sort of validity of [rty]),
  to make sure all components in intersection have the same erasure. This would
  introduce a large set of naming lemmas about [wf_rty] (and consequently
  everything it depends on). Annoying. *)
  (exists T, ∅ ⊢t e' ⋮t T /\ ∅ ⊢t e ⋮t T) /\
  (forall α β (v : value), α ⊧ e ↪*{ β} v -> α ⊧ e' ↪*{ β} v).

(* Semantic refinement preserves denotation. *)
Lemma rtyR_refine τ e1 e2 :
  tm_refine e2 e1 ->
  ⟦ τ ⟧ e1 ->
  ⟦ τ ⟧ e2.
Proof.
  intros [Ht Hr].
  assert (rty_measure τ <= rty_measure τ) by reflexivity.
  revert H. generalize (rty_measure τ) at 2 3 4 as n.
  intros n. revert τ.
  induction n. easy.
  simpl. intuition.
  qauto using basic_typing_tm_unique.
  destruct τ; eauto.
  simpl in *. intuition.
  apply IHn; eauto. lia.
  apply IHn; eauto. lia.
Qed.
