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
Import BasicTypingProp.
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

Lemma rtyR_measure_irrelevant_ i: forall m n ρ e,
    rty_measure ρ <= i ->
    i <= n ->
    i <= m ->
    rtyR n ρ e <-> rtyR m ρ e.
Proof.
  induction i; intros.
  - destruct ρ; inversion H.
  - destruct ρ; inversion H; subst.
    + destruct m, n;
        try solve [ pose proof (rty_measure_gt_0 ρ); lia
                  | pose proof (rty_measure_gt_0 τ); lia ]; split; intros; simpl; auto; inversion H; lia.
    + destruct m, n;
        try solve [ pose proof (rty_measure_gt_0 ρ); lia
                  | pose proof (rty_measure_gt_0 τ); lia ]; split; intros; simpl; auto; inversion H; lia.
    + destruct m, n;
        try solve [ pose proof (rty_measure_gt_0 ρ); lia
                  | pose proof (rty_measure_gt_0 τ); lia ]; split; intros; auto; try lia.
    + destruct m, n;
        try solve [ pose proof (rty_measure_gt_0 ρ); lia
                  | pose proof (rty_measure_gt_0 τ); lia ]; split; intros; auto; try lia.
    + destruct m, n;
        try solve [ pose proof (rty_measure_gt_0 ρ); lia
                  | pose proof (rty_measure_gt_0 τ); lia ]; split; intros; auto; try lia.
      simpl in H2; simpl; intuition. rewrite <- IHi. apply H5. rewrite IHi. apply H4.
      all: try lia.
      rewrite <- open_preserves_rty_measure. lia.
      simpl in H2; simpl; intuition. rewrite <- IHi. apply H5. rewrite IHi. apply H4.
      all: try lia.
      rewrite <- open_preserves_rty_measure. lia.
    + destruct m, n;
        try solve [ pose proof (rty_measure_gt_0 ρ); lia
                  | pose proof (rty_measure_gt_0 τ); lia ]; split; intros; auto; try lia.
      simpl in H2; simpl; intuition. rewrite <- IHi. apply H6. rewrite IHi. apply H5.
      all: try (simpl in *; lia).
      rewrite <- open_preserves_rty_measure. (simpl in *; lia).
      simpl in H2; simpl; intuition. rewrite <- IHi. apply H6. rewrite IHi. apply H5.
      all: try (simpl in *; lia).
      rewrite <- open_preserves_rty_measure. (simpl in *; lia).
Qed.

Lemma rtyR_measure_irrelevant m n ρ e:
  rty_measure ρ <= n ->
  rty_measure ρ <= m ->
  rtyR n ρ e <-> rtyR m ρ e.
Proof.
  apply (rtyR_measure_irrelevant_ (rty_measure ρ)). lia.
Qed.

Lemma rtyR_measure_irrelevant' n ρ e :
  rty_measure ρ <= n ->
  rtyR n ρ e <-> ⟦ ρ ⟧ e.
Proof.
  intros. rewrite (rtyR_measure_irrelevant_ (rty_measure ρ)); eauto.
Qed.

Ltac rewrite_measure_irrelevant :=
  let t := (rewrite <- ?open_preserves_rty_measure,
                    <- ?open_preserves_rty_measure; lia) in
  match goal with
  | H : context [rtyR _ _ _] |- _ =>
      setoid_rewrite rtyR_measure_irrelevant' in H; [ | t .. ]
  | |- context [rtyR _ _ _] =>
      setoid_rewrite rtyR_measure_irrelevant'; [ | t .. ]
  end.

(* Lemma open_is_over (τ: rty) (n: nat) (v_x: value) : is_over (open n v_x τ) <-> is_over τ. *)
(* Proof. *)
(*   induction τ; split; auto. *)
(* Qed. *)


(* A machinery to simplify certain proofs *)
Definition tm_refine e e' :=
  (* Alternatively, we may require [∅ ⊢t e ⋮t ⌊τ⌋] in [rtyR_refine]. However, we
  would need [wf_rty] as a side-condition (or some sort of validity of [rty]),
  to make sure all components in intersection have the same erasure. This would
  introduce a large set of naming lemmas about [wf_rty] (and consequently
  everything it depends on). Annoying. *)
  (exists T, ∅ ⊢t e' ⋮t T /\ ∅ ⊢t e ⋮t T) /\
    (forall  (v : value), e ↪* v -> e' ↪* v).

Lemma rtyR_refine_over: forall b ϕ e1 e2, tm_refine e2 e1 -> ⟦ {:b|ϕ} ⟧ e1 -> ⟦ {:b|ϕ} ⟧ e2.
Proof.
  intros. destruct H as ((T & Ht1 & Ht2) & Hr).
  simpl in *; intuition.
  assert (T = b). eapply basic_typing_tm_unique with (e := e1); eauto. subst. eauto.
Qed.

Lemma rtyR_refine_under: forall b ϕ e1 e2, tm_refine e1 e2 -> ⟦ [:b|ϕ] ⟧ e1 -> ⟦ [:b|ϕ] ⟧ e2.
Proof.
  intros. destruct H as ((T & Ht1 & Ht2) & Hr).
  simpl in *; intuition.
  assert (T = b). eapply basic_typing_tm_unique with (e := e1); eauto. subst. eauto.
Qed.
