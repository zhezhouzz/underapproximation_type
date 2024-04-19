From stdpp Require Import mapset.
From CT Require Import CoreLangClass.

Import Atom.
Import CoreLang.
Import Tactics.
Import NamelessTactics.

(** This file defines the basic type system of λᴱ. *)

Definition ty_of_const (c: constant): base_ty :=
  match c with
  | cnat _ => TNat
  | cbool _ => TBool
  end.

Definition ret_ty_of_op (op: op): base_ty :=
  match op with
  | op_plus_one => TNat
  | op_minus_one => TNat
  | op_eq_zero => TBool
  | op_rannat => TNat
  | op_ranbool => TBool
  end.

Definition ty_of_op (op: op): ty := TNat ⤍ (ret_ty_of_op op).

Definition context := amap ty.
#[global]
Instance context_stale : Stale context := dom.
Arguments context_stale /.

Reserved Notation "Γ '⊢t' t '⋮t' T" (at level 40).
Reserved Notation "Γ '⊢t' t '⋮v' T" (at level 40).

(** Basic typing rules  *)
Inductive tm_has_type : context -> tm -> ty -> Prop :=
| BtValue : forall Γ v T, Γ ⊢t v ⋮v T -> Γ ⊢t v ⋮t T
| BtLetE : forall Γ (e1 e2: tm) T1 T2 (L: aset),
    Γ ⊢t e1 ⋮t T1 ->
    (forall (x: atom), x ∉ L -> (<[ x := T1]> Γ) ⊢t e2 ^^^ x ⋮t T2) ->
    Γ ⊢t (tlete e1 e2) ⋮t T2
| BtEffOp : forall Γ (op: op) (v1: value) (e: tm) (T1 Tx: base_ty) T (L: aset),
    Γ ⊢t v1 ⋮v T1 ->
    (ty_of_op op) = T1 ⤍ Tx ->
    (forall (x: atom), x ∉ L -> (<[x := TBase Tx]> Γ) ⊢t e ^^^ x ⋮t T) ->
    Γ ⊢t tletop op v1 e ⋮t T
| BtApp : forall Γ (v1 v2: value) (e: tm) T1 Tx T (L: aset),
    Γ ⊢t v1 ⋮v T1 ⤍ Tx ->
    Γ ⊢t v2 ⋮v T1 ->
    (forall (x: atom), x ∉ L -> (<[x := Tx]> Γ) ⊢t e ^^^ x ⋮t T) ->
    Γ ⊢t tletapp v1 v2 e ⋮t T
| BtMatchb: forall Γ v e1 e2 T,
    Γ ⊢t v ⋮v TBool ->
    Γ ⊢t e1 ⋮t T ->
    Γ ⊢t e2 ⋮t T ->
    Γ ⊢t (tmatchb v e1 e2) ⋮t T
with value_has_type : context -> value -> ty -> Prop :=
| BtConst : forall Γ (c: constant), Γ ⊢t c ⋮v (ty_of_const c)
| BtVar : forall Γ (x: atom) T,
    Γ !! x = Some T -> Γ ⊢t x ⋮v T
| BtFun : forall Γ Tx T (e: tm) (L: aset),
    (forall (x: atom), x ∉ L -> (<[x := Tx]> Γ) ⊢t e ^^^ x ⋮t T) ->
    Γ ⊢t vlam Tx e ⋮v Tx ⤍ T
| BtFix : forall Γ (Tx: base_ty) T e (L: aset),
    (forall (x: atom), x ∉ L -> (<[x := TBase Tx]>Γ) ⊢t (vlam (Tx ⤍ T) e) ^^^ x ⋮v ((Tx ⤍ T) ⤍ T)) ->
    Γ ⊢t vfix (Tx ⤍ T) (vlam (Tx ⤍ T) e) ⋮v Tx ⤍ T
where "Γ '⊢t' t '⋮t' T" := (tm_has_type Γ t T) and "Γ '⊢t' t '⋮v' T" := (value_has_type Γ t T).

Scheme value_has_type_mutual_rec := Induction for value_has_type Sort Prop
    with tm_has_type_mutual_rec := Induction for tm_has_type Sort Prop.

Global Hint Constructors tm_has_type: core.
Global Hint Constructors value_has_type: core.

(** * Naming related properties of the basic type system *)

Lemma fold_fv_tm (e: tm): fv_tm e = fv e.
Proof. unfold fv; auto. Qed.

Lemma fold_fv_value (e: value): fv_value e = fv e.
Proof. unfold fv; auto. Qed.

Lemma fold_close_value (x: atom) (s: nat) (v: value) : close_value x s v = close x s v.
Proof. unfold close; auto. Qed.

Lemma fold_close_tm (x: atom) (s: nat) (v: tm) : close_tm x s v = close x s v.
Proof. unfold close; auto. Qed.

Lemma fold_subst_value (x: atom) (s: value) (v: value) : value_subst x s v = substitute x s v.
Proof. unfold substitute; auto. Qed.

Lemma fold_subst_tm (x: atom) (s: value) (v: tm) : tm_subst x s v = substitute x s v.
Proof. unfold substitute; auto. Qed.

Lemma fold_body_tm  (e: tm): tm_body e = body e.
Proof. unfold body; auto. Qed.

Ltac fold_nameless :=
  repeat match goal with
    | [H: context [ open_tm _ _ _ ] |- _ ] => setoid_rewrite fold_open_tm in H
    | [H: _ |- context [ open_tm _ _ _ ]  ] => setoid_rewrite fold_open_tm
    | [H: context [ open_value _ _ _ ] |- _ ] => setoid_rewrite fold_open_value in H
    | [H: _ |- context [ open_value _ _ _ ]  ] => setoid_rewrite fold_open_value
    | [H: context [ close_tm _ _ _ ] |- _ ] => setoid_rewrite fold_close_tm in H
    | [H: _ |- context [ close_tm _ _ _ ]  ] => setoid_rewrite fold_close_tm
    | [H: context [ close_value _ _ _ ] |- _ ] => setoid_rewrite fold_close_value in H
    | [H: _ |- context [ close_value _ _ _ ]  ] => setoid_rewrite fold_close_value
    | [H: context [ tm_subst _ _ _ ] |- _ ] => setoid_rewrite fold_subst_tm in H
    | [H: _ |- context [ tm_subst _ _ _ ]  ] => setoid_rewrite fold_subst_tm
    | [H: context [ value_subst _ _ _ ] |- _ ] => setoid_rewrite fold_subst_value in H
    | [H: _ |- context [ value_subst _ _ _ ]  ] => setoid_rewrite fold_subst_value
    | [H: context [ fv_tm _ ] |- _ ] => setoid_rewrite fold_fv_tm in H
    | [H: _ |- context [ fv_tm _ ]  ] => setoid_rewrite fold_fv_tm
    | [H: context [ fv_value _ ] |- _ ] => setoid_rewrite fold_fv_value in H
    | [H: _ |- context [ fv_value _ ]  ] => setoid_rewrite fold_fv_value
    | [H: tm_lc _ |- _ ] => rewrite fold_lc_tm in H
    | [H: _ |- tm_lc _ ] => rewrite fold_lc_tm
    | [H: tm_body _ |- _ ] => rewrite fold_body_tm in H
    | [H: _ |- tm_body _ ] => rewrite fold_body_tm
    end.

Ltac nameless_eval:=
  repeat lc_normalize_one;
  simpl in *;
  repeat match goal with
    | [H: context [ open _ _ _ ] |- _ ] => unfold open in H; simpl in H
    | [H: _ |- context [ open _ _ _ ]  ] => unfold open; simpl
    | [H: context [ close _ _ _ ] |- _ ] => unfold close in H; simpl in H
    | [H: _ |- context [ close _ _ _ ]  ] => unfold close; simpl
    | [H: context [ substitute _ _ _ ] |- _ ] => unfold substitute in H; simpl in H
    | [H: _ |- context [ substitute _ _ _ ]  ] => unfold substitute; simpl
    | [H: context [ fv _ ] |- _ ] => unfold fv in H; simpl in H
    | [H: _ |- context [ fv _ ]  ] => unfold fv; simpl
    end; fold_nameless;
  repeat lc_normalize_one.

Ltac nameless_set_simpl :=
  nameless_eval;
  (repeat match goal with
     | H : fv ({_ ~> _} _) ⊆ _ |- _ =>
         setoid_rewrite <- open_fv' in H
     | H : _ ⊆ dom (<[_:=_]>_) |- _ =>
         setoid_rewrite dom_insert in H
     | H : _ !! _ = _ |- _ =>
         apply elem_of_dom_2 in H
     end).

Lemma basic_typing_contains_fv_tm: forall Γ (e: tm) T, Γ ⊢t e ⋮t T -> fv e ⊆ dom Γ.
Proof.
  apply (tm_has_type_mutual_rec
           (fun Γ (v: value) T P => fv v ⊆ dom Γ)
           (fun Γ (e: tm) T P => fv e ⊆ dom Γ)
        ).
  all:
            intros; simpl in *; eauto;
            try select (forall x, _ ∉ _ -> _) (fun H => auto_pose_fv x; repeat specialize_with x);
            repeat select (_ ⊢t _ ⋮v _) (fun H => apply basic_typing_contains_fv_value in H);
            repeat select (_ ⊢t _ ⋮t _) (fun H => apply basic_typing_contains_fv_tm in H);
            nameless_set_simpl; my_set_solver.
Qed.

Lemma basic_typing_contains_fv_value: forall Γ (e: value) T, Γ ⊢t e ⋮v T -> fv e ⊆ dom Γ.
Proof.
  apply (value_has_type_mutual_rec
           (fun Γ (v: value) T P => fv v ⊆ dom Γ)
           (fun Γ (e: tm) T P => fv e ⊆ dom Γ)
        ).
  all:
    intros; simpl in *; eauto;
    try select (forall x, _ ∉ _ -> _) (fun H => auto_pose_fv x; repeat specialize_with x);
    repeat select (_ ⊢t _ ⋮v _) (fun H => apply basic_typing_contains_fv_value in H);
    repeat select (_ ⊢t _ ⋮t _) (fun H => apply basic_typing_contains_fv_tm in H);
    nameless_set_simpl; my_set_solver.
Qed.

Lemma basic_typing_closed_tm: forall e T, ∅ ⊢t e ⋮t T -> closed e.
Proof.
  intros.
  apply basic_typing_contains_fv_tm in H. my_set_solver.
Qed.

Lemma basic_typing_closed_value: forall v T, ∅ ⊢t v ⋮v T -> closed v.
Proof.
  intros.
  apply basic_typing_contains_fv_value in H. my_set_solver.
Qed.

Lemma basic_typing_regular_value: forall Γ v t, Γ ⊢t v ⋮v t -> lc v.
Proof.
  apply (value_has_type_mutual_rec
           (fun Γ (v: value) t P => lc v)
           (fun Γ (e: tm) t P => lc e)
        ).
  all: intros; nameless_eval; lc_solver_ast;
    try (auto_exists_L; intros; nameless_eval;
         try (eapply basic_typing_regular_tm; eapply H; my_set_solver);
         try (eapply basic_typing_regular_value; eapply H; my_set_solver)
      ); fold_nameless.
Qed.

Lemma basic_typing_regular_tm: forall Γ (v: tm) t, Γ ⊢t v ⋮t t -> lc v.
Proof.
  apply (tm_has_type_mutual_rec
           (fun Γ (v: value) t P => lc v)
           (fun Γ (e: tm) t P => lc e)
        ).
  all: intros; nameless_eval; lc_solver_ast;
    try (auto_exists_L; intros; nameless_eval;
         try (eapply basic_typing_regular_tm; eapply H; my_set_solver);
         try (eapply basic_typing_regular_value; eapply H; my_set_solver)
      ); fold_nameless.
Qed.

Ltac basic_typing_regular_simp :=
  repeat match goal with
    | [H: _ ⊢t _ ⋮v _ |- lc _] => apply basic_typing_regular_value in H; destruct H; auto
    | [H: _ ⊢t _ ⋮t _ |- lc _] => apply basic_typing_regular_tm in H; destruct H; auto
    | [H: _ ⊢t _ ⋮v _ |- body _] => apply basic_typing_regular_value in H; destruct H; auto
    | [H: _ ⊢t _ ⋮t _ |- body _] => apply basic_typing_regular_tm in H; destruct H; auto
    end.

