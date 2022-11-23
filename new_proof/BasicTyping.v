From stdpp Require Import mapset.
From CT Require Import CoreLangProp.
From CT Require Import ListCtx.

Import Atom.
Import CoreLang.
Import Tactics.
Import NamelessTactics.
Import ListCtx.

(* define the basic \S{Ty} function *)

Definition ty_of_const (c: constant): base_ty :=
  match c with
  | cnat _ => TNat
  | cbool _ => TBool
  end.

Definition fst_ty_of_op (op: biop): base_ty :=
  match op with
  | op_plus => TNat
  | op_eq => TNat
  | op_lt => TNat
  | op_rannat => TNat
  end.

Definition snd_ty_of_op (op: biop): base_ty :=
  match op with
  | op_plus => TNat
  | op_eq => TNat
  | op_lt => TNat
  | op_rannat => TNat
  end.

Definition ret_ty_of_op (op: biop): base_ty :=
  match op with
  | op_plus => TNat
  | op_eq => TBool
  | op_lt => TBool
  | op_rannat => TNat
  end.

Definition ty_of_op (op: biop): ty :=
  (fst_ty_of_op op) ⤍ (snd_ty_of_op op) ⤍ (ret_ty_of_op op).

Lemma op_ty_spec: forall op, ty_of_op op = fst_ty_of_op op ⤍ snd_ty_of_op op ⤍ ret_ty_of_op op.
Proof with eauto.
  intros.
  destruct op...
Qed.

(* Lemma op_fst_is_op_snd : forall (op: biop), fst_ty_of_op op = snd_ty_of_op op. *)
(* Proof with eauto. *)
(*   intros.  destruct op... *)
(* Qed. *)

Definition context := listctx ty.

Reserved Notation "Γ '⊢t' t '⋮t' T" (at level 40).
Reserved Notation "Γ '⊢t' t '⋮v' T" (at level 40).

Inductive tm_has_type : context -> tm -> ty -> Prop :=
| T_Err : forall Γ T, Γ ⊢t terr ⋮t T
| T_Value : forall Γ v T, Γ ⊢t v ⋮v T -> Γ ⊢t v ⋮t T
| T_Lete : forall Γ e1 e2 T1 T2 (L: aset),
    Γ ⊢t e1 ⋮t T1 ->
    (forall (x: atom), x ∉ L -> (Γ ++ [(x, T1)]) ⊢t e2 ⋮t T2) ->
    Γ ⊢t (tlete e1 e2) ⋮t T2
| T_LetOp : forall Γ (op: biop) v1 v2 e (T1 T2 Tx: base_ty) T (L: aset),
    Γ ⊢t v1 ⋮v T1 ->
    Γ ⊢t v2 ⋮v T2 ->
    ty_of_op op = T1 ⤍ T2 ⤍ Tx ->
    (forall (x: atom), x ∉ L -> (Γ ++ [(x, TBase Tx)]) ⊢t e ⋮t T) ->
    Γ ⊢t tletbiop op v1 v2 e ⋮t T
| T_LetApp : forall Γ v1 v2 e (T1 Tx: base_ty) T (L: aset),
    Γ ⊢t v1 ⋮v T1 ->
    Γ ⊢t v2 ⋮v T1 ⤍ Tx ->
    (forall (x: atom), x ∉ L -> (Γ ++ [(x, TBase Tx)]) ⊢t e ⋮t T) ->
    Γ ⊢t tletapp v1 v2 e ⋮t T
| T_Matchb: forall Γ v e1 e2 T,
    Γ ⊢t v ⋮v TBool ->
    Γ ⊢t e1 ⋮t T ->
    Γ ⊢t e2 ⋮t T ->
    Γ ⊢t (tmatchb v e1 e2) ⋮t T
with value_has_type : context -> value -> ty -> Prop :=
| T_Const : forall Γ (c: constant), Γ ⊢t c ⋮v (ty_of_const c)
| T_Op : forall Γ (op: biop), Γ ⊢t op ⋮v (ty_of_op op)
| T_Var : forall Γ (x: atom) T,
    ctxfind Γ x = Some T -> Γ ⊢t x ⋮v T
| T_Lam : forall Γ Tx T e (L: aset),
    (forall (x: atom), x ∉ L -> (Γ ++ [(x, Tx)]) ⊢t e ⋮t T) ->
    Γ ⊢t vlam Tx e ⋮v Tx ⤍ T
| T_Fix : forall Γ (Tx: base_ty) T e (L: aset),
    (forall (f: atom), f ∉ L -> (Γ ++ [(f, Tx ⤍ T)]) ⊢t vlam Tx e ⋮v T) ->
    Γ ⊢t vfix (Tx ⤍ T) (vlam Tx e) ⋮v Tx ⤍ T
where "Γ '⊢t' t '⋮t' T" := (tm_has_type Γ t T) and "Γ '⊢t' t '⋮v' T" := (value_has_type Γ t T).

Global Hint Constructors tm_has_type: core.
Global Hint Constructors value_has_type: core.
