From CT Require Import Atom.
From stdpp Require Import stringmap mapset.

(* constant *)
Inductive constant : Type :=
| cbool: bool -> constant
| cnat : nat -> constant.

Global Hint Constructors constant: core.

Inductive base_ty : Type :=
| TNat   : base_ty
| TBool  : base_ty.

Global Hint Constructors base_ty: core.

Inductive ty : Type :=
| TBase : base_ty -> ty
| TArrow : ty -> ty -> ty.

Global Hint Constructors ty: core.

Coercion TBase : base_ty >-> ty.
Coercion cbool : bool >-> constant.
Coercion cnat : nat >-> constant.

Notation " t1 '⤍' t2" := (TArrow t1 t2) (at level 18, right associativity).

Inductive biop : Type :=
| op_plus
| op_eq
| op_lt
| op_rannat.

Global Hint Constructors biop: core.

Inductive value : Type :=
| vbiop (op: biop)
| vconst (c: constant)
| vfvar (atom: atom)
| vbvar (bn: nat)
| vlam (T: ty) (e: tm)
| vfix (Tf: ty) (e: tm)
with tm : Type :=
| terr
| tvalue (v: value)
| tlete (e1: tm) (e2: tm)
| tletbiop (op: biop) (v1: value) (v2: value) (e: tm)
| tletapp (v1: value) (v2: value) (e: tm)
| tmatchb (v: value) (e1: tm) (e2: tm).

Scheme value_mutual_rec := Induction for value Sort Type
    with tm_mutual_rec := Induction for tm Sort Type.

Coercion vbiop : biop >-> value.
Coercion vconst : constant >-> value.
Coercion vfvar : atom >-> value.
Coercion tvalue : value >-> tm.

(* open *)

Fixpoint _open_value (k : nat) (s : value) (v : value): value :=
  match v with
  | vbiop _ => v
  | vconst _ => v
  | vfvar _ => v
  | vbvar n => if decide (k = n) then s else v
  | vlam T e => vlam T (_open_tm (S k) s e)
  | vfix Tf e => vfix Tf (_open_tm (S k) s e)
  end
with _open_tm (k : nat) (s : value) (e : tm): tm :=
       match e with
       | terr => e
       | tvalue v => tvalue (_open_value k s v)
       | tlete e1 e2 => tlete (_open_tm k s e1) (_open_tm (S k) s e2)
       | tletapp v1 v2 e =>
           tletapp (_open_value k s v1) (_open_value k s v2) (_open_tm (S k) s e)
       | tletbiop op v1 v2 e =>
           tletbiop op (_open_value k s v1) (_open_value k s v2) (_open_tm (S k) s e)
       | tmatchb v e1 e2 => tmatchb (_open_value k s v) (_open_tm k s e1) (_open_tm k s e2)
       end.

Notation "'{' k '~v>' s '}' e" := (_open_value k s e) (at level 20, k constr).
Notation "'{' k '~t>' s '}' e" := (_open_tm k s e) (at level 20, k constr).

(* Definition open_value s e := _open_value 0 s e. *)
(* Definition open_tm s e := _open_tm 0 s e. *)
Notation "e '^v^' s" := (_open_value 0 s e) (at level 20).
Notation "e '^t^' s" := (_open_tm 0 s e) (at level 20).

Fixpoint _close_value (x : atom) (s : nat) (v : value): value :=
  match v with
  | vbiop _ => v
  | vconst _ => v
  | vfvar y => if decide (x = y) then vbvar s else v
  | vbvar _ => v
  | vlam T e => vlam T (_close_tm x (S s) e)
  | vfix Tf e => vfix Tf (_close_tm x (S s) e)
  end
with _close_tm (x : atom) (s : nat) (e : tm): tm :=
       match e with
       | terr => e
       | tvalue v => tvalue (_close_value x s v)
       | tlete e1 e2 => tlete (_close_tm x s e1) (_close_tm x (S s) e2)
       | tletapp v1 v2 e =>
           tletapp (_close_value x s v1) (_close_value x s v2) (_close_tm x (S s) e)
       | tletbiop op v1 v2 e =>
           tletbiop op (_close_value x s v1) (_close_value x s v2) (_close_tm x (S s) e)
       | tmatchb v e1 e2 =>
           tmatchb (_close_value x s v) (_close_tm x s e1) (_close_tm x s e2)
       end.

Notation "'{' s '<v~' x '}' e" := (_close_value x s e) (at level 20, s constr).
Notation "'{' s '<t~' x '}' e" := (_close_tm x s e) (at level 20, s constr).

(* Definition close_value x e := _close_value x 0 e. *)
(* Definition close_tm x e := _close_tm x 0 e. *)
Notation "x '\v\' e" := (_close_value x 0 e) (at level 20).
Notation "x '\t\' e" := (_close_tm x 0 e) (at level 20).

Inductive lc: tm -> Prop :=
| lc_vbiop: forall (op: biop), lc op
| lc_const: forall (c: constant), lc c
| lc_vfvar: forall (a: atom), lc (vfvar a)
| lc_vlam: forall T e (L: aset), (forall (x: atom), x ∉ L -> lc (e ^t^ x)) -> lc (vlam T e)
| lc_vfix: forall Tf e (L: aset),
    (forall (f:atom), f ∉ L -> lc ({0 ~t> f} e)) -> lc (vfix Tf e)
| lc_terr: lc terr
| lc_tlete: forall (e1 e2: tm) (L: aset),
    lc e1 -> (forall (x: atom), x ∉ L -> lc (e2 ^t^ x)) -> lc (tlete e1 e2)
| lc_tletapp: forall (v1 v2: value) e (L: aset),
    lc v1 -> lc v2 -> (forall (x: atom), x ∉ L -> lc (e ^t^ x)) -> lc (tletapp v1 v2 e)
| lc_tletbiop: forall op (v1 v2: value) e (L: aset),
    lc v1 -> lc v2 -> (forall (x: atom), x ∉ L -> lc (e ^t^ x)) -> lc (tletbiop op v1 v2 e)
| lc_tmatchb: forall (v: value) e1 e2, lc v -> lc e1 -> lc e2 -> lc (tmatchb v e1 e2).

Global Hint Constructors lc: core.

Definition var_open_value (s: atom) (e: value) := e ^v^ s.
Definition var_open_tm (s: atom) (e: tm) := e ^t^ s.

(* We don't have explicit beta reduction. *)

Fixpoint fv_value (v : value): aset :=
  match v with
  | vbiop _ => ∅
  | vconst _ => ∅
  | vfvar y => {[ y ]}
  | vbvar _ => ∅
  | vlam T e => fv_tm e
  | vfix Tf e => fv_tm e
  end
with fv_tm (e : tm): aset :=
       match e with
       | terr => ∅
       | tvalue v => fv_value v
       | tlete e1 e2 => (fv_tm e1) ∪ (fv_tm e2)
       | tletapp v1 v2 e => (fv_value v1) ∪ (fv_value v2) ∪ (fv_tm e)
       | tletbiop op v1 v2 e => (fv_value v1) ∪ (fv_value v2) ∪ (fv_tm e)
       | tmatchb v e1 e2 => (fv_value v) ∪ (fv_tm e1) ∪ (fv_tm e2)
       end.

Definition closed_value (v: value) := fv_value v ≡ ∅.
Definition closed_tm (e: tm) := fv_tm e ≡ ∅.

Definition body (e: tm) := exists (L: aset), forall (x: atom), x ∉ L -> lc (e ^t^ x).

Fixpoint value_subst (x : atom) (s : value) (v : value): value :=
  match v with
  | vbiop _ => v
  | vconst _ => v
  | vfvar y => if decide (x = y) then s else v
  | vbvar _ => v
  | vlam T e => vlam T (tm_subst x s e)
  | vfix Tf e => vfix Tf (tm_subst x s e)
  end
with tm_subst (x : atom) (s : value) (e : tm): tm :=
       match e with
       | terr => e
       | tvalue v => tvalue (value_subst x s v)
       | tlete e1 e2 => tlete (tm_subst x s e1) (tm_subst x s e2)
       | tletapp v1 v2 e => tletapp (value_subst x s v1) (value_subst x s v2) (tm_subst x s e)
       | tletbiop op v1 v2 e => tletbiop op (value_subst x s v1) (value_subst x s v2) (tm_subst x s e)
       | tmatchb v e1 e2 => tmatchb (value_subst x s v) (tm_subst x s e1) (tm_subst x s e2)
       end.

(* Definition value_subst (x:atom) (s:value) (t:value) : value := (x \v\ t) ^v^ s. *)
(* Definition tm_subst (x:atom) (s:value) (t:tm) : tm := (x \t\ t) ^t^ s. *)

Notation "'{' x ':=' s '}t' t" := (tm_subst x s t) (at level 20).
Notation "'{' x ':=' s '}v' t" := (value_subst x s t) (at level 20).

Notation "x # s" := (x ∉ stale s) (at level 40).
