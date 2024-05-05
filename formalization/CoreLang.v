From CT Require Import Atom.
From stdpp Require Import stringmap mapset.

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

(* notation for function type: \rightbkarrow. *)
Notation " t1 '⤍' t2" := (TArrow t1 t2) (at level 18, right associativity).

Inductive op : Type :=
| op_plus_one
| op_minus_one
| op_eq_zero
| op_rannat
| op_ranbool.

Global Hint Constructors op: core.

Inductive value : Type :=
| vconst (c: constant)
| vfvar (atom: atom)
| vbvar (bn: nat)
| vlam (T: ty) (e: tm)
| vfix (Tf: ty) (e: tm)
(** expressions (computations) (e in Fig. 2) *)
with tm : Type :=
(* We explicitly connect values and expressions (computation) using a standard
return syntax, while in the paper values are implicitly expressions. *)
| treturn (v: value)
| tlete (e1: tm) (e2: tm)
| tletop (op: op) (v1: value) (e: tm)
| tletapp (v1: value) (v2: value) (e: tm)
| tmatchb (v: value) (e1: tm) (e2: tm).

Scheme value_mutual_rec := Induction for value Sort Type
    with tm_mutual_rec := Induction for tm Sort Type.

Coercion vconst : constant >-> value.
Coercion vfvar : atom >-> value.
Coercion treturn : value >-> tm.

(** * Locally nameless representation related definitions *)

(** open *)
Fixpoint open_value (k : nat) (s : value) (v : value): value :=
  match v with
  | vconst _ => v
  | vfvar _ => v
  | vbvar n => if decide (k = n) then s else v
  | vlam T e => vlam T (open_tm (S k) s e)
  | vfix Tf e => vfix Tf (open_tm (S k) s e)
  end
with open_tm (k : nat) (s : value) (e : tm): tm :=
       match e with
       | treturn v => treturn (open_value k s v)
       | tlete e1 e2 => tlete (open_tm k s e1) (open_tm (S k) s e2)
       | tletapp v1 v2 e =>
           tletapp (open_value k s v1) (open_value k s v2) (open_tm (S k) s e)
       | tletop op v1 e =>
           tletop op (open_value k s v1) (open_tm (S k) s e)
       | tmatchb v e1 e2 => tmatchb (open_value k s v) (open_tm k s e1) (open_tm k s e2)
       end.

(** close *)
Fixpoint close_value (x : atom) (s : nat) (v : value): value :=
  match v with
  | vconst _ => v
  | vfvar y => if decide (x = y) then vbvar s else v
  | vbvar _ => v
  | vlam T e => vlam T (close_tm x (S s) e)
  | vfix Tf e => vfix Tf (close_tm x (S s) e)
  end
with close_tm (x : atom) (s : nat) (e : tm): tm :=
       match e with
       | treturn v => treturn (close_value x s v)
       | tlete e1 e2 => tlete (close_tm x s e1) (close_tm x (S s) e2)
       | tletapp v1 v2 e =>
           tletapp (close_value x s v1) (close_value x s v2) (close_tm x (S s) e)
       | tletop op v1 e =>
           tletop op (close_value x s v1) (close_tm x (S s) e)
       | tmatchb v e1 e2 =>
           tmatchb (close_value x s v) (close_tm x s e1) (close_tm x s e2)
       end.

(** free variables *)
Fixpoint fv_value (v : value): aset :=
  match v with
  | vconst _ => ∅
  | vfvar y => {[ y ]}
  | vbvar _ => ∅
  | vlam T e => fv_tm e
  | vfix Tf e => fv_tm e
  end
with fv_tm (e : tm): aset :=
       match e with
       | treturn v => fv_value v
       | tlete e1 e2 => (fv_tm e1) ∪ (fv_tm e2)
       | tletapp v1 v2 e => (fv_value v1) ∪ (fv_value v2) ∪ (fv_tm e)
       | tletop op v1 e => (fv_value v1) ∪ (fv_tm e)
       | tmatchb v e1 e2 => (fv_value v) ∪ (fv_tm e1) ∪ (fv_tm e2)
       end.

(** substitution *)
Fixpoint value_subst (x : atom) (s : value) (v : value): value :=
  match v with
  | vconst _ => v
  | vfvar y => if decide (x = y) then s else v
  | vbvar _ => v
  | vlam T e => vlam T (tm_subst x s e)
  | vfix Tf e => vfix Tf (tm_subst x s e)
  end
with tm_subst (x : atom) (s : value) (e : tm): tm :=
       match e with
       | treturn v => treturn (value_subst x s v)
       | tlete e1 e2 => tlete (tm_subst x s e1) (tm_subst x s e2)
       | tletapp v1 v2 e => tletapp (value_subst x s v1) (value_subst x s v2) (tm_subst x s e)
       | tletop op v1 e => tletop op (value_subst x s v1) (tm_subst x s e)
       | tmatchb v e1 e2 => tmatchb (value_subst x s v) (tm_subst x s e1) (tm_subst x s e2)
       end.

(** locally closed *)
Inductive tm_lc: tm -> Prop  :=
| lc_const: forall (c: constant), tm_lc c
| lc_vfvar: forall (a: atom), tm_lc (vfvar a)
| lc_vlam: forall T e (L: aset), (forall (x: atom), x ∉ L -> tm_lc (open_tm 0 (vfvar x) e)) -> tm_lc (vlam T e)
| lc_vfix: forall Tf e (L: aset), (forall (f:atom), f ∉ L -> tm_lc (open_tm 0 (vfvar f) e)) -> tm_lc (vfix Tf e)
| lc_tlete: forall (e1 e2: tm) (L: aset),
    tm_lc e1 -> (forall (x: atom), x ∉ L -> tm_lc (open_tm 0 (vfvar x) e2)) -> tm_lc (tlete e1 e2)
| lc_tletapp: forall (v1 v2: value) e (L: aset),
    tm_lc v1 -> tm_lc v2 -> (forall (x: atom), x ∉ L -> tm_lc (open_tm 0 (vfvar x) e)) -> tm_lc (tletapp v1 v2 e)
| lc_tletop: forall op (v1: value) e (L: aset),
    tm_lc v1 -> (forall (x: atom), x ∉ L -> tm_lc (open_tm 0 (vfvar x) e)) -> tm_lc (tletop op v1 e)
| lc_tmatchb: forall (v: value) e1 e2, tm_lc v -> tm_lc e1 -> tm_lc e2 -> tm_lc (tmatchb v e1 e2).

Global Hint Constructors tm_lc: core.

Definition tm_body (e: tm) := exists (L: aset), forall (x: atom), x ∉ L -> tm_lc (open_tm 0 (vfvar x) e).

Definition value_lc (v: value) := tm_lc (treturn v).

Definition value_body (v: value) := tm_body (treturn v).

(** TypeClass *)
Class Substable AST  : Type := {
    substitute: atom -> value -> AST -> AST;
    fv: AST -> aset;
  }.

Class Ast AST {H: Substable AST} : Type :=
  {
    open : nat -> value -> AST -> AST;
    close : atom -> nat -> AST -> AST;
    lc : AST -> Prop;
    body : AST -> Prop;
  }.

Definition closed {AST: Type} (v: AST) {H : Substable AST} := fv v ≡ ∅.
Notation "'{' k '~>' s '}' e" := (open k s e) (at level 20, k constr).
Notation "'{' k '~~>' s '}' e" := (open k (vfvar s) e) (at level 20, k constr).
Notation "e '^^' s" := (open 0 s e) (at level 20).
Notation "e '^^^' s" := (open 0 (vfvar s) e) (at level 20).
Notation "'{' s '<~' x '}' e" := (close x s e) (at level 20, s constr).
Notation "x '\' e" := (close x 0 e) (at level 20).
Notation "'{' x ':=' s '}' t" := (substitute x s t) (at level 20).
Notation "x # s" := (x ∉ stale s) (at level 40).

#[export] Instance value_substable : Substable value :=
  {
    substitute := value_subst;
    fv := fv_value;
  }.

#[export] Instance tm_substable : Substable tm :=
  {
    substitute := tm_subst;
    fv := fv_tm;
  }.

#[export] Instance value_ast : Ast value :=
  {
    open := open_value;
    close := close_value;
    lc := value_lc;
    body := value_body;
  }.

#[export] Instance tm_ast : Ast tm :=
  {
    open := open_tm;
    close := close_tm;
    lc := tm_lc;
    body := tm_body;
  }.

(** Syntax Suger *)
Definition mk_app_e_v (e: tm) (v: value) :=
  tlete e (tletapp (vbvar 0) v (vbvar 0)).
