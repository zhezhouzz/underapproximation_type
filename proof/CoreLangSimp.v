(** * MoreStlc: More on the Simply Typed Lambda-Calculus *)

Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From PLF Require Import Types.
From PLF Require Import Smallstep.

Ltac invert :=
  match goal with | H : ?T |- _ =>
                      match type of T with Prop => solve [exfalso; apply H; auto]
                      end
  end.

(* ----------------------------------------------------------------- *)
(** *** Syntax *)

Inductive basic_ty : Type :=
| TNat   : basic_ty
| TBool  : basic_ty.

Inductive constant : Type :=
| cbool: bool -> constant
| cnat : nat -> constant.

Lemma constant_eqb_spec: forall (c c': constant), c = c' \/ c <> c'.
Admitted.

Inductive ty : Type :=
| TBasic : basic_ty -> ty
| TArrow : ty -> ty -> ty.

Coercion TBasic : basic_ty >-> ty.
Coercion cbool : bool >-> constant.
Coercion cnat : nat >-> constant.
Notation " t1 't-->' t2 " := (TArrow t1 t2) (at level 20).

Inductive biop : Type :=
| op_plus
| op_eq
| op_lt.

Inductive value : Type :=
| vconst : constant -> value
| vvar : string -> value
| vlam : string -> ty -> tm -> value
with tm : Type :=
| texn
| tvalue: value -> tm
| tlete: string -> tm -> tm -> tm
| tletbiop: string -> biop -> value -> value -> tm -> tm
| tletapp: string -> value -> value -> tm -> tm.

Scheme value_mutual_rec := Induction for value Sort Type
    with tm_mutual_rec := Induction for tm Sort Type.

Coercion vconst : constant >-> value.
Coercion vvar : string >-> value.
Coercion tvalue : value >-> tm.

Definition is_value (e: tm) :=
  match e with
  | tvalue _ => true
  | _ => false
  end.

Lemma is_value_value_exists: forall e, is_value e = true -> exists v, e = tvalue v.
Proof.
  intros.
  destruct e; inversion H; subst. exists v. auto.
Qed.

Fixpoint subst (x:string) (s:value) (t:tm) : tm :=
  match t with
  |  texn => t
  | tvalue v => tvalue (value_subst x s v)
  | tlete x' t1 t2 =>
      tlete x' (subst x s t1) (if String.eqb x x' then t2 else (subst x s t2))
  | tletbiop x' op v1 v2 t2 =>
      tletbiop x' op (value_subst x s v1) (value_subst x s v2) (if String.eqb x x' then t2 else (subst x s t2))
  | tletapp x' v1 v2 t2 =>
      tletapp x' (value_subst x s v1) (value_subst x s v2)
              (if String.eqb x x' then t2 else (subst x s t2))
  end
with value_subst (x:string) (s:value) (t:value) : value :=
       match t with
       |  vconst _ => t
       |  vvar y => if String.eqb x y then s else t
       |  vlam y T t1 => vlam y T (if String.eqb x y then t1 else (subst x s t1))
       end.

Definition apply_op (op: biop) (a: nat) (b: nat): constant :=
  match op with
  | op_plus =>  (cnat (a + b))
  | op_eq =>  (cbool (Nat.eqb a b))
  | op_lt =>  (cbool (Nat.ltb a b))
  end.

Notation "'[' x ':=' s ']' t" := (subst x s t) (at level 20).
Notation "'[' x ':=' s ']v' t" := (value_subst x s t) (at level 20).

Global Hint Constructors value: core.
Global Hint Constructors tm: core.

Reserved Notation "t1 '-->' t2" (at level 40).

Inductive step : tm -> tm -> Prop :=
| ST_Lete1: forall x e1 e1' e, e1 --> e1' -> (tlete x e1 e) --> (tlete x e1' e)
| ST_Lete2: forall x v1 e, (tlete x (tvalue v1) e) --> (subst x v1 e)
| ST_LetOp: forall x op n1 n2 e,
    (tletbiop x op (vconst (cnat n1)) (vconst (cnat n2)) e) --> (subst x (vconst (apply_op op n1 n2)) e)
| ST_LetAppLam: forall T x y v_x e1 e,
    (tletapp y ((vlam x T e1)) v_x e) --> tlete y (subst x v_x e1) e
where "t1 '-->' t2" := (step t1 t2).

Notation multistep := (multi step).
Notation "t1 '-->*' t2" := (multistep t1 t2) (at level 40).
Notation step_normal_form := (normal_form step).

Lemma value__normal : forall v, step_normal_form (tvalue v).
Proof with eauto.
  intro v. unfold normal_form.
  induction v; intro HF; inversion HF; inversion H.
Qed.

Global Hint Constructors step: core.

Definition op_ret_ty (op: biop): basic_ty :=
  match op with
  | op_plus => TNat
  | op_eq => TBool
  | op_lt => TBool
  end.
