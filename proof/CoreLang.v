(** * MoreStlc: More on the Simply Typed Lambda-Calculus *)

Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From PLF Require Import Types.
From PLF Require Import Smallstep.
From PLF Require Import BasicConstant.

Ltac invert :=
  match goal with | H : ?T |- _ =>
                      match type of T with Prop => solve [exfalso; apply H; auto]
                      end
  end.


(* ----------------------------------------------------------------- *)
(** *** Syntax *)

Inductive basic_ty : Type :=
| TNat   : basic_ty
| TBool  : basic_ty
| TList  : basic_ty -> basic_ty
| TTree  : basic_ty -> basic_ty.

Inductive constant : Type :=
| cnil: basic_ty -> constant
| cleaf: basic_ty -> constant
| ccons: basic_ty -> constant -> constant -> constant
| cnode: basic_ty -> constant -> constant -> constant -> constant
| cbool: bool -> constant
| cnat : nat -> constant.

Inductive ty : Type :=
| TBasic : basic_ty -> ty
| TArrow : ty -> ty -> ty.

Definition state := total_map constant.
(* The second constant is the self reference *)
Definition refinement : Type := state -> constant -> Prop.
Implicit Type HH : refinement.

Inductive oty : Type :=
| BaseOver: basic_ty -> refinement -> oty.

Inductive uty : Type :=
| BaseUnder: basic_ty -> refinement -> uty
| DependArrow : string -> oty -> uty -> uty
| IndependArrow : uty -> uty -> uty.

Inductive outy : Type :=
| Uty: uty -> outy
| Oty: oty -> outy.

Coercion Uty : uty >-> outy.
Coercion Oty : oty >-> outy.
Coercion TBasic : basic_ty >-> ty.
Coercion cbool : bool >-> constant.
Coercion cnat : nat >-> constant.

Definition ot_to_ut (ot: oty) :=
  match ot with
  | BaseOver t phi => BaseUnder t phi
  end.

Notation " t1 't-->' t2 " := (TArrow t1 t2) (at level 20).
Notation "'[[v:' t '|' r ']]'" := (BaseUnder t r) (at level 20).
Notation "'{{v:' t '|' r '}}'" := (BaseOver t r) (at level 20).
Notation " x 'o:' oty 'o-->' retty " := (DependArrow x oty retty) (at level 20).
Notation " uty 'u-->' retty " := (IndependArrow uty retty) (at level 20).

Fixpoint erase (uty: uty) : ty :=
  match uty with
  | [[v: T | _ ]] => T
  | _ o: {{v: T1 | _ }} o--> retty => T1 t--> (erase retty)
  | t1 u--> t2 => (erase t1) t--> (erase t2)
  end.

Definition oty_erase (aty: oty): ty :=
  match aty with
  | ({{v: T | _ }}) => T
  end.

Definition outy_erase (aty: outy): ty :=
  match aty with
  | Uty ty => erase ty
  | Oty ty => oty_erase ty
  end.

Inductive biop : Type :=
| op_plus
| op_eq
| op_lt.

Inductive value : Type :=
| vconst : constant -> value
| vvar : string -> value
| vlam : string -> outy -> tm -> value
| vfix : string -> uty -> string -> oty -> tm -> value
| vexn
with tm : Type :=
| tvalue: value -> tm
| trandom
| tlete: string -> tm -> tm -> tm
| tletbiop: string -> biop -> value -> value -> tm -> tm
| tlets: string -> value -> tm -> tm
| tletcons: string -> basic_ty -> value -> value -> tm -> tm
| tletnode: string -> basic_ty -> value -> value -> value -> tm -> tm
| tletapp: string -> value -> value -> tm -> tm
| tmatchb: value -> tm -> tm -> tm
| tmatchn: value -> tm -> string -> tm -> tm
| tmatchl: basic_ty -> value -> tm -> string -> string -> tm -> tm
| tmatcht: basic_ty -> value -> tm -> string -> string -> string -> tm -> tm.

Scheme value_mutual_rec := Induction for value Sort Type
    with tm_mutual_rec := Induction for tm Sort Type.

Coercion vvar : string >-> value.
Coercion vconst : constant >-> value.

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
  | tvalue v => tvalue (value_subst x s v)
  | trandom => t
  | tlete x' t1 t2 =>
      tlete x' (subst x s t1) (if String.eqb x x' then t2 else (subst x s t2))
  | tletbiop x' op v1 v2 t2 =>
      tletbiop x' op (value_subst x s v1) (value_subst x s v2) (if String.eqb x x' then t2 else (subst x s t2))
  | tlets x' v1 t2 =>
      tlets x' (value_subst x s v1)
            (if String.eqb x x' then t2 else (subst x s t2))
  | tletcons x' T v1 v2 t2 =>
      tletcons x' T (value_subst x s v1) (value_subst x s v2)
               (if String.eqb x x' then t2 else (subst x s t2))
  | tletnode x' T v1 v2 v3 t2 =>
      tletnode x' T (value_subst x s v1) (value_subst x s v2) (value_subst x s v3) (if String.eqb x x' then t2 else (subst x s t2))
  | tletapp x' v1 v2 t2 =>
      tletapp x' (value_subst x s v1) (value_subst x s v2)
              (if String.eqb x x' then t2 else (subst x s t2))
  | tmatchb v1 t1 t2 =>
      tmatchb (value_subst x s v1) (subst x s t1) (subst x s t2)
  | tmatchn v1 t1 n' t2 =>
      tmatchn (value_subst x s v1) (subst x s t1) n'
              (if String.eqb x n' then t2 else (subst x s t2))
  | tmatchl T v e1 h t e2 =>
      tmatchl T (value_subst x s v)
              (subst x s e1)
              h t
              (if String.eqb x h then e2
               else if String.eqb x t then e2
                    else (subst x s e2))
  | tmatcht T v e1 root lt rt e2 =>
      tmatcht T (value_subst x s v)
              (subst x s e1)
              root lt rt
              (if String.eqb x root then e2
               else if String.eqb x lt then e2
                    else if String.eqb x rt then e2
                         else (subst x s e2))
  end
with value_subst (x:string) (s:value) (t:value) : value :=
       match t with
       |  vconst _ => t
       |  vvar y => if String.eqb x y then s else t
       |  vlam y T t1 => vlam y T (if String.eqb x y then t1 else (subst x s t1))
       |  vfix f T_f y T_y t1 =>
            vfix f T_f y T_y
                 (if String.eqb x f then t1
                  else if String.eqb x y then t1
                       else (subst x s t1))
       |  vexn => t
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
| ST_Random: forall n:nat, trandom --> (tvalue (vconst (cnat n)))
| ST_Lete1: forall x e1 e1' e, e1 --> e1' -> (tlete x e1 e) --> (tlete x e1' e)
| ST_Lete2: forall x v1 e, (tlete x (tvalue v1) e) --> (subst x v1 e)
| ST_LetAppLam: forall T x y v_x e1 e, (tletapp y ((vlam x T e1)) v_x e) --> tlete y (subst x v_x e1) e
| ST_LetAppFix: forall f T_f T x y v_x e1 e, (tletapp y ((vfix f T_f x T e1)) v_x e) --> tlete y (subst f (vfix f T_f x T e1) (subst x v_x e1)) e
| ST_Matchb_true: forall e1 e2, (tmatchb true e1 e2) --> e1
| ST_Matchb_false: forall e1 e2, (tmatchb false e1 e2) --> e2
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

