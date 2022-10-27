Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From PLF Require Import Types.
(* From PLF Require Import Smallstep. *)
From PLF Require Import CoreLangSimp.

Definition state := total_map constant.
(* The second constant is the self reference; the refinement is untyped *)
Definition refinement : Type := state -> constant -> Prop.
Implicit Type HH : refinement.

Definition refinment_subst (x:string) (c:constant) (phi:refinement) : refinement :=
  fun st v => phi (t_update st x c) v.

Inductive overbasety : Type :=
| BaseOver: basic_ty -> refinement -> overbasety.

Definition overbasety_subst (x:string) (c:constant) (oty:overbasety) : overbasety :=
  match oty with
  | BaseOver T phi => BaseOver T (refinment_subst x c phi)
  end.

Inductive underarrowty : Type :=
| DependArrow : string -> overbasety -> underty -> underarrowty
| IndependArrow : underarrowty -> underty -> underarrowty
with underty: Type :=
| BaseUnder: basic_ty -> refinement -> underty
| ArrowUnder: underarrowty -> underty.

Scheme underarrowty_mutual_rec := Induction for underarrowty Sort Type
    with underty_mutual_rec := Induction for underty Sort Type.

Fixpoint underarrowty_subst (id:string) (c:constant) (oty:underarrowty) : underarrowty :=
  match oty with
  | DependArrow x t1 t2 => DependArrow x (overbasety_subst id c t1)
                                      (if String.eqb x id then t2 else underty_subst id c t2)
  | IndependArrow t1 t2 => IndependArrow (underarrowty_subst id c t1) (underty_subst id c t2)
  end
with underty_subst (id:string) (c:constant) (oty:underty) : underty :=
       match oty with
       | BaseUnder T phi => BaseUnder T (refinment_subst id c phi)
       | ArrowUnder ty => ArrowUnder (underarrowty_subst id c ty)
       end.

Inductive overunderty : Type :=
| Uty: underty -> overunderty
| Oty: overbasety -> overunderty.

Definition overunderty_subst (id:string) (c:constant) (oty:overunderty) : overunderty :=
  match oty with
  | Uty ty => Uty (underty_subst id c ty)
  | Oty ty => Oty (overbasety_subst id c ty)
  end.

Coercion ArrowUnder :underarrowty >-> underty.
Coercion Uty : underty >-> overunderty.
Coercion Oty : overbasety >-> overunderty.

Definition over_to_under (ot: overbasety) :=
  match ot with
  | BaseOver t phi => BaseUnder t phi
  end.

Notation "'[[v:' t '|' r ']]'" := (BaseUnder t r) (at level 20).
Notation "'{{v:' t '|' r '}}'" := (BaseOver t r) (at level 20).
Notation " x 'o:' overbasety 'o-->' retty " := (DependArrow x overbasety retty) (at level 20).
Notation " underty 'u-->' retty " := (IndependArrow underty retty) (at level 20).

Fixpoint underarrowty_erase (ut: underarrowty) : ty :=
  match ut with
  | _ o: {{v: T1 | _ }} o--> retty => T1 t--> (underty_erase retty)
  | t1 u--> t2 => (underarrowty_erase t1) t--> (underty_erase t2)
  end
with underty_erase (ut: underty) : ty :=
       match ut with
       | [[v: T | _ ]] => T
       | ArrowUnder ut => underarrowty_erase ut
       end.

Definition overbasety_erase (aty: overbasety): ty :=
  match aty with
  | ({{v: T | _ }}) => T
  end.

Definition overunderty_erase (aty: overunderty): ty :=
  match aty with
  | Uty ty => underty_erase ty
  | Oty ty => overbasety_erase ty
  end.

Notation " 'u\_' ty '_/' " := (underty_erase ty) (at level 30).
Notation " 'o\_' ty '_/' " := (overbasety_erase ty) (at level 30).
Notation " 'ou\_' ty '_/' " := (overunderty_erase ty) (at level 30).
