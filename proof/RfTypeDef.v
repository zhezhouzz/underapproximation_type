Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From PLF Require Import Types.
(* From PLF Require Import Smallstep. *)
From PLF Require Import CoreLangSimp.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.

Definition state := total_map constant.
(* The second constant is the self reference; the refinement is untyped *)
Definition refinement : Type := state -> constant -> Prop.
Implicit Type HH : refinement.

Inductive overbasety : Type :=
| BaseOver: base_ty -> refinement -> overbasety.

Global Hint Constructors overbasety: core.

Inductive underty : Type :=
| BaseUnder: base_ty -> refinement -> underty
| DependArrow : string -> overbasety -> underty -> underty
| IndependArrow : underty -> underty -> underty.

Global Hint Constructors underty: core.

(* Fixpoint well_formed_underty_bool (tau: underty): bool := *)
(*   match tau with *)
(*   | BaseUnder _ _ => true *)
(*   | DependArrow _ _ tau => well_formed_underty_bool tau *)
(*   | IndependArrow t1 t2 => *)
(*       if well_formed_underty_bool t2 *)
(*       then *)
(*         match t1 with *)
(*         | BaseUnder _ _ => false *)
(*         | _ => well_formed_underty_bool t1 *)
(*         end *)
(*       else *)
(*         false *)
(*   end. *)

Inductive well_formed_underty: underty -> Prop :=
| well_formed_underty_base: forall T phi, well_formed_underty (BaseUnder T phi)
| well_formed_underty_oarr: forall a T phi tau, well_formed_underty tau -> well_formed_underty (DependArrow a (BaseOver T phi) tau)
| well_formed_underty_arrarr1: forall a tau_a tau tau',
    well_formed_underty (DependArrow a tau_a tau) -> well_formed_underty tau' ->
    well_formed_underty (IndependArrow (DependArrow a tau_a tau) tau')
| well_formed_underty_arrarr2: forall t1 t2 tau',
    well_formed_underty (IndependArrow t1 t2) -> well_formed_underty tau' ->
    well_formed_underty (IndependArrow (IndependArrow t1 t2) t2).

Global Hint Constructors well_formed_underty: core.

(* Definition well_formed_underty (tau : underty): Prop := well_formed_underty_bool tau = true. *)

Inductive overunderty : Type :=
| Uty: underty -> overunderty
| Oty: overbasety -> overunderty.

Global Hint Constructors overunderty: core.

Inductive well_formed_type: overunderty -> Prop :=
| well_formed_type_over: forall (tau: overbasety), well_formed_type (Oty tau)
| well_formed_type_under: forall tau, well_formed_underty tau -> well_formed_type (Uty tau).

Global Hint Constructors well_formed_type: core.

Coercion Uty : underty >-> overunderty.
Coercion Oty : overbasety >-> overunderty.

Definition over_to_under (ot: overbasety) :=
  match ot with
  | BaseOver t phi => BaseUnder t phi
  end.

Notation "'[[v:' t '|' r ']]'" := (BaseUnder t r) (at level 80).
Notation "'{{v:' t '|' r '}}'" := (BaseOver t r) (at level 80).
Notation " x 'o:' overbasety 'o-->' retty " := (DependArrow x overbasety retty) (at level 80).
Notation " underty 'u-->' retty " := (IndependArrow underty retty) (at level 80).

Fixpoint underty_erase (ut: underty) : ty :=
  match ut with
  | [[v: T | _ ]] => T
  | _ o: {{v: T1 | _ }} o--> retty => T1 t--> (underty_erase retty)
  | t1 u--> t2 => (underty_erase t1) t--> (underty_erase t2)
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

Definition eval_cid (st: state) (v: cid) :=
  match v with
  | vconst c => c
  | vvar name => st name
  end.

Definition mk_op_retty_from_cids (op: biop) (cid1 cid2: cid): underty :=
  BaseUnder (ret_ty_of_op op) (fun st c => eval_op op (eval_cid st cid1) (eval_cid st cid2) c).

(* closed_refinement *)

Definition is_closed_refinement (_: base_ty) (phi: refinement): Prop := forall st st', phi st = phi st'.

Global Hint Unfold is_closed_refinement: core.

Lemma constant_refinement_is_closed: forall (c: constant) (phi: refinement),
    (forall st : state, phi st = (fun c' => c = c')) -> (forall T, is_closed_refinement T phi).
Proof.
  intros.
  intros st st'. rewrite H. rewrite H. auto.
Qed.

(* subst id *)
Definition refinement_subst_id (x1 x2: string) (phi: refinement): refinement :=
  (fun st c => phi (t_update st x1 (st x2)) c).

Definition over_subst_id (x1 x2: string) (oty: overbasety): overbasety :=
  match oty with
  | BaseOver T phi => BaseOver T (refinement_subst_id x1 x2 phi)
  end.

Fixpoint under_subst_id (x1 x2: string) (uty: underty): underty :=
  match uty with
  | BaseUnder T phi => BaseUnder T (refinement_subst_id x1 x2 phi)
  | DependArrow x xty retty =>
      if String.eqb x1 x then
        DependArrow x (over_subst_id x1 x2 xty) retty
      else
        DependArrow x (over_subst_id x1 x2 xty) (under_subst_id x1 x2 retty)
  | IndependArrow t1 t2 =>
      IndependArrow (under_subst_id x1 x2 t1) (under_subst_id x1 x2 t2)
  end.

Definition overunder_subst_id (x1: string) (c2: string) (outy: overunderty): overunderty :=
  match outy with
  | Uty uty => under_subst_id x1 c2 uty
  | Oty oty => over_subst_id x1 c2 oty
  end.

Notation " '<r[' x '|id->' y ']>' uty " := (refinement_subst_id x y uty) (at level 40).
Notation " '<u[' x '|id->' y ']>' uty " := (under_subst_id x y uty) (at level 40).
Notation " '<o[' x '|id->' y ']>' uty " := (over_subst_id x y uty) (at level 40).
Notation " '<ou[' x '|id->' y ']>' uty " := (overunder_subst_id x y uty) (at level 40).

(* subst constant *)
Definition refinement_subst_c (x1: string) (c2: constant) (phi: refinement): refinement :=
  (fun st => phi (t_update st x1 c2)).

Lemma refinement_subst_c_closed_rf: forall x1 c2 T phi,
    is_closed_refinement T phi -> refinement_subst_c x1 c2 phi = phi.
Proof.
  intros.
  unfold is_closed_refinement in H. unfold refinement_subst_c.
  apply functional_extensionality. intros st.
  erewrite H. reflexivity.
Qed.

Definition over_subst_c (x1: string) (c2: constant) (oty: overbasety): overbasety :=
  match oty with
  | BaseOver T phi => BaseOver T (refinement_subst_c x1 c2 phi)
  end.

Fixpoint under_subst_c (x1: string) (c2: constant) (uty: underty): underty :=
  match uty with
  | BaseUnder T phi => BaseUnder T (refinement_subst_c x1 c2 phi)
  | DependArrow x xty retty =>
      if String.eqb x1 x then
        DependArrow x (over_subst_c x1 c2 xty) retty
      else
        DependArrow x (over_subst_c x1 c2 xty) (under_subst_c x1 c2 retty)
  | IndependArrow t1 t2 =>
      IndependArrow (under_subst_c x1 c2 t1) (under_subst_c x1 c2 t2)
  end.

Definition overunder_subst_c (x1: string) (c2: constant) (outy: overunderty): overunderty :=
  match outy with
  | Uty uty => under_subst_c x1 c2 uty
  | Oty oty => over_subst_c x1 c2 oty
  end.

Notation " '<r[' x '|c->' y ']>' uty " := (refinement_subst_c x y uty) (at level 40).
Notation " '<u[' x '|c->' y ']>' uty " := (under_subst_c x y uty) (at level 40).
Notation " '<o[' x '|c->' y ']>' uty " := (over_subst_c x y uty) (at level 40).
Notation " '<ou[' x '|c->' y ']>' uty " := (overunder_subst_c x y uty) (at level 40).

Definition under_subst_cid (x1: string) (cv: cid) (outy: underty): underty :=
  match cv with
  | vconst c => under_subst_c x1 c outy
  | vvar id => under_subst_id x1 id outy
  end.

(* appear free *)

Definition appear_free_in_refinement (name: string) (phi: refinement): Prop :=
  (exists st (c1 c2:constant),
      phi (t_update st name c1) <> phi (t_update st name c2)
  ).

Definition appear_free_in_overbasety (name: string) (oty: overbasety): Prop :=
  match oty with
  | {{v: _ | phi }} => appear_free_in_refinement name phi
  end.

Fixpoint appear_free_in_underty (name: string) (uty: underty): Prop :=
  match uty with
  | [[v: _ | phi ]] => appear_free_in_refinement name phi
  | DependArrow x xty retty =>
      appear_free_in_overbasety name xty \/ (x <> name /\ appear_free_in_underty name retty)
  | IndependArrow t1 t2 =>
      appear_free_in_underty name t1 \/ appear_free_in_underty name t2
  end.

Definition appear_free_in_overunderty (name: string) (overunderty: overunderty): Prop :=
  match overunderty with
  | Uty uty => appear_free_in_underty name uty
  | Oty oty => appear_free_in_overbasety name oty
  end.

Notation " x '\FVty' e " := (appear_free_in_overunderty x e) (at level 40).

(* \S{Ty}: *)

Definition mk_eq_constant c := [[v: ty_of_const c | (fun _ v => v = c) ]].
Definition mk_bot ty := [[v: ty | (fun _ _ => False) ]].
Definition mk_top ty := [[v: ty | (fun _ _ => True) ]].
Definition mk_over_top ty := {{v: ty | (fun _ _ => True) }}.
Definition mk_eq_var ty name := [[v: ty | (fun state v => v = (state name)) ]].
Definition mk_op op a b :=
  (a o: (mk_over_top (fst_ty_of_op op)) o--> (b o: (mk_over_top (fst_ty_of_op op)) o--> ([[v: ret_ty_of_op op | (fun st c => eval_op op (st a) (st b) c ) ]]))).
