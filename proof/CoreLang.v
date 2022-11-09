Set Warnings "-notation-overridden,-parsing".
From CT Require Import Maps.
From Coq Require Import Arith.PeanoNat.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Classical.

Definition relation (X : Type) := X -> X -> Prop.
Definition deterministic {X : Type} (R : relation X) :=
  forall x y1 y2 : X, R x y1 -> R x y2 -> y1 = y2.

Inductive multi {X : Type} (R : relation X) : relation X :=
  | multi_refl : forall (x : X), multi R x x
  | multi_step : forall (x y z : X),
                    R x y ->
                    multi R y z ->
                    multi R x z.

Global Hint Constructors multi : core.

Theorem multi_R : forall (X : Type) (R : relation X) (x y : X),
    R x y -> (multi R) x y.
Proof.
  intros X R x y H.
  apply multi_step with y. apply H. apply multi_refl.
Qed.


Theorem multi_trans :
  forall (X : Type) (R : relation X) (x y z : X),
      multi R x y  ->
      multi R y z ->
      multi R x z.
Proof.
  intros X R x y z G H.
  induction G.
    - (* multi_refl *) assumption.
    - (* multi_step *)
      apply multi_step with y. assumption.
      apply IHG. assumption.
Qed.

Definition normal_form {X : Type}
              (R : relation X) (t : X) : Prop :=
  ~ exists t', R t t'.

(* ----------------------------------------------------------------- *)
(** *** Syntax *)

Inductive base_ty : Type :=
| TNat   : base_ty
| TBool  : base_ty.

Inductive constant : Type :=
| cbool: bool -> constant
| cnat : nat -> constant.

Global Hint Constructors constant: core.

Lemma constant_eqb_spec: forall (c c': constant), c = c' \/ c <> c'.
Proof with eauto.
  destruct c, c'...
  - destruct b, b0.
    + left...
    + right... intro HH. inversion HH...
    + right... intro HH. inversion HH...
    + left...
  - right. intro HH. inversion HH.
  - right. intro HH. inversion HH.
  - destruct (Nat.eq_dec n n0).
    left... right... intro HH. inversion HH...
Qed.

Inductive ty : Type :=
| TBase : base_ty -> ty
| TArrow : ty -> ty -> ty.

Global Hint Constructors ty: core.

Coercion TBase : base_ty >-> ty.
Coercion cbool : bool >-> constant.
Coercion cnat : nat >-> constant.
Notation " t1 't-->' t2 " := (TArrow t1 t2) (at level 20).

Inductive biop : Type :=
| op_plus
| op_eq
| op_lt
| op_rannat.

Inductive cid: Type :=
| vconst: constant -> cid
| vvar: string -> cid.

Global Hint Constructors cid: core.

Inductive value : Type :=
| vbiop: biop -> value
| cid_value : cid -> value
| vlam : string -> ty -> tm -> value
| vfix : string -> ty -> string -> base_ty -> tm -> value
with tm : Type :=
| texn
| tvalue: value -> tm
| tlete: string -> tm -> tm -> tm
| tletbiop: string -> biop -> value -> value -> tm -> tm
| tletapp: string -> value -> value -> tm -> tm
| tmatchb: value -> tm -> tm -> tm.

Scheme value_mutual_rec := Induction for value Sort Type
    with tm_mutual_rec := Induction for tm Sort Type.

Coercion cid_value : cid >-> value.
Coercion vconst : constant >-> cid.
Coercion vvar : string >-> cid.
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
  | texn => t
  | tvalue v => tvalue (value_subst x s v)
  | tlete x' t1 t2 =>
      tlete x' (subst x s t1) (if String.eqb x x' then t2 else (subst x s t2))
  | tletbiop x' op v1 v2 t2 =>
      tletbiop x' op (value_subst x s v1) (value_subst x s v2) (if String.eqb x x' then t2 else (subst x s t2))
  | tletapp x' v1 v2 t2 =>
      tletapp x' (value_subst x s v1) (value_subst x s v2)
              (if String.eqb x x' then t2 else (subst x s t2))
  | tmatchb v1 t1 t2 => tmatchb (value_subst x s v1) (subst x s t1) (subst x s t2)
  end
with value_subst (x:string) (s:value) (t:value) : value :=
       match t with
       | vbiop _ => t
       | vconst _ => t
       | vvar y => if String.eqb x y then s else t
       | vlam y T t1 => vlam y T (if String.eqb x y then t1 else (subst x s t1))
       |  vfix f T_f y T_y t1 =>
            vfix f T_f y T_y
                 (if String.eqb x f then t1
                  else if String.eqb x y then t1
                       else (subst x s t1))
       end.

Notation "'[' x ':=' s ']' t" := (subst x s t) (at level 20).
Notation "'[' x ':=' s ']v' t" := (value_subst x s t) (at level 20).

Global Hint Constructors value: core.
Global Hint Constructors tm: core.

Lemma subst_letapp_penetrate: forall a c_a x v1 v2, ([a := c_a] tletapp x v1 v2 x) = (tletapp x ([a := c_a]v v1) ([a := c_a]v v2) x).
Proof.
  intros. simpl.
  destruct (eqb_spec a x); reflexivity.
Qed.

Global Hint Resolve subst_letapp_penetrate: core.
Global Hint Rewrite subst_letapp_penetrate: core.

Inductive eval_op: biop -> constant -> constant -> constant -> Prop :=
| eval_op_plus: forall (a b: nat), eval_op op_plus a b (a + b)
| eval_op_eq: forall (a b: nat), eval_op op_eq a b (Nat.eqb a b)
| eval_op_lt: forall (a b: nat), eval_op op_lt a b (Nat.ltb a b)
| eval_op_rannat: forall (a b c: nat), eval_op op_rannat a b c.

Global Hint Constructors eval_op: core.

Reserved Notation "t1 '-->' t2" (at level 40).

Inductive step : tm -> tm -> Prop :=
| ST_LetOp: forall x op c1 c2 c3 e, eval_op op c1 c2 c3 -> (tletbiop x op c1 c2 e) --> (subst x c3 e)
| ST_Lete1: forall x e1 e1' e, e1 --> e1' -> (tlete x e1 e) --> (tlete x e1' e)
| ST_Lete2: forall x v1 e, (tlete x (tvalue v1) e) --> (subst x v1 e)
| ST_LetAppLam: forall T x y v_x e1 e, (tletapp y ((vlam x T e1)) v_x e) --> tlete y (subst x v_x e1) e
| ST_LetAppFix: forall f T_f T x y v_x e1 e, (tletapp y ((vfix f T_f x T e1)) v_x e) --> tlete y (subst f (vfix f T_f x T e1) (subst x v_x e1)) e
| ST_Matchb_true: forall e1 e2, (tmatchb true e1 e2) --> e1
| ST_Matchb_false: forall e1 e2, (tmatchb false e1 e2) --> e1
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
  (fst_ty_of_op op) t--> ((snd_ty_of_op op) t--> (ret_ty_of_op op)).

Lemma op_ty_spec: forall op, ty_of_op op = fst_ty_of_op op t--> (snd_ty_of_op op t--> ret_ty_of_op op).
Proof with eauto.
  intros.
  destruct op...
Qed.

Lemma op_fst_is_op_snd : forall (op: biop), fst_ty_of_op op = snd_ty_of_op op.
Proof with eauto.
  intros.  destruct op...
Qed.

Global Hint Resolve op_ty_spec: core.

(* appear free *)

Fixpoint appear_free_in_tvalue (id:string) (v:value) : Prop :=
  match v with
  | vconst _ => False
  | vbiop _ => False
  | vvar x => x = id
  | vlam x xty e => x <> id /\ appear_free_in_ttm id e
  | vfix f fty x xty e => f <> id /\ x <> id /\ appear_free_in_ttm id e
  end
with appear_free_in_ttm (id:string) (e:tm) : Prop :=
       match e with
       | tvalue v => appear_free_in_tvalue id v
       | tlete x e_x e => appear_free_in_ttm id e_x \/ (x <> id /\ appear_free_in_ttm id e)
       | tletbiop x op v1 v2 e =>
           appear_free_in_tvalue id v1 \/ appear_free_in_tvalue id v2 \/ (x <> id /\ appear_free_in_ttm id e)
       | tletapp x v1 v2 e =>
           appear_free_in_tvalue id v1 \/ appear_free_in_tvalue id v2 \/ (x <> id /\ appear_free_in_ttm id e)
       | tmatchb v e1 e2 =>
           appear_free_in_tvalue id v \/ appear_free_in_ttm id e1 \/ appear_free_in_ttm id e2
       | vexn => False
       end.

Notation " x '\FVtm' e " := (appear_free_in_ttm x e) (at level 40).
Notation " x '\FVvalue' e " := (appear_free_in_tvalue x e) (at level 40).

Lemma lemma1: forall x t s t0,
  ~ (x \FVtm t \/ s <> x /\ x \FVtm t0) -> (~ x \FVtm t /\ (s = x \/ ~ x \FVtm t0)).
Proof with eauto.
  intros. split. intro HH. apply H. left...
  destruct (classic (s = x)). left... right...
Qed.


Lemma lemma2: forall x v v0 s t,
 ~ (x \FVvalue v \/ x \FVvalue v0 \/ s <> x /\ x \FVtm t) -> (~ x \FVtm v /\ ~  x \FVvalue v0 /\ (s = x \/ ~ x \FVtm t)).
Proof with eauto.
  intros; split. destruct (classic (s = x))...
  split. destruct (classic (x \FVvalue v0))...
  destruct (classic (s = x))... right. intro HH. apply H. right. right...
Qed.


Lemma lemma3: forall x v t t0,
 ~ (x \FVvalue v \/ x \FVtm t \/ x \FVtm t0) -> (~ x \FVvalue v /\ ~ x \FVtm t /\ ~ x \FVtm t0).
Proof with eauto.
  intros; split. destruct (classic (x \FVvalue v))...
  split. destruct (classic (x \FVtm t))... destruct (classic (x \FVtm t0))...
Qed.

Lemma not_free_rewrite: forall e x v, ~ x \FVtm e -> [x := v] e = e.
Proof with eauto.
  intro e.
  apply (tm_mutual_rec
           (fun e => forall x v, ~ appear_free_in_tvalue x e -> ([x := v]v e = e))
           (fun e => forall x v, ~ appear_free_in_ttm x e -> ([x := v] e = e))
        ); simpl; intros...
  - destruct c... destruct (eqb_spec x s); subst... exfalso...
  - destruct (eqb_spec x s); subst... assert ([x := v] t0 = t0)... rewrite H1...
  - destruct (eqb_spec x s); subst... destruct (eqb_spec x s0); subst...
    assert ([x := v] t0 = t0)... apply H... rewrite H1...
  - eapply H in H0. rewrite H0...
  - apply lemma1 in H1. destruct H1.
    assert ([x := v] t = t)... rewrite H3.
    destruct (eqb_spec x s); subst...
    destruct H2; subst. exfalso...
    assert ([x := v] t0 = t0)... rewrite H4...
  - apply lemma2 in H2. destruct H2. destruct H3. simpl in H2.
    eapply H in H2... rewrite H2.
    eapply H0 in H3... rewrite H3.
    destruct (eqb_spec x s); subst... destruct H4. exfalso...
    eapply H1 in H4.  rewrite H4...
  - apply lemma2 in H2. destruct H2. destruct H3. simpl in H2.
    eapply H in H2... rewrite H2.
    eapply H0 in H3... rewrite H3.
    destruct (eqb_spec x s); subst... destruct H4. exfalso...
    eapply H1 in H4.  rewrite H4...
  - apply lemma3 in H2. destruct H2. destruct H3.
    rewrite H... rewrite H0... rewrite H1...
Qed.

Global Hint Resolve not_free_rewrite: core.

