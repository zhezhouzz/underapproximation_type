(** * MoreStlc: More on the Simply Typed Lambda-Calculus *)

Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From PLF Require Import Types.
From PLF Require Import Smallstep.
(* From PLF Require Import Stlc. *)

Ltac invert :=
  match goal with | H : ?T |- _ =>
                      match type of T with Prop => solve [exfalso; apply H; auto]
                      end
  end.

Module CoreLang.

  (* ----------------------------------------------------------------- *)
  (** *** Syntax *)

  Inductive ty : Type :=
  | TArrow : ty -> ty -> ty
  | TNat   : ty
  | TBool  : ty
  (* | TList  : ty -> ty *)
  (* | TTree  : ty -> ty *)
  .

  (* Inductive datatype_constant : Type := *)
  (* | dnil *)
  (* | dleaf *)
  (* | dcons: datatype_constant -> datatype_constant -> datatype_constant *)
  (* | dnode: datatype_constant -> datatype_constant -> datatype_constant. *)

  (* Inductive datatype_constant : Type := *)
  (* |  *)

  Inductive constant : Type :=
  (* | cdt: datatype_constructor -> constant -> constant -> constant *)
  | cbool: bool -> constant
  | cnat : nat -> constant.

  Inductive biop : Type :=
  (* | dc: datatype_constructor -> biop *)
  | op_plus
  | op_eq
  | op_lt.

  Inductive value : Type :=
  | vconst : constant -> value
  | vvar : string -> value
  | vlam : string -> ty -> tm -> value
  | vfix : string -> ty -> string -> ty -> tm -> value
  | vexn
  with tm : Type :=
  | tvalue: value -> tm
  | trandom
  | tlete: string -> tm -> tm -> tm
  | tletbiop: string -> biop -> value -> value -> tm -> tm
  | tletapp: string -> value -> value -> tm -> tm
  | tite: value -> tm -> tm -> tm.

  Scheme value_mutual_rec := Induction for value Sort Type
  with tm_mutual_rec := Induction for tm Sort Type.

  Definition x : string := "x".
  Definition y : string := "y".
  Definition z : string := "z".

  Global Hint Unfold x : core.
  Global Hint Unfold y : core.
  Global Hint Unfold z : core.

  Coercion vvar : string >-> value.

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
    | tletapp x' v1 v2 t2 =>
        tletapp x' (value_subst x s v1) (value_subst x s v2) (if String.eqb x x' then t2 else (subst x s t2))
    | tite v1 t1 t2 =>
        tite (value_subst x s v1) (subst x s t1) (subst x s t2)
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

  Definition apply_op (op: biop) (a: nat) (b: nat): value :=
    match op with
    (* | dc dc =>  *)
    | op_plus => vconst (cnat (a + b))
    | op_eq => vconst (cbool (Nat.eqb a b))
    | op_lt => vconst (cbool (Nat.ltb a b))
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
  | ST_LetOp: forall x op n1 n2 e,
      (tletbiop x op (vconst (cnat n1)) (vconst (cnat n2)) e) --> (subst x (apply_op op n1 n2) e)
  | ST_LetOpExn1: forall x op v1 e,
      (tletbiop x op v1 vexn e) --> (tvalue vexn)
  | ST_LetOpExn2: forall x op v2 e,
      (tletbiop x op vexn v2 e) --> (tvalue vexn)
  | ST_LetAppLam: forall T x y v_x e1 e,
      (tletapp y ((vlam x T e1)) v_x e) --> tlete y (subst x v_x e1) e
  | ST_LetAppFix: forall f T_f T x y v_x e1 e,
      (tletapp y ((vfix f T_f x T e1)) v_x e) --> tlete y (subst f (vfix f T_f x T e1) (subst x v_x e1)) e
  | ST_LetAppExn1: forall y v_x e,
      (tletapp y vexn v_x e) --> (tvalue vexn)
  | ST_IteTrue: forall e1 e2,
      (tite (vconst (cbool true)) e1 e2) --> e1
  | ST_IteFalse: forall e1 e2,
      (tite (vconst (cbool false)) e1 e2) --> e2
  | ST_IteExn: forall e1 e2,
      (tite vexn e1 e2) --> (tvalue vexn)

  where "t1 '-->' t2" := (step t1 t2).

  Notation multistep := (multi step).
  Notation "t1 '-->*' t2" := (multistep t1 t2) (at level 40).
  Notation step_normal_form := (normal_form step).

  Lemma value__normal : forall v, step_normal_form (tvalue v).
  Proof with eauto.
    intro v. unfold normal_form.
    induction v; intro HF; inversion HF; inversion H.
  Qed.

  Global Hint Constructors step.

  Definition op_ret_ty (op: biop): ty :=
    match op with
    | op_plus => TNat
    | op_eq => TBool
    | op_lt => TBool
    end.

  Definition context := partial_map ty.

  Reserved Notation "Gamma '|-' t '\in' T" (at level 40).

  Inductive has_type : context -> tm -> ty -> Prop :=
  | T_Random : forall Gamma, has_type Gamma trandom TNat
  | T_Value : forall Gamma value T,
      value_has_type Gamma value T -> has_type Gamma (tvalue value) T
  | T_Lete : forall Gamma x e1 e2 T1 T2,
      has_type Gamma e1 T1 ->
      has_type (update Gamma x T1) e2 T2 ->
      has_type Gamma (tlete x e1 e2) T2
  | T_LetOp : forall Gamma x op v1 v2 e2 T2,
      value_has_type Gamma v1 TNat ->
      value_has_type Gamma v2 TNat ->
      has_type (update Gamma x (op_ret_ty op)) e2 T2 ->
      has_type Gamma (tletbiop x op v1 v2 e2) T2
  | T_LetApp : forall Gamma y v_x v_f e2 T_x T_y T2,
      value_has_type Gamma v_x T_x ->
      value_has_type Gamma v_f (TArrow T_x T_y) ->
      has_type (update Gamma y T_y) e2 T2 ->
      has_type Gamma (tletapp y v_f v_x e2) T2
  | T_Ite: forall Gamma v e1 e2 T,
      value_has_type Gamma v TBool ->
      has_type Gamma e1 T ->
      has_type Gamma e2 T ->
      has_type Gamma (tite v e1 e2) T
  with value_has_type : context -> value -> ty -> Prop :=
  | T_ConstantB : forall Gamma b, value_has_type Gamma (vconst (cbool b)) TBool
  | T_ConstantI : forall Gamma n, value_has_type Gamma (vconst (cnat n)) TNat
  | T_Var : forall Gamma x T, Gamma x = Some T -> value_has_type Gamma (vvar x) T
  | T_Lam : forall Gamma x T11 T12 t12,
      has_type (update Gamma x T11) t12 T12 ->
      value_has_type Gamma (vlam x T11 t12) (TArrow T11 T12)
  | T_Fix : forall Gamma f x T11 T12 t12,
      has_type (update (update Gamma f (TArrow T11 T12)) x T11) t12 T12 ->
      value_has_type Gamma (vfix f (TArrow T11 T12) x T11 t12) (TArrow T11 T12)
  | T_Exn : forall Gamma T, value_has_type Gamma vexn T

  where "Gamma '|-' t '\in' T" := (has_type Gamma t T).
  Notation "Gamma '|-' t '\Vin' T" := (value_has_type Gamma t T) (at level 40).

  Global Hint Constructors has_type: core.
  Global Hint Constructors value_has_type: core.

  Lemma op_type_safe: forall op n1 n2, empty |- apply_op op n1 n2 \Vin op_ret_ty op.
  Proof.
    intro op.
    destruct op; simpl; intros; auto.
  Qed.

  Global Hint Resolve op_type_safe: core.

  Lemma nat_value_n_exists: forall v, empty |- v \Vin TNat -> (exists n, v = (vconst (cnat n))) \/ v = vexn.
  Proof.
    intros.
    destruct v; inversion H; subst.
    - left. exists n. auto.
    - inversion H2.
    - right. auto.
  Qed.

  Lemma bool_value_b_exists: forall v, empty |- v \Vin TBool -> (exists b, v = (vconst (cbool b))) \/ v = vexn.
  Proof.
    intros.
    destruct v; inversion H; subst.
    - left. exists b. auto.
    - inversion H2.
    - right. auto.
  Qed.

  Lemma arrow_value_lam_exists: forall v T1 T2,
      empty |- v \Vin (TArrow T1 T2) ->
              (exists f x e, v = vfix f (TArrow T1 T2) x T1 e) \/ (exists x e, v = vlam x T1 e) \/ v = vexn.
  Proof.
    intros.
    destruct v; inversion H; subst.
    - inversion H2.
    - right. left. exists s. exists t0. auto.
    - left. exists s, s0, t1. auto.
    - right. auto.
  Qed.

  Lemma has_type_inv_value: forall Gamma v T, Gamma |- tvalue v \in T -> Gamma |- v \Vin T.
  Proof.
    intros.
    inversion H. auto.
  Qed.

  (* ================================================================= *)
  (** ** Properties of Typing *)

  (** The proofs of progress and preservation for this enriched system
    are essentially the same (though of course longer) as for the pure
    STLC. *)

  (* ----------------------------------------------------------------- *)
  (** *** Progress *)

  Theorem progress : forall e T,
      empty |- e \in T -> (is_value e = true) \/ (exists e', e --> e').
  Proof with eauto.
    (* Theorem: Suppose empty |- t : T.  Then either
       1. t is a value, or
       2. t --> t' for some t'.
     Proof: By induction on the given typing derivation. *)
    intros t T Ht.
    remember empty as Gamma.
    generalize dependent HeqGamma.
    induction Ht; intros HeqGamma; subst; simpl.
    - right. exists (tvalue (vconst (cnat 0)))...
    - left...
    - right. destruct IHHt1; auto.
      destruct (is_value_value_exists _ H) as (v' & Hv'). subst. exists ([ x0 := v' ] e2)...
      destruct H as (e1' & He'). exists (tlete x0 e1' e2)...
    - right.
      apply nat_value_n_exists in H0.
      apply nat_value_n_exists in H.
      destruct H0, H.
      + destruct H0 as (n2 & H2). destruct H as (n1 & H1). subst. exists ([x0 := (apply_op op n1 n2)] e2)...
      + exists (tvalue vexn). subst...
      + exists (tvalue vexn). subst...
      + exists (tvalue vexn). subst...
   - right.
     destruct (arrow_value_lam_exists _ _ _ H0).
     + destruct H1 as (f & x & e1 & H1). subst. exists (tlete y0 ([f:= (vfix f (TArrow T_x T_y) x T_x e1)] [x:= v_x] e1) e2)...
     + destruct H1.
       destruct H1 as (x & e1 & H1). subst. exists (tlete y0 ([x:= v_x] e1) e2)...
       subst. eauto...
   - right.
      destruct (bool_value_b_exists _ H).
     + destruct H0 as (b & Hb); destruct b; subst...
     + exists (tvalue vexn). subst...
  Qed.

  Definition halts (t:tm) : Prop :=  exists t', t -->* t' /\ is_value t' = true.

  Lemma value_halts : forall v, halts (tvalue v).
  Proof.
    intros v. unfold halts. exists (tvalue v). split; auto.
  Qed.

  (* logical relation *)
  Fixpoint R (T:ty) (t:tm) : Prop :=
    empty |- t \in T /\ halts t /\
                    (match T with
                     | TBool => True
                     | TNat => True
                     | TArrow T1 T2 => forall f y v,
                         R T1 (tvalue v) ->
                         R T2 (tlete f t (tletapp y (vvar f) v
                                                  (tvalue (vvar y))))
                     end).

  Lemma R_halts : forall {T} {t}, R T t -> halts t.
  Proof.
    intros.
    destruct T; unfold R in H; destruct H as [_ [H _]]; assumption.
  Qed.

  Lemma R_typable_empty : forall {T} {t}, R T t -> empty |- t \in T.
  Proof.
    intros.
    destruct T; unfold R in H; destruct H as [H _]; assumption.
  Qed.


  Lemma weakening1:  forall c T Gamma Gamma', includedin Gamma Gamma' -> Gamma |- vconst c \Vin T -> Gamma' |- vconst c \Vin T.
  Proof.
    intros.
    inversion H0; subst; auto.
  Qed.

  Lemma weakening2: (forall (s : string) (T : ty) (Gamma Gamma' : partial_map ty),
        includedin Gamma Gamma' -> Gamma |- s \Vin T -> Gamma' |- s \Vin T).
  Proof.
    intros.
    inversion H0; subst; auto.
  Qed.

  Lemma weakening3: (forall (s : string) (t : ty) (t0 : tm),
        (forall (T : ty) (Gamma Gamma' : partial_map ty),
         includedin Gamma Gamma' -> Gamma |- t0 \in T -> Gamma' |- t0 \in T) ->
        forall (T : ty) (Gamma Gamma' : partial_map ty),
        includedin Gamma Gamma' -> Gamma |- vlam s t t0 \Vin T -> Gamma' |- vlam s t t0 \Vin T).
  Proof.
    intros.
    inversion H1; subst; eauto 7 using includedin_update.
  Qed.

  Lemma weakening4:
    forall (s : string) (t : ty) (s0 : string) (t0 : ty) (t1 : tm),
      (forall (T : ty) (Gamma Gamma' : partial_map ty), includedin Gamma Gamma' -> Gamma |- t1 \in T -> Gamma' |- t1 \in T) ->
      forall (T : ty) (Gamma Gamma' : partial_map ty),
        includedin Gamma Gamma' -> Gamma |- vfix s t s0 t0 t1 \Vin T -> Gamma' |- vfix s t s0 t0 t1 \Vin T.
  Proof.
    intros.
    inversion H1; subst; eauto 7 using includedin_update.
  Qed.


  Lemma weakening5: (forall (T : ty) (Gamma Gamma' : partial_map ty),
                        includedin Gamma Gamma' -> Gamma |- vexn \Vin T -> Gamma' |- vexn \Vin T).
  Proof.
    intros.
    inversion H0; subst; eauto 7 using includedin_update.
  Qed.

  Lemma weakening6:
    (forall v : value,
        (forall (T : ty) (Gamma Gamma' : partial_map ty), includedin Gamma Gamma' -> Gamma |- v \Vin T -> Gamma' |- v \Vin T) ->
        forall (T : ty) (Gamma Gamma' : partial_map ty),
          includedin Gamma Gamma' -> Gamma |- tvalue v \in T -> Gamma' |- tvalue v \in T).
  Proof.
    intros.
    inversion H1; subst; eauto 7 using includedin_update.
  Qed.

  Lemma weakening7:
    (forall (T : ty) (Gamma Gamma' : partial_map ty),
        includedin Gamma Gamma' -> Gamma |- trandom \in T -> Gamma' |- trandom \in T).
  Proof.
    intros.
    inversion H0; subst; eauto 7 using includedin_update.
  Qed.

  Lemma weakening8:
    (forall (s : string) (t : tm),
        (forall (T : ty) (Gamma Gamma' : partial_map ty), includedin Gamma Gamma' -> Gamma |- t \in T -> Gamma' |- t \in T) ->
        forall t0 : tm,
          (forall (T : ty) (Gamma Gamma' : partial_map ty),
              includedin Gamma Gamma' -> Gamma |- t0 \in T -> Gamma' |- t0 \in T) ->
          forall (T : ty) (Gamma Gamma' : partial_map ty),
            includedin Gamma Gamma' -> Gamma |- tlete s t t0 \in T -> Gamma' |- tlete s t t0 \in T).
  Proof.
    intros.
    inversion H2; subst; eauto 7 using includedin_update.
  Qed.

  Lemma weakening9:
    (forall (s : string) (b : biop) (v : value),
        (forall (T : ty) (Gamma Gamma' : partial_map ty),
            includedin Gamma Gamma' -> Gamma |- v \Vin T -> Gamma' |- v \Vin T) ->
        forall v0 : value,
          (forall (T : ty) (Gamma Gamma' : partial_map ty),
              includedin Gamma Gamma' -> Gamma |- v0 \Vin T -> Gamma' |- v0 \Vin T) ->
          forall t : tm,
            (forall (T : ty) (Gamma Gamma' : partial_map ty), includedin Gamma Gamma' -> Gamma |- t \in T -> Gamma' |- t \in T) ->
            forall (T : ty) (Gamma Gamma' : partial_map ty),
              includedin Gamma Gamma' -> Gamma |- tletbiop s b v v0 t \in T -> Gamma' |- tletbiop s b v v0 t \in T).
  Proof.
    intros.
    inversion H3; subst; eauto 7 using includedin_update.
  Qed.

  Lemma weakening10:
    (forall (s : string) (v : value),
        (forall (T : ty) (Gamma Gamma' : partial_map ty),
            includedin Gamma Gamma' -> Gamma |- v \Vin T -> Gamma' |- v \Vin T) ->
        forall v0 : value,
          (forall (T : ty) (Gamma Gamma' : partial_map ty),
              includedin Gamma Gamma' -> Gamma |- v0 \Vin T -> Gamma' |- v0 \Vin T) ->
          forall t : tm,
            (forall (T : ty) (Gamma Gamma' : partial_map ty), includedin Gamma Gamma' -> Gamma |- t \in T -> Gamma' |- t \in T) ->
            forall (T : ty) (Gamma Gamma' : partial_map ty),
              includedin Gamma Gamma' -> Gamma |- tletapp s v v0 t \in T -> Gamma' |- tletapp s v v0 t \in T).
  Proof.
    intros.
    inversion H3; subst; eauto 7 using includedin_update.
  Qed.

  Lemma weakening11:
        (forall v : value,
            (forall (T : ty) (Gamma Gamma' : partial_map ty),
                includedin Gamma Gamma' -> Gamma |- v \Vin T -> Gamma' |- v \Vin T) ->
            forall t : tm,
              (forall (T : ty) (Gamma Gamma' : partial_map ty), includedin Gamma Gamma' -> Gamma |- t \in T -> Gamma' |- t \in T) ->
              forall t0 : tm,
                (forall (T : ty) (Gamma Gamma' : partial_map ty),
                    includedin Gamma Gamma' -> Gamma |- t0 \in T -> Gamma' |- t0 \in T) ->
                forall (T : ty) (Gamma Gamma' : partial_map ty),
                  includedin Gamma Gamma' -> Gamma |- tite v t t0 \in T -> Gamma' |- tite v t t0 \in T).
  Proof.
    intros.
    inversion H3; subst; eauto 7 using includedin_update.
  Qed.

  Lemma weakening : forall e T Gamma Gamma',
      includedin Gamma Gamma' ->
      (Gamma  |- e \in T -> Gamma' |- e \in T).
  Proof.
    intro e.
    apply
      (tm_mutual_rec
         (fun v => forall T Gamma Gamma', includedin Gamma Gamma' -> (Gamma  |- v \Vin T -> Gamma' |- v \Vin T))
         (fun e => forall T Gamma Gamma', includedin Gamma Gamma' -> (Gamma  |- e \in T -> Gamma' |- e \in T))
         weakening1 weakening2 weakening3
         weakening4 weakening5 weakening6 weakening7 weakening8 weakening9 weakening10 weakening11
      ).
  Qed.

  Lemma value_weakening : forall e T Gamma Gamma',
      includedin Gamma Gamma' ->
      (Gamma  |- e \Vin T -> Gamma' |- e \Vin T).
  Proof.
    intros.
    apply weakening with (e:=tvalue e) (T:=T) in H; auto.
    inversion H; subst; auto.
  Qed.

  Lemma weakening_empty : forall Gamma t T,
      empty |- t \in T  -> Gamma |- t \in T.
  Proof.
    intros Gamma t T.
    eapply weakening.
    discriminate.
  Qed.

  Lemma value_weakening_empty : forall Gamma t T,
      empty |- t \Vin T  -> Gamma |- t \Vin T.
  Proof.
    intros Gamma t T H.
    assert (empty |- (tvalue t) \in T). auto.
    apply weakening_empty with (Gamma:=Gamma) in H0. inversion H0; subst; auto.
  Qed.

  Lemma substitution_preserves_typing1:
  (forall (c : constant) (Gamma : partial_map ty) (x : string) (U : ty) (v : value) (T : ty),
        (x |-> U; Gamma) |- vconst c \Vin T -> empty |- v \Vin U -> Gamma |- [x := v ]v vconst c \Vin T).
  Proof.
    intros. inversion H; clear H; subst; simpl; eauto.
  Qed.

  Lemma substitution_preserves_typing2:
    (forall (s : string) (Gamma : partial_map ty) (x : string) (U : ty) (v : value) (T : ty),
        (x |-> U; Gamma) |- s \Vin T -> empty |- v \Vin U -> Gamma |- [x := v ]v s \Vin T).
  Proof.
    intros. inversion H; clear H; subst; simpl; eauto.
    rename s into y. rename x0 into x.
    destruct (eqb_spec x y); subst.
      + (* x=y *)
        rewrite update_eq in H3.
        injection H3 as H3; subst.
        apply value_weakening_empty. assumption.
      + (* x<>y *)
        apply T_Var. rewrite update_neq in H3; auto.
  Qed.

  Lemma substitution_preserves_typing3:
  (forall (s : string) (t : ty) (t0 : tm),
        (forall (Gamma : partial_map ty) (x : string) (U : ty) (v : value) (T : ty),
         (x |-> U; Gamma) |- t0 \in T -> empty |- v \Vin U -> Gamma |- [x := v] t0 \in T) ->
        forall (Gamma : partial_map ty) (x : string) (U : ty) (v : value) (T : ty),
        (x |-> U; Gamma) |- vlam s t t0 \Vin T -> empty |- v \Vin U -> Gamma |- [x := v ]v vlam s t t0 \Vin T).
  Proof.
    intros. inversion H0; clear H0; subst; simpl; eauto.
    rename s into y. rename x0 into x.
    destruct (eqb_spec x y); subst; apply T_Lam.
    + (* x=y *)
      rewrite update_shadow in H7. assumption.
    + (* x<>y *)
      apply H with (U:=U); auto.
      rewrite update_permute; auto.
  Qed.

  Lemma substitution_preserves_typing_fix:
    forall (s : string) (t0 : ty) (s0 : string) (t1 : ty) (t2 : tm),
      (forall (Gamma : partial_map ty) (x : string) (U : ty) (v : value) (T : ty),
          (x |-> U; Gamma) |- t2 \in T -> empty |- v \Vin U -> Gamma |- [x := v] t2 \in T) ->
      forall (Gamma : partial_map ty) (x0 : string) (U : ty) (v : value) (T : ty),
        (x0 |-> U; Gamma) |- vfix s t0 s0 t1 t2 \Vin T -> empty |- v \Vin U -> Gamma |- [x0 := v ]v vfix s t0 s0 t1 t2 \Vin T.
  Proof.
    intros. inversion H0; clear H0; subst; simpl; eauto.
    rename s into y. rename x0 into x.
    destruct (eqb_spec x y); subst; apply T_Fix.
    + (* x=y *)
      rewrite update_shadow in H9. assumption.
    + (* x<>y *)
      destruct (eqb_spec x s0); subst.
      - rewrite update_permute in H9; auto.
        rewrite update_shadow in H9; auto.
        rewrite update_permute in H9; auto.
      - apply H with (U:=U); auto.
        rewrite update_permute; auto.
        rewrite (update_permute _ _ x y); auto.
  Qed.

  Lemma substitution_preserves_typing4:
    (forall (Gamma : partial_map ty) (x : string) (U : ty) (v : value) (T : ty),
        (x |-> U; Gamma) |- vexn \Vin T -> empty |- v \Vin U -> Gamma |- [x := v ]v vexn \Vin T).
  Proof.
    intros. inversion H0; clear H0; subst; simpl; eauto.
    Qed.

  Lemma substitution_preserves_typing5:
    (forall v : value,
        (forall (Gamma : partial_map ty) (x : string) (U : ty) (v0 : value) (T : ty),
            (x |-> U; Gamma) |- v \Vin T -> empty |- v0 \Vin U -> Gamma |- [x := v0 ]v v \Vin T) ->
        forall (Gamma : partial_map ty) (x : string) (U : ty) (v0 : value) (T : ty),
          (x |-> U; Gamma) |- tvalue v \in T -> empty |- v0 \Vin U -> Gamma |- [x := v0] tvalue v \in T).
  Proof.
    intros. inversion H0; clear H0; subst; simpl; eauto.
  Qed.

  Lemma substitution_preserves_typing6:
    (forall (Gamma : partial_map ty) (x : string) (U : ty) (v : value) (T : ty),
        (x |-> U; Gamma) |- trandom \in T -> empty |- v \Vin U -> Gamma |- [x := v] trandom \in T).
  Proof.
    intros. inversion H; clear H; subst; simpl; eauto.
  Qed.

  Lemma substitution_preserves_typing7:
    (forall (s : string) (t : tm),
        (forall (Gamma : partial_map ty) (x : string) (U : ty) (v : value) (T : ty),
            (x |-> U; Gamma) |- t \in T -> empty |- v \Vin U -> Gamma |- [x := v] t \in T) ->
        forall t0 : tm,
          (forall (Gamma : partial_map ty) (x : string) (U : ty) (v : value) (T : ty),
              (x |-> U; Gamma) |- t0 \in T -> empty |- v \Vin U -> Gamma |- [x := v] t0 \in T) ->
          forall (Gamma : partial_map ty) (x : string) (U : ty) (v : value) (T : ty),
            (x |-> U; Gamma) |- tlete s t t0 \in T -> empty |- v \Vin U -> Gamma |- [x := v] tlete s t t0 \in T).
    Proof.
      intros. inversion H1; clear H1; subst; simpl; eauto.
      rename s into y. rename x0 into x.
      destruct (eqb_spec x y); subst.
      + (* x=y *)
        rewrite update_shadow in H9.
        apply T_Lete with (T1:= T1); auto. apply H with (U:=U); auto.
      + (* x<>y *)
        apply T_Lete with (T1:= T1); auto. apply H with (U:=U); auto.
        rewrite update_permute in H9; auto. apply H0 with (U:=U); auto.
    Qed.

  Lemma substitution_preserves_typing8:
    (forall (s : string) (b : biop) (v : value),
        (forall (Gamma : partial_map ty) (x : string) (U : ty) (v0 : value) (T : ty),
            (x |-> U; Gamma) |- v \Vin T -> empty |- v0 \Vin U -> Gamma |- [x := v0 ]v v \Vin T) ->
        forall v0 : value,
          (forall (Gamma : partial_map ty) (x : string) (U : ty) (v1 : value) (T : ty),
              (x |-> U; Gamma) |- v0 \Vin T -> empty |- v1 \Vin U -> Gamma |- [x := v1 ]v v0 \Vin T) ->
          forall t : tm,
            (forall (Gamma : partial_map ty) (x : string) (U : ty) (v1 : value) (T : ty),
                (x |-> U; Gamma) |- t \in T -> empty |- v1 \Vin U -> Gamma |- [x := v1] t \in T) ->
            forall (Gamma : partial_map ty) (x : string) (U : ty) (v1 : value) (T : ty),
              (x |-> U; Gamma) |- tletbiop s b v v0 t \in T ->  empty |- v1 \Vin U -> Gamma |- [x := v1] tletbiop s b v v0 t \in T).
    Proof.
      intros. inversion H2; clear H2; subst; simpl; eauto.
      rename s into y. rename x0 into x.
      destruct (eqb_spec x y); subst.
      + (* x=y *)
        rewrite update_shadow in H13.
        apply T_LetOp; auto. apply H with (U:=U); auto. apply H0 with (U:=U); auto.
      + (* x<>y *)
        apply T_LetOp; auto. apply H with (U:=U); auto.
        rewrite update_permute in H13; auto. apply H0 with (U:=U); auto.
        rewrite update_permute in H13; auto. apply H1 with (U:=U); auto.
    Qed.

  Lemma substitution_preserves_typing9:
    (forall (s : string) (v : value),
        (forall (Gamma : partial_map ty) (x : string) (U : ty) (v0 : value) (T : ty),
            (x |-> U; Gamma) |- v \Vin T -> empty |- v0 \Vin U -> Gamma |- [x := v0 ]v v \Vin T) ->
        forall v0 : value,
          (forall (Gamma : partial_map ty) (x : string) (U : ty) (v1 : value) (T : ty),
              (x |-> U; Gamma) |- v0 \Vin T -> empty |- v1 \Vin U -> Gamma |- [x := v1 ]v v0 \Vin T) ->
          forall t : tm,
            (forall (Gamma : partial_map ty) (x : string) (U : ty) (v1 : value) (T : ty),
                (x |-> U; Gamma) |- t \in T -> empty |- v1 \Vin U -> Gamma |- [x := v1] t \in T) ->
            forall (Gamma : partial_map ty) (x : string) (U : ty) (v1 : value) (T : ty),
              (x |-> U; Gamma) |- tletapp s v v0 t \in T -> empty |- v1 \Vin U -> Gamma |- [x := v1] tletapp s v v0 t \in T).
   Proof.
      intros. inversion H2; clear H2; subst; simpl; eauto.
      rename s into y. rename x0 into x.
      destruct (eqb_spec x y); subst.
      + (* x=y *)
        rewrite update_shadow in H12.
        apply T_LetApp with (T_x:=T_x) (T_y:=T_y); auto. apply H0 with (U:=U); auto. apply H with (U:=U); auto.
      + (* x<>y *)
        rewrite update_permute in H12; auto.
        apply T_LetApp with (T_x:=T_x) (T_y:=T_y); auto.
        apply H0 with (U:=U); auto.
        apply H with (U:=U); auto.
        apply H1 with (U:=U); auto.
   Qed.

  Lemma substitution_preserves_typing10:
    (forall v : value,
        (forall (Gamma : partial_map ty) (x : string) (U : ty) (v0 : value) (T : ty),
            (x |-> U; Gamma) |- v \Vin T -> empty |- v0 \Vin U -> Gamma |- [x := v0 ]v v \Vin T) ->
        forall t : tm,
          (forall (Gamma : partial_map ty) (x : string) (U : ty) (v0 : value) (T : ty),
              (x |-> U; Gamma) |- t \in T -> empty |- v0 \Vin U -> Gamma |- [x := v0] t \in T) ->
          forall t0 : tm,
            (forall (Gamma : partial_map ty) (x : string) (U : ty) (v0 : value) (T : ty),
                (x |-> U; Gamma) |- t0 \in T -> empty |- v0 \Vin U -> Gamma |- [x := v0] t0 \in T) ->
            forall (Gamma : partial_map ty) (x : string) (U : ty) (v0 : value) (T : ty),
              (x |-> U; Gamma) |- tite v t t0 \in T -> empty |- v0 \Vin U -> Gamma |- [x := v0] tite v t t0 \in T).
  Proof.
    intros. inversion H2; clear H2; subst; simpl; eauto.
  Qed.


  Lemma substitution_preserves_typing : forall t Gamma x U v T,
      (x |-> U ; Gamma) |- t \in T ->  empty |- v \Vin U  ->  Gamma |- [x:=v]t \in T.
  Proof with eauto.
    intro t.
    apply (tm_mutual_rec
             (fun ev => forall Gamma x U v T, (x |-> U ; Gamma) |- ev \Vin T ->  empty |- v \Vin U ->  Gamma |- [x:=v]v ev \Vin T)
             (fun e => forall Gamma x U v T, (x |-> U ; Gamma) |- e \in T ->  empty |- v \Vin U ->  Gamma |- [x:=v] e \in T)
             substitution_preserves_typing1 substitution_preserves_typing2 substitution_preserves_typing3
             substitution_preserves_typing_fix
             substitution_preserves_typing4 substitution_preserves_typing5 substitution_preserves_typing6
             substitution_preserves_typing7 substitution_preserves_typing8 substitution_preserves_typing9
             substitution_preserves_typing10
          ).
  Qed.

  Theorem preservation : forall t t' T,
      empty |- t \in T  ->
                    t --> t'  ->
                    empty |- t' \in T.
  Proof with eauto.
    intros t t' T HT. generalize dependent t'.
    remember empty as Gamma.
    induction HT;
      intros t' HE; subst; inversion HE; subst...
    - apply substitution_preserves_typing with T1...
      inversion HT1; subst...
    - apply substitution_preserves_typing with (op_ret_ty op)...
    - apply T_Lete with (T1 := T_y)...
      apply substitution_preserves_typing with T_x...
      inversion H0; subst...
    - apply T_Lete with (T1 := T_y)...
      apply substitution_preserves_typing with (TArrow T_x T_y)...
      apply substitution_preserves_typing with T_x...
      inversion H0; subst...
  Qed.

End CoreLang.
