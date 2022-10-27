Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From PLF Require Import Types.
From PLF Require Import Smallstep.
From PLF Require Import CoreLangSimp.

Ltac invert :=
  match goal with | H : ?T |- _ =>
                      match type of T with Prop => solve [exfalso; apply H; auto]
                      end
  end.

Import CoreLangSimp.

Definition context := partial_map ty.

Reserved Notation "Gamma '|-' t '\in' T" (at level 40).

Inductive has_type : context -> tm -> ty -> Prop :=
| T_Exn : forall Gamma T, has_type Gamma texn T
| T_Value : forall Gamma value T,
    value_has_type Gamma value T -> has_type Gamma (tvalue value) T
| T_Lete : forall Gamma x e1 e2 T1 T2,
    has_type Gamma e1 T1 ->
    has_type (update Gamma x T1) e2 T2 ->
    has_type Gamma (tlete x e1 e2) T2
| T_LetOp : forall Gamma x op v1 v2 e2 T2,
    value_has_type Gamma v1 (TBasic TNat) ->
    value_has_type Gamma v2 (TBasic TNat) ->
    has_type (update Gamma x (TBasic (op_ret_ty op))) e2 T2 ->
    has_type Gamma (tletbiop x op v1 v2 e2) T2
| T_LetApp : forall Gamma y v_x v_f e2 T_x T_y T2,
    value_has_type Gamma v_x T_x ->
    value_has_type Gamma v_f (TArrow T_x T_y) ->
    has_type (update Gamma y T_y) e2 T2 ->
    has_type Gamma (tletapp y v_f v_x e2) T2
with value_has_type : context -> value -> ty -> Prop :=
| T_ConstantB : forall Gamma b, value_has_type Gamma (vconst (cbool b)) (TBasic TBool)
| T_ConstantI : forall Gamma n, value_has_type Gamma (vconst (cnat n)) (TBasic TNat)
| T_Var : forall Gamma x T, Gamma x = Some T -> value_has_type Gamma (vvar x) T
| T_Lam : forall Gamma x T11 T12 t12,
    has_type (update Gamma x T11) t12 T12 ->
    value_has_type Gamma (vlam x T11 t12) (TArrow T11 T12)

where "Gamma '|-' t '\in' T" := (has_type Gamma t T).
Notation "Gamma '|-' t '\Vin' T" := (value_has_type Gamma t T) (at level 40).

Global Hint Constructors has_type: core.
Global Hint Constructors value_has_type: core.

Lemma op_type_safe: forall op n1 n2, empty |- vconst (apply_op op n1 n2) \Vin TBasic (op_ret_ty op).
Proof.
  intro op.
  destruct op; simpl; intros; auto.
Qed.

Global Hint Resolve op_type_safe: core.

Lemma nat_value_n_exists: forall v, empty |- v \Vin (TBasic TNat) -> (exists n, v = (vconst (cnat n))).
Proof.
  intros.
  destruct v; inversion H; subst; eauto.
  - inversion H2.
Qed.

Lemma bool_value_b_exists: forall v, empty |- v \Vin (TBasic TBool) -> (exists b, v = (vconst (cbool b))).
Proof.
  intros.
  destruct v; inversion H; subst; eauto.
  - inversion H2.
Qed.

Lemma basic_value_const_exists:
  forall T v, empty |- v \Vin (TBasic T) -> (exists c, v = vconst c).
Proof.
  intros.
  destruct v; inversion H; subst; eauto. inversion H2.
Qed.

Lemma arrow_value_lam_exists: forall v T1 T2,
    empty |- v \Vin (TArrow T1 T2) -> exists x e, v = vlam x T1 e.
Proof.
  intros.
  destruct v; inversion H; subst; eauto.
  - inversion H2.
Qed.

Lemma has_type_inv_value: forall Gamma v T, Gamma |- tvalue v \in T -> Gamma |- v \Vin T.
Proof.
  intros.
  inversion H; subst; auto.
Qed.

Definition halts (t:tm) : Prop :=  exists t', t -->* t' /\ is_value t' = true.

Lemma value_halts : forall v, halts (tvalue v).
Proof.
  intros v. unfold halts. exists (tvalue v). split; auto.
Qed.

Lemma weakening : forall e T Gamma Gamma',
    includedin Gamma Gamma' ->
    (Gamma  |- e \in T -> Gamma' |- e \in T).
Proof.
  intro e.
  apply (tm_mutual_rec
         (fun v => forall T Gamma Gamma', includedin Gamma Gamma' -> (Gamma  |- v \Vin T -> Gamma' |- v \Vin T))
         (fun e => forall T Gamma Gamma', includedin Gamma Gamma' -> (Gamma  |- e \in T -> Gamma' |- e \in T))).
  - intros; inversion H0; subst; eauto 7 using includedin_update.
  - intros; inversion H0; subst; eauto 7 using includedin_update.
  - intros; inversion H1; subst; eauto 7 using includedin_update.
  - intros; inversion H0; subst; eauto 7 using includedin_update.
  - intros. inversion H1; subst; eauto 7 using includedin_update.
  - intros; inversion H2; subst; eauto 7 using includedin_update.
  - intros; inversion H3; subst; eauto 7 using includedin_update.
  (* - intros. inversion H2; subst; eauto 7 using includedin_update. *)
  - intros; inversion H3; subst; eauto 7 using includedin_update.
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
  intro c. induction c; intros; inversion H; clear H; subst; simpl; eauto.
Qed.

Lemma substitution_preserves_typing2:
  (forall (s : string) (Gamma : partial_map ty) (x : string) (U : ty) (v : value) (T : ty),
      (x |-> U; Gamma) |- s \Vin T -> empty |- v \Vin U -> Gamma |- [x := v ]v s \Vin T).
Proof.
  intros. inversion H; clear H; subst; simpl; eauto.
  rename s into y. rename x into x.
  destruct (eqb_spec x y); subst.
  + (* x=y *)
    rewrite update_eq in H3.
    injection H3 as H3; subst.
    apply value_weakening_empty. assumption.
  + (* x<>y *)
    apply T_Var. rewrite update_neq in H3; auto.
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
  rename s into y. rename x into x.
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
  rename s into y. rename x into x.
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
  rename s into y. rename x into x.
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

Lemma substitution_preserves_typing : forall t Gamma x U v T,
    (x |-> U ; Gamma) |- t \in T ->  empty |- v \Vin U  ->  Gamma |- [x:=v]t \in T.
Proof with eauto.
  intro t.
  apply (tm_mutual_rec
           (fun ev => forall Gamma x U v T, (x |-> U ; Gamma) |- ev \Vin T ->  empty |- v \Vin U ->  Gamma |- [x:=v]v ev \Vin T)
           (fun e => forall Gamma x U v T, (x |-> U ; Gamma) |- e \in T ->  empty |- v \Vin U ->  Gamma |- [x:=v] e \in T)
           substitution_preserves_typing1 substitution_preserves_typing2).
  - intros. inversion H0; subst; simpl; eauto.
    destruct (eqb_spec x s); subst.
    + rewrite update_shadow in H7...
    + rewrite update_permute in H7...
  - intros. inversion H0; subst; simpl; eauto.
  - intros. inversion H0; subst; simpl; eauto.
  - apply substitution_preserves_typing7.
  - apply substitution_preserves_typing8.
  - apply substitution_preserves_typing9.
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
  - apply substitution_preserves_typing with (TBasic (op_ret_ty op))...
  (* - apply substitution_preserves_typing with TNat... *)
  - apply T_Lete with (T1 := T_y)...
    apply substitution_preserves_typing with T_x...
    inversion H0; subst...
Qed.
