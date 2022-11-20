Set Warnings "-notation-overridden,-parsing".
From CT Require Import Maps.
From CT Require Import CoreLang.

(* Ltac invert := *)
(*   match goal with | H : ?T \N- _ => *)
(*                       match type of T with Prop => solve [exfalso; apply H; auto] *)
(*                       end *)
(*   end. *)

Import CoreLang.

Definition context := partial_map ty.

Reserved Notation "Gamma '\N-' t '\Tin' T" (at level 40).

Inductive has_type : context -> tm -> ty -> Prop :=
| T_Exn : forall Gamma T, has_type Gamma texn T
| T_Value : forall Gamma value T, value_has_type Gamma value T -> has_type Gamma (tvalue value) T
| T_Lete : forall Gamma x e1 e2 T1 T2,
    has_type Gamma e1 T1 ->
    has_type (update Gamma x T1) e2 T2 ->
    has_type Gamma (tlete x e1 e2) T2
| T_LetOp : forall Gamma x op v1 v2 e (T1 T2 Tx: base_ty) T,
    value_has_type Gamma v1 T1 ->
    value_has_type Gamma v2 T2 ->
    ty_of_op op = T1 t--> (T2 t--> Tx) ->
    has_type (update Gamma x Tx) e T ->
    has_type Gamma (tletbiop x op v1 v2 e) T
| T_LetApp : forall Gamma y v_x v_f e2 T_x T_y T2,
    value_has_type Gamma v_x T_x ->
    value_has_type Gamma v_f (TArrow T_x T_y) ->
    has_type (update Gamma y T_y) e2 T2 ->
    has_type Gamma (tletapp y v_f v_x e2) T2
| T_Matchb: forall Gamma v e1 e2 T,
    value_has_type Gamma v TBool ->
    has_type Gamma e1 T ->
    has_type Gamma e2 T ->
    has_type Gamma (tmatchb v e1 e2) T
with value_has_type : context -> value -> ty -> Prop :=
| T_Const : forall Gamma (c: constant), value_has_type Gamma c (ty_of_const c)
| T_Op : forall Gamma (op: biop), value_has_type Gamma (vbiop op) (ty_of_op op)
| T_Var : forall Gamma x T, Gamma x = Some T -> value_has_type Gamma (vvar x) T
| T_Lam : forall Gamma x T11 T12 t12,
    has_type (update Gamma x T11) t12 T12 ->
    value_has_type Gamma (vlam x T11 t12) (TArrow T11 T12)
| T_Fix : forall Gamma f x (T11: base_ty) T12 t12,
    has_type (update (update Gamma f (T11 t--> T12)) x T11) t12 T12 ->
    value_has_type Gamma (vfix f (T11 t--> T12) x T11 t12) (T11 t--> T12)

where "Gamma '\N-' t '\Tin' T" := (has_type Gamma t T).
Notation "Gamma '\N-' t '\Vin' T" := (value_has_type Gamma t T) (at level 40).

Global Hint Constructors has_type: core.
Global Hint Constructors value_has_type: core.

Lemma eval_op_is_type_safe: forall Gamma op (c1 c2 c3: constant) (T1 T2 T3: base_ty),
    Gamma \N- c1 \Vin T1 -> Gamma \N- c2 \Vin T2 ->
                                 eval_op op c1 c2 c3 -> ty_of_op op = T1 t--> (T2 t--> T3) ->
                                 empty \N- c3 \Vin T3.
Proof.
  intros Gamma op c1 c2 c3 T1 T2 T3 Hc1 Hc2 Heval Hop.
  destruct Heval; inversion Hc1; subst; inversion Hc2; subst; inversion Hop; subst; constructor.
Qed.

Global Hint Resolve eval_op_is_type_safe: core.

Lemma base_value_const_exists:
  forall T v, empty \N- v \Vin (TBase T) -> (exists c, v = vconst c).
Proof.
  intros.
  destruct v; inversion H; subst; eauto.
  - inversion H2.
Qed.

Global Hint Resolve base_value_const_exists: core.

Lemma nat_const_n_exists: forall Gamma (c: constant), Gamma \N- c \Vin (TBase TNat) -> (exists (n: nat), c = n).
Proof with eauto.
  intros. destruct c; inversion H; subst...
Qed.

Global Hint Resolve nat_const_n_exists: core.

Lemma bool_const_b_exists: forall Gamma (c: constant), Gamma \N- c \Vin (TBase TBool) -> (exists (n: bool), c = n).
Proof with eauto.
  intros. destruct c; inversion H; subst...
Qed.

Global Hint Resolve bool_const_b_exists: core.

Lemma nat_value_n_exists: forall v, empty \N- v \Vin (TBase TNat) -> (exists n, v = (vconst (cnat n))).
Proof with eauto.
  intros. assert (exists c, v = vconst c) as HH...
  destruct HH as (c & Hc); subst... eapply nat_const_n_exists in H... destruct H; subst...
Qed.

Global Hint Resolve nat_value_n_exists: core.

Lemma bool_value_b_exists: forall v, empty \N- v \Vin (TBase TBool) -> (exists b, v = (vconst (cbool b))).
Proof with eauto.
  intros. assert (exists c, v = vconst c) as HH...
  destruct HH as (c & Hc); subst... eapply bool_const_b_exists in H... destruct H; subst...
Qed.

Global Hint Resolve bool_value_b_exists: core.

Lemma base_value_cid_exists:
  forall Gamma T v, Gamma \N- v \Vin (TBase T) -> ((exists c, v = vconst c) \/ (exists (name: string), v = name)).
Proof.
  intros.
  destruct v; inversion H.
  subst. left; eauto. right; eauto.
Qed.

Global Hint Resolve base_value_cid_exists: core.

Lemma nat_value_cid_exists: forall Gamma v, Gamma \N- v \Vin (TBase TNat) ->
                                               ((exists n, v = (vconst (cnat n))) \/ (exists (name: string), v = name)).
Proof with eauto.
  intros. assert ((exists c : constant, v = c) \/ (exists name : string, v = name)) as HH... destruct HH.
  - left. destruct H0; subst... apply nat_const_n_exists in H... destruct H. exists x0. inversion H...
  - right. destruct H0; subst...
Qed.

Lemma bool_value_cid_exists: forall Gamma v, Gamma \N- v \Vin (TBase TBool) ->
                                               ((exists n, v = (vconst (cbool n))) \/ (exists (name: string), v = name)).
Proof with eauto.
  intros. assert ((exists c : constant, v = c) \/ (exists name : string, v = name)) as HH... destruct HH.
  - left. destruct H0; subst... apply bool_const_b_exists in H... destruct H. exists x0. inversion H...
  - right. destruct H0; subst...
Qed.

(* Lemma arrow_value_lam_exists: forall v T1 T2, *)
(*     empty \N- v \Vin (TArrow T1 T2) -> exists x e, v = vlam x T1 e. *)
(* Proof. *)
(*   intros. *)
(*   destruct v; inversion H; subst; eauto. *)
(*   - destruct b; inversion H3; inversion H2. *)
(*   - inversion H2. *)
(* Qed. *)

Lemma has_type_inv_value: forall Gamma v T, Gamma \N- tvalue v \Tin T -> Gamma \N- v \Vin T.
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
    (Gamma  \N- e \Tin T -> Gamma' \N- e \Tin T).
Proof.
  intro e.
  apply (tm_mutual_rec
         (fun v => forall T Gamma Gamma', includedin Gamma Gamma' -> (Gamma  \N- v \Vin T -> Gamma' \N- v \Vin T))
         (fun e => forall T Gamma Gamma', includedin Gamma Gamma' -> (Gamma  \N- e \Tin T -> Gamma' \N- e \Tin T))).
  - intros; inversion H0; subst; eauto 7 using includedin_update.
  - intros; inversion H0; subst; eauto 7 using includedin_update.
  - intros; inversion H1; subst; eauto 7 using includedin_update.
  - intros; inversion H1; subst; eauto 7 using includedin_update.
  - intros; inversion H0; subst; eauto 7 using includedin_update.
  - intros; inversion H1; subst; eauto 7 using includedin_update.
  - intros; inversion H2; subst; eauto 7 using includedin_update.
  - intros; inversion H3; subst; eauto 7 using includedin_update.
  (* - intros. inversion H2; subst; eauto 7 using includedin_update. *)
  - intros; inversion H3; subst; eauto 7 using includedin_update.
  - intros; inversion H3; subst; eauto 7 using includedin_update.
Qed.

Lemma value_weakening : forall e T Gamma Gamma',
    includedin Gamma Gamma' ->
    (Gamma  \N- e \Vin T -> Gamma' \N- e \Vin T).
Proof.
  intros.
  apply weakening with (e:=tvalue e) (T:=T) in H; auto.
  inversion H; subst; auto.
Qed.

Lemma weakening_empty : forall Gamma t T,
    empty \N- t \Tin T  -> Gamma \N- t \Tin T.
Proof.
  intros Gamma t T.
  eapply weakening.
  discriminate.
Qed.

Lemma value_weakening_empty : forall Gamma t T,
    empty \N- t \Vin T  -> Gamma \N- t \Vin T.
Proof.
  intros Gamma t T H.
  assert (empty \N- (tvalue t) \Tin T). auto.
  apply weakening_empty with (Gamma:=Gamma) in H0. inversion H0; subst; auto.
Qed.

Lemma substitution_preserves_typing2:
  (forall (s : string) (Gamma : partial_map ty) (x : string) (U : ty) (v : value) (T : ty),
      (x |-> U; Gamma) \N- s \Vin T -> empty \N- v \Vin U -> Gamma \N- [x := v ]v s \Vin T).
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

Lemma substitution_preserves_typing7:
  (forall (s : string) (t : tm),
      (forall (Gamma : partial_map ty) (x : string) (U : ty) (v : value) (T : ty),
          (x |-> U; Gamma) \N- t \Tin T -> empty \N- v \Vin U -> Gamma \N- [x := v] t \Tin T) ->
      forall t0 : tm,
        (forall (Gamma : partial_map ty) (x : string) (U : ty) (v : value) (T : ty),
            (x |-> U; Gamma) \N- t0 \Tin T -> empty \N- v \Vin U -> Gamma \N- [x := v] t0 \Tin T) ->
        forall (Gamma : partial_map ty) (x : string) (U : ty) (v : value) (T : ty),
          (x |-> U; Gamma) \N- tlete s t t0 \Tin T -> empty \N- v \Vin U -> Gamma \N- [x := v] tlete s t t0 \Tin T).
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

Lemma substitution_preserves_typing9:
  (forall (s : string) (v : value),
      (forall (Gamma : partial_map ty) (x : string) (U : ty) (v0 : value) (T : ty),
          (x |-> U; Gamma) \N- v \Vin T -> empty \N- v0 \Vin U -> Gamma \N- [x := v0 ]v v \Vin T) ->
      forall v0 : value,
        (forall (Gamma : partial_map ty) (x : string) (U : ty) (v1 : value) (T : ty),
            (x |-> U; Gamma) \N- v0 \Vin T -> empty \N- v1 \Vin U -> Gamma \N- [x := v1 ]v v0 \Vin T) ->
        forall t : tm,
          (forall (Gamma : partial_map ty) (x : string) (U : ty) (v1 : value) (T : ty),
              (x |-> U; Gamma) \N- t \Tin T -> empty \N- v1 \Vin U -> Gamma \N- [x := v1] t \Tin T) ->
          forall (Gamma : partial_map ty) (x : string) (U : ty) (v1 : value) (T : ty),
            (x |-> U; Gamma) \N- tletapp s v v0 t \Tin T -> empty \N- v1 \Vin U -> Gamma \N- [x := v1] tletapp s v v0 t \Tin T).
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
    (x |-> U ; Gamma) \N- t \Tin T ->  empty \N- v \Vin U  ->  Gamma \N- [x:=v]t \Tin T.
Proof with eauto.
  intro t.
  apply (tm_mutual_rec
           (fun ev => forall Gamma x U v T, (x |-> U ; Gamma) \N- ev \Vin T ->  empty \N- v \Vin U ->  Gamma \N- [x:=v]v ev \Vin T)
           (fun e => forall Gamma x U v T, (x |-> U ; Gamma) \N- e \Tin T ->  empty \N- v \Vin U ->  Gamma \N- [x:=v] e \Tin T)).
  - intro c. induction c; intros; inversion H; clear H; subst; simpl; eauto; constructor...
  - intro c. destruct c.
    + intros. inversion H; clear H; subst; simpl; eauto; constructor...
    + apply substitution_preserves_typing2.
  - intros. inversion H0; subst; simpl; eauto.
    destruct (eqb_spec x s); subst.
    + rewrite update_shadow in H7...
    + rewrite update_permute in H7...
  - intros. inversion H0; subst; simpl; eauto.
    destruct (eqb_spec x s); subst.
    + rewrite update_shadow in H9...
    + destruct (eqb_spec x s0); subst.
      { rewrite <- update_permute in H9... rewrite update_shadow in H9... rewrite update_permute in H9... }
      { rewrite (update_permute _ _ s x) in H9... rewrite (update_permute _ _ s0 x) in H9... }
  - intros. inversion H0; subst; simpl; eauto.
  - intros. inversion H0; subst; simpl; eauto.
  - apply substitution_preserves_typing7.
  - intros x op v1 Hv1 v2 Hv2 e He Gamma y U v_y T HTop HTs.
    inversion HTop; subst. inversion H8; subst. simpl.
    destruct (eqb_spec y x); subst; eapply T_LetOp...
    + erewrite <- update_shadow...
    + eapply He... erewrite update_permute...
  - apply substitution_preserves_typing9.
  - intros v Hv e1 He1 e2 He2 Gamma y U v_y T HTmatch HTy. inversion HTmatch; subst.
    simpl. eapply T_Matchb...
Qed.

Theorem preservation : forall t t' T,
    empty \N- t \Tin T  ->
                  t --> t'  ->
                  empty \N- t' \Tin T.
Proof with eauto.
  intros t t' T HT. generalize dependent t'.
  remember empty as Gamma.
  induction HT;
    intros t' HE; subst; inversion HE; subst...
  - apply substitution_preserves_typing with T1...
    inversion HT1; subst...
  - eapply substitution_preserves_typing...
  - apply T_Lete with (T1 := T_y)...
    apply substitution_preserves_typing with T_x...
    inversion H0; subst...
  - apply T_Lete with (T1 := T_y)...
    apply substitution_preserves_typing with (TArrow T_x T_y)...
    apply substitution_preserves_typing with T_x...
    inversion H0; subst...
Qed.

Theorem const_ctx_independent_v : forall Gamma (c:constant) T,
    Gamma \N- c \Vin T <-> empty \N- c \Vin T.
Proof with eauto.
  intros Gamma c T. split; intro HH.
  - inversion HH...
  - eapply value_weakening_empty...
Qed.

Theorem const_ctx_independent : forall Gamma (c:constant) T,
    Gamma \N- c \Tin T <-> empty \N- c \Tin T.
Proof with eauto.
  intros Gamma c T. split; intro HH.
  - inversion HH; subst. inversion H1; subst...
  - inversion HH; subst. rewrite <- const_ctx_independent_v in H1...
Qed.

Lemma constant_base_ty_unique: forall (c_x: constant) Gamma1 Gamma2 T1 T2,
    Gamma1 \N- c_x \Tin T1  -> Gamma2 \N- c_x \Tin T2 -> T1 = T2.
Proof with eauto.
  intros.
  inversion H; subst.
  inversion H0; subst.
  inversion H3; subst.
  inversion H4; subst...
Qed.


Lemma ty_implies_ty_of_const_eq: forall (T:base_ty) (c_x: constant), empty \N- c_x \Tin T -> (ty_of_const c_x) = T.
Proof with eauto.
  intros.
  inversion H; subst.
  inversion H2; subst...
Qed.


Global Hint Resolve ty_implies_ty_of_const_eq: core.
Global Hint Rewrite ty_implies_ty_of_const_eq: core.

(* Term Ordering *)

Definition term_order (e e': tm) :=
  (forall (v: value), e -->* v -> e' -->* v) /\ (forall Gamma T, Gamma \N- e \Tin T -> Gamma \N- e' \Tin T).

Notation " e1 '<-<' e2 " := (term_order e1 e2) (at level 90).

Notation " e1 '<=<' e2 " := (term_order e1 e2 /\ term_order e2 e1) (at level 90).

Lemma term_order_fst: forall (e e': tm), term_order e e' -> (forall (v: value), e -->* v -> e' -->* v).
Proof. intros. destruct H. auto. Qed.

Global Hint Resolve term_order_fst: core.

Lemma term_order_snd: forall (e e': tm), term_order e e' -> (forall Gamma T, Gamma \N- e \Tin T -> Gamma \N- e' \Tin T).
Proof. intros. destruct H. eapply H1. auto. Qed.

Global Hint Resolve term_order_snd: core.

Lemma term_order_eq_trans (e1 e2 e3: tm): e1 <=< e2 -> e2 <=< e3 -> e1 <=< e3.
Proof with eauto.
  intros.
  destruct H as ((HT1 & HE1) & HT2 & HE2).
  destruct H0 as ((HT1' & HE1') & HT2' & HE2').
  repeat split...
Qed.

Lemma term_order_const_bound (e1: tm) (c2: constant): e1 <-< c2 -> (forall (v: value), e1 -->* v -> v = c2).
Proof with eauto.
  intros. destruct H... apply H in H0. inversion H0; subst... inversion H2.
Qed.

Global Hint Resolve term_order_const_bound: core.

Lemma term_order_trans (e1 e2 e3: tm): e1 <-< e2 -> e2 <-< e3 -> e1 <-< e3.
Proof with eauto.
  intros.
  destruct H as (HT1 & HE1).
  destruct H0 as (HT1' & HE1').
  repeat split...
Qed.

Global Hint Resolve term_order_trans: core.
