Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From PLF Require Import Types.
From PLF Require Import Smallstep.
From PLF Require Import CoreLang.

Ltac invert :=
  match goal with | H : ?T |- _ =>
                      match type of T with Prop => solve [exfalso; apply H; auto]
                      end
  end.

Import CoreLang.

Definition context := partial_map ty.

Reserved Notation "Gamma '|-' t '\in' T" (at level 40).

Inductive has_type : context -> tm -> ty -> Prop :=
| T_Random : forall Gamma, has_type Gamma trandom (TBasic TNat)
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
| T_LetS : forall Gamma x v1 e2 T2,
    value_has_type Gamma v1 TNat ->
    has_type (update Gamma x TNat) e2 T2 ->
    has_type Gamma (tlets x v1 e2) T2
| T_LetCons : forall Gamma x T v1 v2 e2 T2,
    value_has_type Gamma v1 (TBasic T) ->
    value_has_type Gamma v2 (TBasic (TList T)) ->
    has_type (update Gamma x (TBasic (TList T))) e2 T2 ->
    has_type Gamma (tletcons x T v1 v2 e2) T2
| T_LetNode : forall Gamma x T v1 v2 v3 e2 T2,
    value_has_type Gamma v1 (TBasic T) ->
    value_has_type Gamma v2 (TBasic (TTree T)) ->
    value_has_type Gamma v3 (TBasic (TTree T)) ->
    has_type (update Gamma x (TBasic (TTree T))) e2 T2 ->
    has_type Gamma (tletnode x T v1 v2 v3 e2) T2
| T_LetApp : forall Gamma y v_x v_f e2 T_x T_y T2,
    value_has_type Gamma v_x T_x ->
    value_has_type Gamma v_f (TArrow T_x T_y) ->
    has_type (update Gamma y T_y) e2 T2 ->
    has_type Gamma (tletapp y v_f v_x e2) T2
| T_Matchb: forall Gamma v e1 e2 T,
    value_has_type Gamma v (TBasic TBool) ->
    has_type Gamma e1 T ->
    has_type Gamma e2 T ->
    has_type Gamma (tmatchb v e1 e2) T
| T_Matchn: forall Gamma v e1 y e2 T,
    value_has_type Gamma v (TBasic TNat) ->
    has_type Gamma e1 T ->
    has_type (update Gamma y TNat) e2 T ->
    has_type Gamma (tmatchn v e1 y e2) T
| T_Matchl: forall Gamma T v e1 h t e2 Tbody,
    h <> t ->
    value_has_type Gamma v (TList T) ->
    has_type Gamma e1 Tbody ->
    has_type (update (update Gamma h T) t (TList T)) e2 Tbody ->
    has_type Gamma (tmatchl T v e1 h t e2) Tbody
| T_Matcht: forall Gamma T v e1 root lt rt e2 Tbody,
    root <> lt -> root <> rt -> lt <> rt ->
    value_has_type Gamma v (TTree T) ->
    has_type Gamma e1 Tbody ->
    has_type (update (update (update Gamma root T) lt (TTree T)) rt (TTree T)) e2 Tbody ->
    has_type Gamma (tmatcht T v e1 root lt rt e2) Tbody
with value_has_type : context -> value -> ty -> Prop :=
| T_ConstantB : forall Gamma b, value_has_type Gamma (vconst (cbool b)) (TBasic TBool)
| T_ConstantI : forall Gamma n, value_has_type Gamma (vconst (cnat n)) (TBasic TNat)
| T_ConstantNil : forall Gamma T, value_has_type Gamma (vconst (cnil T)) (TBasic (TList T))
| T_ConstantLeaf : forall Gamma T, value_has_type Gamma (vconst (cleaf T)) (TBasic (TTree T))
| T_ConstantCons : forall Gamma T c1 c2,
    value_has_type Gamma (vconst c1) (TBasic T) ->
    value_has_type Gamma (vconst c2) (TBasic (TList T)) ->
    value_has_type Gamma (vconst (ccons T c1 c2)) (TBasic (TList T))
| T_ConstantNode : forall Gamma T c1 c2 c3,
    value_has_type Gamma (vconst c1) (TBasic T) ->
    value_has_type Gamma (vconst c2) (TBasic (TTree T)) ->
    value_has_type Gamma (vconst c3) (TBasic (TTree T)) ->
    value_has_type Gamma (vconst (cnode T c1 c2 c3)) (TBasic (TTree T))
| T_Var : forall Gamma x T, Gamma x = Some T -> value_has_type Gamma (vvar x) T
| T_Lam : forall Gamma x T11 T12 t12,
    has_type (update Gamma x (outy_erase T11)) t12 T12 ->
    value_has_type Gamma (vlam x T11 t12) (TArrow (outy_erase T11) T12)
| T_Fix : forall Gamma f x x' tau11 tau12 t12,
    has_type (update (update Gamma f ((oty_erase tau11) t--> (erase tau12))) x (oty_erase tau11)) t12 (erase tau12) ->
    value_has_type Gamma (vfix f (x' o: tau11 o--> tau12) x tau11 t12) ((oty_erase tau11) t--> (erase tau12))
| T_Exn : forall Gamma T, value_has_type Gamma vexn T

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

Lemma nat_value_n_exists: forall v, empty |- v \Vin (TBasic TNat) -> (exists n, v = (vconst (cnat n))) \/ v = vexn.
Proof.
  intros.
  destruct v; inversion H; subst.
  - left. exists n. auto.
  - inversion H2.
  - right. auto.
Qed.

Lemma bool_value_b_exists: forall v, empty |- v \Vin (TBasic TBool) -> (exists b, v = (vconst (cbool b))) \/ v = vexn.
Proof.
  intros.
  destruct v; inversion H; subst.
  - left. exists b. auto.
  - inversion H2.
  - right. auto.
Qed.

Lemma list_value_list_exists:
  forall T v, empty |- v \Vin (TBasic (TList T)) ->
                 (exists h t, empty |- (vconst h) \Vin (TBasic T) /\
                                   empty |- (vconst t) \Vin (TBasic (TList T)) /\
                                             v = (vconst (ccons T h t))) \/
                   v = (vconst (cnil T)) \/
                   v = vexn.
Proof.
  intros.
  destruct v; inversion H; subst.
  - right. left; auto.
  - left. exists c1, c2. repeat split; auto.
  - inversion H2.
  - right. auto.
Qed.

Lemma tree_value_tree_exists:
  forall T v, empty |- v \Vin (TBasic (TTree T)) ->
                 (exists root lt rt,
                     empty |- (vconst root) \Vin (TBasic T) /\
                               empty |- (vconst lt) \Vin (TBasic (TTree T)) /\
                                         empty |- (vconst rt) \Vin (TBasic (TTree T)) /\
                                                   v = (vconst (cnode T root lt rt))) \/
                   v = (vconst (cleaf T)) \/
                   v = vexn.
Proof.
  intros.
  destruct v; inversion H; subst.
  - right. left; auto.
  - left. exists c1, c2, c3. repeat split; auto.
  - inversion H2.
  - right. auto.
Qed.

Lemma basic_value_const_exists:
  forall T v, empty |- v \Vin (TBasic T) -> (exists c, v = vconst c) \/ v = vexn.
Proof.
  intros.
  destruct v; inversion H; subst; try inversion H2; try (try (right; eexists; reflexivity); try (left; eauto)).
Qed.

Lemma arrow_value_lam_exists: forall v T1 T2,
    empty |- v \Vin (TArrow T1 T2) ->
            (exists f x tau_x x' tau2 e,
                (oty_erase tau_x = T1) /\
                  (erase tau2 = T2) /\
                  v = vfix f (x' o: tau_x o--> tau2) x tau_x e) \/
              (exists x tau_x e,
                  (outy_erase tau_x = T1) /\
                    v = vlam x tau_x e) \/ v = vexn.
Proof.
  intros.
  destruct v; inversion H; subst.
  - inversion H2.
  - right. left. exists s, o, t. split; auto.
  - left. exists s, s0, o, x', tau12, t. repeat split; auto.
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
    destruct (is_value_value_exists _ H) as (v' & Hv'). subst. exists ([ x := v' ] e2)...
    destruct H as (e1' & He'). exists (tlete x e1' e2)...
  - right.
    apply nat_value_n_exists in H0.
    apply nat_value_n_exists in H.
    destruct H0, H.
    + destruct H0 as (n2 & H2). destruct H as (n1 & H1). subst. exists ([x := vconst (apply_op op n1 n2)] e2)...
    + exists (tvalue vexn). subst...
    + exists (tvalue vexn). subst...
    + exists (tvalue vexn). subst...
  - right.
    destruct (nat_value_n_exists _ H); subst; eauto. destruct H0; subst; eauto.
  - right.
    destruct (basic_value_const_exists _ _ H); subst; eauto.
    destruct H1 as (c1 & Hc1).
    destruct (list_value_list_exists _ _ H0); subst; eauto.
    + destruct H1 as (h & t & Hhead & Htail & Hcons); subst; eauto.
    + destruct H1; subst; eauto.
  - right.
    destruct (basic_value_const_exists _ _ H); subst; eauto.
    destruct H2 as (c1 & Hc1); subst; eauto.
    destruct (basic_value_const_exists _ _ H0); subst; eauto.
    destruct H2 as (c2 & Hc2); subst; eauto.
    destruct (basic_value_const_exists _ _ H1); subst; eauto.
    destruct H2 as (c3 & Hc3); subst; eauto.
  - right.
    destruct (arrow_value_lam_exists _ _ _ H0).
    + destruct H1 as (f & x & tau_x &x' & tau2 & e1 & H11 & H12 & H13). subst.
      exists (tlete y ([f:= (vfix f (x' o: tau_x o--> tau2) x tau_x e1)] [x:= v_x] e1) e2)...
    + destruct H1.
      destruct H1 as (x & tau_x & e1 & H11 & H12). subst. exists (tlete y ([x:= v_x] e1) e2)...
      subst. eauto...
  - right.
    destruct (bool_value_b_exists _ H).
    + destruct H0 as (b & Hb); destruct b; subst...
    + exists (tvalue vexn). subst...
  - right.
    destruct (nat_value_n_exists _ H).
    + destruct H0 as (b & Hb); destruct b; subst...
    + exists (tvalue vexn). subst...
  - right.
    destruct (list_value_list_exists _ _ H0).
    + destruct H1 as (ch & ct & Hhead & Htail & Hcons); subst; eauto.
    + destruct H1; subst; eauto.
  - right.
    destruct (tree_value_tree_exists _ _ H2).
    + destruct H3 as (croot & clt & crt & Hroot & Hlt & Hrt & Hnode). subst; eauto.
    + destruct H3; subst; eauto.
Qed.

Definition halts (t:tm) : Prop :=  exists t', t -->* t' /\ is_value t' = true.

Lemma value_halts : forall v, halts (tvalue v).
Proof.
  intros v. unfold halts. exists (tvalue v). split; auto.
Qed.

(* logical relation *)
(* Fixpoint R (T:ty) (t:tm) : Prop := *)
(*   empty |- t \in T /\ halts t /\ *)
(*                   (match T with *)
(*                    | TBasic _ => True *)
(*                    | TArrow T1 T2 => forall f y v, *)
(*                        R T1 (tvalue v) -> *)
(*                        R T2 (tlete f t (tletapp y (vvar f) v *)
(*                                                 (tvalue (vvar y)))) *)
(*                    end). *)

(* Lemma R_halts : forall {T} {t}, R T t -> halts t. *)
(* Proof. *)
(*   intros. *)
(*   destruct T; unfold R in H; destruct H as [_ [H _]]; assumption. *)
(* Qed. *)

(* Lemma R_typable_empty : forall {T} {t}, R T t -> empty |- t \in T. *)
(* Proof. *)
(*   intros. *)
(*   destruct T; unfold R in H; destruct H as [H _]; assumption. *)
(* Qed. *)


Lemma weakening : forall e T Gamma Gamma',
    includedin Gamma Gamma' ->
    (Gamma  |- e \in T -> Gamma' |- e \in T).
Proof.
  intro e.
  apply (tm_mutual_rec
         (fun v => forall T Gamma Gamma', includedin Gamma Gamma' -> (Gamma  |- v \Vin T -> Gamma' |- v \Vin T))
         (fun e => forall T Gamma Gamma', includedin Gamma Gamma' -> (Gamma  |- e \in T -> Gamma' |- e \in T))).
  - intro c.
    induction c; intros; inversion H0; subst; auto.
    + constructor. apply IHc1 with (Gamma:=Gamma); auto. apply IHc2 with (Gamma:=Gamma); auto.
    + constructor. apply IHc1 with (Gamma:=Gamma); auto. apply IHc2 with (Gamma:=Gamma); auto. apply IHc3 with (Gamma:=Gamma); auto.
  - intros; inversion H0; subst; eauto 7 using includedin_update.
  - intros; inversion H1; subst; eauto 7 using includedin_update.
  - intros; inversion H1; subst; eauto 7 using includedin_update.
  - intros; inversion H0; subst; eauto 7 using includedin_update.
  - intros; inversion H1; subst; eauto 7 using includedin_update.
  - intros; inversion H0; subst; eauto 7 using includedin_update.
  - intros; inversion H2; subst; eauto 7 using includedin_update.
  - intros; inversion H3; subst; eauto 7 using includedin_update.
  - intros; inversion H2; subst; eauto 7 using includedin_update.
  - intros; inversion H3; subst; eauto 7 using includedin_update.
  - intros; inversion H4; subst; eauto 7 using includedin_update.
  - intros; inversion H3; subst; eauto 7 using includedin_update.
  - intros; inversion H3; subst; eauto 7 using includedin_update.
  - intros; inversion H3; subst; eauto 7 using includedin_update.
  - intros; inversion H3; subst; eauto 9 using includedin_update.
  - intros; inversion H3; subst; eauto 9 using includedin_update.
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

Lemma substitution_preserves_typing3:
  forall (s : string) (o : outy) (t0 : tm),
    (forall (Gamma : partial_map ty) (x : string) (U : ty) (v : value) (T : ty),
        (x |-> U; Gamma) |- t0 \in T -> empty |- v \Vin U -> Gamma |- [x := v] t0 \in T) ->
    forall (Gamma : partial_map ty) (x : string) (U : ty) (v : value) (T : ty),
      (x |-> U; Gamma) |- vlam s o t0 \Vin T -> empty |- v \Vin U -> Gamma |- [x := v ]v vlam s o t0 \Vin T.
Proof.
  intros. inversion H0; clear H0; subst; simpl; eauto.
  rename s into y. rename x into x.
  destruct (eqb_spec x y); subst; apply T_Lam.
  + (* x=y *)
    rewrite update_shadow in H7. assumption.
  + (* x<>y *)
    apply H with (U:=U); auto.
    rewrite update_permute; auto.
Qed.


Lemma substitution_preserves_typing_fix:
  forall (s : string) (u : uty) (s0 : string) (o : oty) (t0 : tm),
    (forall (Gamma : partial_map ty) (x : string) (U : ty) (v : value) (T : ty),
        (x |-> U; Gamma) |- t0 \in T -> empty |- v \Vin U -> Gamma |- [x := v] t0 \in T) ->
    forall (Gamma : partial_map ty) (x : string) (U : ty) (v : value) (T : ty),
      (x |-> U; Gamma) |- vfix s u s0 o t0 \Vin T -> empty |- v \Vin U -> Gamma |- [x := v ]v vfix s u s0 o t0 \Vin T.
Proof.
  intros. inversion H0; clear H0; subst; simpl; eauto.
  rename s into y. rename x into x.
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

Lemma substitution_preserves_typing_cons:
  forall (s : string) (b : basic_ty) (v : value),
    (forall (Gamma : partial_map ty) (x : string) (U : ty) (v0 : value) (T : ty),
        (x |-> U; Gamma) |- v \Vin T -> empty |- v0 \Vin U -> Gamma |- [x := v0 ]v v \Vin T) ->
    forall v0 : value,
      (forall (Gamma : partial_map ty) (x : string) (U : ty) (v1 : value) (T : ty),
          (x |-> U; Gamma) |- v0 \Vin T -> empty |- v1 \Vin U -> Gamma |- [x := v1 ]v v0 \Vin T) ->
      forall t0 : tm,
        (forall (Gamma : partial_map ty) (x : string) (U : ty) (v1 : value) (T : ty),
            (x |-> U; Gamma) |- t0 \in T -> empty |- v1 \Vin U -> Gamma |- [x := v1] t0 \in T) ->
        forall (Gamma : partial_map ty) (x : string) (U : ty) (v1 : value) (T : ty),
          (x |-> U; Gamma) |- tletcons s b v v0 t0 \in T -> empty |- v1 \Vin U -> Gamma |- [x := v1] tletcons s b v v0 t0 \in T.
Proof.
  intros. inversion H2; clear H2; subst; simpl; eauto.
  rename s into y. rename x into x.
  destruct (eqb_spec x y); subst.
  + (* x=y *)
    rewrite update_shadow in H13.
    apply T_LetCons; auto. apply H with (U:=U); auto. apply H0 with (U:=U); auto.
  + (* x<>y *)
    apply T_LetCons; auto. apply H with (U:=U); auto.
    rewrite update_permute in H13; auto. apply H0 with (U:=U); auto.
    rewrite update_permute in H13; auto. apply H1 with (U:=U); auto.
Qed.

Lemma substitution_preserves_typing_node:
  forall (s : string) (b : basic_ty) (v : value),
    (forall (Gamma : partial_map ty) (x : string) (U : ty) (v0 : value) (T : ty),
        (x |-> U; Gamma) |- v \Vin T -> empty |- v0 \Vin U -> Gamma |- [x := v0 ]v v \Vin T) ->
    forall v0 : value,
      (forall (Gamma : partial_map ty) (x : string) (U : ty) (v1 : value) (T : ty),
          (x |-> U; Gamma) |- v0 \Vin T -> empty |- v1 \Vin U -> Gamma |- [x := v1 ]v v0 \Vin T) ->
      forall v1 : value,
        (forall (Gamma : partial_map ty) (x : string) (U : ty) (v2 : value) (T : ty),
            (x |-> U; Gamma) |- v1 \Vin T -> empty |- v2 \Vin U -> Gamma |- [x := v2 ]v v1 \Vin T) ->
        forall t0 : tm,
          (forall (Gamma : partial_map ty) (x : string) (U : ty) (v2 : value) (T : ty),
              (x |-> U; Gamma) |- t0 \in T -> empty |- v2 \Vin U -> Gamma |- [x := v2] t0 \in T) ->
          forall (Gamma : partial_map ty) (x : string) (U : ty) (v2 : value) (T : ty),
            (x |-> U; Gamma) |- tletnode s b v v0 v1 t0 \in T ->
                                                           empty |- v2 \Vin U -> Gamma |- [x := v2] tletnode s b v v0 v1 t0 \in T.
Proof.
  intros. inversion H3; clear H3; subst; simpl; eauto.
  rename s into y. rename x into x.
  destruct (eqb_spec x y); subst.
  + (* x=y *)
    rewrite update_shadow in H16.
    apply T_LetNode; auto. apply H with (U:=U); auto. apply H0 with (U:=U); auto. apply H1 with (U:=U); auto.
  + (* x<>y *)
    apply T_LetNode; auto. apply H with (U:=U); rewrite update_permute in H16; auto.
    apply H0 with (U:=U); auto. apply H1 with (U:=U); auto.
    rewrite update_permute in H16; auto.
    apply H2 with (U:=U); auto.
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
            (x |-> U; Gamma) |- tmatchb v t t0 \in T -> empty |- v0 \Vin U -> Gamma |- [x := v0] tmatchb v t t0 \in T).
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
           substitution_preserves_typing1 substitution_preserves_typing2
           substitution_preserves_typing3
           substitution_preserves_typing_fix
           substitution_preserves_typing4 substitution_preserves_typing5 substitution_preserves_typing6
           substitution_preserves_typing7 substitution_preserves_typing8).
  - intros. inversion H1; clear H1; subst; simpl; eauto.
    rename s into y. rename x into x.
    destruct (eqb_spec x y); subst.
    + (* x=y *)
      rewrite update_shadow in H9. eauto.
    + (* x<>y *)
      rewrite update_permute in H9...
  - apply substitution_preserves_typing_cons.
  - apply substitution_preserves_typing_node.
  - apply substitution_preserves_typing9.
  - apply substitution_preserves_typing10.
  - intros. inversion H2; clear H2; subst; simpl; eauto.
    rename s into y. destruct (eqb_spec x y); subst.
    + rewrite update_shadow in H12...
    + rewrite update_permute in H12...
  - intros. inversion H2; clear H2; subst; simpl; eauto.
    rename s into y.
    destruct (eqb_spec x y); subst. { rewrite update_shadow in H15... }
    destruct (eqb_spec x s0); subst; auto. { rewrite update_permute in H15; auto. rewrite update_shadow in H15; auto. rewrite update_permute in H15; auto... }
    rewrite (update_permute _ _ y x) in H15; auto. rewrite update_permute in H15; auto...
  - intros. inversion H2; clear H2; subst; simpl; eauto.
    rename s into y.
    destruct (eqb_spec x y); subst.
    + rewrite update_shadow in H18...
    + destruct (eqb_spec x s0); subst; auto. { rewrite (update_permute _ _ y s0) in H18; auto. rewrite update_shadow in H18... }
      destruct (eqb_spec x s1); subst; auto. { rewrite (update_permute _ _ y s1) in H18; auto. rewrite (update_permute _ _ s0 s1) in H18; auto.  rewrite update_shadow in H18... }
      rewrite (update_permute _ _ y x) in H18; auto. rewrite (update_permute _ _ s0 x) in H18; auto. rewrite (update_permute _ _ s1 x) in H18...
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
  - apply substitution_preserves_typing with TNat...
  - apply substitution_preserves_typing with (TBasic (TList T))...
  - apply substitution_preserves_typing with (TBasic (TTree T))...
  - apply T_Lete with (T1 := T_y)...
    apply substitution_preserves_typing with T_x...
    inversion H0; subst...
  - apply T_Lete with (T1 := T_y)...
    apply substitution_preserves_typing with (TArrow T_x T_y)...
    apply substitution_preserves_typing with T_x...
    inversion H0; subst...
  - destruct (nat_value_n_exists _ H).
    + destruct H0 as (n' & Hn'); subst; eauto.
      inversion Hn'; subst.
      apply substitution_preserves_typing with TNat...
    + inversion H0.
  - destruct (list_value_list_exists _ _ H0).
    + destruct H1 as (cch & cct & Hhead & Htail & Hcons); subst; eauto.
      inversion Hcons; subst.
      apply substitution_preserves_typing with (TList T)...
      apply substitution_preserves_typing with T...
      rewrite update_permute...
    + destruct H1; inversion H1.
  - destruct (tree_value_tree_exists _ _ H2).
    + destruct H3 as (croot' & clt' & crt' & Hroot & Hlt & Hrt & Hnode). subst; eauto.
      inversion Hnode; subst.
      apply substitution_preserves_typing with (TTree T)...
      apply substitution_preserves_typing with (TTree T)...
      apply substitution_preserves_typing with T...
      rewrite update_permute...
      rewrite (update_permute _ _ root rt)...
      rewrite update_permute...
    + destruct H3; inversion H3.
Qed.
