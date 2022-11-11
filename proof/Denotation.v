Set Warnings "-notation-overridden,-parsing".
From CT Require Import Maps.
From CT Require Import CoreLang.
From CT Require Import NormalTypeSystem.
From CT Require Import LinearContext.
From CT Require Import RfTypeDef.
From CT Require Import TermMeet.
From CT Require Import TypeClosed.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.PropExtensionality.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Logic.Classical.
From Coq Require Import Lists.List.
From Coq Require Import FunInd.
From Coq Require Import Recdef.

Import CoreLang.
Import NormalTypeSystem.
Import LinearContext.
Import RfTypeDef.
Import TypeClosed.
Import NoDup.
Import TermMeet.
Import Ax.
Import ListNotations.

(* Definition empstate: state := t_empty (cbool false). *)

Definition context := linear_context overunderty.

Definition empty_ctx: context := nil.

(* logical relation *)

Definition is_constant (v: value) :=
  match v with
  | vconst _ => True
  | _ => False
  end.

(* Definition phi_sat_nst (phi: refinement) (nst: state) (c: constant) := *)
(*   (forall st, st \NSTin nst -> closed_refinement_under_state st phi /\ phi st c). *)

(* Lemma term_order_implies_phi_sat_nst_order: forall T phi x e1 e2 st c, *)
(*     e1 <-< e2 -> *)
(*     phi_sat_nst phi (x |-> (e2, T); st) c -> phi_sat_nst phi (x |-> (e1, T); st) c. *)
(* Proof with eauto. *)
(*   intros. *)
(*   intros stst Hinc. assert (stst \NSTin (x |-> (e2, T); st))... eapply term_order_implies_include in H... *)
(* Qed. *)

(* Global Hint Rewrite term_order_implies_phi_sat_nst_order: core. *)

(* overtype denotation *)
Inductive overbase_tmR_aux: state -> overbasety -> constant -> Prop :=
| over_tmR_base : forall (nst: state) (T: base_ty) (phi: refinement) (c: constant),
    st_type_closed (state_to_tystate nst) ({{v: T | phi}}) ->
    empty \N- c \Vin T -> phi nst c -> overbase_tmR_aux nst ({{v: T | phi}}) c.

Global Hint Constructors overbase_tmR_aux: core.

Lemma over_tmR_aux_has_type: forall T phi st c, overbase_tmR_aux st ({{v: T | phi}}) c -> empty \N- c \Vin T.
Proof. intros. inversion H. auto. Qed.

Inductive is_application: tm -> tm -> tm -> Prop :=
(* | Is_app_value_value: forall (x: string) (v1 v2:value), is_application v1 v2 (tletapp x v1 v2 x) *)
| Is_application: forall (e1 e2: tm) (x1 x2 x: string),
    x1 <> x2 -> ~ x1 \FVtm e2 ->
    is_application e1 e2
                   (tlete x1 e1 (tlete x2 e2 (tletapp x x1 x2 x))).

(* Axiom *)
Lemma exists_fresh_var_of_value: forall (e: value), exists (x1 x2: string), x1 <> x2 /\ ~ x1 \FVvalue e.
Proof with eauto.
  intros.
  destruct (exists_not_free_var_in_tm e).
  destruct (exist_fresh_var x).
  exists x, x0. split...
Qed.

Definition application_exists: forall (e1 e2: value) x, exists (x1 x2: string),
    is_application e1 e2 (tlete x1 e1 (tlete x2 e2 (tletapp x x1 x2 x))).
Proof with eauto.
  intros. destruct (exists_fresh_var_of_value e2) as (x1 & x2 & HH & HHH).
  exists x1, x2. constructor...
Qed.

Global Hint Constructors is_application: core.

(* Lemma for is application *)

Lemma value_value_is_application: forall x (v1 v2: value),
  exists (e3: tm), is_application v1 v2 e3 /\ ((tletapp x v1 v2 x) <=< e3).
Proof with eauto.
  intros. destruct (application_exists v1 v2 x) as (x1 & x2 & H).
  exists (tlete x1 v1 (tlete x2 v2 (tletapp x x1 x2 x))). split...
  inversion H; subst. eapply eta_app_value_value...
Qed.

Fixpoint under_tmR_aux (nst: state) (tau: underty) (e: tm) : Prop :=
  st_type_closed (state_to_tystate nst) tau /\
    (* well_formed_type tau /\ *)
    empty \N- e \Tin u\_ tau _/ /\
    (match tau with
     | [[v: T | phi ]] => (forall (c: constant), empty \N- vconst c \Vin T -> phi nst c -> e -->* c)
     | x o: t1 o--> t2 =>
         forall (c_x: constant),
           overbase_tmR_aux nst t1 c_x ->
           (forall e3, is_application e c_x e3 ->
                  under_tmR_aux (update nst x c_x) t2 e3)
     | t1 u--> t2 =>
         forall (e_x: tm),
           under_tmR_aux nst t1 e_x ->
           (forall e3, is_application e e_x e3 -> under_tmR_aux nst t2 e3)
     end).

Lemma under_tmR_aux_bot: forall T, under_tmR_aux empty (mk_bot T) texn.
Proof with eauto.
  intros.
  constructor... apply st_type_closed_in_ctx_bot.
  split... intros. inversion H0.
Qed.

Lemma under_tmR_aux_is_closed: forall tau st e,
    under_tmR_aux st tau e -> st_type_closed_in_ctx (state_to_tystate st) l_empty tau.
Proof with eauto.
  intro tau. induction tau; simpl; intros st e H...
  - destruct H. constructor...
  - destruct H. constructor...
  - destruct H. constructor...
Qed.

Lemma under_tmR_has_type: forall tau st e, under_tmR_aux st tau e -> empty \N- e \Tin u\_ tau _/.
Proof with eauto.
  intro tau. induction tau; simpl; intros st e H...
  - destruct H. destruct H0...
  - destruct o. destruct H. destruct H0...
  - destruct H. destruct H0...
Qed.

Global Hint Resolve under_tmR_has_type: core.

Lemma under_tmR_aux_implies_wf: forall tau st e, under_tmR_aux st tau e -> well_formed_type tau.
  Proof with eauto.
  intro tau. induction tau; simpl; intros st e H...
  - destruct H. destruct H0...
  - destruct H. destruct H0...
Qed.

Global Hint Resolve under_tmR_aux_implies_wf: core.

Lemma step_preserve_under_denotation: forall (e e': tm),
    e <-< e' -> (forall st tau, under_tmR_aux st tau e -> under_tmR_aux st tau e').
Proof with eauto.
  intros e e' Ht st tau He.
  assert (well_formed_type tau)...
  generalize dependent st. generalize dependent H. generalize dependent e. generalize dependent e'.
  induction tau; intros.
  - destruct Ht. inversion He; subst. destruct H3. constructor...
  - inversion He; subst. destruct H1. constructor... split...
    intros. inversion H4; subst...
    assert (under_tmR_aux (s |-> c_x; st) tau (tlete x1 e (tlete x2 c_x (tletapp x x1 x2 x)))).
    eapply H2...
    eapply IHtau... eapply eta_self1...
  - inversion He; subst. destruct H1. constructor... split...
    intros. inversion H4; subst...
    assert (under_tmR_aux st tau2 (tlete x1 e (tlete x2 e_x (tletapp x x1 x2 x)))).
    eapply H2...
    eapply IHtau2... eapply eta_self1...
Qed.

(* Lemma step_preserve_over_denotation: forall (e e': constant), *)
(*     e' <-< e -> (forall st tau, overbase_tmR_aux st tau e -> overbase_tmR_aux st tau e'). *)
(* Proof with eauto. *)
(*   intros e e' Ht st tau He. *)
(*   assert (well_formed_type tau)... *)
(*   generalize dependent st. generalize dependent H. generalize dependent e. generalize dependent e'. *)
(*   induction tau; intros. *)
(*   - destruct Ht. inversion He; subst. destruct H3. constructor... *)
(*   - inversion He; subst. destruct H1. constructor... split... *)
(*     intros. inversion H4; subst... *)
(*     assert (under_tmR_aux (s |-> (tvalue c_x, o\_ o _/); st) tau (tlete x1 e (tlete x2 c_x (tletapp x x1 x2 x)))). *)
(*     eapply H2... *)
(*     eapply IHtau... eapply eta_self1... *)
(*   - inversion He; subst. destruct H1. constructor... split... *)
(*     intros. inversion H4; subst... *)
(*     assert (under_tmR_aux st tau2 (tlete x1 e (tlete x2 e_x (tletapp x x1 x2 x)))). *)
(*     eapply H2... *)
(*     eapply IHtau2... eapply eta_self1... *)
(* Qed. *)

Inductive tmR_aux: state -> overunderty -> tm -> Prop :=
| tmR_oty: forall st oty c, overbase_tmR_aux st oty c -> tmR_aux st oty c
| tmR_uty: forall st uty e, under_tmR_aux st uty e -> tmR_aux st uty e.

Global Hint Constructors tmR_aux: core.

Lemma tmR_has_type: forall tau st e, tmR_aux st tau e -> empty \N- e \Tin ou\_ tau _/.
Proof with eauto.
  intros. destruct H...
  - inversion H; subst; auto.
  - eapply under_tmR_has_type...
Qed.

Global Hint Resolve tmR_has_type: core.

Lemma tmR_implies_ty_of_const_eq: forall st T phi (c_x: constant), tmR_aux st ({{v:T | phi}}) c_x -> empty \N- c_x \Tin T.
Proof with eauto.
  intros. inversion H; subst... inversion H3; subst...
Qed.

Global Hint Resolve tmR_implies_ty_of_const_eq: core.
Global Hint Rewrite tmR_implies_ty_of_const_eq: core.

Lemma denotation_is_closed: forall st tau e,
    tmR_aux st tau e -> st_type_closed_in_ctx (state_to_tystate st) l_empty tau.
Proof with eauto.
  intros. destruct H...
  - inversion H; subst; auto. constructor...
  - apply under_tmR_aux_is_closed in H...
Qed.

Global Hint Resolve denotation_is_closed: core.

Global Hint Constructors has_type: core.
Global Hint Constructors value_has_type: core.

Lemma make_over_top_is_close: forall st T, st_type_closed st ({{v:T | fun (_ : state) (_ : constant) => True}}).
Proof with auto.
  intros.
  constructor... intro x'. intros. rewrite not_appear_free_in_refinement_alt. intros...
Qed.

Global Hint Resolve make_over_top_is_close: core.

Lemma make_op_ret_is_close: forall st op a b,
    st a = Some (fst_ty_of_op op) ->
    st b = Some (snd_ty_of_op op) ->
    st_type_closed st (mk_op_ret op a b).
Proof with auto.
  intros.
  constructor... intro x'. intros. rewrite not_appear_free_in_refinement_alt. intros...
  apply functional_extensionality. intro x. apply propositional_extensionality.
  split; intro He.
  - destruct He as (c_a & c_b & Ha & Hb & Heval).
    exists c_a, c_b.
    destruct (eqb_spec x' a); subst. rewrite H1 in H. inversion H.
    destruct (eqb_spec x' b); subst. rewrite H1 in H0. inversion H0.
    split... rewrite update_neq... split... rewrite update_neq...
  - destruct He as (c_a & c_b & Ha & Hb & Heval).
    exists c_a, c_b.
    destruct (eqb_spec x' a); subst. rewrite H1 in H. inversion H.
    destruct (eqb_spec x' b); subst. rewrite H1 in H0. inversion H0.
    rewrite update_neq in Ha... rewrite update_neq in Hb...
Qed.

Global Hint Resolve make_op_ret_is_close: core.

Lemma erase_const: forall a c st,
    st\_ a |-> c; st _/ = (a |-> ty_of_const c; st\_ st _/).
Proof with auto.
  intros.
  apply functional_extensionality. intros. unfold state_to_tystate.
  destruct (eqb_spec a x); subst...
  rewrite update_eq... rewrite update_eq... rewrite update_neq... rewrite update_neq...
Qed.

Lemma op_value_type_safe: forall op,
    empty \N- vbiop op \Tin fst_ty_of_op op t--> (fst_ty_of_op op t--> ret_ty_of_op op).
Proof with auto.
  intros. destruct op; simpl; constructor; constructor...
Qed.

Global Hint Resolve op_value_type_safe: core.

Lemma op_value_type_safe1: forall op x x1 x2 (c_x1: constant),
    x1 <> x2 ->
    ty_of_const c_x1 = fst_ty_of_op op ->
    empty \N- tlete x1 (vbiop op) (tlete x2 c_x1 (tletapp x x1 x2 x)) \Tin snd_ty_of_op op t--> ret_ty_of_op op.
Proof with auto.
  intros.
  destruct op;
    eapply T_Lete; eauto; eapply T_Lete; eauto; rewrite H0; simpl.
  - eapply T_LetApp with (T_x:=TNat) (T_y:=(TNat t--> TNat)); eauto; simpl...
    eapply T_Var... rewrite update_eq...
    eapply T_Var... rewrite update_permute... rewrite update_eq...
    eapply T_Value... eapply T_Var... rewrite update_eq...
  - eapply T_LetApp with (T_x:=TNat) (T_y:=(TNat t--> TBool)); eauto; simpl...
    eapply T_Var... rewrite update_eq...
    eapply T_Var... rewrite update_permute... rewrite update_eq...
    eapply T_Value... eapply T_Var... rewrite update_eq...
  - eapply T_LetApp with (T_x:=TNat) (T_y:=(TNat t--> TBool)); eauto; simpl...
    eapply T_Var... rewrite update_eq...
    eapply T_Var... rewrite update_permute... rewrite update_eq...
    eapply T_Value... eapply T_Var... rewrite update_eq...
  - eapply T_LetApp with (T_x:=TNat) (T_y:=(TNat t--> TNat)); eauto; simpl...
    eapply T_Var... rewrite update_eq...
    eapply T_Var... rewrite update_permute... rewrite update_eq...
    eapply T_Value... eapply T_Var... rewrite update_eq...
Qed.

Lemma op_value_type_safe2: forall op x x0 x1 x2 x3 x4 (c_x1 c_x2: constant),
    x1 <> x2 -> x0 <> x3 ->
    ty_of_const c_x1 = fst_ty_of_op op ->
    ty_of_const c_x2 = snd_ty_of_op op ->
    empty \N- tlete x0 (tlete x1 (vbiop op) (tlete x2 c_x1 (tletapp x x1 x2 x))) (tlete x3 c_x2 (tletapp x4 x0 x3 x4)) \Tin
                    ret_ty_of_op op.
Proof with auto.
  intros.
  destruct op.
  - apply T_Lete with (T1 := (TNat t--> TNat))... apply op_value_type_safe1...
    apply T_Lete with (T1 := TNat)... apply T_Value... simpl in H2. rewrite <- H2. eapply T_Const...
    apply T_LetApp with (T_x := TNat) (T_y := TNat)... eapply T_Var... rewrite update_eq... eapply T_Var... rewrite update_permute... rewrite update_eq... eapply T_Value... eapply T_Var... rewrite update_eq...
  - apply T_Lete with (T1 := (TNat t--> TBool))... apply op_value_type_safe1...
    apply T_Lete with (T1 := TNat)... apply T_Value... simpl in H2. rewrite <- H2. eapply T_Const...
    apply T_LetApp with (T_x := TNat) (T_y := TBool)... eapply T_Var... rewrite update_eq... eapply T_Var... rewrite update_permute... rewrite update_eq... eapply T_Value... eapply T_Var... rewrite update_eq...
  - apply T_Lete with (T1 := (TNat t--> TBool))... apply op_value_type_safe1...
    apply T_Lete with (T1 := TNat)... apply T_Value... simpl in H2. rewrite <- H2. eapply T_Const...
    apply T_LetApp with (T_x := TNat) (T_y := TBool)... eapply T_Var... rewrite update_eq... eapply T_Var... rewrite update_permute... rewrite update_eq... eapply T_Value... eapply T_Var... rewrite update_eq...
  - apply T_Lete with (T1 := (TNat t--> TNat))... apply op_value_type_safe1...
    apply T_Lete with (T1 := TNat)... apply T_Value... simpl in H2. rewrite <- H2. eapply T_Const...
    apply T_LetApp with (T_x := TNat) (T_y := TNat)... eapply T_Var... rewrite update_eq... eapply T_Var... rewrite update_permute... rewrite update_eq... eapply T_Value... eapply T_Var... rewrite update_eq...
Qed.

Lemma over_tmR_aux_const: forall st T phi c,
    overbase_tmR_aux st ({{v:T | phi}}) c -> ty_of_const c = T.
Proof.
  intros.
  inversion H; subst. inversion H4; subst... reflexivity.
Qed.

Global Hint Resolve over_tmR_aux_const: core.

Lemma mk_op_has_denotation: forall st op a b,
    a <> b -> tmR_aux st (mk_op op a b) (vbiop op).
Proof with eauto.
  intros. constructor... constructor...
  - constructor...
    + constructor... constructor... constructor...
    + constructor... constructor... constructor...
      apply make_op_ret_is_close... rewrite update_permute... rewrite update_eq... rewrite update_eq...
  - split. simpl...
    intros c_x1 Hc_x1D e3 Happ. inversion Happ; subst. clear Happ.
    constructor...
    + constructor... constructor... constructor... rewrite erase_const.
      apply make_op_ret_is_close...
      rewrite update_permute... rewrite update_eq... erewrite over_tmR_aux_const... rewrite update_eq...
    + split. simpl... apply op_value_type_safe1...
      intros c_x2 He_x2D e3 Happ. inversion Happ; subst. clear Happ.
      constructor...
      { simpl. apply make_op_ret_is_close... rewrite erase_const. rewrite erase_const.
        rewrite update_permute... rewrite update_eq... erewrite over_tmR_aux_const... rewrite erase_const. rewrite erase_const. rewrite update_eq... erewrite over_tmR_aux_const...
      }
      split. simpl... apply op_value_type_safe2...
      intros c_res Hc_resT.
      intros. destruct H4 as (c_a & c_b & HcaD & HcbD & HH).
      rewrite update_eq in HcbD. inversion HcbD; subst.
      rewrite update_permute in HcaD... rewrite update_eq in HcaD... inversion HcaD; subst.
      apply eta_op_reducetion...
Qed.

Global Hint Resolve mk_op_has_denotation: core.

Lemma nst_no_free_implies_eq_close_over: forall x T T' phi,
    ~ appear_free_in_overbasety x ({{v:T | phi}}) ->
    (forall tyst, st_type_closed tyst ({{v:T | phi}}) <-> st_type_closed (x |-> T'; tyst) ({{v:T | phi}})).
Proof with eauto.
  intros. simpl in H.
  split.
  intros.
  - inversion H0; subst. constructor... unfold phi_sat_tyst. unfold phi_sat_tyst in H3. intros x' Hx. apply H3...
    destruct (eqb_spec x x'); subst. rewrite update_eq in Hx. inversion Hx. rewrite update_neq in Hx...
  - intros H0. inversion H0; subst. constructor... unfold phi_sat_tyst. unfold phi_sat_tyst in H3. intros x' Hx. intro.
    destruct (eqb_spec x x'); subst.
    + apply H. apply H1.
    + assert ((x |-> T'; tyst) x' = None). rewrite update_neq... apply H3 in H2. apply H2...
Qed.

Lemma nst_no_free_implies_eq_close_under: forall x T T' phi,
    ~ appear_free_in_underty x ([[v:T | phi]]) ->
    (forall tyst, st_type_closed tyst ([[v:T | phi]]) <-> st_type_closed (x |-> T'; tyst) ([[v:T | phi]])).
Proof with eauto.
  intros. simpl in H.
  split.
  intros.
  - inversion H0; subst. constructor... unfold phi_sat_tyst. unfold phi_sat_tyst in H3. intros x' Hx. apply H3...
    destruct (eqb_spec x x'); subst. rewrite update_eq in Hx. inversion Hx. rewrite update_neq in Hx...
  - intros H0. inversion H0; subst. constructor... unfold phi_sat_tyst. unfold phi_sat_tyst in H3. intros x' Hx. intro.
    destruct (eqb_spec x x'); subst.
    + apply H. apply H1.
    + assert ((x |-> T'; tyst) x' = None). rewrite update_neq... apply H3 in H2. apply H2...
Qed.

Lemma tmR_nst_no_free_implies_eq_over: forall T phi st x e_x_hat,
    ~ appear_free_in_overbasety x ({{v:T | phi}}) ->
    (forall e, overbase_tmR_aux (x |-> e_x_hat; st) ({{v:T | phi}}) e <-> overbase_tmR_aux st ({{v:T | phi}}) e).
Proof with eauto.
  intros T phi st x e_x_hat Hfree.
  split; intros HH.
  - simpl in Hfree. rewrite not_appear_free_in_refinement_alt in Hfree.
    inversion HH; subst. erewrite <- Hfree in H5... constructor... rewrite erase_const in H1...
    rewrite nst_no_free_implies_eq_close_over...
  - simpl in Hfree. rewrite not_appear_free_in_refinement_alt in Hfree.
    inversion HH; subst. constructor... rewrite erase_const ... rewrite <- nst_no_free_implies_eq_close_over...
    erewrite <- Hfree...
Qed.

Lemma tmR_nst_no_free_implies_eq_aux: forall (tau: underty) st x e_x_hat,
    ~ appear_free_in_underty x tau -> (forall e, under_tmR_aux (x |-> e_x_hat; st) tau e <-> under_tmR_aux st tau e).
Proof with eauto.
  intros tau.
  induction tau; intros st x e_x_hat Hfree; split.
  - simpl in Hfree.
    intros HH. inversion HH; subst. destruct H0. constructor... rewrite erase_const in H...
    apply nst_no_free_implies_eq_close_under with (T' := ty_of_const e_x_hat) (T:=b) (tyst :=  st\_ st _/) in Hfree...
    rewrite Hfree...
    split... intros. apply H1... rewrite not_appear_free_in_refinement_alt in Hfree. rewrite <- Hfree...
  - simpl in Hfree. rewrite not_appear_free_in_refinement_alt in Hfree.
    intros HH. inversion HH; subst. destruct H0. constructor... rewrite erase_const...
    rewrite <- nst_no_free_implies_eq_close_under...
    split... intros. apply H1... erewrite Hfree...
  - intros. inversion H; subst... constructor...
    assert (type_ctx_no_dup empty ((x, Uty (tau) )::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H2. apply l_find_right_most_none_neq_hd in H2. exfalso...
    split...
    destruct H1. intros c_x Hc_xD e3 Happ. inversion Happ; subst. setoid_rewrite state_permute in H2...
    rewrite <- IHtau... apply H2... destruct o. rewrite tmR_nst_no_free_implies_eq_over...
    + intro HH. apply Hfree. constructor...
    + intro HH. apply Hfree. constructor... destruct o.
    assert (type_ctx_no_dup empty ((x, Oty ({{v:b | r}}))::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H5. apply l_find_right_most_none_neq_hd in H5. exfalso...
  - intros. inversion H; subst... constructor...
    assert (type_ctx_no_dup empty ((x, Uty tau)::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H2. apply l_find_right_most_none_neq_hd in H2. exfalso...
    split...
    destruct H1. intros c_x Hc_xD e3 Happ. inversion Happ; subst. setoid_rewrite state_permute.
    rewrite IHtau... apply H2... destruct o. rewrite <- tmR_nst_no_free_implies_eq_over...
    + intro HH. apply Hfree. constructor...
    + intro HH. apply Hfree. constructor... destruct o.
      assert (type_ctx_no_dup empty ((x, Oty ({{v:b | r}}))::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H5. apply l_find_right_most_none_neq_hd in H5. exfalso...
  - intros. inversion H; subst... destruct H1...
    assert (type_ctx_no_dup empty ((x, Uty tau1)::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H3. apply l_find_right_most_none_neq_hd in H3. exfalso...
  - intros. inversion H; subst... destruct H1...
    assert (type_ctx_no_dup empty ((x, Uty tau1)::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H3. apply l_find_right_most_none_neq_hd in H3. exfalso...
Qed.

Lemma tmR_nst_no_free_implies_eq: forall (tau: underty) st x e_x_hat,
    ~ appear_free_in_underty x tau -> (forall e, tmR_aux (x |-> e_x_hat; st) tau e <-> tmR_aux st tau e).
Proof with eauto.
  intros.
  split; intros.
  - inversion H0; subst. rewrite tmR_nst_no_free_implies_eq_aux in H3...
  - inversion H0; subst. constructor... rewrite tmR_nst_no_free_implies_eq_aux ...
Qed.

(* The denotation does not guarantee the well-formedness (inv_ctx). *)
(* The denotation implies no dup. *)
Inductive tmR_in_ctx_aux: state -> context -> overunderty -> tm -> Prop :=
| tmR_in_ctx_aux_nil: forall (nst: state) (tau: overunderty) e,
    tmR_aux nst tau e -> tmR_in_ctx_aux nst [] tau e
| tmR_in_ctx_aux_cons_overbase:
  forall (nst: state) (x: string) (T:base_ty) (phi: refinement) (Gamma: context) (tau: overunderty) e,
    st_type_closed_ctx (st\_ nst _/) ((x, (Oty ({{v: T | phi}}))) :: Gamma) ->
    st_type_closed_in_ctx (st\_ nst _/) ((x, (Oty ({{v: T | phi}}))) :: Gamma) tau ->
    well_formed_type ({{v: T | phi}}) ->
    (forall (c_x: constant), tmR_aux nst ({{v: T | phi}}) c_x ->
                        tmR_in_ctx_aux (update nst x c_x) Gamma tau (tlete x (vconst c_x) e)) ->
    tmR_in_ctx_aux nst ((x, (Oty ({{v: T | phi}}))) :: Gamma) tau e
| tmR_in_ctx_aux_cons_under:
  forall (nst: state) (x: string) (T:base_ty) (phi: refinement) (Gamma: context) (tau: overunderty) e,
    st_type_closed_ctx (st\_ nst _/) ((x, (Uty ([[v: T | phi]]))) :: Gamma) ->
    st_type_closed_in_ctx (st\_ nst _/) ((x, (Uty ([[v: T | phi]]))) :: Gamma) tau ->
    (exists e_x_hat, tmR_aux nst ([[v: T | phi]]) e_x_hat /\
                  (forall e_x, tmR_aux nst ([[v: T | phi]]) e_x ->
                          (forall (v_x_hat: constant), e_x -->* v_x_hat ->
                              tmR_in_ctx_aux (update nst x v_x_hat) Gamma tau (tlete x e_x e)
                          )
    )) ->
    tmR_in_ctx_aux nst ((x, (Uty ([[v: T | phi]]))) :: Gamma) tau e
| tmR_in_ctx_aux_cons_oarr: forall (nst: state) (x: string) a T phi (tau_b: underty) (Gamma: context) (tau: overunderty) e,
    st_type_closed_ctx (st\_ nst _/) ((x, Uty (a o: ({{v: T | phi}}) o--> tau_b)) :: Gamma) ->
    st_type_closed_in_ctx (st\_ nst _/) ((x, Uty (a o: ({{v: T | phi}}) o--> tau_b)) :: Gamma) tau ->
    (forall e_x, tmR_aux nst (a o: ({{v: T | phi}}) o--> tau_b) e_x ->
            tmR_in_ctx_aux nst Gamma tau (tlete x e_x e)) ->
    tmR_in_ctx_aux nst ((x, Uty (a o: ({{v: T | phi}}) o--> tau_b)) :: Gamma) tau e
| tmR_in_ctx_aux_cons_underarr: forall (nst: state) (x: string) (t1 t2: underty) (Gamma: context) (tau: overunderty) e,
    st_type_closed_ctx (st\_ nst _/) ((x, Uty (t1 u--> t2)) :: Gamma) ->
    st_type_closed_in_ctx (st\_ nst _/) ((x, Uty (t1 u--> t2)) :: Gamma) tau ->
    (forall e_x, tmR_aux nst (t1 u--> t2) e_x ->
            tmR_in_ctx_aux nst Gamma tau (tlete x e_x e)) ->
    tmR_in_ctx_aux nst ((x, Uty (t1 u--> t2)) :: Gamma) tau e.

Global Hint Constructors tmR_in_ctx_aux: core.

Lemma empty_tmR_in_ctx_term_is_closed: forall st tau e,
    tmR_in_ctx_aux st [] tau e -> forall x, ~ x \FVtm e.
Proof with eauto.
  intros. inversion H; subst. inversion H0; subst.
  - intro. simpl in H2. inversion H2.
  - apply under_tmR_has_type in H1. eapply closed_term_has_no_free_var in H1. apply H1.
Qed.

Global Hint Resolve empty_tmR_in_ctx_term_is_closed: core.

Lemma tmR_aux_to_over: forall st T phi21 (c_x: constant),
    tmR_in_ctx_aux st [] ({{v:T | phi21}}) c_x <-> overbase_tmR_aux st ({{v:T | phi21}}) c_x.
Proof with eauto.
  split; intros.
  inversion H; subst... inversion H0; subst...
  constructor...
Qed.

Lemma tmR_in_ctx_to_under: forall st (tau: underty) e, tmR_in_ctx_aux st [] tau e <-> under_tmR_aux st tau e.
Proof with eauto.
  intros.
  split; intros.
  inversion H; subst... inversion H0; subst...
  constructor...
Qed.

Lemma tmR_in_ctx_aux_implies_closed_ctx: forall st Gamma tau e,
    tmR_in_ctx_aux st Gamma tau e -> st_type_closed_ctx (state_to_tystate st) Gamma.
Proof with eauto.
  intros. destruct H...
Qed.

Lemma tmR_in_ctx_aux_implies_closed: forall st Gamma tau e,
    tmR_in_ctx_aux st Gamma tau e -> st_type_closed_in_ctx (state_to_tystate st) Gamma tau.
Proof with eauto.
  intros. destruct H...
Qed.


Lemma tmR_in_ctx_aux_implies_no_dup: forall st Gamma tau e,
    tmR_in_ctx_aux st Gamma tau e -> type_ctx_no_dup (state_to_tystate st) Gamma.
Proof with eauto.
  intros. destruct H...
Qed.

Global Hint Resolve tmR_in_ctx_aux_implies_no_dup: core.


Lemma tmR_in_ctx_aux_implies_fst_closed: forall st Gamma x tau_x tau e,
    tmR_in_ctx_aux st ((x, tau_x)::Gamma) tau e -> st_type_closed (state_to_tystate st) tau_x.
Proof with eauto.
  intros. apply tmR_in_ctx_aux_implies_closed_ctx in H. apply st_type_closed_ctx_implies_head_closed in H.
  destruct H... destruct H0...
  inversion H0...
Qed.


(* Definition tmR tau e := forall st, tmR_aux st tau e. *)
(* Definition tmR_in_ctx Gamma tau e := forall st, tmR_in_ctx_aux st Gamma tau e. *)

(* denotation in ctx \S{Ty} *)

Lemma mk_eq_constant_is_itsefl_in_ctx: forall st Gamma (c c': constant),
    tmR_in_ctx_aux st Gamma (mk_eq_constant c) c' <-> c' = c.
Proof with eauto.
  intros. induction Gamma... split; intros. inversion H; subst. inversion H0; subst. inversion H3; subst. destruct H2.
  assert (c' -->* c)... inversion H5; subst... inversion H6.
  subst. constructor... constructor... constructor... constructor... intro x'. intros. rewrite not_appear_free_in_refinement_alt. intros...
  split... simpl... intros. subst...
  destruct a. assert (type_ctx_no_dup empty ((s, o)::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H. apply l_find_right_most_none_neq_hd in H. exfalso...
Qed.

(* denotation in ctx Lemmas *)

Lemma denotation_ctx_implies_well_formed_type: forall st Gamma tau e,
    tmR_in_ctx_aux st Gamma tau e -> well_formed_type tau.
Proof with eauto.
  intros.
  intros. apply tmR_in_ctx_aux_implies_closed in H...
Qed.

Global Hint Resolve denotation_ctx_implies_well_formed_type: core.

Lemma denotation_ctx_implies_last_well_formed_type: forall st Gamma x tau tau' e,
    tmR_in_ctx_aux st (Gamma ++ ((x, tau)::nil)) tau' e -> well_formed_type tau.
Proof with eauto.
  intros. apply tmR_in_ctx_aux_implies_closed_ctx in H. apply destructst_type_closed_ctx in H.
  destruct H as (HH1 & HH2 & HH3 & HH4 & HH5)...
Qed.


Global Hint Resolve denotation_ctx_implies_last_well_formed_type: core.

Lemma step_preserve_ctx_denotation_over: forall Gamma (e e': tm),
    e' <-< e -> (forall st (tau: overbasety), tmR_in_ctx_aux st Gamma tau e -> tmR_in_ctx_aux st Gamma tau e').
Proof with eauto.
  intros Gamma.
  induction Gamma; simpl; intros e e' Hts st tau He...
  - inversion He; subst. inversion H; subst. constructor...
    destruct (exists_not_free_var_in_tm c).
    assert (type_ctx_no_dup empty ((x, Oty tau)::nil)). constructor...
    apply type_ctx_no_dup_implies_head_free in H1. apply l_find_right_most_none_neq_hd in H1. exfalso...
  (* eapply term_order_const_bound with (v:=c) in Hts... subst... *)
  - inversion He; subst.
    + constructor...
      intros c_x Hc_xD. assert (tmR_in_ctx_aux (x |-> c_x; st) Gamma tau (tlete x c_x e)) as Hv1...
      eapply IHGamma... apply eta_self2...
    + constructor...
      destruct H6 as (e_x_hat & He_x_hatD & HH).
      exists e_x_hat. split...
      intros e_x He_xD v_x_hat HvE.
      assert (tmR_in_ctx_aux (x |-> v_x_hat; st) Gamma tau (tlete x e_x e))...
      eapply IHGamma... apply eta_self2...
    + constructor...
      intros e_x He_xD.
      assert (tmR_in_ctx_aux st Gamma tau (tlete x e_x e))...
      eapply IHGamma... apply eta_self2...
    + constructor...
      intros e_x He_xD.
      assert (tmR_in_ctx_aux st Gamma tau (tlete x e_x e))...
      eapply IHGamma... apply eta_self2...
Qed.

Lemma step_preserve_ctx_denotation: forall Gamma (e e': tm),
    e <-< e' -> (forall st (tau: underty), tmR_in_ctx_aux st Gamma tau e -> tmR_in_ctx_aux st Gamma tau e').
Proof with eauto.
  intros Gamma.
  induction Gamma; simpl; intros e e' Hts st tau He...
  - rewrite tmR_in_ctx_to_under. rewrite tmR_in_ctx_to_under in He. eapply step_preserve_under_denotation...
  - inversion He; subst.
    + constructor...
      intros c_x Hc_xD. assert (tmR_in_ctx_aux (x |-> c_x; st) Gamma tau (tlete x c_x e)) as Hv1...
      eapply IHGamma... apply eta_self2...
    + constructor...
      destruct H6 as (e_x_hat & He_x_hatD & HH).
      exists e_x_hat. split...
      intros e_x He_xD v_x_hat HvE.
      assert (tmR_in_ctx_aux (x |-> v_x_hat; st) Gamma tau (tlete x e_x e))...
      eapply IHGamma... apply eta_self2...
    + constructor...
      intros e_x He_xD.
      assert (tmR_in_ctx_aux st Gamma tau (tlete x e_x e))...
      eapply IHGamma... apply eta_self2...
    + constructor...
      intros e_x He_xD.
      assert (tmR_in_ctx_aux st Gamma tau (tlete x e_x e))...
      eapply IHGamma... apply eta_self2...
Qed.

Lemma denotation_in_ctx_implies_not_free_not_in_ctx: forall Gamma st tau e x,
    tmR_in_ctx_aux st Gamma tau e -> l_find_right_most Gamma x = None -> ~ x \FVtm e.
Proof with auto.
  intros.
  induction Gamma. inversion H; subst. apply tmR_has_type in H1. eapply empty_has_type_implies_closed in H1... apply H1.
  destruct a.
 apply tmR_in_ctx_aux_implies_no_dup in H.
  apply type_ctx_no_dup_implies_head_free in H. apply l_find_right_most_none_neq_hd in H. exfalso...
Qed.

Global Hint Resolve denotation_in_ctx_implies_not_free_not_in_ctx: core.

(* Global Hint Unfold lcontxt_to_basic_ctx: core. *)

(* Finally, we define the denotation *)
Definition tmR_in_ctx_all_st Gamma tau e := tmR_in_ctx_aux empty Gamma tau e.

Global Hint Unfold tmR_in_ctx_all_st: core.

Definition well_founded_constraint (x: string) (phi: refinement) :=
  fun st c => (exists c_x, st x = Some c_x /\ const_order c c_x) /\ phi st c.
