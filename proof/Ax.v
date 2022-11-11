Set Warnings "-notation-overridden,-parsing".
From CT Require Import Maps.
From CT Require Import CoreLang.
From CT Require Import NormalTypeSystem.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.

From Coq Require Import Lists.List.
Import ListNotations.
Import NormalTypeSystem.

Definition state_permute {A:Type}: forall (st: string -> option A) x y (a a': A),
    (x |-> a; y |-> a'; st) = (y |-> a'; x |-> a; st).
Admitted.

Definition exists_not_free_var_in_tm: forall e, exists x, ~ x \FVtm e.
Admitted.

Lemma term_has_no_free_var: forall e x, ~ x \FVtm e.
Admitted.

Lemma value_has_no_free_var: forall e x, ~ x \FVvalue e.
Proof with eauto.
  intros... intro H...
  assert (~ x \FVtm e). apply term_has_no_free_var. apply H0. simpl...
Qed.

Local Hint Resolve term_has_no_free_var: core.
Local Hint Resolve value_has_no_free_var: core.

Lemma closed_term_has_no_free_var: forall e x T, empty \N- e \Tin T -> ~ x \FVtm e.
Proof with eauto.
  intros...
Qed.

Lemma lete_preserve_not_free: forall e x a e_a, ~ x \FVtm e_a -> ~ x \FVtm e -> ~ x \FVtm tlete a e_a e.
Proof with eauto.
Admitted.

Global Hint Resolve lete_preserve_not_free: core.

Definition const_order (c1 c2: constant) : Prop :=
  match c1 with
  | cbool c1 =>
      match c2 with
      | cbool c2 =>
          (match c1 with
           | true => False
           | false => c2 = true
           end)
      | cnat _ => True
      end
  | cnat c1 =>
      match c2 with
      | cbool _ => False
      | cnat c2 => c1 < c2
      end
  end.

(* for vfix Axiom *)
Lemma const_order_is_well_founded: well_founded const_order.
Admitted.


(* Facts *)
Theorem multi_preservation : forall t t' T,
    empty \N- t \Tin T  -> t -->* t'  ->
                  empty \N- t' \Tin T.
Proof with eauto.
Admitted.

Theorem preservation_value : forall t (v: value) T,
    empty \N- t \Tin T  -> t -->* v  ->
                  empty \N- v \Vin T.
Proof with eauto.
  intros. eapply multi_preservation in H...
  inversion H...
Qed.

Lemma ty_unique: forall Gamma e T1 T2,
    Gamma \N- e \Tin T1  -> Gamma \N- e \Tin T2 -> T1 = T2.
Proof with eauto.
Admitted.

Lemma empty_has_type_implies_closed: forall e T, empty \N- e \Tin T -> (forall x, ~ x \FVtm e).
Admitted.

Global Hint Resolve empty_has_type_implies_closed: core.


(* Term Fact *)

Lemma let_reduction_spec: forall x e_x e (v: value),
    tlete x e_x e -->* v <-> (exists (v_x: value), e_x -->* v_x /\ (subst x v_x e) -->* v).
Admitted.

Lemma letapp_reduction_spec: forall x (f: value) (v1 : value) (v : value) e,
    tletapp x f v1 e -->* v <->
    (exists x_m T_m e_m,
        f = vlam x_m T_m e_m /\ (tlete x (subst x_m v1 e_m) e) -->* v).
Admitted.

Lemma reduction_eq_implies_ty_eq: forall e1 e2,
    (forall (v: value), e1 -->* v -> e2 -->* v) ->
    (forall Gamma T, Gamma \N- e1 \Tin T -> Gamma \N- e2 \Tin T).
Admitted.

Lemma reduction_eq_implies_eq: forall e1 e2,
    (forall (v: value), e1 -->* v <-> e2 -->* v) -> e1 <=< e2.
Proof with eauto.
  intros.
  split.
  - split. intros... rewrite <- H... apply reduction_eq_implies_ty_eq... intros... rewrite <- H...
  - split. intros... rewrite H... apply reduction_eq_implies_ty_eq... intros... rewrite H...
Qed.

Lemma reduction_sub_implies_sub: forall e1 e2,
    (forall (v: value), e1 -->* v -> e2 -->* v) -> e1 <-< e2.
Proof with eauto.
  intros.
  split.
  - intros...
  - intros... eapply reduction_eq_implies_ty_eq...
Qed.

Lemma subst_letx_same: forall x1 v1 x,
  (if (x1 =? x)%string then x else if (x1 =? x)%string then v1 else x) = x.
Proof with eauto.
  intros.
  destruct (eqb_spec x1 x); subst...
Qed.

Lemma value_only_reduce_to_itself: forall (v v': value),
    v -->* v' <-> v = v'.
Proof with eauto.
  intros. split; intro H...
  - inversion H; subst... inversion H0.
  - subst...
Qed.

Lemma reduce_value_prop_same: forall (v: value) P,
   (exists (v_x : value), v -->* v_x /\ P v_x) <-> P v.
Proof with eauto.
  split; intros.
  - setoid_rewrite value_only_reduce_to_itself in H. destruct H as (v' & Hv' & HH); subst...
  - exists v. split...
Qed.

Lemma reduce_value_prop_same_post: forall (v: value) P,
   (exists (v_x : value), P v_x /\ v_x -->* v) <-> P v.
Proof with eauto.
  split; intros.
  - setoid_rewrite value_only_reduce_to_itself in H. destruct H as (v' & Hv' & HH); subst...
  - exists v. split...
Qed.

Lemma eta_app_value_value: forall x (v1 v2: value) x1 x2,
    x1 <> x2 -> ~ x1 \FVtm v2 ->
    tletapp x v1 v2 x <=< tlete x1 v1 (tlete x2 v2 (tletapp x x1 x2 x)).
Proof with eauto.
  intros. apply reduction_eq_implies_eq.
  split; intro HE.
  - rewrite let_reduction_spec. rewrite reduce_value_prop_same.
    simpl. rewrite not_free_rewrite_value...
    rewrite let_reduction_spec. rewrite reduce_value_prop_same. rewrite <- eqb_neq in H. rewrite H. rewrite eqb_refl.
    simpl. rewrite not_free_rewrite_value... rewrite eqb_refl.
    destruct (eqb_spec x2 x); destruct (eqb_spec x1 x); simpl; subst...
    + rewrite <- eqb_neq in n... rewrite n...
    + rewrite <- eqb_neq in n... rewrite n...
  - rewrite let_reduction_spec in HE. rewrite reduce_value_prop_same in HE.
    simpl in HE. rewrite not_free_rewrite_value in HE... rewrite <- eqb_neq in H. rewrite H in HE. rewrite eqb_refl in HE.
    rewrite let_reduction_spec in HE. rewrite reduce_value_prop_same in HE.
    simpl in HE. rewrite not_free_rewrite_value in HE... rewrite eqb_refl in HE.
    destruct (eqb_spec x2 x); destruct (eqb_spec x1 x); simpl in HE; subst...
    + rewrite <- eqb_neq in n... rewrite n in HE...
    + rewrite <- eqb_neq in n... rewrite n in HE...
Qed.

(* Meet Axiom *)
(* meet operation, the trick here is encode the conjunction into the target language, via a poly equal operator.
  let x1 = e1 in
  let x2 = e2 in
  let x3 = x1 ==T x2 in
  match x3 with
  | true -> x2
  | false -> err

 *)

Lemma meet_of_two_terms_exists: forall e1 e2 T,
    empty \N- e1 \Tin T -> empty \N- e2 \Tin T ->
    (exists e3, (empty \N- e3 \Tin T) /\ (forall c, e3 -->* c <-> e1 -->* c /\ e2 -->* c)).
Admitted.

Lemma eta_reduction: forall x1 (f: value) x2 (v: value),
    ~ x1 \FVvalue v ->
    (tletapp x2 f v x2) <=< (tlete x1 f (tletapp x2 x1 v x2)).
Proof with eauto.
    intros. apply reduction_eq_implies_eq.
    split. intro HE.
    - rewrite let_reduction_spec. rewrite reduce_value_prop_same.
      simpl. rewrite not_free_rewrite_value...
      rewrite eqb_refl.
      destruct (eqb_spec x1 x2); simpl. apply HE. apply HE.
    - rewrite let_reduction_spec. rewrite reduce_value_prop_same.
      simpl. rewrite not_free_rewrite_value...
      rewrite eqb_refl.
      destruct (eqb_spec x1 x2); simpl. intro HE. apply HE. intro HE. apply HE.
 Qed.

Ltac lete_eta_step := rewrite let_reduction_spec; simpl;
                      try (rewrite reduce_value_prop_same; simpl);
                      try (rewrite reduce_value_prop_same_post; simpl);
                      try (rewrite not_free_rewrite_value; simpl);
                      try rewrite eqb_refl; try rewrite eqb_neq.

Ltac lete_eta_step_in H := rewrite let_reduction_spec in H; simpl in H;
                           try (rewrite reduce_value_prop_same in H; simpl);
                           try (rewrite reduce_value_prop_same_post in H; simpl);
                           try (rewrite reduce_value_prop_same in H; simpl in H);
                           try rewrite not_free_rewrite_value in H;
                           try rewrite eqb_refl in H; try rewrite eqb_neq in H.

Lemma reduce_letapp_value_prop_same: forall (v: value) P,
   (exists (v_x : value), v -->* v_x /\ P v_x) <-> P v.
Proof with eauto.
  split; intros.
  - setoid_rewrite value_only_reduce_to_itself in H. destruct H as (v' & Hv' & HH); subst...
  - exists v. split...
Qed.

Lemma tail_lete: forall x e (v: value), tlete x e x -->* v <-> e -->* v.
Proof with eauto.
  intros. split; intro HE.
  - lete_eta_step_in HE. destruct HE as (v_x & HEv_x & Hvv). rewrite value_only_reduce_to_itself in Hvv; subst...
  - lete_eta_step. exists v. split...
Qed.

Ltac letapp_eta_step Hletapp := rewrite letapp_reduction_spec; intro Hletapp;
                                destruct Hletapp as (x_m & T_m & e_m & Happ1 & Happ2); inversion Happ1; subst; clear Happ1;
                                try (lete_eta_step_in Happ2; rewrite reduce_value_prop_same_post in Happ2).

Lemma eta_application_const_to_lete_const: forall x2 x T e (c_x: constant),
    (tlete x c_x e) <=< (tletapp x2 (vlam x T e) c_x x2).
Proof with eauto.
    intros. apply reduction_eq_implies_eq.
    split. intro HE.
    - lete_eta_step_in HE.
      rewrite letapp_reduction_spec. exists x, T, e. split... rewrite tail_lete...
    - lete_eta_step. letapp_eta_step Hletapp...
 Qed.

Lemma eta_lete_const_to_subst: forall x e (c_x: constant),
    (subst x c_x e) <=< (tlete x c_x e).
Proof with eauto.
    intros. apply reduction_eq_implies_eq.
    split. intro HE.
    - rewrite let_reduction_spec. rewrite reduce_value_prop_same. apply HE.
    - rewrite let_reduction_spec. rewrite reduce_value_prop_same.  trivial.
Qed.

Lemma eta_in_lam: forall a T e1 e2,
    (forall (v: value), e1 -->* v -> e2 -->* v) <-> (forall (v: value), vlam a T e1 -->* v -> vlam a T e2 -->* v).
Admitted.

Lemma eta_lete_const_to_subst_in_lam: forall a T x e (c_x: constant),
    (vlam a T (subst x c_x e)) <=< (vlam a T (tlete x c_x e)).
Proof with eauto.
  intros. apply reduction_eq_implies_eq.
  split.
  - apply eta_in_lam. intros v' HE. lete_eta_step...
  - apply eta_in_lam. intros v' HE. lete_eta_step_in HE...
Qed.

Lemma eta_closed_term_can_captured_by_lam: forall a e_a Ta x T e,
    empty \N- e_a \Tin Ta -> a <> x ->
    (vlam x T (tlete a e_a e)) <=< (tlete a e_a (vlam x T e)).
Proof with eauto.
Admitted.

Lemma eta_matchb_true: forall e1 e2, e1 <=< (tmatchb true e1 e2).
Proof with auto.
  intros. apply reduction_eq_implies_eq.
  split.
  - intros. econstructor...
  - intros. inversion H; subst... inversion H0; subst...
Qed.

Lemma eta_matchb_false: forall e1 e2, e2 <=< (tmatchb false e1 e2).
Proof with auto.
  intros. apply reduction_eq_implies_eq.
  split.
  - intros. econstructor...
  - intros. inversion H; subst... inversion H0; subst...
Qed.

Lemma eta1: forall x1 x T e x2 (c_x:constant) x0,
    x1<> x2 -> ~ x1 \FVtm c_x ->
    (tlete x1 (vlam x T e) (tlete x2 c_x (tletapp x0 x1 x2 x0))) <=< (tlete x c_x e).
Proof with eauto.
    intros. apply reduction_eq_implies_eq.
    split. intro HE.
    - lete_eta_step_in HE.
      lete_eta_step.
      destruct (eqb_spec x1 x2); destruct (eqb_spec x1 x0); simpl in HE; subst...
      + exfalso...
      + exfalso...
      + lete_eta_step_in HE. rewrite letapp_reduction_spec in HE.
Admitted.

Lemma eta11: forall x e_x e x1 x2 x0 T1 T2,
    ~ x1 \FVtm e_x ->
    (tlete x e_x e) <-< tlete x1 (vlam x (T1 t--> T2) e) (tlete x2 e_x (tletapp x0 x1 x2 x0)).
Proof with eauto.
  intros. apply reduction_sub_implies_sub.
  intros v HE.
  lete_eta_step. lete_eta_step.
  lete_eta_step_in HE. destruct HE as (v_x & HE1 & HE2).
  exists v_x. split...
  - assert ([x1 := vlam x (T1 t--> T2) e] e_x = e_x) as He_x... rewrite He_x...
  -
Admitted.

Lemma eta2: forall e2 (c2: constant) x1 e1 x2 x,
    e2 -->* c2 ->
    (tlete x1 e1 (tlete x2 c2 (tletapp x x1 x2 x))) <-< (tlete x1 e1 (tlete x2 e2 (tletapp x x1 x2 x))).
Proof with eauto.
  intros. apply reduction_sub_implies_sub.
  intros v HE.
  lete_eta_step_in HE. lete_eta_step.
  destruct HE as (v_x & HE1 & HE2). exists v_x. split...
  lete_eta_step_in HE2. lete_eta_step...
  exists c2. split... assert ([x1 := v_x] e2 = e2)... rewrite H0...
Qed.

Lemma eta3: forall x1 a (e_a: tm) Ta e1 x2 e2 x,
    empty \N- e_a \Tin Ta ->
    (tlete a e_a (tlete x1 e1 (tlete x2 e2 (tletapp x x1 x2 x))))
      <=< (tlete x1 (tlete a e_a e1) (tlete x2 (tlete a e_a e2) (tletapp x x1 x2 x))).
Proof with eauto.
  intros. apply reduction_eq_implies_eq.
  split. intro HE.
  - lete_eta_step_in HE.
    destruct HE as (v_a & HE1 & HE).
    lete_eta_step_in HE. destruct HE as (v1 & HE2 & HE).
    lete_eta_step... exists v1... split... lete_eta_step... lete_eta_step...
    destruct (eqb_spec a x1); subst...
    + simpl in HE. lete_eta_step_in HE...
      destruct HE as (v2 & HEv2 & HE). exists v2. split... lete_eta_step. exists v1. split...
Admitted.

(* Lemma eta_subst_in_const: forall a (e_x: constant) (c: constant), c <-< (tlete a e_x c). *)
(* Proof with eauto. *)
(*   intros. apply reduction_sub_implies_sub. *)
(*   intros v HE. *)
(*   lete_eta_step... *)
(* Qed. *)

Lemma eta_subst_in_const: forall a (e_x: tm) (v_x: constant) (c: constant),
    c <-< (tlete a e_x c).
Proof with eauto.
Admitted.

Lemma eta_snd_let_reduce: forall x1 e1 x2 e2 x (c2: constant),
    e2 -->* c2 ->
    (tlete x1 e1 (tlete x2 c2 (tletapp x x1 x2 x))) <-< (tlete x1 e1 (tlete x2 e2 (tletapp x x1 x2 x))).
Proof with eauto.
  intros. apply reduction_sub_implies_sub.
  intros v HE.
  lete_eta_step_in HE. destruct HE as (v1 & H1 & HE)...
  lete_eta_step_in HE.
  lete_eta_step. exists v1. split... lete_eta_step. exists c2. split... rewrite not_free_rewrite...
Qed.

Lemma eta4: forall x1 (v1: value) x2 (c2: constant) x,
    x1 <> x2 ->
    tlete x1 v1 (tlete x2 c2 (tletapp x x1 x2 x)) <-< tletapp x v1 c2 x.
Proof with eauto.
  intros. apply reduction_sub_implies_sub.
  intros v HE.
  lete_eta_step_in HE. lete_eta_step_in HE.
  destruct (eqb_spec x1 x2); subst; simpl in HE; simpl...
  - exfalso...
  - rewrite eqb_refl in HE. rewrite not_free_rewrite_value in HE...
    destruct (eqb_spec x2 x); subst; simpl in HE; simpl...
    + destruct (eqb_spec x1 x); subst; simpl in HE; simpl...
    + destruct (eqb_spec x1 x); subst; simpl in HE; simpl...
      destruct (eqb_spec x2 x); subst; simpl in HE; simpl... exfalso...
      destruct (eqb_spec x2 x); subst; simpl in HE; simpl... exfalso...
Qed.

Lemma subst_var_value: forall x v1, [x := x ]v v1 = v1.
Admitted.

Lemma subst_var: forall x v1, [x := x ] v1 = v1.
Admitted.

Lemma eta5: forall x1 (v1: value) x2 (id2: string) x,
    x1 <> x2 -> ~ x1 \FVvalue id2 ->
    (tlete x1 v1 (tlete x2 id2 (tletapp x x1 x2 x))) <-< tletapp x v1 id2 x.
Proof with eauto.
  intros. apply reduction_sub_implies_sub.
  intros v HE.
  lete_eta_step_in HE. lete_eta_step_in HE.
  destruct (eqb_spec x1 id2); subst; simpl in HE; simpl...
  - exfalso... apply H0... constructor...
  - destruct (eqb_spec x1 x2); subst; simpl in HE; simpl... exfalso...
    destruct (eqb_spec x2 id2); subst; simpl in HE; simpl; try rewrite eqb_refl in HE; try rewrite eqb_neq in HE...
    + rewrite subst_var_value in HE.
      destruct (eqb_spec id2 x); subst; simpl in HE; simpl; try rewrite eqb_refl in HE; try rewrite eqb_neq in HE...
      destruct (eqb_spec x1 x); subst; simpl in HE; simpl; try rewrite eqb_refl in HE; try rewrite eqb_neq in HE...
      rewrite subst_var in HE.
      destruct (eqb_spec x1 x); subst; simpl in HE; simpl...
    + rewrite not_free_rewrite_value in HE...
      destruct (eqb_spec x2 x); subst; simpl in HE; simpl...
      destruct (eqb_spec x1 x); subst; simpl in HE; simpl...
      destruct (eqb_spec x1 x); subst; simpl in HE; simpl...
      destruct (eqb_spec x2 x); subst; simpl in HE; simpl... exfalso...
      destruct (eqb_spec x2 x); subst; simpl in HE; simpl... exfalso...
Qed.

Lemma eta6: forall x1 (v1 v2: value) x2 x,
    ~ x1 \FVvalue v2 -> x1 <> x2 ->
    tlete x1 v1 (tlete x2 v2 (tletapp x x1 x2 x)) <-< tletapp x v1 v2 x.
Proof with eauto.
  intros. apply reduction_sub_implies_sub.
  intros v HE.
  lete_eta_step_in HE... lete_eta_step_in HE...
  destruct (eqb_spec x1 x2); subst; simpl in HE; simpl; try rewrite eqb_refl in HE; try rewrite eqb_neq in HE... exfalso...
  - rewrite not_free_rewrite_value in HE...
    destruct (eqb_spec x2 x); subst; simpl in HE; simpl; try rewrite eqb_refl in HE; try rewrite eqb_neq in HE...
    + destruct (eqb_spec x1 x); subst; simpl in HE; simpl; try rewrite eqb_refl in HE; try rewrite eqb_neq in HE...
    + destruct (eqb_spec x2 x); subst; simpl in HE; simpl; try rewrite eqb_refl in HE; try rewrite eqb_neq in HE... exfalso...
      destruct (eqb_spec x1 x); subst; simpl in HE; simpl; try rewrite eqb_refl in HE; try rewrite eqb_neq in HE...
      destruct (eqb_spec x2 x); subst; simpl in HE; simpl; try rewrite eqb_refl in HE; try rewrite eqb_neq in HE... exfalso...
      destruct (eqb_spec x2 x); subst; simpl in HE; simpl; try rewrite eqb_refl in HE; try rewrite eqb_neq in HE... exfalso...
Qed.

Ltac super_eqb_spec x1 x HE :=
  destruct (eqb_spec x1 x); subst; simpl in HE; simpl; try rewrite eqb_refl in HE; try rewrite eqb_neq in HE; eauto.

Lemma eta7: forall x v1 v2 e, (tlete x (tletapp x v1 v2 x) e) <=< (tletapp x v1 v2 e).
Proof with eauto.
    intros. apply reduction_eq_implies_eq.
    split; intro HE.
    - lete_eta_step_in HE... destruct HE as (v_x & Hv_x & HH).
      rewrite letapp_reduction_spec in Hv_x. destruct Hv_x as (x_m & T_m & e_m & H1 & H2); subst...
      rewrite letapp_reduction_spec. exists x_m, T_m, e_m. split... lete_eta_step_in H2...
      destruct H2 as (v_m & Hv_m & Hvm2). lete_eta_step... exists v_x. split... eapply multi_trans...
    - rewrite letapp_reduction_spec in HE. destruct HE as (x_m & T_m & e_m & H1 & H2); subst...
      lete_eta_step_in H2... destruct H2 as (v_m & Hv_m & Hvm2).
      lete_eta_step... exists v_m. split... rewrite letapp_reduction_spec.
      exists x_m, T_m, e_m. split... lete_eta_step. exists v_m. split...
Qed.


Lemma eta8: forall x op v1 v2 e, (tlete x (tletbiop x op v1 v2 x) e) <=< (tletbiop x op v1 v2 e).
Admitted.

Lemma eta_op: forall x1 e1 x2 e2 x op (c1 c2: constant),
    e1 -->* c1 -> e2 -->* c2 ->
    (tletbiop x op c1 c2 x) <-< tlete x1 e1 (tlete x2 e2 (tletbiop x op x1 x2 x)).
Admitted.

Lemma eta9: forall x1 a (c_a: tm) e1 x2 e2 x op,
    (tlete x1 (tlete a c_a e1) (tlete x2 (tlete a c_a e2) (tletbiop x op x1 x2 x)))
      <-< (tlete a c_a (tlete x1 e1 (tlete x2 e2 (tletbiop x op x1 x2 x)))).
Admitted.

Lemma eta10: forall op x1 (id1 id2: cid) x2 x,
    x1 <> x2 -> ~ x1 \FVvalue id2 ->
    (tlete x1 id1 (tlete x2 id2 (tletbiop x op x1 x2 x))) <-< (tletbiop x op id1 id2 x).
Admitted.

Lemma eta_drop_lete_not_bot: forall x e_x0 e,
    (exists c0 : constant, e_x0 -->* c0) ->
    ~ x \FVtm e ->
    (tlete x e_x0 e) <=< e.
Proof with eauto.
    intros. apply reduction_eq_implies_eq.
    split; intro HE.
    - lete_eta_step_in HE... destruct HE as (v_x & Hv_x & HH). rewrite not_free_rewrite in HH...
    - lete_eta_step. destruct H as (v_x & Hv_x). exists v_x. split... rewrite not_free_rewrite...
Qed.


Lemma eta_lete_neq: forall x a c_x e_x e,
    a <> x ->
    tlete x (tlete a c_x e_x) (tlete a c_x e) <-< tlete a c_x (tlete x e_x e).
Proof with eauto.
  intros. apply reduction_sub_implies_sub.
  intros v HE.
  lete_eta_step_in HE...
  destruct HE as (v_x & Hv_x & HE). rewrite not_free_rewrite in HE...
  super_eqb_spec x a HE. exfalso...
  - lete_eta_step_in HE... destruct HE as (v' & Hv' & HE). lete_eta_step. exists v'. split...
    lete_eta_step_in Hv_x... rewrite not_free_rewrite...
    destruct Hv_x as (vx' & Hvx' & Hv_x). rewrite not_free_rewrite in Hv_x... lete_eta_step. exists v_x. split...
    super_eqb_spec a x HE. exfalso...
    rewrite not_free_rewrite... rewrite not_free_rewrite in HE... rewrite not_free_rewrite in HE...
    rewrite not_free_rewrite...
Qed.

Lemma eta_fix1: forall x1 f (T: base_ty) tau x e x2 (c_x': constant) x0,
    (tletapp x (vfix f (T t--> tau) x T e) c_x' x) <-< (tlete x1 (vfix f (T t--> tau) x T e) (tlete x2 c_x' (tletapp x0 x1 x2 x0))).
Proof with eauto.
  intros. apply reduction_sub_implies_sub.
  intros v HE.
Admitted.

Lemma eta_fix2: forall x1 f (T: base_ty) tau x e x2 (c_x: constant),
    tlete x1 (vlam f (T t--> tau) (tlete x c_x e))
          (tlete x2 (vfix f (T t--> tau) x T e) (tletapp x x1 x2 x)) <-<
          tletapp x (vfix f (T t--> tau) x T e) c_x x.
Admitted.

Lemma eta_fix3: forall x1 f (T: base_ty) tau x e x2 (c_x': constant),
    tlete x1 (vlam x T (vlam f (T t--> tau) e)) (tlete x2 c_x' (tletapp x x1 x2 x)) <-<
          vlam f (T t--> tau) (tlete x c_x' e).
Admitted.

Lemma eta_fix4: forall a f (Tx: base_ty) tau x e (c_x: constant),
    tlete a c_x (vlam x Tx (vlam f (Tx t--> tau) e)) <-< vlam x Tx (vlam f (Tx t--> tau) (tlete a c_x e)).
Proof with eauto.
  intros. apply reduction_sub_implies_sub.
  intros v HE.
  lete_eta_step_in HE...
  generalize dependent v. rewrite <- eta_in_lam.
  intros v HE.
  super_eqb_spec a x HE.
  - generalize dependent v. rewrite <- eta_in_lam. intros v HE.
    lete_eta_step. rewrite not_free_rewrite...
  - generalize dependent v. rewrite <- eta_in_lam. intros v HE.
    super_eqb_spec a f HE.
    lete_eta_step. rewrite not_free_rewrite...
Qed.

Lemma eta_fix5: forall a f (Tx: base_ty) tau x e (c_x: constant),
    vfix f (Tx t--> tau) x Tx (tlete a c_x e) <-< tlete a c_x (vfix f (Tx t--> tau) x Tx e).
Admitted.

Lemma eta_fix6: forall a f (Tx: base_ty) tau x e e_x,
    tlete a e_x (vlam x Tx (vlam f (Tx t--> tau) e)) <-< vlam x Tx (vlam f (Tx t--> tau) (tlete a e_x e)).
Proof with eauto.
  intros. apply reduction_sub_implies_sub.
  intros v HE.
  lete_eta_step_in HE... destruct HE as (v_x & Hv_x & HE).
  generalize dependent v. rewrite <- eta_in_lam. intros v HE.
  super_eqb_spec a x HE.
  - generalize dependent v. rewrite <- eta_in_lam. intros v HE.
    lete_eta_step. exists v_x. split... rewrite not_free_rewrite...
  - generalize dependent v. rewrite <- eta_in_lam. intros v HE.
    super_eqb_spec a f HE. lete_eta_step. exists v_x. split... rewrite not_free_rewrite...
    lete_eta_step. exists v_x. split...
Qed.

Lemma eta_fix7: forall a f (Tx: base_ty) tau x e (e_x: tm),
    vfix f (Tx t--> tau) x Tx (tlete a e_x e) <-< tlete a e_x (vfix f (Tx t--> tau) x Tx e).
Admitted.

Lemma eta_self1: forall x1 e e' x2 (c_x: tm) x,
    e <-< e' ->
    tlete x1 e (tlete x2 c_x (tletapp x x1 x2 x)) <-< tlete x1 e' (tlete x2 c_x (tletapp x x1 x2 x)).
Proof with eauto.
  intros. apply reduction_sub_implies_sub.
  intros v HE. destruct H as (HEE & HEE').
  lete_eta_step_in HE... destruct HE as (v' & Hv' & HE). apply HEE in Hv'.
  lete_eta_step. exists v'. split...
Qed.


Lemma eta_op_reducetion: forall op c_a c_b c_res x x0 x1 x2 x3 x4,
    eval_op op c_a c_b c_res ->
    tlete x0 (tlete x1 (vbiop op) (tlete x2 c_a (tletapp x x1 x2 x))) (tlete x3 c_b (tletapp x4 x0 x3 x4)) -->* c_res.
Admitted.

Lemma eta_self2: forall x c_x e e',
    e <-< e' ->
    tlete x c_x e <-< tlete x c_x e'.
Proof with eauto.
  intros. apply reduction_sub_implies_sub.
  intros v HE. destruct H as (HEE & HEE').
  lete_eta_step_in HE... destruct HE as (vc & Hv' & HE). rewrite not_free_rewrite in HE...
  lete_eta_step. exists vc. split... rewrite not_free_rewrite...
Qed.

Lemma eta_a3: forall id (c_x: constant) e1 e2,
  tlete id c_x (tmatchb c_x e1 e2) <-< tlete id c_x (tmatchb id e1 e2).
Proof with eauto.
  intros. apply reduction_sub_implies_sub.
  intros v HE.
  lete_eta_step_in HE... rewrite not_free_rewrite in HE... rewrite not_free_rewrite in HE...
  lete_eta_step. rewrite not_free_rewrite... rewrite not_free_rewrite...
Qed.

Lemma eta_a4: forall id a (c_x: tm),
    id <> a -> tlete a c_x id <-< id.
Proof with eauto.
  intros. apply reduction_sub_implies_sub.
  intros v HE.
  lete_eta_step_in HE... destruct HE as (v' & Hv' & HE).
  super_eqb_spec a id HE. exfalso...
Qed.

Lemma eta_match1: forall a c_x (c: constant) e1 e2,
    tlete a c_x (tmatchb c e1 e2) <-< tmatchb c (tlete a c_x e1) (tlete a c_x e2).
Proof with eauto.
  intros. apply reduction_sub_implies_sub.
  intros v HE.
  lete_eta_step_in HE... destruct HE as (v' & Hv' & HE). rewrite not_free_rewrite in HE... rewrite not_free_rewrite in HE...
Admitted.

Lemma eta_match2: forall (a id: string) (c_x: tm) e1 e2,
    id <> a ->
    tmatchb id (tlete a c_x e1) (tlete a c_x e2) <-< tlete a c_x (tmatchb id e1 e2).
Admitted.

Lemma eta_match3: forall (id: string) (c: constant) (e_x: tm) e1 e2,
    tlete id e_x (tmatchb c e1 e2) <-< tlete id e_x (tmatchb id e1 e2).
Proof with eauto.
  intros. apply reduction_sub_implies_sub.
  intros v HE.
  lete_eta_step_in HE... destruct HE as (v' & Hv' & HE).
  lete_eta_step. exists v'.
Admitted.


Lemma eta_a2: forall id c_x, tlete id c_x id <-< c_x.
Proof with eauto.
  intros. apply reduction_sub_implies_sub.
  intros v HE.
  lete_eta_step_in HE... destruct HE as (v' & Hv' & HE).
  eapply multi_trans...
Qed.
