Set Warnings "-notation-overridden,-parsing".

From CT Require Import Maps.
From CT Require Import CoreLang.
From CT Require Import NormalTypeSystem.
From CT Require Import LinearContext.
From CT Require Import RfTypeDef.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Lists.List.
From Coq Require Import FunInd.
From Coq Require Import Recdef.

Import CoreLang.
Import NormalTypeSystem.
Import LinearContext.
Import Ax.
Import RfTypeDef.
Import ListNotations.


Local Hint Resolve term_has_no_free_var: core.

Lemma value_has_no_free_var: forall e x, ~ x \FVvalue e.
Proof with eauto.
  intros... intro H...
  assert (~ x \FVtm e). apply term_has_no_free_var. apply H0. simpl...
Qed.

Lemma closed_term_has_no_free_var: forall e x T, empty \N- e \Tin T -> ~ x \FVtm e.
Proof with eauto.
  intros...
Qed.

Lemma lete_preserve_not_free: forall e x a e_a, ~ x \FVtm e_a -> ~ x \FVtm e -> ~ x \FVtm tlete a e_a e.
Proof with eauto.
  intros.
  intro HH. simpl in HH. destruct HH...
  destruct H1...
Qed.
Local Hint Resolve value_has_no_free_var: core.

Global Hint Resolve lete_preserve_not_free: core.

Theorem preservation_value : forall t (v: value) T,
    empty \N- t \Tin T  -> t -->* v  ->
                  empty \N- v \Vin T.
Proof with eauto.
  intros. eapply multi_preservation in H...
  inversion H...
Qed.

Lemma empty_has_type_implies_closed_aux: forall e Gamma T, Gamma \N- e \Tin T -> (forall x, Gamma x = None -> ~ x \FVtm e).
Proof with eauto.
  intros e.
  apply (tm_mutual_rec
           (fun (v: value) => forall Gamma T, Gamma \N- v \Vin T -> (forall x, Gamma x = None -> ~ x \FVvalue v))
           (fun e => forall Gamma T, Gamma \N- e \Tin T -> (forall x, Gamma x = None -> ~ x \FVtm e))
        ); simpl; intros.
  - intro HH...
  - destruct c... intro HH. subst. inversion H; subst... rewrite H0 in H3. inversion H3.
  - inversion H0; subst... intro HH. destruct HH; subst.
    eapply H in H7... rewrite update_neq...
  - inversion H0; subst... intro HH. destruct HH as (HH1 & HH2 & HH3); subst.
    eapply H in H9... rewrite update_neq... rewrite update_neq...
  - intro HH...
  - inversion H0; subst...
  - inversion H1; subst... intro HH. destruct HH.
    + eapply H in H8...
    + destruct H3. eapply H0 in H9... rewrite update_neq...
  - inversion H2; subst... intro HH. destruct HH.
    + eapply H in H11...
    + destruct H4.
      eapply H0 in H12...
      destruct H4. eapply H1 in H14... rewrite update_neq...
  - inversion H2; subst... intro HH. destruct HH.
    + eapply H in H11...
    + destruct H4.
      eapply H0 in H10...
      destruct H4. eapply H1 in H12... rewrite update_neq...
  - inversion H2; subst... intro HH. destruct HH.
     + eapply H in H8...
    + destruct H4.
      eapply H0 in H10... eapply H1 in H11...
Qed.

Lemma empty_has_type_implies_closed: forall e T, empty \N- e \Tin T -> (forall x, ~ x \FVtm e).
Proof with eauto.
  intros.
  eapply empty_has_type_implies_closed_aux...
Qed.

Global Hint Resolve empty_has_type_implies_closed: core.


(* Term Fact *)

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

Ltac super_eqb_spec x1 x HE :=
  destruct (eqb_spec x1 x); subst; simpl in HE; simpl; try rewrite eqb_refl in HE; try rewrite eqb_neq in HE; eauto.

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

Lemma eta_lete_const_to_subst_in_lam: forall a T x e (c_x: constant),
    (vlam a T (subst x c_x e)) <=< (vlam a T (tlete x c_x e)).
Proof with eauto.
  intros. apply reduction_eq_implies_eq.
  split.
  - apply eta_in_lam. intros v' HE. lete_eta_step...
  - apply eta_in_lam. intros v' HE. lete_eta_step_in HE...
Qed.

Ltac handle_lam v HE := generalize dependent v; rewrite <- eta_in_lam; intros v HE.

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
    split; intro HE.
    - lete_eta_step_in HE. lete_eta_step_in HE.
      super_eqb_spec x1 x2 HE. exfalso...
      rewrite letapp_reduction_spec in HE.
      destruct HE as (x_m & T_m & e_m & Happ1 & HE); inversion Happ1; subst; clear Happ1.
      lete_eta_step_in HE. destruct HE as (v_x & Hv_x & HE).
      lete_eta_step.
      super_eqb_spec x2 x0 HE.
      + super_eqb_spec x1 x0 HE. exfalso... rewrite value_only_reduce_to_itself in HE; subst.
        super_eqb_spec x0 x_m Hv_x. rewrite not_free_rewrite in Hv_x... rewrite not_free_rewrite in Hv_x...
        rewrite not_free_rewrite...
      + super_eqb_spec x1 x0 HE.
        super_eqb_spec x2 x0 HE. exfalso... rewrite value_only_reduce_to_itself in HE; subst.
        super_eqb_spec x2 x_m Hv_x. rewrite not_free_rewrite in Hv_x... rewrite not_free_rewrite in Hv_x...
        rewrite not_free_rewrite...
        super_eqb_spec x2 x_m HE.
        super_eqb_spec x_m x0 HE. exfalso... rewrite value_only_reduce_to_itself in HE; subst...
        super_eqb_spec x2 x0 HE. exfalso... rewrite value_only_reduce_to_itself in HE; subst...
        rewrite not_free_rewrite in Hv_x... rewrite not_free_rewrite in Hv_x...
        rewrite not_free_rewrite...
   - lete_eta_step_in HE.
     lete_eta_step. lete_eta_step. rewrite not_free_rewrite in HE... rewrite not_free_rewrite...
     super_eqb_spec x1 x2 HE. exfalso...
     rewrite letapp_reduction_spec. exists x, T, e. split...
     lete_eta_step. exists v. split. rewrite not_free_rewrite...
     super_eqb_spec x1 x0 HE.
     super_eqb_spec x0 x0 HE... exfalso...
     super_eqb_spec x0 x0 HE... exfalso...
Qed.

Lemma eta11: forall x e_x e x1 x2 x0 T1 T2,
    ~ x1 \FVtm e_x -> x1 <> x2 ->
    (tlete x e_x e) <-< tlete x1 (vlam x (T1 t--> T2) e) (tlete x2 e_x (tletapp x0 x1 x2 x0)).
Proof with eauto.
  intros. apply reduction_sub_implies_sub.
  intros v HE.
  lete_eta_step_in HE. destruct HE as (v_x & HE1 & HE2).
  lete_eta_step. lete_eta_step.
  exists v_x. split... rewrite not_free_rewrite...
  rewrite not_free_rewrite... rewrite not_free_rewrite in HE2...
  super_eqb_spec x1 x2 HE2. exfalso...
  rewrite letapp_reduction_spec. exists x, (T1 t--> T2), e. split...
  lete_eta_step. exists v. split. rewrite not_free_rewrite...
  super_eqb_spec x1 x0 HE2; rewrite eqb_refl...
Qed.

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

Lemma subst_var: forall x v1, [x := x ] v1 = v1.
Proof with eauto.
  intros x v.
  apply (tm_mutual_rec
           (fun (v: value) => [x := x ]v v = v)
           (fun v => [x := x ] v = v)
        ); simpl; intros...
  - destruct c... destruct (eqb_spec x s); subst...
  - destruct (eqb_spec x s); subst... rewrite H...
  - destruct (eqb_spec x s); subst... destruct (eqb_spec x s0); subst... rewrite H...
  - rewrite H...
  - rewrite H.
    destruct (eqb_spec x s); subst... rewrite H0...
  - rewrite H. rewrite H0.
    destruct (eqb_spec x s); subst... rewrite H1...
  - rewrite H. rewrite H0.
    destruct (eqb_spec x s); subst... rewrite H1...
  - rewrite H. rewrite H0. rewrite H1...
Qed.

Lemma subst_var_value: forall x v1, [x := x ]v v1 = v1.
Proof with eauto.
  intros x v.
  apply (value_mutual_rec
           (fun (v: value) => [x := x ]v v = v)
           (fun v => [x := x ] v = v)
        ); simpl; intros...
  - destruct c... destruct (eqb_spec x s); subst...
  - destruct (eqb_spec x s); subst... rewrite H...
  - destruct (eqb_spec x s); subst... destruct (eqb_spec x s0); subst... rewrite H...
  - rewrite H...
  - rewrite H.
    destruct (eqb_spec x s); subst... rewrite H0...
  - rewrite H. rewrite H0.
    destruct (eqb_spec x s); subst... rewrite H1...
  - rewrite H. rewrite H0.
    destruct (eqb_spec x s); subst... rewrite H1...
  - rewrite H. rewrite H0. rewrite H1...
Qed.

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

Ltac handle_fix v HE := generalize dependent v; rewrite <- eta_in_fix; intros v HE.

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
  rewrite letapp_reduction_spec in HE.
  destruct HE as (x_m & T_m & e_m & H1 & HE); inversion H1; subst...
Qed.


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
Proof with eauto.
  intros. apply reduction_sub_implies_sub.
  intros v HE.
  lete_eta_step.
  handle_fix v HE.
  super_eqb_spec a f HE. lete_eta_step_in HE. rewrite not_free_rewrite in HE...
  super_eqb_spec a x HE. lete_eta_step_in HE. rewrite not_free_rewrite in HE...
  lete_eta_step_in HE. rewrite not_free_rewrite in HE... rewrite not_free_rewrite...
Qed.

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

Lemma eta_self1: forall x1 e e' x2 (c_x: tm) x,
    e <-< e' ->
    tlete x1 e (tlete x2 c_x (tletapp x x1 x2 x)) <-< tlete x1 e' (tlete x2 c_x (tletapp x x1 x2 x)).
Proof with eauto.
  intros. apply reduction_sub_implies_sub.
  intros v HE. destruct H as (HEE & HEE').
  lete_eta_step_in HE... destruct HE as (v' & Hv' & HE). apply HEE in Hv'.
  lete_eta_step. exists v'. split...
Qed.


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

Ltac handle_match v HE := generalize dependent v; rewrite <- tmatch_reduction_spec; split; intros v HE.

Lemma eta_match1: forall a c_x (c: constant) e1 e2,
    tlete a c_x (tmatchb c e1 e2) <-< tmatchb c (tlete a c_x e1) (tlete a c_x e2).
Proof with eauto.
  intros. apply reduction_sub_implies_sub.
  intros v HE.
  lete_eta_step_in HE... destruct HE as (v' & Hv' & HE). rewrite not_free_rewrite in HE... rewrite not_free_rewrite in HE...
  handle_match v HE.
  - lete_eta_step. exists v'. split... rewrite not_free_rewrite...
  - lete_eta_step. exists v'. split... rewrite not_free_rewrite...
Qed.

Lemma eta_a2: forall id c_x, tlete id c_x id <-< c_x.
Proof with eauto.
  intros. apply reduction_sub_implies_sub.
  intros v HE.
  lete_eta_step_in HE... destruct HE as (v' & Hv' & HE).
  eapply multi_trans...
Qed.


Lemma meet_of_two_terms_term_order: forall e1 e2 e3 T,
    empty \N- e1 \Tin T -> empty \N- e2 \Tin T -> empty \N- e3 \Tin T ->
    (forall c, e3 -->* c <-> e1 -->* c /\ e2 -->* c) -> (e3 <-< e1) /\ (e3 <-< e2).
Proof with eauto.
  intros.
  split.
  - split... intros. rewrite H2 in H3... destruct H3...
    intros. apply weakening_empty with (Gamma := Gamma) in H1.
    assert (T0 = T). eapply ty_unique... subst... eapply  weakening_empty...
  - split... intros. rewrite H2 in H3... destruct H3...
    intros. apply weakening_empty with (Gamma := Gamma) in H1.
    assert (T0 = T). eapply ty_unique... subst... eapply  weakening_empty...
Qed.

Lemma meet_of_three_terms_exists: forall e1 e2 e3 T,
    empty \N- e1 \Tin T -> empty \N- e2 \Tin T -> empty \N- e3 \Tin T ->
    (exists e, (empty \N- e \Tin T) /\
            (forall c, e -->* c <-> e1 -->* c /\ e2 -->* c /\ e3 -->* c)).
Proof with eauto.
  intros.
  destruct (meet_of_two_terms_exists e1 e2 T) as (e4 & HTe4 & HEe4)...
  destruct (meet_of_two_terms_exists e4 e3 T) as (e5 & HTe5 & HEe5)...
  exists e5. split...
  intro c. split... intros... apply HEe5 in H2. destruct H2. apply HEe4 in H2. destruct H2...
  intros. rewrite HEe5. destruct H2 as (H21 & H22 & H23)... split... apply HEe4...
Qed.

Lemma meet_of_three_terms_term_order: forall e1 e2 e3 e T,
    empty \N- e1 \Tin T -> empty \N- e2 \Tin T -> empty \N- e3 \Tin T -> empty \N- e \Tin T ->
    (forall c, e -->* c <-> e1 -->* c /\ e2 -->* c /\ e3 -->* c) -> (e <-< e1) /\ (e <-< e2) /\ (e <-< e3).
Proof with eauto.
  intros.
  split.
  - split... intros. rewrite H3 in H4... destruct H4...
    intros. apply weakening_empty with (Gamma := Gamma) in H2.
    assert (T0 = T). eapply ty_unique... subst... eapply  weakening_empty...
  - split...
    split...
    intros. rewrite H3 in H4... destruct H4... destruct H5...
    intros. assert (Gamma \N- e \Tin T). eapply weakening_empty in H2...
    assert (T = T0). eapply ty_unique... subst... eapply  weakening_empty...
    split...
    intros. rewrite H3 in H4... destruct H4... destruct H5...
    intros. assert (Gamma \N- e \Tin T). eapply weakening_empty in H2...
    assert (T = T0). eapply ty_unique... subst... eapply  weakening_empty...
Qed.


Lemma meet_of_four_terms_exists: forall e1 e2 e3 e4 T,
    empty \N- e1 \Tin T -> empty \N- e2 \Tin T -> empty \N- e3 \Tin T -> empty \N- e4 \Tin T ->
    (exists e, (empty \N- e \Tin T) /\
            (forall c, e -->* c <-> e1 -->* c /\ e2 -->* c /\ e3 -->* c /\ e4 -->* c)).
Proof with eauto.
  intros.
  destruct (meet_of_three_terms_exists e1 e2 e3 T) as (e123 & HTe123 & HEe123)...
  destruct (meet_of_two_terms_exists e123 e4 T) as (e & HTe & HEe)...
  exists e. split...
  intro c. split... intros... apply HEe in H3. destruct H3. apply HEe123 in H3... destruct H3 as (Ha & Hb & Hc)...
  intros. rewrite HEe. destruct H3 as (H21 & H22 & H23 & H4)... split... apply HEe123...
Qed.

Lemma meet_of_four_terms_term_order: forall e1 e2 e3 e4 e T,
    empty \N- e1 \Tin T -> empty \N- e2 \Tin T -> empty \N- e3 \Tin T -> empty \N- e4 \Tin T -> empty \N- e \Tin T ->
    (forall c, e -->* c <-> e1 -->* c /\ e2 -->* c /\ e3 -->* c /\ e4 -->* c) -> (e <-< e1) /\ (e <-< e2) /\ (e <-< e3) /\ (e <-< e4).
Proof with eauto.
  intros.
  split.
  - split... intros. rewrite H4 in H5... destruct H5...
    intros. apply weakening_empty with (Gamma := Gamma) in H3.
    assert (T0 = T). eapply ty_unique... subst... eapply  weakening_empty...
  - split...
    split...
    intros. rewrite H4 in H5... destruct H5... destruct H6...
    intros. apply weakening_empty with (Gamma := Gamma) in H3.
    assert (T0 = T). eapply ty_unique... subst... eapply  weakening_empty...
    split...
    split. intros. rewrite H4 in H5... destruct H5... destruct H6... destruct H7...
    intros. apply weakening_empty with (Gamma := Gamma) in H3.
    assert (T0 = T). eapply ty_unique... subst... eapply  weakening_empty...
    split. intros. rewrite H4 in H5... destruct H5... destruct H6... destruct H7...
    intros. apply weakening_empty with (Gamma := Gamma) in H3.
    assert (T0 = T). eapply ty_unique... subst... eapply  weakening_empty...
Qed.
