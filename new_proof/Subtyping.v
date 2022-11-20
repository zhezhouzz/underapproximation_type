Set Warnings "-notation-overridden,-parsing".
From CT Require Import Maps.
From CT Require Import CoreLang.
From CT Require Import NormalTypeSystem.
From CT Require Import LinearContext.
From CT Require Import RfTypeDef.
From CT Require Import TypeClosed.
From CT Require Import Denotation.
From CT Require Import TermMeet.
From CT Require Import DenotationAux.
From CT Require Import WellFormed.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Lists.List.

Import CoreLang.
Import LinearContext.
Import TypeClosed.
Import Denotation.
Import TermMeet.
Import Ax.
Import NoDup.
Import DenotationAux.
Import WellFormed.
Import ListNotations.

Inductive tmR_sub_in_ctx_aux: state -> context -> overunderty -> overunderty -> Prop :=
| tmR_sub_in_ctx_aux_nil: forall (nst: state) (tau1 tau2: underty),
    st_type_closed_in_ctx (st\_ nst _/) nil tau1 -> st_type_closed_in_ctx (st\_ nst _/) nil tau2 ->
    u\_ tau1 _/ = u\_ tau2 _/ ->
    (forall e, tmR_in_ctx_aux nst [] tau1 e -> tmR_in_ctx_aux nst [] tau2 e) ->
    tmR_sub_in_ctx_aux nst [] tau1 tau2
| tmR_sub_in_ctx_aux_cons_overbase:
  forall (nst: state) (x: string) (T:base_ty) (phi: refinement) (Gamma: context) (tau1 tau2: underty),
    ctx_inv nst ((x, (Oty ({{v: T | phi}}))) :: Gamma) ->
    st_type_closed_in_ctx (st\_ nst _/) ((x, (Oty ({{v: T | phi}}))) :: Gamma) tau1 ->
    st_type_closed_in_ctx (st\_ nst _/) ((x, (Oty ({{v: T | phi}}))) :: Gamma) tau2 ->
    well_formed_type ({{v: T | phi}}) ->
    (forall (c_x: constant), tmR_aux nst ({{v: T | phi}}) c_x ->
                        tmR_sub_in_ctx_aux (update nst x c_x) Gamma tau1 tau2) ->
    u\_ tau1 _/ = u\_ tau2 _/ ->
    tmR_sub_in_ctx_aux nst ((x, (Oty ({{v: T | phi}}))) :: Gamma) tau1 tau2
| tmR_sub_in_ctx_aux_cons_under:
  forall (nst: state) (x: string) (T:base_ty) (phi: refinement) (Gamma: context) (tau1 tau2: underty),
    ctx_inv nst ((x, (Uty ([[v: T | phi]]))) :: Gamma) ->
    st_type_closed_in_ctx (st\_ nst _/) ((x, (Uty ([[v: T | phi]]))) :: Gamma) tau1 ->
    st_type_closed_in_ctx (st\_ nst _/) ((x, (Uty ([[v: T | phi]]))) :: Gamma) tau2  ->
    well_formed_type ([[v: T | phi]]) ->
    (exists e_x_hat, tmR_aux nst ([[v: T | phi]]) e_x_hat /\
                  (forall e_x, tmR_aux nst ([[v: T | phi]]) e_x ->
                          (forall (v_x_hat: constant), e_x -->* v_x_hat ->
                                                  tmR_sub_in_ctx_aux (update nst x v_x_hat) Gamma tau1 tau2
    ))) ->
    u\_ tau1 _/ = u\_ tau2 _/ ->
    tmR_sub_in_ctx_aux nst ((x, (Uty ([[v: T | phi]]))) :: Gamma) tau1 tau2
| tmR_sub_in_ctx_aux_cons_oarr: forall (nst: state) (x: string) a T phi (tau_b: underty) (Gamma: context) (tau1 tau2: underty),
    ctx_inv nst ((x, Uty (a o: ({{v: T | phi}}) o--> tau_b)) :: Gamma) ->
    st_type_closed_in_ctx (st\_ nst _/) ((x, Uty (a o: ({{v: T | phi}}) o--> tau_b)) :: Gamma) tau1 ->
    st_type_closed_in_ctx (st\_ nst _/) ((x, Uty (a o: ({{v: T | phi}}) o--> tau_b)) :: Gamma) tau2 ->
    well_formed_type (a o: {{v: T | phi}} o--> tau_b) ->
    tmR_sub_in_ctx_aux nst Gamma tau1 tau2 ->
    tmR_sub_in_ctx_aux nst ((x, Uty (a o: ({{v: T | phi}}) o--> tau_b)) :: Gamma) tau1 tau2
| tmR_sub_in_ctx_aux_cons_underarr: forall (nst: state) (x: string) (t1 t2: underty) (Gamma: context) (tau1 tau2: overunderty),
    ctx_inv nst ((x, Uty (t1 u--> t2)) :: Gamma) ->
    st_type_closed_in_ctx (st\_ nst _/) ((x, Uty (t1 u--> t2)) :: Gamma) tau1 ->
    st_type_closed_in_ctx (st\_ nst _/) ((x, Uty (t1 u--> t2)) :: Gamma) tau2 ->
    well_formed_type (t1 u--> t2) ->
    tmR_sub_in_ctx_aux nst Gamma tau1 tau2 ->
    tmR_sub_in_ctx_aux nst ((x, Uty (t1 u--> t2)) :: Gamma) tau1 tau2.

Global Hint Constructors tmR_sub_in_ctx_aux: core.

Lemma tmR_sub_in_ctx_aux_implies_ty_eq: forall st Gamma t1 t2,
    tmR_sub_in_ctx_aux st Gamma t1 t2 ->  ou\_ t1 _/ =  ou\_ t2 _/.
Proof with eauto.
  intros.
  induction H...
Qed.

Global Hint Resolve tmR_sub_in_ctx_aux_implies_ty_eq: core.
(* subtyping judgement *)
Inductive is_subtype : context -> overunderty -> overunderty -> Prop :=
| Sub_UBase: forall Gamma T phi1 phi2,
    tmR_sub_in_ctx_aux empty Gamma ([[v: T | phi1 ]]) ([[v: T | phi2 ]]) ->
    (* (forall e, tmR_in_ctx_all_st Gamma ([[v: T | phi1 ]]) e -> tmR_in_ctx_all_st Gamma ([[v: T | phi2 ]]) e) -> *)
    is_subtype Gamma ([[v: T | phi1 ]]) ([[v: T | phi2 ]])
| Sub_IndependArrow_IndependArrow: forall Gamma (tau11 tau21: underty) (tau12 tau22: underty),
    is_subtype Gamma tau21 tau11 ->
    is_subtype Gamma tau12 tau22 ->
    well_formed_type (tau11 u--> tau12) -> well_formed_type (tau21 u--> tau22) ->
    is_subtype Gamma (tau11 u--> tau12) (tau21 u--> tau22)
| Sub_DependArrow_DependArrow: forall Gamma T x phi11 phi21 (tau12 tau22: underty),
    (* We flip the overty to underty here *)
    is_subtype Gamma ({{v: T | phi21 }}) ({{v: T | phi11 }}) ->
    (* (forall (c: constant), tmR_in_ctx_aux empty Gamma ({{v: T | phi21 }}) c -> tmR_in_ctx_aux empty Gamma ({{v: T | phi11 }}) c) -> *)
    is_subtype (Gamma <l> x :l: ({{v: T | phi21 }})) tau12 tau22 ->
    well_formed_type (x o: {{v: T | phi11 }} o--> tau12) -> well_formed_type (x o: {{v: T | phi21 }} o--> tau22) ->
    is_subtype Gamma (x o: {{v: T | phi11 }} o--> tau12) (x o: {{v: T | phi21 }} o--> tau22).

Notation "Gamma '\C-' t1 '\<:' t2" := (is_subtype Gamma t1 t2) (at level 40).

Global Hint Constructors is_subtype: core.

Lemma subtyping_same_ty: forall Gamma tau1 tau2,
  Gamma \C- tau1 \<: tau2 -> ou\_ tau1 _/ =  ou\_ tau2 _/.
Proof with eauto.
  intros.
  induction H; simpl...
  - simpl in IHis_subtype1. simpl in IHis_subtype2. rewrite IHis_subtype1. rewrite IHis_subtype2...
  - simpl in IHis_subtype1. simpl in IHis_subtype2. rewrite IHis_subtype2...
Qed.

Lemma tmR_sub_implies_well_formed_type: forall st Gamma tau1 tau2,
  tmR_sub_in_ctx_aux st Gamma tau1 tau2 ->
  st_type_closed_in_ctx (st\_ st _/) Gamma tau1 /\ st_type_closed_in_ctx (st\_ st _/) Gamma tau2.
Proof with eauto.
  intros.
  destruct H; split...
Qed.

Global Hint Resolve tmR_sub_implies_well_formed_type: core.

(* Lemma st_type_closed_in_ctx_empty_implies_st_type_closed: forall st tau *)
(*  st_type_closed_in_ctx empty [] tau21 st_type_closed (st\_ st _/) tau21 *)

Lemma subtyping_implies_type_closed: forall Gamma tau1 tau2,
  Gamma \C- tau1 \<: tau2 -> well_formed_type tau1 /\ well_formed_type tau2.
Proof with eauto.
  intros.
  induction H; simpl...
Qed.

Lemma st_type_closed_in_ctx_construct_arrarr: forall st Gamma (t1 t2: underty),
    st_type_closed_in_ctx st Gamma t1 ->
    st_type_closed_in_ctx st Gamma t2 ->
    st_type_closed_in_ctx st Gamma (t1 u--> t2).
Proof with eauto.
  intros. rewrite st_type_close_arrarr_app_ctx_aux...
Qed.

Lemma subtyping_soundness_arrarr: forall Gamma st (tau11 tau12 tau21 tau22: underty),
    (* u\_ tau21 _/ = u\_ tau11 _/ -> u\_ tau12 _/ = u\_ tau22 _/ -> *)
    st_type_closed_in_ctx (st\_ st _/) Gamma (tau11 u--> tau12) ->
    st_type_closed_in_ctx (st\_ st _/) Gamma (tau21 u--> tau22) ->
    (tmR_sub_in_ctx_aux st Gamma tau21 tau11) ->
    (tmR_sub_in_ctx_aux st Gamma tau12 tau22) ->
    tmR_sub_in_ctx_aux st Gamma (tau11 u--> tau12) (tau21 u--> tau22).
Proof with eauto.
  intro Gamma.
  induction Gamma; simpl; intros st tau11 tau12 tau21 tau22 Hwf1 Hwf2
                                 (* HT1 HT2 *)
                                 Hsub1 Hsub2.
  - assert (st_type_closed_in_ctx (st\_ st _/) [] tau21 /\ st_type_closed_in_ctx (st\_ st _/) [] tau11) as (Hclosed21 & Hclosed11)...
    assert (st_type_closed_in_ctx (st\_ st _/) [] tau12 /\ st_type_closed_in_ctx (st\_ st _/) [] tau22) as (Hclosed12 & Hclosed22)...
    inversion Hsub1; subst... inversion Hsub2; subst... constructor...
    simpl. rewrite H4. rewrite <- H8...
    intros.
    rewrite tmR_in_ctx_to_under. rewrite tmR_in_ctx_to_under in H.
    setoid_rewrite tmR_in_ctx_to_under in H5. setoid_rewrite tmR_in_ctx_to_under in H9.
    inversion H; subst. destruct H7.
    constructor...
    { constructor... inversion Hclosed21... inversion Hclosed22... }
    split... simpl. rewrite H4... rewrite <- H8...
  - destruct a as (a & tau_a).
    inversion Hsub1; subst...
    + inversion Hsub2; subst... constructor...
      intros c_x Hc_xD. apply IHGamma...
      { rewrite erase_const.
        assert (empty \N- c_x \Tin T)... apply ty_implies_ty_of_const_eq in H. rewrite H.
        eapply st_type_closed_in_ctx_construct_arrarr... }
      { rewrite erase_const.
        assert (empty \N- c_x \Tin T)... apply ty_implies_ty_of_const_eq in H. rewrite H.
        eapply st_type_closed_in_ctx_construct_arrarr... }
      simpl... rewrite H10... rewrite H17...
    + inversion Hsub2; subst...
      destruct H9 as (e_x_hat1 & He_x_hat1D & HH1).
      destruct H16 as (e_x_hat2 & He_x_hat2D & HH2).
      destruct (meet_of_two_terms_exists e_x_hat1 e_x_hat2 T) as (e_x_hat & HT & HE); try tmR_implies_has_type...
      constructor...
      exists e_x_hat... split. eapply meet_of_two_terms_implies_denotation in HE...
      intros e_x He_xD v_x_hat HvE. apply IHGamma...
      { rewrite erase_const.
        assert (empty \N- e_x \Tin T). apply tmR_has_type in He_xD...
        assert (empty \N- v_x_hat \Vin T). eapply preservation_value...
        assert (empty \N- v_x_hat \Tin T)...
        apply ty_implies_ty_of_const_eq in H1. rewrite H1.
        eapply st_type_closed_in_ctx_construct_arrarr...
        apply st_type_closed_in_ctx_destruct_underbase_front in H7...
        apply st_type_closed_in_ctx_destruct_underbase_front in H13...
      }
      { rewrite erase_const.
        assert (empty \N- e_x \Tin T). apply tmR_has_type in He_xD...
        assert (empty \N- v_x_hat \Vin T). eapply preservation_value...
        assert (empty \N- v_x_hat \Tin T)...
        apply ty_implies_ty_of_const_eq in H1. rewrite H1.
        eapply st_type_closed_in_ctx_construct_arrarr...
        apply st_type_closed_in_ctx_destruct_underbase_front in H6...
        apply st_type_closed_in_ctx_destruct_underbase_front in H14...
      }
      simpl. rewrite H10... rewrite H17...
    + inversion Hsub1; subst...
      inversion Hsub2; subst...
      constructor... apply IHGamma...
      { eapply st_type_closed_in_ctx_construct_arrarr...
        apply st_type_closed_in_ctx_destruct_oarr_front in H15...
        apply st_type_closed_in_ctx_destruct_oarr_front in H19... }
       { eapply st_type_closed_in_ctx_construct_arrarr...
        apply st_type_closed_in_ctx_destruct_oarr_front in H6...
        apply st_type_closed_in_ctx_destruct_oarr_front in H20... }
    + inversion Hsub1; subst...
      inversion Hsub2; subst...
      constructor... apply IHGamma...
      { eapply st_type_closed_in_ctx_construct_arrarr...
        apply st_type_closed_in_ctx_destruct_arrar_front in H5...
        apply st_type_closed_in_ctx_destruct_arrar_front in H12... }
      { eapply st_type_closed_in_ctx_construct_arrarr...
        apply st_type_closed_in_ctx_destruct_arrar_front in H3...
        apply st_type_closed_in_ctx_destruct_arrar_front in H18... }
Qed.

Lemma subtype_over_sub_phi: forall st T (phi11 phi21: refinement),
    st_type_closed (st\_ st _/) ({{v:T | phi11}}) ->
    (forall c, phi21 st c -> phi11 st c) ->
    (forall c : constant, overbase_tmR_aux st ({{v:T | phi21}}) c -> overbase_tmR_aux st ({{v:T | phi11}}) c).
Proof with eauto.
  intros.
  inversion H1; subst.
  constructor...
Qed.

Lemma subtype_under_sub_phi: forall st T (phi11 phi21: refinement),
    st_type_closed (st\_ st _/) ([[v:T | phi21]]) ->
    (forall c, phi21 st c -> phi11 st c) ->
    (forall e : tm, under_tmR_aux st ([[v:T | phi11]]) e -> under_tmR_aux st ([[v:T | phi21]]) e).
Proof with eauto.
  intros.
  inversion H1; subst. destruct H3.
  constructor...
Qed.
(* Lemma subtype_over_under_flip: forall st T phi11 phi21, *)
(*   (forall e : tm, under_tmR_aux st ([[v:T | phi11]]) e -> under_tmR_aux st ([[v:T | phi21]]) e) -> *)
(*   (forall c : constant, overbase_tmR_aux st ({{v:T | phi21}}) c -> overbase_tmR_aux st ({{v:T | phi11}}) c). *)
(* i Proof with eauto. *)
(*   intros. *)
(*   inversion H0; subst. *)
(*   constructor... assert  *)

Lemma subtyping_soundness_oarr: forall Gamma st x T phi11 phi21 tau12 tau22,
    u\_ tau12 _/ = u\_ tau22 _/ ->
    well_formed_type (x o: {{v:T | phi11}} o--> tau12) -> well_formed_type (x o: {{v:T | phi21}} o--> tau22) ->
    (tmR_sub_in_ctx_aux st Gamma ({{v: T | phi21 }}) ({{v: T | phi11 }})) ->
    (tmR_sub_in_ctx_aux st (Gamma <l> x :l: ({{v:T | phi21}})) tau12 tau22) ->
    tmR_sub_in_ctx_aux st Gamma (x o: {{v:T | phi11}} o--> tau12) (x o: {{v:T | phi21}} o--> tau22).
Proof with eauto.
  intro Gamma.
  induction Gamma; simpl; intros st x T phi11 phi21 tau12 tau22 HeqT Hwf1 Hwf2 Hsub1 Hsub2...
  - inversion Hsub1; subst...
    (* inversion Hsub2; subst... *)
    (* constructor... *)
    (* {  *)
    (* admit. admit. simpl. rewrite HeqT... *)
    (* intros. *)
    (* rewrite tmR_in_ctx_to_under. rewrite tmR_in_ctx_to_under in H. inversion Hsub1; subst. *)
    (* inversion Hsub2; subst. *)
    (* constructor... split... simpl. rewrite <- HeqT... apply under_tmR_has_type in H... *)
    (* intros. inversion H1; subst. simpl. assert (tmR_sub_in_ctx_aux (x |-> c_x; st) [] tau12 tau22)... *)
    (* inversion H7; subst. setoid_rewrite tmR_in_ctx_to_under in H15. apply H15... *)
    (* inversion H; subst. destruct H17. apply H18... setoid_rewrite tmR_aux_to_over in Hsub1... *)
  - destruct a as (a & tau_a).
    inversion Hsub1; subst...
    inversion Hsub2; subst...
    constructor... apply st_type_closed_in_ctx_construct_oarr...
    apply (st_type_closed_in_ctx_last_samety ((a, Uty (t1 u--> t2)) :: Gamma) (st\_ st _/)  x T phi21 phi11)...
    constructor... apply st_type_closed_in_ctx_construct_oarr...
    (* + inversion Hsub2; subst... *)
    (*   constructor... *)
    (*   intros c_x Hc_xD. *)
    (*   assert (tmR_sub_in_ctx_aux (a |-> (tvalue c_x, T0); st) (Gamma ++ ((x, Oty ({{v:T | phi21}}))::nil)) tau12 tau22)... *)
    (*   inversion H;  *)

    (*   simpl... rewrite H19... *)
    (* + constructor... *)
    (*   destruct H10 as (e_x_hat1 & He_x_hat1D & HH1). *)
    (*   destruct H18 as (e_x_hat2 & He_x_hat2D & HH2). *)
    (*   destruct (meet_of_two_terms_exists e_x_hat1 e_x_hat2 T0) as (e_x_hat & HT & HE); try tmR_implies_has_type... *)
    (*   exists e_x_hat... split. eapply meet_of_two_terms_implies_denotation in HE... *)
    (*   apply meet_of_two_terms_term_order in HE... destruct HE as (HEE1 & HEE2). *)
    (*   apply IHGamma... simpl. rewrite H19... *)
    (* + inversion Hsub2; subst... *)
    (* + inversion Hsub2; subst... *)
Qed.

Lemma subtyping_soundness: forall Gamma tau1 tau2,
    Gamma \C- tau1 \<: tau2 -> tmR_sub_in_ctx_aux empty Gamma tau1 tau2.
Proof with eauto.
  intros.
  induction H...
  - apply subtyping_soundness_arrarr...
    apply tmR_sub_implies_well_formed_type in IHis_subtype1. destruct IHis_subtype1.
    apply tmR_sub_implies_well_formed_type in IHis_subtype2. destruct IHis_subtype2.
    apply st_type_closed_in_ctx_construct_arrarr...
    apply tmR_sub_implies_well_formed_type in IHis_subtype1. destruct IHis_subtype1.
    apply tmR_sub_implies_well_formed_type in IHis_subtype2. destruct IHis_subtype2.
    apply st_type_closed_in_ctx_construct_arrarr...
  - apply subtyping_soundness_oarr... apply subtyping_same_ty in H0...
Qed.

Lemma subtyping_denotation_implies_denotation_base: forall Gamma st tau1 tau2,
    tmR_sub_in_ctx_aux st Gamma tau1 tau2 ->
    (forall e, tmR_in_ctx_aux st Gamma tau1 e -> tmR_in_ctx_aux st Gamma tau2 e).
Proof with eauto.
  intro Gamma.
  induction Gamma; simpl; intros st tau1 tau2 HD e He1...
  - inversion HD; subst...
  - inversion HD; subst...
    + inversion He1; subst. constructor...
    + inversion He1; subst.
      destruct H6 as (e_x_hat1 & He_x_hat1D & HH1).
      destruct H14 as (e_x_hat2 & He_x_hat2D & HH2).
      destruct (meet_of_two_terms_exists e_x_hat1 e_x_hat2 T) as (e_x_hat & HT & HE); try tmR_implies_has_type...
      constructor...
      exists e_x_hat... split. eapply meet_of_two_terms_implies_denotation in HE...
      intros e_x He_xD v_x_hat HvE.
      (* apply meet_of_two_terms_term_order in HE... destruct HE as (HEE1 & HEE2). *)
      eapply IHGamma...
    + inversion He1; subst...
    + inversion He1; subst...
Qed.

Lemma is_subtype_spec: forall Gamma t1 t2,
    is_subtype Gamma t1 t2 ->
    (forall e, tmR_in_ctx_all_st Gamma t1 e -> tmR_in_ctx_all_st Gamma t2 e).
Proof with eauto.
  intros. apply subtyping_soundness in H.
  eapply subtyping_denotation_implies_denotation_base...
Qed.
