Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From PLF Require Import CoreLangSimp.
From PLF Require Import NormalTypeSystemSimp.
From PLF Require Import LinearContext.
From PLF Require Import RfTypeDef.
From PLF Require Import TypeClosedSimp.
From PLF Require Import DenotationSimp.
From PLF Require Import TermMeet.
From PLF Require Import DenotationAux.
From PLF Require Import WellFormedSimp.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Lists.List.

Import CoreLangSimp.
Import LinearContext.
Import TermOrdering.
Import Nstate.
Import TypeClosedSimp.
Import DenotationSimp.
Import TermMeet.
Import DenotationAux.
Import WellFormedSimp.
Import ListNotations.

Inductive tmR_sub_in_ctx_aux: nstate -> context -> overunderty -> overunderty -> Prop :=
| tmR_sub_in_ctx_aux_nil: forall (nst: nstate) (tau1 tau2: underty),
    well_formed_type tau1 -> well_formed_type tau2 ->
    u\_ tau1 _/ = u\_ tau2 _/ ->
    (forall e, tmR_in_ctx_aux nst [] tau1 e -> tmR_in_ctx_aux nst [] tau2 e) ->
    tmR_sub_in_ctx_aux nst [] tau1 tau2
| tmR_sub_in_ctx_aux_cons_overbase:
  forall (nst: nstate) (x: string) (T:base_ty) (phi: refinement) (Gamma: context) (tau1 tau2: underty),
    nst x = None ->
    l_find_right_most Gamma x = None ->
    well_formed_type tau1 -> well_formed_type tau2 ->
    well_formed_type ({{v: T | phi}}) ->
    (forall (c_x: constant), tmR_aux nst ({{v: T | phi}}) c_x ->
                        tmR_sub_in_ctx_aux (update nst x (tvalue c_x, T)) Gamma tau1 tau2) ->
    u\_ tau1 _/ = u\_ tau2 _/ ->
    tmR_sub_in_ctx_aux nst ((x, (Oty ({{v: T | phi}}))) :: Gamma) tau1 tau2
| tmR_sub_in_ctx_aux_cons_under:
  forall (nst: nstate) (x: string) (T:base_ty) (phi: refinement) (Gamma: context) (tau1 tau2: underty),
    nst x = None ->
    l_find_right_most Gamma x = None ->
    well_formed_type tau1 -> well_formed_type tau2 ->
    well_formed_type ([[v: T | phi]]) ->
    (exists e_x_hat, tmR_aux nst ([[v: T | phi]]) e_x_hat /\
                  tmR_sub_in_ctx_aux (update nst x (e_x_hat, T)) Gamma tau1 tau2) ->
    u\_ tau1 _/ = u\_ tau2 _/ ->
    tmR_sub_in_ctx_aux nst ((x, (Uty ([[v: T | phi]]))) :: Gamma) tau1 tau2
| tmR_sub_in_ctx_aux_cons_oarr: forall (nst: nstate) (x: string) a T phi (tau_b: underty) (Gamma: context) (tau1 tau2: underty),
    nst x = None ->
    l_find_right_most Gamma x = None ->
    well_formed_type tau1 -> well_formed_type tau2 ->
    well_formed_type (a o: {{v: T | phi}} o--> tau_b) ->
    tmR_sub_in_ctx_aux nst Gamma tau1 tau2 ->
    tmR_sub_in_ctx_aux nst ((x, Uty (a o: ({{v: T | phi}}) o--> tau_b)) :: Gamma) tau1 tau2
| tmR_sub_in_ctx_aux_cons_underarr: forall (nst: nstate) (x: string) (t1 t2: underty) (Gamma: context) (tau1 tau2: overunderty),
    nst x = None ->
    l_find_right_most Gamma x = None ->
    well_formed_type tau1 -> well_formed_type tau2 ->
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

Lemma subtyping_implies_well_formed_type: forall Gamma tau1 tau2,
  Gamma \C- tau1 \<: tau2 -> well_formed_type tau1 /\ well_formed_type tau2.
Proof with eauto.
  intros.
  induction H; simpl...
Qed.

Lemma term_order_implies_subtyping_order: forall e_x_hat e_x_hat1 T a st Gamma tau12 tau22,
    e_x_hat <-< e_x_hat1 ->
    tmR_sub_in_ctx_aux (a |-> (e_x_hat1, T); st) Gamma tau12 tau22 ->
    tmR_sub_in_ctx_aux (a |-> (e_x_hat, T); st) Gamma tau12 tau22.
Admitted.

Global Hint Resolve term_order_implies_subtyping_order: core.

Lemma subtyping_soundness_arrarr: forall Gamma st tau11 tau12 tau21 tau22,
    u\_ tau21 _/ = u\_ tau11 _/ -> u\_ tau12 _/ = u\_ tau22 _/ ->
    well_formed_type (tau11 u--> tau12) -> well_formed_type (tau21 u--> tau22) ->
    (tmR_sub_in_ctx_aux st Gamma tau21 tau11) ->
    (tmR_sub_in_ctx_aux st Gamma tau12 tau22) ->
    tmR_sub_in_ctx_aux st Gamma (tau11 u--> tau12) (tau21 u--> tau22).
Proof with eauto.
  intro Gamma.
  induction Gamma; simpl; intros st tau11 tau12 tau21 tau22 HT1 HT2 Hwf1 Hwf2 Hsub1 Hsub2.
  - inversion Hsub1; subst... inversion Hsub2; subst... constructor... simpl. rewrite H4. rewrite <- H8...
    intros.
    rewrite tmR_in_ctx_to_under. rewrite tmR_in_ctx_to_under in H.
    setoid_rewrite tmR_in_ctx_to_under in H5. setoid_rewrite tmR_in_ctx_to_under in H9.
    inversion H; subst. destruct H7.
    constructor... split... simpl. rewrite H4... rewrite <- H8...
  - destruct a as (a & tau_a).
    inversion Hsub1; subst...
    + inversion Hsub2; subst... constructor... simpl... rewrite H11... rewrite H19...
    + inversion Hsub2; subst...
      destruct H10 as (e_x_hat1 & He_x_hat1D & HH1).
      destruct H18 as (e_x_hat2 & He_x_hat2D & HH2).
      destruct (meet_of_two_terms_exists e_x_hat1 e_x_hat2 T) as (e_x_hat & HT & HE); try tmR_implies_has_type...
      constructor...
      exists e_x_hat... split. eapply meet_of_two_terms_implies_denotation in HE...
      apply meet_of_two_terms_term_order in HE... destruct HE as (HEE1 & HEE2).
      apply IHGamma... simpl. rewrite H11... rewrite H19...
    + inversion Hsub2; subst...
    + inversion Hsub2; subst...
Qed.

Lemma subtype_over_sub_phi: forall st T phi11 phi21,
  (forall c, phi_sat_nst phi21 st c -> phi_sat_nst phi11 st c) ->
  (forall c : constant, overbase_tmR_aux st ({{v:T | phi21}}) c -> overbase_tmR_aux st ({{v:T | phi11}}) c).
Proof with eauto.
  intros.
  inversion H0; subst.
  constructor...
Qed.

Lemma subtype_under_sub_phi: forall st T phi11 phi21,
  (forall c, phi_sat_nst phi21 st c -> phi_sat_nst phi11 st c) ->
  (forall e : tm, under_tmR_aux st ([[v:T | phi11]]) e -> under_tmR_aux st ([[v:T | phi21]]) e).
Proof with eauto.
  intros.
  inversion H0; subst. destruct H2.
  constructor...
Qed.

(* Lemma subtype_over_under_flip: forall st T phi11 phi21, *)
(*   (forall e : tm, under_tmR_aux st ([[v:T | phi11]]) e -> under_tmR_aux st ([[v:T | phi21]]) e) -> *)
(*   (forall c : constant, overbase_tmR_aux st ({{v:T | phi21}}) c -> overbase_tmR_aux st ({{v:T | phi11}}) c). *)
(* Proof with eauto. *)
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
  - constructor... simpl. rewrite HeqT...
    intros.
    rewrite tmR_in_ctx_to_under. rewrite tmR_in_ctx_to_under in H. inversion Hsub1; subst.
    (* inversion Hsub2; subst. *)
    (* constructor... split... simpl. rewrite <- HeqT... apply under_tmR_has_type in H... *)
    (* intros. inversion H1; subst. simpl. assert (tmR_sub_in_ctx_aux (x |-> (tvalue c_x, T); st) [] tau12 tau22)... *)
    (* inversion H7; subst. setoid_rewrite tmR_in_ctx_to_under in H15. apply H15... *)
    (* inversion H; subst. destruct H17. apply H18... setoid_rewrite tmR_aux_to_over in Hsub1... *)
  - destruct a as (a & tau_a).
    inversion Hsub1; subst...
    + inversion Hsub2; subst...
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
  - apply subtyping_soundness_arrarr... apply subtyping_same_ty in H... apply subtyping_same_ty in H0...
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
      destruct H7 as (e_x_hat1 & He_x_hat1D & HH1).
      destruct H17 as (e_x_hat2 & He_x_hat2D & HH2).
      destruct (meet_of_two_terms_exists e_x_hat1 e_x_hat2 T) as (e_x_hat & HT & HE); try tmR_implies_has_type...
      constructor...
      exists e_x_hat... split. eapply meet_of_two_terms_implies_denotation in HE...
      intros e_x He_xD.
      apply meet_of_two_terms_term_order in HE... destruct HE as (HEE1 & HEE2).
      eapply IHGamma...
    + inversion He1; subst...
    + inversion He1; subst...
Qed.

(* Lemma subtyping_denotation_implies_denotation_base_rev: forall Gamma st T phi1 phi2 , *)
(*     (forall e, tmR_in_ctx_aux st Gamma ([[v: T | phi1]]) e -> tmR_in_ctx_aux st Gamma ([[v: T | phi2]]) e) -> *)
(*     tmR_sub_in_ctx_aux st Gamma ([[v: T | phi1]]) ([[v: T | phi2]]). *)
(* Proof with eauto. *)
(*   intro Gamma. *)
(*   induction Gamma; simpl; intros st T phi1 phi2 HD... *)
(*   - destruct a as (a & tau_a). *)
(*     destruct tau_a. *)
(*     admit. destruct o. *)
(*     constructor... admit. admit. intros c_x Hc_xD. *)
(*     admit. *)
(*   -  *)
(*   - *)
(*   - inversion HD; subst... *)
(*   - inversion HD; subst... *)
(*     + inversion He1; subst. constructor... *)
(*     + inversion He1; subst. *)
(*       destruct H9 as (e_x_hat1 & He_x_hat1D & HH1). *)
(*       destruct H16 as (e_x_hat2 & He_x_hat2D & HH2). *)
(*       destruct (meet_of_two_terms_exists e_x_hat1 e_x_hat2 T) as (e_x_hat & HT & HE); try tmR_implies_has_type... *)
(*       constructor... *)
(*       exists e_x_hat... split. eapply meet_of_two_terms_implies_denotation in HE... *)
(*       intros e_x He_xD. *)
(*       apply meet_of_two_terms_term_order in HE... destruct HE as (HEE1 & HEE2). *)
(*       eapply IHGamma... *)
(*     + inversion He1; subst... *)
(*     + inversion He1; subst... *)
(* Qed *)

Lemma is_subtype_spec: forall Gamma t1 t2,
    is_subtype Gamma t1 t2 ->
    (forall e, tmR_in_ctx_all_st Gamma t1 e -> tmR_in_ctx_all_st Gamma t2 e).
Proof with eauto.
  intros. apply subtyping_soundness in H.
  eapply subtyping_denotation_implies_denotation_base...
Qed.

(* Lemma tmR_empty_under: forall st (tau11: underty) e, *)
(*   tmR_in_ctx_aux st [] tau11 e <-> under_tmR_aux st tau11 e. *)
(* Admitted. *)

(* Lemma tmR_empty_implies_forall_state: forall Gamma tau e, *)
(*     (forall st, tmR_in_ctx_aux st Gamma tau e) <-> tmR_in_ctx_aux empty Gamma tau e. *)
(* Admitted. *)

(* Lemma subtyping_arrarr: forall Gamma st (t1 t2: underty) e, *)
(*     ctx_inv st Gamma -> *)
(*     (forall e_x, tmR_in_ctx_aux st Gamma t1 e_x -> *)
(*             (forall e3, is_application e e_x e3 -> tmR_in_ctx_aux st Gamma t2 e3)) -> *)
(*     (tmR_in_ctx_aux st Gamma (t1 u--> t2) e). *)
(* Proof with eauto. *)
(*   intro Gamma. *)
(*   induction Gamma; simpl; intros st t1 t2 e Hinv He... *)
(*   - admit. *)
(*   - destruct a as (a, tau_a). assert (well_formed_type tau_a). admit. *)
(*     destruct tau_a. admit. destruct o. *)
(*     constructor... admit. admit. *)
(*     intros c_x Hc_xD. *)
(*     eapply IHGamma...  intros. inversion H1; subst... clear H1. *)
(*     assert (tmR_in_ctx_aux (a |-> (c_x, b); st) Gamma (t1 u--> t2) (tlete a c_x e)). *)
(*     apply IHGamma... intros e_x He_xD e3 Happ. *)

(*     assert (tmR_in_ctx_aux st ((a, Oty ({{v:b | r}})) :: Gamma) t2 e3). eapply He with (e_x:= (tlete a c_x e_x))... *)


(*     destruct Hinv. ctx_inv. destruct a as (a, tau_a). *)


(*     constructor... admit. *)
