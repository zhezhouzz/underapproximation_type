(** * MoreStlc: More on the Simply Typed Lambda-Calculus *)

Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From PLF Require Import CoreLangSimp.
From PLF Require Import NormalTypeSystemSimp.
From PLF Require Import LinearContext.
From PLF Require Import RfTypeDef.
From PLF Require Import TypeClosedSimp.
From PLF Require Import TermOrdering.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Lists.List.
From Coq Require Import FunInd.
From Coq Require Import Recdef.

Import CoreLangSimp.
Import NormalTypeSystemSimp.
Import LinearContext.
Import RfTypeDef.
Import TypeClosedSimp.
Import NoDup.
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
Admitted.

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

Lemma mk_op_has_denotation: forall st op a b, tmR_aux st (mk_op op a b) (vbiop op).
Admitted.

Global Hint Resolve mk_op_has_denotation: core.

Lemma tmR_nst_no_free_implies_eq: forall st x e_x_hat (tau: underty),
    ~ appear_free_in_underty x tau -> (forall e, tmR_aux (x |-> e_x_hat; st) tau e <-> tmR_aux st tau e).
Admitted.

(* Definition E_Hat_Oracle: state -> base_ty -> refinement -> tm. *)
(* Admitted. *)

(* Definition E_Hat_Oracle_spec st T phi e := (e = E_Hat_Oracle st T phi). *)

(* The denotation does not guarantee the well-formedness (inv_ctx). *)
(* The denotation implies no dup. *)
Inductive tmR_in_ctx_aux: state -> context -> overunderty -> tm -> Prop :=
| tmR_in_ctx_aux_nil: forall (nst: state) (tau: overunderty) e,
    tmR_aux nst tau e -> tmR_in_ctx_aux nst [] tau e
| tmR_in_ctx_aux_cons_overbase:
  forall (nst: state) (x: string) (T:base_ty) (phi: refinement) (Gamma: context) (tau: overunderty) e,
    nst x = None ->
    l_find_right_most Gamma x = None ->
    well_formed_type tau ->
    well_formed_type ({{v: T | phi}}) ->
    (forall (c_x: constant), tmR_aux nst ({{v: T | phi}}) c_x ->
                        tmR_in_ctx_aux (update nst x c_x) Gamma tau (tlete x (vconst c_x) e)) ->
    tmR_in_ctx_aux nst ((x, (Oty ({{v: T | phi}}))) :: Gamma) tau e
| tmR_in_ctx_aux_cons_under:
  forall (nst: state) (x: string) (T:base_ty) (phi: refinement) (Gamma: context) (tau: overunderty) e,
    nst x = None ->
    l_find_right_most Gamma x = None ->
    well_formed_type tau ->
    well_formed_type ([[v: T | phi]]) ->
    (exists e_x_hat, tmR_aux nst ([[v: T | phi]]) e_x_hat /\
                  (forall e_x, tmR_aux nst ([[v: T | phi]]) e_x ->
                          (forall (v_x_hat: constant), e_x -->* v_x_hat ->
                              tmR_in_ctx_aux (update nst x v_x_hat) Gamma tau (tlete x e_x e)
                          )
    )) ->
    tmR_in_ctx_aux nst ((x, (Uty ([[v: T | phi]]))) :: Gamma) tau e
| tmR_in_ctx_aux_cons_oarr: forall (nst: state) (x: string) a T phi (tau_b: underty) (Gamma: context) (tau: overunderty) e,
    nst x = None ->
    l_find_right_most Gamma x = None ->
    well_formed_type tau ->
    well_formed_type (a o: {{v: T | phi}} o--> tau_b) ->
    (forall e_x, tmR_aux nst (a o: ({{v: T | phi}}) o--> tau_b) e_x ->
            tmR_in_ctx_aux nst Gamma tau (tlete x e_x e)) ->
    tmR_in_ctx_aux nst ((x, Uty (a o: ({{v: T | phi}}) o--> tau_b)) :: Gamma) tau e
| tmR_in_ctx_aux_cons_underarr: forall (nst: state) (x: string) (t1 t2: underty) (Gamma: context) (tau: overunderty) e,
    nst x = None ->
    l_find_right_most Gamma x = None ->
    well_formed_type tau ->
    well_formed_type (t1 u--> t2) ->
    (forall e_x, tmR_aux nst (t1 u--> t2) e_x ->
            tmR_in_ctx_aux nst Gamma tau (tlete x e_x e)) ->
    tmR_in_ctx_aux nst ((x, Uty (t1 u--> t2)) :: Gamma) tau e.

Global Hint Constructors tmR_in_ctx_aux: core.

Lemma empty_tmR_in_ctx_term_is_closed: forall st tau e,
    tmR_in_ctx_aux st [] tau e -> forall x, ~ x \FVtm e.
Admitted.

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

Lemma tmR_in_ctx_aux_implies_no_dup: forall st Gamma tau e,
    tmR_in_ctx_aux st Gamma tau e -> type_ctx_no_dup (state_to_tystate st) Gamma.
Admitted.

Global Hint Resolve tmR_in_ctx_aux_implies_no_dup: core.

Lemma tmR_in_ctx_aux_implies_closed: forall st Gamma tau e,
    tmR_in_ctx_aux st Gamma tau e -> st_type_closed_in_ctx (state_to_tystate st) Gamma tau.
Admitted.

Lemma tmR_in_ctx_aux_implies_fst_closed: forall st Gamma x tau_x tau e,
    tmR_in_ctx_aux st ((x, tau_x)::Gamma) tau e -> st_type_closed (state_to_tystate st) tau_x.
Admitted.

Lemma tmR_in_ctx_aux_implies_closed_ctx: forall st Gamma tau e,
    tmR_in_ctx_aux st Gamma tau e -> st_type_closed_ctx (state_to_tystate st) Gamma.
Admitted.

(* Definition tmR tau e := forall st, tmR_aux st tau e. *)
(* Definition tmR_in_ctx Gamma tau e := forall st, tmR_in_ctx_aux st Gamma tau e. *)

(* denotation in ctx \S{Ty} *)

Lemma mk_eq_constant_is_itsefl_in_ctx: forall st Gamma (c c': constant),
    tmR_in_ctx_aux st Gamma (mk_eq_constant c) c' <-> c' = c.
Admitted.

(* denotation in ctx Lemmas *)

Lemma denotation_ctx_implies_well_formed_type: forall st Gamma tau e,
    tmR_in_ctx_aux st Gamma tau e -> well_formed_type tau.
Admitted.

Global Hint Resolve denotation_ctx_implies_well_formed_type: core.

Lemma denotation_ctx_implies_last_well_formed_type: forall st Gamma x tau tau' e,
    tmR_in_ctx_aux st (Gamma ++ ((x, tau)::nil)) tau' e -> well_formed_type tau.
Admitted.

Global Hint Resolve denotation_ctx_implies_last_well_formed_type: core.

Lemma under_variable_has_same_type_in_ctx: forall st Gamma x (tau: underty),
    tmR_in_ctx_aux st (Gamma ++ ((x, Uty tau)::nil)) tau x.
Admitted.

Lemma over_variable_has_eq_type_in_ctx: forall st Gamma x T phi,
    tmR_in_ctx_aux st (Gamma ++ ((x, Oty ({{v: T | phi}}))::nil)) (mk_eq_var T x) x.
Admitted.

Lemma under_variable_has_eq_type_in_ctx: forall st Gamma x T phi,
    tmR_in_ctx_aux st (Gamma ++ ((x, Uty ([[v: T | phi]]))::nil)) (mk_eq_var T x) x.
Admitted.

Lemma eta_self1: forall x c_x e e',
    e <-< e' ->
    tlete x c_x e <-< tlete x c_x e'.
Admitted.

Lemma step_preserve_ctx_denotation_over: forall Gamma (e e': tm),
    e' <-< e -> (forall st (tau: overbasety), tmR_in_ctx_aux st Gamma tau e -> tmR_in_ctx_aux st Gamma tau e').
Proof with eauto.
  intros Gamma.
  induction Gamma; simpl; intros e e' Hts st tau He...
  - inversion He; subst. inversion H; subst. constructor... eapply term_order_const_bound in Hts... subst...
  - inversion He; subst.
    + constructor...
      intros c_x Hc_xD. assert (tmR_in_ctx_aux (x |-> c_x; st) Gamma tau (tlete x c_x e)) as Hv1...
      eapply IHGamma... apply eta_self1...
    + constructor...
      destruct H8 as (e_x_hat & He_x_hatD & HH).
      exists e_x_hat. split...
      intros e_x He_xD v_x_hat HvE.
      assert (tmR_in_ctx_aux (x |-> v_x_hat; st) Gamma tau (tlete x e_x e))...
      eapply IHGamma... apply eta_self1...
    + constructor...
      intros e_x He_xD.
      assert (tmR_in_ctx_aux st Gamma tau (tlete x e_x e))...
      eapply IHGamma... apply eta_self1...
    + constructor...
      intros e_x He_xD.
      assert (tmR_in_ctx_aux st Gamma tau (tlete x e_x e))...
      eapply IHGamma... apply eta_self1...
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
      eapply IHGamma... apply eta_self1...
    + constructor...
      destruct H8 as (e_x_hat & He_x_hatD & HH).
      exists e_x_hat. split...
      intros e_x He_xD v_x_hat HvE.
      assert (tmR_in_ctx_aux (x |-> v_x_hat; st) Gamma tau (tlete x e_x e))...
      eapply IHGamma... apply eta_self1...
    + constructor...
      intros e_x He_xD.
      assert (tmR_in_ctx_aux st Gamma tau (tlete x e_x e))...
      eapply IHGamma... apply eta_self1...
    + constructor...
      intros e_x He_xD.
      assert (tmR_in_ctx_aux st Gamma tau (tlete x e_x e))...
      eapply IHGamma... apply eta_self1...
Qed.

Lemma denotation_in_ctx_implies_not_free_not_in_ctx: forall Gamma st tau e x,
    tmR_in_ctx_aux st Gamma tau e -> l_find_right_most Gamma x = None -> ~ x \FVtm e.
Admitted.

Global Hint Resolve denotation_in_ctx_implies_not_free_not_in_ctx: core.

Lemma tmR_in_ctx_pre_weakening: forall nst Gamma1 Gamma2 Gamma3 (tau: overunderty),
    (Gamma1 ++ Gamma2) = Gamma3 ->
    st_type_closed_in_ctx (state_to_tystate nst) Gamma2 tau ->
    (forall e, tmR_in_ctx_aux nst Gamma2 tau e -> tmR_in_ctx_aux nst Gamma3 tau e).
Admitted.

Lemma tmR_in_ctx_post_weakening: forall nst Gamma1 Gamma2 Gamma3 (tau: overunderty),
    (Gamma1 ++ Gamma2) = Gamma3 ->
    st_type_closed_in_ctx (st\_ nst _/) Gamma1 tau ->
    (forall e, tmR_in_ctx_aux nst Gamma1 tau e -> tmR_in_ctx_aux nst Gamma3 tau e).
Admitted.

(* Global Hint Unfold lcontxt_to_basic_ctx: core. *)

(* Finally, we define the denotation *)
Definition tmR_in_ctx_all_st Gamma tau e := tmR_in_ctx_aux empty Gamma tau e.

Global Hint Unfold tmR_in_ctx_all_st: core.

(* for vfix Axiom *)
Definition const_order: constant -> constant -> Prop.
Admitted.

Lemma const_order_is_well_founded: well_founded const_order.
Admitted.

Definition well_founded_constraint (x: string) (phi: refinement) :=
  fun st c => (exists c_x, st x = Some c_x /\ const_order c c_x) /\ phi st c.
