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

(* overtype denotation *)
Inductive overbase_tmR_aux: nstate -> overbasety -> constant -> Prop :=
| over_tmR_base : forall (nst: nstate) (T: base_ty) (phi: refinement) (c: constant),
    empty |- c \Vin T -> phi_sat_nst phi nst c -> overbase_tmR_aux nst ({{v: T | phi}}) c.

Global Hint Constructors overbase_tmR_aux: core.

Lemma over_tmR_aux_has_type: forall T phi st c, overbase_tmR_aux st ({{v: T | phi}}) c -> empty |- c \Vin T.
Proof. intros. inversion H. auto. Qed.

Inductive is_application: tm -> tm -> tm -> Prop :=
(* | Is_app_value_value: forall (x: string) (v1 v2:value), is_application v1 v2 (tletapp x v1 v2 x) *)
| Is_application: forall (e1 e2: tm) (x1 x2 x: string),
    x1 <> x2 -> ~ x1 \FVtm e2 ->
    is_application e1 e2
                   (tlete x1 e1 (tlete x2 e2 (tletapp x x1 x2 x))).

Lemma exists_fresh_var_of_value: forall (e: value), exists (x1 x2: string), x1 <> x2 /\ ~ x1 \FVvalue e.
Admitted.

Definition application_exists: forall e1 e2 x, exists (x1 x2: string),
    is_application e1 e2 (tlete x1 e1 (tlete x2 e2 (tletapp x x1 x2 x))).
Admitted.

Global Hint Constructors is_application: core.

(* Lemma for is application *)

Lemma term_order_eq_trans (e1 e2 e3: tm): e1 <=< e2 -> e2 <=< e3 -> e1 <=< e3.
Admitted.

Lemma value_value_is_application: forall x (v1 v2: value),
    exists (e3: tm), is_application v1 v2 e3 /\ ((tletapp x v1 v2 x) <=< e3).
Proof with eauto.
  intros. destruct (application_exists v1 v2 x) as (x1 & x2 & H).
  exists (tlete x1 v1 (tlete x2 v2 (tletapp x x1 x2 x))). split...
  eapply term_order_eq_trans. eapply eta_reduction... admit.
Admitted.

Fixpoint under_tmR_aux (nst: nstate) (tau: underty) (e: tm) : Prop :=
  well_formed_type tau /\
    empty |- e \Tin u\_ tau _/ /\
              (match tau with
               | [[v: T | phi ]] => (forall (c: constant), empty |- vconst c \Vin T -> phi_sat_nst phi nst c -> e -->* c)
               | x o: t1 o--> t2 =>
                   forall (c_x: constant),
                     overbase_tmR_aux nst t1 c_x ->
                     (forall e3, is_application e c_x e3 -> under_tmR_aux (update nst x c_x) t2 e3)
               | t1 u--> t2 =>
                   forall (e_x: value),
                     under_tmR_aux nst t1 e_x ->
                     (forall e3, is_application e e_x e3 -> under_tmR_aux nst t2 e3)
               end).

Lemma under_tmR_has_type: forall tau st e, under_tmR_aux st tau e -> empty |- e \Tin u\_ tau _/.
Proof with eauto.
  intro tau. induction tau; simpl; intros st e H...
  - destruct H. destruct H0...
  - destruct o. destruct H. destruct H0...
  - destruct H. destruct H0...
Qed.

Global Hint Resolve under_tmR_has_type: core.

Inductive tmR_aux: nstate -> overunderty -> tm -> Prop :=
| tmR_oty: forall st oty c, overbase_tmR_aux st oty c -> tmR_aux st oty c
| tmR_uty: forall st uty e, under_tmR_aux st uty e -> tmR_aux st uty e.

Global Hint Constructors tmR_aux: core.

Lemma tmR_has_type: forall tau st e, tmR_aux st tau e -> empty |- e \Tin ou\_ tau _/.
Proof with eauto.
  intros. destruct H...
  - inversion H; subst; auto.
  - eapply under_tmR_has_type...
Qed.

Global Hint Resolve tmR_has_type: core.

Lemma denotation_is_closed: forall st tau e, tmR_aux st tau e -> st_type_closed_in_ctx st l_empty tau.
Admitted.

Global Hint Resolve denotation_is_closed: core.

Global Hint Constructors has_type: core.
Global Hint Constructors value_has_type: core.

Lemma mk_op_has_denotation: forall st op a b, tmR_aux st (mk_op op a b) (vbiop op).
Admitted.

Global Hint Resolve mk_op_has_denotation: core.

(* The denotation does not guarantee the well-formedness (inv_ctx). *)
(* The denotation implies no dup. *)
Inductive tmR_in_ctx_aux: nstate -> context -> overunderty -> tm -> Prop :=
| tmR_in_ctx_aux_nil: forall (nst: nstate) (tau: overunderty) e,
    tmR_aux nst tau e -> tmR_in_ctx_aux nst [] tau e
| tmR_in_ctx_aux_cons_overbase:
  forall (nst: nstate) (x: string) (T:base_ty) (phi: refinement) (Gamma: context) (tau: overunderty) e,
    nst x = None ->
    l_find_right_most Gamma x = None ->
    well_formed_type tau ->
    well_formed_type ({{v: T | phi}}) ->
    (forall (c_x: constant), tmR_aux nst ({{v: T | phi}}) c_x ->
                        tmR_in_ctx_aux (update nst x c_x) Gamma tau (tlete x (vconst c_x) e)) ->
    tmR_in_ctx_aux nst ((x, (Oty ({{v: T | phi}}))) :: Gamma) tau e
| tmR_in_ctx_aux_cons_under:
  forall (nst: nstate) (x: string) (T:base_ty) (phi: refinement) (Gamma: context) (tau: overunderty) e,
    nst x = None ->
    l_find_right_most Gamma x = None ->
    well_formed_type tau ->
    well_formed_type ([[v: T | phi]]) ->
    (exists e_x_hat, tmR_aux nst ([[v: T | phi]]) e_x_hat /\
                  (forall e_x, tmR_aux nst ([[v: T | phi]]) e_x ->
                          tmR_in_ctx_aux (update nst x e_x_hat) Gamma tau (tlete x e_x e))) ->
    tmR_in_ctx_aux nst ((x, (Uty ([[v: T | phi]]))) :: Gamma) tau e
| tmR_in_ctx_aux_cons_oarr: forall (nst: nstate) (x: string) a T phi (tau_b: underty) (Gamma: context) (tau: overunderty) e,
    nst x = None ->
    l_find_right_most Gamma x = None ->
    well_formed_type tau ->
    well_formed_type (a o: {{v: T | phi}} o--> tau_b) ->
    (forall e_x, tmR_aux nst (a o: ({{v: T | phi}}) o--> tau_b) e_x ->
            tmR_in_ctx_aux nst Gamma tau (tlete x e_x e)) ->
    tmR_in_ctx_aux nst ((x, Uty (a o: ({{v: T | phi}}) o--> tau_b)) :: Gamma) tau e
| tmR_in_ctx_aux_cons_underarr: forall (nst: nstate) (x: string) (t1 t2: underty) (Gamma: context) (tau: overunderty) e,
    nst x = None ->
    l_find_right_most Gamma x = None ->
    well_formed_type tau ->
    well_formed_type (t1 u--> t2) ->
    (forall e_x, tmR_aux nst (t1 u--> t2) e_x ->
            tmR_in_ctx_aux nst Gamma tau (tlete x e_x e)) ->
    tmR_in_ctx_aux nst ((x, Uty (t1 u--> t2)) :: Gamma) tau e.

Global Hint Constructors tmR_in_ctx_aux: core.

Lemma tmR_in_ctx_aux_implies_no_dup: forall st Gamma tau e, tmR_in_ctx_aux st Gamma tau e -> type_ctx_no_dup Gamma.
Admitted.

Global Hint Resolve tmR_in_ctx_aux_implies_no_dup: core.

(* Definition tmR tau e := forall st, tmR_aux st tau e. *)
(* Definition tmR_in_ctx Gamma tau e := forall st, tmR_in_ctx_aux st Gamma tau e. *)

(* denotation in ctx \S{Ty} *)

Lemma mk_eq_constant_is_itsefl_in_ctx: forall st Gamma (c c': constant),
    tmR_in_ctx_aux st Gamma (mk_eq_constant c) c' <-> c' = c.
Admitted.

(* denotation in ctx Lemmas *)

Lemma denotation_ctx_implies_last_well_formed_type: forall st Gamma x tau tau' e,
    tmR_in_ctx_aux st (Gamma ++ ((x, tau)::nil)) tau' e -> well_formed_type tau.
Admitted.

Lemma under_variable_has_same_type_in_ctx: forall st Gamma x (tau: underty),
    tmR_in_ctx_aux st (Gamma ++ ((x, Uty tau)::nil)) tau x.
Admitted.

Lemma over_variable_has_eq_type_in_ctx: forall st Gamma x T phi,
    tmR_in_ctx_aux st (Gamma ++ ((x, Oty ({{v: T | phi}}))::nil)) (mk_eq_var T x) x.
Admitted.

Lemma under_variable_has_eq_type_in_ctx: forall st Gamma x T phi,
    tmR_in_ctx_aux st (Gamma ++ ((x, Uty ([[v: T | phi]]))::nil)) (mk_eq_var T x) x.
Admitted.

Lemma step_preserve_under_denotation: forall (e e': tm),
    e <-< e' -> (forall st tau, under_tmR_aux st tau e -> under_tmR_aux st tau e').
Admitted.

Lemma step_preserve_ctx_denotation: forall (e e': tm),
    e <-< e' -> (forall st Gamma tau, tmR_in_ctx_aux st Gamma tau e -> tmR_in_ctx_aux st Gamma tau e').
Admitted.

(* Work only when there is no dup in Gamma! *)
Fixpoint erase_ctx (Gamma: lcontxt) :=
  match Gamma with
  | nil => empty
  | (x, tau)::Gamma =>
      match tau with
      | Oty _ | Uty ([[v: _ | _ ]]) => update (erase_ctx Gamma) x (ou\_ tau _/)
      | _ => (erase_ctx Gamma)
      end
  end.

Lemma tmR_in_ctx_aux_implies_has_type: forall Gamma st tau e,
    type_ctx_no_dup Gamma ->
    tmR_in_ctx_aux st Gamma tau e -> (erase_ctx Gamma) |- e \Tin ou\_ tau _/.
Admitted.

Lemma tmR_in_ctx_pre_weakening: forall nst Gamma1 Gamma2 Gamma3 (tau: overunderty),
    (Gamma1 ++ Gamma2) = Gamma3 ->
    st_type_closed_in_ctx nst Gamma2 tau ->
    (forall e, tmR_in_ctx_aux nst Gamma2 tau e -> tmR_in_ctx_aux nst Gamma3 tau e).
Admitted.

Lemma tmR_in_ctx_post_weakening: forall nst Gamma1 Gamma2 Gamma3 (tau: overunderty),
    (Gamma1 ++ Gamma2) = Gamma3 ->
    st_type_closed_in_ctx nst Gamma1 tau ->
    (forall e, tmR_in_ctx_aux nst Gamma1 tau e -> tmR_in_ctx_aux nst Gamma3 tau e).
Admitted.

Global Hint Resolve tmR_in_ctx_aux_implies_has_type: core.

(* Global Hint Unfold lcontxt_to_basic_ctx: core. *)

(* Finally, we define the denotation *)
Definition tmR_in_ctx_all_st Gamma tau e := tmR_in_ctx_aux empty Gamma tau e.

Global Hint Unfold tmR_in_ctx_all_st: core.
