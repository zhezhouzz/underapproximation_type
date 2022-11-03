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

Definition empstate: state := t_empty (cbool false).

Definition context := linear_context overunderty.

Definition empty_ctx: context := nil.

(* Definition constant_base_type_infer (c: constant) := *)
(*   match c with *)
(*   | cbool _ => TBool *)
(*   | cnat _ => TNat *)
(*   end. *)

(* Lemma constant_base_type_infer_refl (c: constant) : forall (T:base_ty), empty |- vconst c \Vin T <-> constant_base_type_infer c = T. *)
(* Proof with eauto. *)
(*   induction c; split; intros; subst; eauto; try (inversion H; eauto); try constructor... *)
(* Qed. *)

(* Global Hint Rewrite constant_base_type_infer_refl: core. *)

(* Lemma constant_base_type_infer_spec (c: constant) : empty |- vconst c \Vin (constant_base_type_infer c). *)
(* Proof with eauto. *)
(*   induction c; constructor... *)
(* Qed. *)

(* Global Hint Resolve constant_base_type_infer_spec: core. *)

(* Definition phi_is_bot (phi: refinement):= forall st c, ~ phi st c. *)

(* logical relation *)

Definition is_constant (v: value) :=
  match v with
  | vconst _ => True
  | _ => False
  end.

(* overtype denotation *)
Inductive overbase_tmR_aux: state -> overbasety -> constant -> Prop :=
| over_tmR_base : forall (st: state) (T: base_ty) (phi: refinement) (c: constant),
    empty |- c \Vin T -> phi st c -> overbase_tmR_aux st ({{v: T | phi}}) c.

Global Hint Constructors overbase_tmR_aux: core.

Lemma over_tmR_aux_has_type: forall T phi st c, overbase_tmR_aux st ({{v: T | phi}}) c -> empty |- c \Vin T.
Proof. intros. inversion H. auto. Qed.

Inductive is_application: tm -> value -> tm -> Prop :=
(* | Is_app_value_value: forall (x: string) (v1 v2:value), is_application v1 v2 (tletapp x v1 v2 x) *)
| Is_application: forall (e1: tm) (v2: value) (x1 x2: string),
    ~ x1 \FVvalue v2 ->
    is_application e1 v2
                   (tlete x1 e1
                          (tletapp x2 x1 v2 x2)
                   ).

Global Hint Constructors is_application: core.

(* Lemma for is application *)

Lemma exists_fresh_var_of_value: forall (e: value), exists (x: string), ~ x \FVvalue e.
Admitted.

Lemma value_value_is_application: forall x (v1 v2: value),
    exists (e3: tm), is_application v1 v2 e3 /\ ((tletapp x v1 v2 x) <=< e3).
Proof with eauto.
  intros. destruct (exists_fresh_var_of_value v2) as (a & Ha).
  exists (tlete a v1 (tletapp x a v2 x)). split...
  eapply eta_reduction...
Qed.

Fixpoint under_tmR_aux (st: state) (tau: underty) (e: tm) : Prop :=
  well_formed_type tau /\
    empty |- e \Tin u\_ tau _/ /\
                    (match tau with
                     | [[v: T | phi ]] =>
                         (forall (c: constant), empty |- vconst c \Vin T -> (phi st c -> e -->* (vconst c)))
                     | x o: t1 o--> t2 =>
                         forall (c_x: constant),
                           overbase_tmR_aux st t1 c_x ->
                           (forall e3, is_application e c_x e3 -> under_tmR_aux (t_update st x c_x) t2 e3)
                     | t1 u--> t2 =>
                         forall (e_x: value),
                           under_tmR_aux st t1 e_x ->
                           (forall e3, is_application e e_x e3 -> under_tmR_aux st t2 e3)
                     end).

Lemma under_tmR_has_type: forall tau st e, under_tmR_aux st tau e -> empty |- e \Tin u\_ tau _/.
Proof with eauto.
  intro tau. induction tau; simpl; intros st e H...
  - destruct H. destruct H0...
  - destruct o. destruct H. destruct H0...
  - destruct H. destruct H0...
Qed.

Global Hint Resolve under_tmR_has_type: core.

Inductive tmR_aux: state -> overunderty -> tm -> Prop :=
| tmR_oty: forall st oty c, overbase_tmR_aux st oty c -> tmR_aux st oty c
| tmR_uty: forall st uty e, under_tmR_aux st uty e -> tmR_aux st uty e.

Global Hint Constructors tmR_aux: core.

Lemma denotation_has_base_type: forall st tau e, tmR_aux st tau e -> empty |- e \Tin ou\_ tau _/.
Proof with eauto.
  intros. destruct H...
  - inversion H; subst; auto.
  - eapply under_tmR_has_type...
Qed.

Lemma denotation_is_closed: forall tau e, tmR_aux empstate tau e -> type_closed l_empty tau.
Admitted.

Global Hint Resolve denotation_is_closed: core.

Global Hint Constructors has_type: core.
Global Hint Constructors value_has_type: core.

Lemma constant_has_denotation (c: constant): forall st, tmR_aux st (mk_eq_constant c) (vconst c).
Proof with eauto.
  intros.
  destruct c. unfold mk_eq_constant.
  - constructor... constructor... constructor... simpl... constructor... constructor... intros; subst...
  - constructor... constructor... simpl. split. clear st. constructor. constructor. intros; subst...
Qed.

Global Hint Resolve constant_has_denotation: core.

Lemma mk_op_has_denotation: forall st op a b, tmR_aux st (mk_op op a b) (vbiop op).
Admitted.

Global Hint Resolve mk_op_has_denotation: core.

(* The denotation does not guarantee the well-formedness. *)
Inductive tmR_in_ctx_aux: state -> context -> overunderty -> tm -> Prop :=
| tmR_in_ctx_aux_nil: forall (st: state) (tau: overunderty) e, tmR_aux st tau e -> tmR_in_ctx_aux st [] tau e
| tmR_in_ctx_aux_cons_overbase: forall (st: state) (x: string) (T:base_ty) (phi: refinement) (Gamma: context) (tau: overunderty) e,
    well_formed_type tau ->
    well_formed_type ({{v: T | phi}}) ->
    (forall c_x, overbase_tmR_aux st ({{v: T | phi}}) c_x ->
            tmR_in_ctx_aux (t_update st x c_x) Gamma tau (tlete x (vconst c_x) e)) ->
    tmR_in_ctx_aux st ((x, (Oty ({{v: T | phi}}))) :: Gamma) tau e
| tmR_in_ctx_aux_cons_under: forall (st: state) (x: string) (T:base_ty) (phi: refinement) (Gamma: context) (tau: overunderty) e,
    well_formed_type tau ->
    well_formed_type ([[v: T | phi]]) ->
    (exists e_x_hat, under_tmR_aux st ([[v: T | phi]]) e_x_hat /\
                  (forall e_x, under_tmR_aux st ([[v: T | phi]]) e_x ->
                          (forall (c_x: constant),
                              empty |- (vconst c_x) \Vin T -> e_x_hat -->* (vconst c_x) ->
                                      tmR_in_ctx_aux (t_update st x c_x) Gamma tau (tlete x e_x e))
                  )
    ) ->
    tmR_in_ctx_aux st ((x, (Uty ([[v: T | phi]]))) :: Gamma) tau e
| tmR_in_ctx_aux_cons_underarr: forall (st: state) (x: string) (tau_x: underty) (Gamma: context) (tau: overunderty) e,
    well_formed_type tau ->
    well_formed_type tau_x ->
    (forall e_x, under_tmR_aux st tau_x e_x ->
            tmR_in_ctx_aux st Gamma tau (tlete x e_x e)) ->
    tmR_in_ctx_aux st ((x, Uty tau_x) :: Gamma) tau e.

Global Hint Constructors tmR_in_ctx_aux: core.

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

Lemma tmR_in_ctx_aux_implies_has_type: forall Gamma st tau e,
    tmR_in_ctx_aux st Gamma tau e -> (lcontxt_to_basic_ctx Gamma) |- e \Tin ou\_ tau _/.
Admitted.

Lemma tmR_in_ctx_pre_weakening: forall Gamma1 Gamma2 Gamma3 (tau: overunderty),
    (Gamma1 ++ Gamma2) = Gamma3 ->
    type_closed Gamma2 tau ->
    (forall st e, tmR_in_ctx_aux st Gamma2 tau e -> tmR_in_ctx_aux st Gamma3 tau e).
Admitted.

Lemma tmR_in_ctx_post_weakening: forall Gamma1 Gamma2 Gamma3 (tau: overunderty),
    (Gamma1 ++ Gamma2) = Gamma3 ->
    type_closed Gamma1 tau ->
    (forall st e, tmR_in_ctx_aux st Gamma1 tau e -> tmR_in_ctx_aux st Gamma3 tau e).
Admitted.

Global Hint Resolve tmR_in_ctx_aux_implies_has_type: core.

Global Hint Unfold lcontxt_to_basic_ctx: core.

(* Finally, we define the denotation *)
Definition tmR_in_ctx_all_st Gamma tau e := forall st, tmR_in_ctx_aux st Gamma tau e.

Global Hint Unfold tmR_in_ctx_all_st: core.
