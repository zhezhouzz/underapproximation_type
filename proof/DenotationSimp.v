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
| Is_application: forall (e1: tm) (v2: value) (x1 x2: string),
    x1 <> x2 -> ~ x2 \FVvalue v2 ->
    is_application e1 v2
                   (tlete x1 e1
                          (tletapp x2 x1 v2 x2)
                   ).

Global Hint Constructors is_application: core.

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

Definition tmR := tmR_aux empstate.
Definition tmR_in_ctx := tmR_in_ctx_aux empstate.

(* denotation in ctx \S{Ty} *)

Lemma mk_eq_constant_is_itsefl_in_ctx: forall Gamma (c c': constant),
    tmR_in_ctx Gamma (mk_eq_constant c) c' <-> c' = c.
Admitted.

Lemma under_variable_has_same_type_in_ctx: forall Gamma x (tau: underty),
    tmR_in_ctx (Gamma ++ ((x, Uty tau)::nil)) tau x.
Admitted.

Lemma over_variable_has_eq_type_in_ctx: forall Gamma x T phi,
    tmR_in_ctx (Gamma ++ ((x, Oty ({{v: T | phi}}))::nil)) (mk_eq_var T x) x.
Admitted.

Lemma under_variable_has_eq_type_in_ctx: forall Gamma x T phi,
    tmR_in_ctx (Gamma ++ ((x, Uty ([[v: T | phi]]))::nil)) (mk_eq_var T x) x.
Admitted.

(* denotation in ctx Lemmas *)

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
    (forall e, tmR_in_ctx Gamma2 tau e -> tmR_in_ctx Gamma3 tau e).
Admitted.

Lemma tmR_in_ctx_post_weakening: forall Gamma1 Gamma2 Gamma3 (tau: overunderty),
    (Gamma1 ++ Gamma2) = Gamma3 ->
    type_closed Gamma1 tau ->
    (forall e, tmR_in_ctx Gamma1 tau e -> tmR_in_ctx Gamma3 tau e).
Admitted.

Global Hint Resolve tmR_in_ctx_aux_implies_has_type: core.

Global Hint Unfold lcontxt_to_basic_ctx: core.

Lemma tmR_in_ctx_id_eq_c: forall Gamma (id: string) c,
    tmR_in_ctx Gamma (mk_eq_constant c) id ->
    (forall e tau, tmR_in_ctx Gamma tau e <-> tmR_in_ctx Gamma tau (subst id c e)).
Admitted.

Global Hint Constructors well_formed_underty: core.

(* Assume there is no dup in the context to simplify the proof. *)
Lemma tmR_in_ctx_preserve_oarr: forall Gamma st x (tau_x: overbasety) e (tau: underty),
    type_ctx_no_dup (Gamma <l> x :l: Oty tau_x) ->
    tmR_in_ctx_aux st (Gamma <l> x :l: Oty tau_x) tau e ->
    tmR_in_ctx_aux st Gamma (x o: tau_x o--> tau) (vlam x (o\_ tau_x _/) e).
Proof with eauto.
  intro Gamma.
  induction Gamma; simpl; intros st x tau_x e tau Hctx HD.
  - setoid_rewrite app_nil_l in HD. inversion HD; subst. constructor... constructor... constructor... inversion H3; subst. constructor...
    split.
    + simpl. constructor... constructor... apply tmR_in_ctx_aux_implies_has_type in HD.
      unfold lcontxt_to_basic_ctx in HD. unfold ncontxt_to_basic_ctx in HD. simpl in HD...
    + intros c_x Hc_xD e3 Happ. inversion Happ; subst. simpl.
      eapply step_preserve_under_denotation... eapply eta_reduction...
      eapply step_preserve_under_denotation... eapply eta_application_const_to_lete_const...
      assert (tmR_in_ctx_aux (x !-> c_x; st) [] tau (tlete x c_x e)) as HH... inversion HH; subst. inversion H1...
   - destruct a as (a & tau_a).
     inversion HD; subst.
     + constructor... constructor... inversion H3; subst. destruct tau_x... intros c_x Hc_xD.
       eapply step_preserve_ctx_denotation. apply eta_lete_const_to_subst.
       simpl... destruct (eqb_spec a x)... exfalso. eapply type_ctx_no_dup_fst_last_diff_name...
       eapply step_preserve_ctx_denotation... apply eta_lete_const_to_subst_in_lam...
       eapply IHGamma...
       apply type_ctx_no_dup_ctx_sub with (Gamma1 := (a, Oty ({{v:T | phi}}))::nil)...
     + constructor... constructor... inversion H3; subst. destruct tau_x...
       destruct H7 as (e_x_hat & He_x_hatD & HH).
       exists e_x_hat. split...
       intros e_x He_xD c_x Hc_xT Hc_xE.
       eapply step_preserve_ctx_denotation... eapply eta_closed_term_can_captured_by_lam...
       apply IHGamma...
       apply type_ctx_no_dup_ctx_sub with (Gamma1 := (a, Uty ([[v:T | phi]]))::nil)...
     + constructor... constructor... inversion H3; subst. destruct tau_x...
       intros e_x He_xD.
       eapply step_preserve_ctx_denotation... eapply eta_closed_term_can_captured_by_lam...
       apply IHGamma...
       apply type_ctx_no_dup_ctx_sub with (Gamma1 := (a, Uty tau_x0)::nil)...
Qed.

Lemma tmR_in_ctx_preserve_arrarr: forall Gamma st x (tau_x: underty) e (tau: underty),
    type_ctx_no_dup (Gamma <l> x :l: Uty tau_x) ->
    tmR_in_ctx_aux st (Gamma <l> x :l: Uty tau_x) tau e ->
    tmR_in_ctx_aux st Gamma (tau_x u--> tau) (vlam x (u\_ tau_x _/) e).
Proof with eauto.
  intro Gamma.
  induction Gamma; simpl; intros st x tau_x e tau Hctx HD.
  - setoid_rewrite app_nil_l in HD. setoid_rewrite app_nil_l in Hctx.
Admitted.

Lemma tmR_in_ctx_preserve_arrarr_application: forall Gamma st x (v1 v2: value) tau tau1,
    tmR_in_ctx_aux st Gamma (tau1 u--> tau) v1 ->
    tmR_in_ctx_aux st Gamma tau1 v2 ->
    tmR_in_ctx_aux st Gamma tau (tletapp x v1 v2 x).
Proof with eauto.
  intro Gamma.
  induction Gamma; simpl; intros st x v1 v2 tau tau_x Hv1D Hv2D.
  - inversion Hv1D; subst. inversion Hv2D; subst. constructor. inversion H; subst. inversion H3; subst.
    destruct H2 as (Hv1T & HH). inversion H0; subst. apply HH with (e3:=tletapp x v1 v2 x) in H5...
    admit.
  - destruct a as (a & tau_a).
    inversion Hv1D; subst.
    + constructor... admit.
    admit.
Admitted.

Lemma tmR_in_ctx_preserve_oarr_c_application: forall Gamma st x (v1: value) (c2: constant) tau a T phi,
    tmR_in_ctx_aux st Gamma (a o: {{v: T | phi}} o--> tau) v1 ->
    tmR_in_ctx_aux st Gamma ([[v: T | phi]]) c2 ->
    tmR_in_ctx_aux st Gamma (under_subst_c a c2 tau) (tletapp x v1 c2 x).
Admitted.

Lemma tmR_in_ctx_preserve_oarr_var_application: forall Gamma st x (v1: value) (name2: string) tau a T phi,
    tmR_in_ctx_aux st Gamma (a o: {{v: T | phi}} o--> tau) v1 ->
    tmR_in_ctx_aux st Gamma ([[v: T | phi]]) name2 ->
    tmR_in_ctx_aux st Gamma (under_subst_id a name2 tau) (tletapp x v1 name2 x).
Admitted.

Lemma tmR_in_ctx_preserve_matchb_true: forall Gamma (v: value) e1 e2 tau,
    tmR_in_ctx Gamma (mk_eq_constant true) v ->
    tmR_in_ctx Gamma tau e1 ->
    tmR_in_ctx Gamma tau (tmatchb v e1 e2).
Proof with eauto.
  intros Gamma v e1 e2 tau Hv He1.
  assert ((exists n : bool, v = n) \/ (exists name : string, v = name)) as HH. apply tmR_in_ctx_aux_implies_has_type in Hv. inversion Hv; subst. apply bool_value_cid_exists in H1...
  destruct HH.
  - destruct H as (b & Hb); subst. apply mk_eq_constant_is_itsefl_in_ctx in Hv. rewrite Hv.
    eapply step_preserve_ctx_denotation... apply eta_matchb_true...
  - destruct H as (id & Hid); subst. rewrite tmR_in_ctx_id_eq_c... simpl. rewrite eqb_refl.
    apply step_preserve_ctx_denotation with (e:= ([id := true] e1))... apply eta_matchb_true...
    rewrite <- tmR_in_ctx_id_eq_c...
Qed.

Lemma tmR_in_ctx_preserve_matchb_false: forall Gamma (v: value) e1 e2 tau,
    tmR_in_ctx Gamma (mk_eq_constant false) v ->
    tmR_in_ctx Gamma tau e2 ->
    tmR_in_ctx Gamma tau (tmatchb v e1 e2).
Proof with eauto.
  intros Gamma v e1 e2 tau Hv He1.
  assert ((exists n : bool, v = n) \/ (exists name : string, v = name)) as HH. apply tmR_in_ctx_aux_implies_has_type in Hv. inversion Hv; subst. apply bool_value_cid_exists in H1...
  destruct HH.
  - destruct H as (b & Hb); subst. apply mk_eq_constant_is_itsefl_in_ctx in Hv. rewrite Hv.
    eapply step_preserve_ctx_denotation... apply eta_matchb_false...
  - destruct H as (id & Hid); subst. rewrite tmR_in_ctx_id_eq_c... simpl. rewrite eqb_refl.
    apply step_preserve_ctx_denotation with (e:= ([id := false] e2))... apply eta_matchb_false...
    rewrite <- tmR_in_ctx_id_eq_c...
Qed.
