(** * MoreStlc: More on the Simply Typed Lambda-Calculus *)

Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From PLF Require Import CoreLangSimp.
From PLF Require Import NormalTypeSystemSimp.
From PLF Require Import LinearContext.
From PLF Require Import RfTypeDef.
From PLF Require Import WellFormedSimp.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Lists.List.
From Coq Require Import FunInd.
From Coq Require Import Recdef.

Import CoreLangSimp.
Import LinearContext.
Import WellFormedSimp.
Import ListNotations.

Definition empstate: state := t_empty (cbool false).

Definition context := linear_context overunderty.

Definition empty_ctx: context := nil.

Definition constant_basic_type_infer (c: constant) :=
  match c with
  | cbool _ => TBool
  | cnat _ => TNat
  end.

Lemma constant_basic_type_infer_refl (c: constant) : forall (T:basic_ty), empty |- vconst c \Vin T <-> constant_basic_type_infer c = T.
Proof with eauto.
  induction c; split; intros; subst; eauto; try (inversion H; eauto).
Qed.

Global Hint Rewrite constant_basic_type_infer_refl: core.

Lemma constant_basic_type_infer_spec (c: constant) : empty |- vconst c \Vin (constant_basic_type_infer c).
Proof with eauto.
  induction c...
Qed.

Global Hint Resolve constant_basic_type_infer_spec: core.

Definition mk_eq_constant c := [[v: constant_basic_type_infer c | (fun _ v => v = c) ]].
Definition mk_bot ty := [[v: ty | (fun _ _ => False) ]].
Definition mk_top ty := [[v: ty | (fun _ _ => True) ]].
Definition mk_eq_var ty name := [[v: ty | (fun state v => v = (state name)) ]].

Lemma mk_eq_is_well_formed: forall x T phi, well_formed ((x, Oty ({{v:T | phi}}))::nil) (mk_eq_var T x).
Proof with eauto.
  intros x T phi. repeat constructor...
  unfold is_closed_refinement... intros st st'. rewrite t_update_eq. rewrite t_update_eq. reflexivity.
Qed.

Global Hint Resolve mk_eq_is_well_formed: core.

Lemma mk_eq_constant_spec (c: constant) : empty |- vconst c \in u\_ mk_eq_constant c _/.
Proof with eauto.
  induction c...
Qed.

Lemma subst_constant_eq: forall x c_x c, (<u[ x |c-> c_x ]> mk_eq_constant c) = mk_eq_constant c.
Proof.
  intros. simpl.
  assert (<r[ x |c-> c_x ]> (fun (_ : state) (v : constant) => v = c) = (fun (_ : state) (v : constant) => v = c)).
  unfold refinement_subst_c.
  apply functional_extensionality. intros. apply functional_extensionality. auto.
  setoid_rewrite H. auto.
Qed.

Lemma constant_eq_is_close: forall Gamma c, well_formed Gamma (mk_eq_constant c).
Proof.
  intros. constructor.
  assert (well_formed_refinement nil (constant_basic_type_infer c) (fun (_ : state) (v : constant) => v = c)).
  constructor.
  unfold is_closed_refinement; destruct c; simpl; reflexivity.
  apply well_formed_refinement_weakening with (Gamma1 := (erase_basetypectx (lcontxt_to_baseconctx Gamma))) (Gamma2 := nil); auto.
  rewrite app_nil_r. auto.
Qed.

Global Hint Resolve mk_eq_constant_spec: core.
Global Hint Resolve subst_constant_eq: core.
Global Hint Resolve constant_eq_is_close: core.

Definition phi_is_bot (phi: refinement):= forall st c, ~ phi st c.

(* logical relation *)

Definition is_constant (v: value) :=
  match v with
  | vconst _ => True
  | _ => False
  end.

(* overtype denotation *)
Inductive overbase_tmR: overbasety -> constant -> Prop :=
| over_tmR_base : forall (T: basic_ty) phi (c: constant),
    empty |- c \Vin T -> is_closed_refinement T phi -> phi empstate c -> overbase_tmR ({{v: T | phi}}) c.

Lemma over_tmR_has_type: forall T phi c, overbase_tmR ({{v: T | phi}}) c -> empty |- c \Vin T.
Proof. intros. inversion H. auto. Qed.


Inductive is_application: tm -> tm -> tm -> Prop :=
| Is_application: forall (e1: tm) (e2: tm) (x1 x2 x3: string),
    x1 <> x2 -> x2 <> x3 -> x3 <> x1 ->
    ~ x1 \FVtm e2 ->
    is_application e1 e2 (tlete x1 e1
                                (tlete x2 e2
                                       (tletapp x3 x1 x2 x3)
                                )
                         ).

Global Hint Constructors is_application: core.

(* Inductive underbase_tmR: underty -> tm -> Prop := *)
(* | under_tmR_base : forall (T: basic_ty) phi (e: tm), *)
(*     empty |- e \in T -> is_closed_refinement T phi -> *)
(*                   (forall (c: constant), *)
(*                       empty |- vconst c \Vin T -> *)
(*                               phi empstate c -> *)
(*                               e -->* (vconst c) *)
(*                   ) -> underbase_tmR ([[v: T | phi]]) e. *)

(* Inductive under_fo_tmR: underty -> tm -> Prop := *)
(* | under_fo_tmR_base : forall (T: basic_ty) phi (e: tm), *)
(*     empty |- e \in T -> is_closed_refinement T phi -> *)
(*                   (forall (c: constant), *)
(*                       empty |- vconst c \Vin T -> *)
(*                               phi empstate c -> *)
(*                               e -->* (vconst c) *)
(*                   ) -> under_fo_tmR ([[v: T | phi]]) e *)
(* | under_fo_tmR_oarr: forall (x: string) (t1: overbasety) (t2: underty) (e: tm), *)
(*     empty |- e \in u\_ x o: t1 o--> t2 _/ -> *)
(*                            (forall (c_x: constant), *)
(*                                overbase_tmR t1 c_x -> *)
(*                                (forall e3, is_application e c_x e3 -> under_fo_tmR (under_subst_c x c_x t2) e3) *)
(*                            ) -> under_fo_tmR (x o: t1 o--> t2) e. *)

(* Lemma under_fo_tmR_has_type: forall tau c, under_fo_tmR tau c -> empty |- c \in u\_ tau _/. *)
(* Proof. intros. inversion H; auto. Qed. *)


Fixpoint num_arr_sym_arr (tau: underarrowty): nat :=
  match tau with
  | x o: t1 o--> t2 => 1 + (num_arr_sym t2)
  | t1 u--> t2 => (num_arr_sym_arr t1) + (num_arr_sym t2)
  end
with num_arr_sym (tau: underty): nat :=
       match tau with
       | [[v: _ | _ ]] => 0
       | ArrowUnder tau => num_arr_sym_arr tau
       end.

Definition num_arr_sym_order (t1 t2: underty) :=
  num_arr_sym t1 < num_arr_sym t2.

Global Hint Constructors Acc: core.

Lemma num_arr_sym_order_wf' : forall num, forall tau, num_arr_sym tau <= num -> Acc num_arr_sym_order tau.
Admitted.

Theorem num_arr_sym_order_wf : well_founded num_arr_sym_order.
  red; intro; eapply num_arr_sym_order_wf'; eauto.
Defined.

Lemma under_subst_c_wf: forall x c_x t2, num_arr_sym_order (under_subst_c x c_x t2) t2.
Admitted.

Global Hint Resolve under_subst_c_wf: core.


(* Cannot figure out mutual recursive definition. *)
Lemma underty_wf_arrarr_arr1: forall (t1:underarrowty) (t2: underty), num_arr_sym_order t1 t2.
Admitted.

Lemma underty_wf_arrarr_arr2: forall (t1:underty) (t2: underty), num_arr_sym_order t1 t2.
Admitted.

Lemma underty_wf_subst: forall (t1:underty) (t2: underty), num_arr_sym_order t1 t2.
Admitted.

Definition under_tmR_wf: underty -> tm -> Prop.
Proof.
  refine (@well_founded_induction_type
            underty num_arr_sym_order
            num_arr_sym_order_wf
            (fun _ => tm -> Prop)
            (fun (ty: underty)
               (under_tmR_wf: forall (tau': underty), num_arr_sym_order tau' ty -> tm -> Prop)
               (e: tm) =>
               empty |- e \in u\_ ty _/ /\
                               (match ty with
                                | [[v: T | phi ]] =>
                                    is_closed_refinement T phi /\
                                      (forall (c: constant),
                                          empty |- vconst c \Vin T ->
                                                  (phi empstate c -> e -->* (vconst c))
                                      )
                                | x o: t1 o--> t2 =>
                                    forall (c_x: constant),
                                      overbase_tmR t1 c_x ->
                                      (forall e3, is_application e c_x e3 ->
                                             under_tmR_wf (under_subst_c x c_x t2)
                                                           (underty_wf_subst (under_subst_c x c_x t2) ty) e3)
                                | t1 u--> t2 =>
                                    forall (e_x: tm),
                                      under_tmR_wf t1 (underty_wf_arrarr_arr1 t1 ty) e_x ->
                                      (forall e3, is_application e e_x e3 -> under_tmR_wf t2 (underty_wf_arrarr_arr2 t2 ty) e3)
                                end)
            )
         ).
Qed.

Definition under_tmR_spec_base: forall (T: basic_ty) phi (e: tm),
    under_tmR_wf ([[v: T | phi]]) e <->
    empty |- e \in T /\ is_closed_refinement T phi /\
                    (forall (c: constant),
                        empty |- vconst c \Vin T ->
                                phi empstate c ->
                                e -->* (vconst c)
                    ).
Admitted.

Definition under_tmR_spec_arrarr: forall (t1: underarrowty) (t2: underty) (e: tm),
    under_tmR_wf (t1 u--> t2) e <->
    empty |- e \in u\_ t1 u--> t2 _/ /\
                    (forall (v_x: value),
                        under_tmR_wf t1 v_x ->
                        (forall e3, is_application e v_x e3 -> under_tmR_wf t2 e3)
                    ).
Admitted.

Definition under_tmR_spec_oarr: forall (x: string) (t1: overbasety) (t2: underty) (e: tm),
    under_tmR_wf (x o: t1 o--> t2) e <->
    empty |- e \in u\_ x o: t1 o--> t2 _/ /\
                             (forall (c_x: constant),
                                 overbase_tmR t1 c_x ->
                                 (forall e3, is_application e c_x e3 -> under_tmR_wf (under_subst_c x c_x t2) e3)
                             ).
Admitted.

(* Non strictly positive occurrence  *)
(* Inductive under_tmR: underty -> tm -> Prop := *)
(* | under_tmR_base : forall (T: basic_ty) phi (e: tm), *)
(*     empty |- e \in T -> is_closed_refinement T phi -> *)
(*                   (forall (c: constant), *)
(*                       empty |- vconst c \Vin T -> *)
(*                               phi empstate c -> *)
(*                               e -->* (vconst c) *)
(*                   ) -> under_tmR ([[v: T | phi]]) e *)
(* | under_tmR_arrarr: forall (t1 t2: underarrowty) (e: tm), *)
(*     empty |- e \in u\_ t1 u--> t2 _/ -> *)
(*                   (forall (v_x: value), *)
(*                       under_tmR t1 v_x -> *)
(*                       (forall e3, is_application e v_x e3 -> under_tmR t2 e3) *)
(*                   ) -> under_tmR (t1 u--> t2) e. *)
(* | under_tmR_oarr: forall (x: string) (t1: overbasety) (t2: underty) (e: tm), *)
(*     empty |- e \in u\_ x o: t1 o--> t2 _/ -> *)
(*                            (forall (c_x: constant), *)
(*                                overbase_tmR t1 c_x -> *)
(*                                (forall e3, is_application e c_x e3 -> under_tmR_aux (under_subst_c x c_x t2) e3) *)
(*                            ) -> under_tmR (t1 u--> t2) e. *)

(* A wrong imp *)
(* Fixpoint under_arr_tmR_aux (ty:underarrowty) (e:tm) : Prop := *)
(*   empty |- e \in u\_ ty _/ /\ *)
(*                   (match ty with *)
(*                    | x o: t1 o--> t2 => False *)
(*                    | t1 u--> t2 => *)
(*                        forall (e_x: tm), *)
(*                          under_arr_tmR_aux t1 e_x -> *)
(*                          (forall e3, is_application e e_x e3 -> under_tmR_aux t2 e3) *)
(*                    end) *)
(* with under_tmR_aux (ty:underty) (e:tm) : Prop := *)
(*        (match ty with *)
(*         | [[v: T | phi ]] => *)
(*             empty |- e \in u\_ ty _/ /\ *)
(*                             is_closed_refinement T phi /\ *)
(*                             (forall (c: constant), *)
(*                                 empty |- vconst c \Vin T -> *)
(*                                         (phi empstate c -> e -->* (vconst c)) *)
(*                             ) *)
(*         | ArrowUnder ty => under_arr_tmR_aux ty e *)
(*         end). *)

(* Definition under_tmR (ty:underty) (e:tm) : Prop := *)
(*   (match ty with *)
(*    | [[v: _ | _ ]] => under_tmR_aux ty e *)
(*    | _ u--> _ => under_tmR_aux ty e *)
(*    | x o: t1 o--> t2 => *)
(*        empty |- e \in u\_ ty _/ /\ *)
(*                        forall (c_x: constant), *)
(*                          overbase_tmR t1 c_x -> (forall e3, is_application e c_x e3 -> under_tmR_aux (under_subst_c x c_x t2) e3) *)
(*    end). *)

Definition under_tmR := under_tmR_wf.

Lemma under_tmR_has_type: forall tau e, under_tmR tau e -> empty |- e \in u\_ tau _/.
Proof with eauto.
  intros.
  destruct tau.
  - rewrite under_tmR_spec_base in H. destruct H...
  - destruct u.
    + rewrite under_tmR_spec_oarr in H. destruct H...
    + rewrite under_tmR_spec_arrarr in H. destruct H...
Qed.

Global Hint Resolve under_tmR_has_type: core.

Inductive tmR: overunderty -> tm -> Prop :=
| tmR_oty: forall oty c, overbase_tmR oty c -> tmR oty c
| tmR_uty: forall uty e, under_tmR uty e -> tmR uty e.

Lemma denotation_has_basic_type: forall tau e, tmR tau e -> empty |- e \in ou\_ tau _/.
Admitted.

Global Hint Constructors tmR: core.

Lemma tmR_constant (c: constant): tmR (mk_eq_constant c) (vconst c).
Proof with eauto.
  constructor.
  destruct c; simpl; unfold mk_eq_constant; rewrite under_tmR_spec_base; split; auto; split; intros; subst...
Qed.

Global Hint Resolve tmR_constant: core.

Fixpoint ty_subst_under_ctx (id:string) (c_id: constant) (Gamma: context) (tau: overunderty) : context * overunderty :=
  match Gamma with
  | nil => (Gamma, overunderty_subst id c_id tau)
  | (x, xty) :: Gamma =>
      if String.eqb x id
      then
        ((x, overunder_subst_c id c_id xty) :: Gamma, tau)
      else
        let (Gamma', tau') := (ty_subst_under_ctx id c_id Gamma tau) in
        ((x, overunder_subst_c id c_id xty) :: Gamma', tau')
  end.

(* Definition ty_subst_under_ctx_prop (id:string) (c_id: constant) (Gamma: context) (tau: overunderty) (Gamma': context) (tau': overunderty) := ty_subst_under_ctx id c_id Gamma tau = (Gamma', tau'). *)

Lemma ty_subst_under_ctx_not_in_ctx (id:string) (c_id: constant) (Gamma: context) (tau: overunderty):
  l_find_right_most Gamma id = None ->
  ty_subst_under_ctx id c_id Gamma tau = (Gamma, (overunderty_subst id c_id tau)).
Admitted.

Fixpoint name_not_free_in_ctx_and_ty (id:string) (Gamma: context) (tau: overunderty):=
  match Gamma with
  | nil => ~ id \FVty tau
  | (x, xty)::Gamma =>
      ~ id \FVty xty /\
        if String.eqb x id then True else name_not_free_in_ctx_and_ty id Gamma tau
  end.

Inductive tmR_in_ctx: context -> overunderty -> tm -> Prop :=
| tmR_in_ctx_nil: forall (ty: overunderty) e, tmR ty e -> tmR_in_ctx [] ty e
| tmR_in_ctx_cons_overbase: forall (x: string) (tau_x: overbasety) (Gamma: context) (tau: overunderty) e,
    (forall c_x Gamma' (tau': overunderty),
        overbase_tmR tau_x c_x ->
        ty_subst_under_ctx x c_x Gamma tau = (Gamma', tau') ->
        tmR_in_ctx Gamma' tau' (tlete x (vconst c_x) e)) ->
    tmR_in_ctx ((x, (Oty tau_x)) :: Gamma) tau e
| tmR_in_ctx_cons_underbase: forall (x: string) (T:basic_ty) (phi: refinement) (Gamma: context) (tau: overunderty) e,
    (exists e_x_hat, under_tmR ([[v: T | phi]]) e_x_hat /\
                  (forall e_x, under_tmR ([[v: T | phi]]) e_x ->
                          (forall (c_x: constant) Gamma' (tau': overunderty),
                              empty |- (vconst c_x) \Vin T -> e_x_hat -->* (vconst c_x) ->
                                      ty_subst_under_ctx x c_x Gamma tau = (Gamma', tau') ->
                                      tmR_in_ctx Gamma' tau' (tlete x e_x e))
                  )
    ) ->
    tmR_in_ctx ((x, (Uty ([[v: T | phi]]))) :: Gamma) tau e
| tmR_in_ctx_cons_underarr: forall (x: string) (tau_x: underarrowty) (Gamma: context) (tau: overunderty) e,
    (forall e_x, under_tmR tau_x e_x ->
            tmR_in_ctx Gamma tau (tlete x e_x e)) ->
    tmR_in_ctx ((x, Uty tau_x) :: Gamma) tau e.

Global Hint Constructors tmR_in_ctx: core.


Lemma tmR_in_ctx_preserve_oarr: forall Gamma x (tau_x: overbasety) e (tau: underty),
    tmR_in_ctx (Gamma <l> x :l: Oty tau_x) tau e ->
    tmR_in_ctx Gamma (x o: tau_x o--> tau) (vlam x (o\_ tau_x _/) e).
Admitted.

Lemma tmR_in_ctx_preserve_arrarr: forall Gamma x (tau_x: underarrowty) e (tau: underty),
    tmR_in_ctx (Gamma <l> x :l: Uty tau_x) tau e ->
    tmR_in_ctx Gamma (tau_x u--> tau) (vlam x (u\_ tau_x _/) e).
Admitted.

Lemma tmR_in_ctx_preserve_arrarr_application: forall Gamma x (v1 v2: value) tau tau1,
    tmR_in_ctx Gamma (tau1 u--> tau) v1 ->
    tmR_in_ctx Gamma tau1 v2 ->
    tmR_in_ctx Gamma tau (tletapp x v1 v2 x).
Admitted.

Lemma tmR_in_ctx_preserve_oarr_c_application: forall Gamma x (v1: value) (c2: constant) tau a T phi,
    tmR_in_ctx Gamma (a o: {{v: T | phi}} o--> tau) v1 ->
    tmR_in_ctx Gamma ([[v: T | phi]]) c2 ->
    tmR_in_ctx Gamma (under_subst_c a c2 tau) (tletapp x v1 c2 x).
Admitted.

Lemma tmR_in_ctx_preserve_oarr_var_application: forall Gamma x (v1: value) (name2: string) tau a T phi,
    tmR_in_ctx Gamma (a o: {{v: T | phi}} o--> tau) v1 ->
    tmR_in_ctx Gamma ([[v: T | phi]]) name2 ->
    tmR_in_ctx Gamma (under_subst_id a name2 tau) (tletapp x v1 name2 x).
Admitted.
