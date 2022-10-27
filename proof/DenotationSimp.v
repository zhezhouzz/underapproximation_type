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
Definition over_tmR (ty:overbasety) (c:constant) : Prop :=
  empty |- c \Vin o\_ ty _/ /\
            (match ty with
             | {{v: T | phi }} =>
                 is_closed_refinement T phi /\ (phi empstate c)
             end).

Inductive is_application: tm -> value -> tm -> Prop :=
| Is_application: forall (e1: tm) (v2: value) (f x: string),
    ~ f \FVtm v2 -> is_application e1 v2 (tlete f e1 (tletapp x f v2 x)).

(* Inductive under_tmR: underty -> tm -> Prop := *)
(* | under_tmR_base: forall (T: basic_ty) phi e, *)
(*     empty |- e \in T -> is_closed_refinement T phi -> *)
(*                   (forall (c: constant), empty |- vconst c \Vin T -> *)
(*                                             (phi empstate c -> e -->* (vconst c))) -> *)
(*                   under_tmR ([[v: T | phi]]) e *)
(* | under_tmR_oarr: forall x t1 t2 e, *)
(*     empty |- e \in u\_ x o: t1 o--> t2 _/ -> *)
(*                            (forall (c_x: constant), *)
(*                                over_tmR t1 c_x -> *)
(*                                (forall e3, is_application e c_x e3 -> under_tmR (under_subst_c x c_x t2) e3)) -> *)
(*                            under_tmR (x o: t1 o--> t2) e *)
(* | under_tmR_arrarr: forall t1 t2 e, *)
(*     empty |- e \in u\_ t1 u--> t2 _/ -> *)
(*                   ( forall (e_x: tm), *)
(*                       under_tmR t1 e_x -> *)
(*                       (forall (v_x: value), *)
(*                           empty |- v_x \Vin u\_ t1 _/ -> *)
(*                                   e -->* v_x -> *)
(*                                   (forall e3, is_application e v_x e3 -> under_tmR t2 e3) *)
(*                   )) -> *)
(*                   under_tmR (t1 u--> t2) e. *)


Fixpoint under_arr_tmR_aux (ty:underarrowty) (e:tm) : Prop :=
  empty |- e \in u\_ ty _/ /\
                  (match ty with
                   | x o: t1 o--> t2 => False
                   | t1 u--> t2 =>
                       forall (e_x: tm),
                         under_arr_tmR_aux t1 e_x ->
                         (forall (v_x: value),
                             empty |- v_x \Vin u\_ t1 _/ ->
                                     e -->* v_x ->
                                     (forall e3, is_application e v_x e3 -> under_tmR_aux t2 e3)
                         )
                   end)
with under_tmR_aux (ty:underty) (e:tm) : Prop :=
       empty |- e \in u\_ ty _/ /\
                       (match ty with
                        | [[v: T | phi ]] =>
                            is_closed_refinement T phi /\
                              (forall (c: constant),
                                  empty |- vconst c \Vin T ->
                                          (phi empstate c -> e -->* (vconst c))
                              )
                        | ArrowUnder ty => under_arr_tmR_aux ty e
                        end).

Definition under_tmR (ty:underty) (e:tm) : Prop :=
  empty |- e \in u\_ ty _/ /\
                  (match ty with
                   | [[v: _ | _ ]] => under_tmR_aux ty e
                   | _ u--> _ => under_tmR_aux ty e
                   | x o: t1 o--> t2 =>
                       forall (c_x: constant),
                         over_tmR t1 c_x -> (forall e3, is_application e c_x e3 -> under_tmR_aux (under_subst_c x c_x t2) e3)
                   end).

Inductive tmR: overunderty -> tm -> Prop :=
| tmR_oty: forall oty c, over_tmR oty c -> tmR oty c
| tmR_uty: forall uty e, under_tmR uty e -> tmR uty e.

Lemma tmR_constant (c: constant): tmR (mk_eq_constant c) (vconst c).
Proof with eauto.
  constructor. split; auto.
  destruct c; simpl; split; auto; split; intros; subst...
Qed.

Global Hint Resolve tmR_constant: core.

Fixpoint ty_subst_under_ctx (id:string) (c_id: constant) (Gamma: context) (tau: underty) : context * underty :=
  match Gamma with
  | nil => (Gamma, underty_subst id c_id tau)
  | (x, xty) :: Gamma =>
      if String.eqb x id
      then
        ((x, overunder_subst_c id c_id xty) :: Gamma, tau)
      else
        let (Gamma', tau') := (ty_subst_under_ctx id c_id Gamma tau) in
        ((x, overunder_subst_c id c_id xty) :: Gamma', tau')
  end.

Lemma ty_subst_under_ctx_not_in_ctx (id:string) (c_id: constant) (Gamma: context) (tau: underty):
  l_find_right_most Gamma id = None ->
  ty_subst_under_ctx id c_id Gamma tau = (Gamma, (underty_subst id c_id tau)).
Admitted.


Inductive tmR_in_ctx: context -> underty -> tm -> Prop :=
| tmR_in_ctx_nil: forall (ty: underty) e, tmR ty e -> tmR_in_ctx [] ty e
| tmR_in_ctx_cons_over: forall (x: string) (T:basic_ty) (phi: refinement) (Gamma: context) tau e,
    (forall c_x Gamma' tau', over_tmR ({{v: T | phi}}) c_x ->
                        (Gamma', tau') = ty_subst_under_ctx x c_x Gamma tau ->
                        tmR_in_ctx Gamma' tau' (subst x (vconst c_x) e)) ->
    tmR_in_ctx ((x, (Oty ({{v: T | phi}}))) :: Gamma) tau e
| tmR_in_ctx_cons_under: forall (x: string) (T:basic_ty) (phi: refinement) (Gamma: context) tau e,
    (exists e_x, under_tmR ([[v: T | phi]]) e_x /\
              (forall (c_x: constant) Gamma' tau',
                  empty |- (vconst c_x) \Vin T -> e_x -->* (vconst c_x) ->
                          (Gamma', tau') = ty_subst_under_ctx x c_x Gamma tau ->
                          tmR_in_ctx Gamma' tau' (subst x (vconst c_x) e))) ->
    tmR_in_ctx ((x, (Uty ([[v: T | phi]]))) :: Gamma) tau e
| tmR_in_ctx_cons_ind_arrow: forall (x: string) t1 t2 (Gamma: context) (ty: underty) e,
    (exists e_x, under_tmR (t1 u--> t2) e_x /\
              tmR_in_ctx Gamma ty (tlete x e_x e)) ->
    tmR_in_ctx ((x, Uty (t1 u--> t2)) :: Gamma) ty e
| tmR_in_ctx_cons_d_arrow: forall (x: string) (y: string) (t1: overbasety) (t2: underty) (Gamma: context) (ty: underty) e,
    (exists e_x, under_tmR (y o: t1 o--> t2) e_x /\
              tmR_in_ctx Gamma ty (tlete x e_x e)) ->
    tmR_in_ctx ((x, Uty (y o: t1 o--> t2)) :: Gamma) ty e.

Global Hint Constructors tmR_in_ctx: core.

Lemma tmR_in_ctx_pre_weakening: forall Gamma1 Gamma2 Gamma3 (tau: underty),
    (Gamma1 ++ Gamma2) = Gamma3 ->
    well_formed Gamma2 tau ->
    (forall e, tmR_in_ctx Gamma2 tau e -> tmR_in_ctx Gamma3 tau e).
Admitted.

Definition context_not_free_in_term {A: Type} (ctx: linear_context A) (e: tm): Prop.
Admitted.

Definition context_not_free_in_underty {A: Type} (ctx: linear_context A) (tau: underty): Prop.
Admitted.

Lemma tmR_in_ctx_post_weakening: forall Gamma (tau: underty),
    well_formed l_empty tau ->
    (forall e, tmR_in_ctx l_empty tau e -> tmR_in_ctx Gamma tau e).
Proof with eauto.
Admitted.
