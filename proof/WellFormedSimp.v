Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From PLF Require Import CoreLangSimp.
From Coq Require Import Lists.List.
From Coq Require Import Strings.String.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.
From PLF Require Import NormalTypeSystemSimp.
From PLF Require Import RfTypeDef.
From PLF Require Import LinearContext.
Import ListNotations.

Definition is_closed_refinement (_: basic_ty) (phi: refinement): Prop := forall st st', phi st = phi st'.

Global Hint Unfold is_closed_refinement: core.

Lemma constant_refinement_is_closed: forall (c: constant) (phi: refinement),
    (forall st : state, phi st = (fun c' => c = c')) -> (forall T, is_closed_refinement T phi).
Proof.
  intros.
  intros st st'. rewrite H. rewrite H. auto.
Qed.

Example true_refinement: is_closed_refinement TNat (fun _ c => c = 3).
Proof.
  unfold is_closed_refinement. reflexivity.
Qed.

Definition ncontxt := linear_context ty.
Definition lcontxt := linear_context overunderty.

Inductive well_formed_refinement : ncontxt -> basic_ty -> refinement -> Prop :=
| well_formed_refinement_nil: forall T phi, is_closed_refinement T phi -> well_formed_refinement [] T phi
| well_formed_refinement_cons: forall x T Gamma Tphi phi,
    (forall (c_x: constant), empty |- c_x \Vin T ->
                                well_formed_refinement Gamma Tphi (fun st c => phi (t_update st x c_x) c)) ->
    well_formed_refinement ((x, T) :: Gamma) Tphi phi.

Global Hint Constructors well_formed_refinement: core.

Lemma well_formed_refinement_weakening: forall Gamma1 Gamma2 Gamma3 T phi,
    (Gamma1 ++ Gamma2) = Gamma3 ->
    well_formed_refinement Gamma2 T phi ->
    well_formed_refinement Gamma3 T phi.
Admitted.

Module ExCloseRefinement.

(* When the type dismatch, it is equal to bottom refinement *)
Example true_refinement2: is_closed_refinement TNat (fun _ c => c = true).
Proof.
  unfold is_closed_refinement. reflexivity.
Qed.

Example refinement_under_nctx: well_formed_refinement [("x"%string, TBasic TNat)] TNat (fun st c => c = st "x"%string).
Proof.
  constructor. intros.
  apply nat_value_n_exists in H. destruct H; subst.
  constructor. constructor; subst; auto.
Qed.

End ExCloseRefinement.

Inductive overunderbasety : Type :=
| Obase: basic_ty -> refinement -> overunderbasety
| Ubase: basic_ty -> refinement -> overunderbasety.

Definition basecontxt := linear_context overunderbasety.

Fixpoint erase_basetypectx (Gamma: basecontxt): ncontxt :=
  match Gamma with
  | nil => nil
  | (x, Obase T _)::Gamma => (x, TBasic T)::(erase_basetypectx Gamma)
  | (x, Ubase T _)::Gamma => (x, TBasic T)::(erase_basetypectx Gamma)
  end.

Inductive well_formed_in_basectx : basecontxt -> overunderty -> Prop :=
| well_formed_base1: forall Gamma (T: basic_ty) phi,
    well_formed_refinement (erase_basetypectx Gamma) T phi ->
    well_formed_in_basectx Gamma ({{v: T | phi}})
| well_formed_base2: forall Gamma (T: basic_ty) phi,
    well_formed_refinement (erase_basetypectx Gamma) T phi ->
    well_formed_in_basectx Gamma ([[v: T | phi]])
| well_formed_oarr: forall Gamma x T phi (tau: underty),
    well_formed_in_basectx Gamma ({{v: T | phi}}) ->
    well_formed_in_basectx ((x, Obase T phi) :: Gamma) tau ->
    well_formed_in_basectx Gamma (x o: ({{v: T | phi}}) o--> tau)
| well_formed_arrarr: forall Gamma (tau_x: underarrowty) (tau: underty),
    well_formed_in_basectx Gamma tau_x ->
    well_formed_in_basectx Gamma tau ->
    well_formed_in_basectx Gamma (tau_x u--> tau).

Global Hint Constructors well_formed_in_basectx: core.

(* before erase refinement from the context, we will erase all function type bindings *)
Fixpoint lcontxt_to_baseconctx (Gamma: lcontxt): basecontxt :=
  match Gamma with
  | nil => nil
  | (x, ty)::Gamma =>
      match ty with
      | Oty ({{v: T | phi }}) => (x, Obase T phi)::(lcontxt_to_baseconctx Gamma)
      | Uty ([[v: T | phi ]]) => (x, Ubase T phi)::(lcontxt_to_baseconctx Gamma)
      | _ => (lcontxt_to_baseconctx Gamma)
      end
  end.

Definition well_formed (Gamma: lcontxt) (tau: overunderty):=
  well_formed_in_basectx (lcontxt_to_baseconctx Gamma) tau.

Fixpoint erase_arr_bindings (Gamma: lcontxt): lcontxt :=
  match Gamma with
  | nil => nil
  | (x, ty)::Gamma =>
      match ty with
      | Oty _ | Uty ([[v: _ | _ ]]) => (x, ty)::(erase_arr_bindings Gamma)
      | _ => (erase_arr_bindings Gamma)
      end
  end.

Lemma erase_arr_bindings_spec: forall (Gamma: lcontxt),
    lcontxt_to_baseconctx Gamma = lcontxt_to_baseconctx (erase_arr_bindings Gamma).
Proof.
  intro Gamma. induction Gamma; auto.
  destruct a as (x, xty).
  destruct xty. destruct u; simpl; auto.
  - rewrite IHGamma. auto.
  - destruct o. simpl. rewrite IHGamma. auto.
Qed.

Lemma well_fromed_even_without_arr_bindings: forall Gamma tau,
    well_formed Gamma tau <-> well_formed (erase_arr_bindings Gamma) tau.
Proof.
  unfold well_formed. setoid_rewrite <- erase_arr_bindings_spec. reflexivity.
Qed.

Inductive well_formed_ctx: lcontxt -> Prop :=
| well_formed_ctx_nil: well_formed_ctx nil
| well_fromed_ctx_cons: forall x tau_x Gamma,
    well_formed_ctx Gamma ->
    well_formed Gamma tau_x ->
    well_formed_ctx (Gamma <l> x :l: tau_x).

Global Hint Constructors well_formed_ctx: core.

Lemma well_formed_ctx_prefix: forall Gamma1 Gamm2,
    well_formed_ctx (Gamma1 ++ Gamm2) -> well_formed_ctx Gamma1.
Admitted.

Lemma well_fromed_ctx_even_without_arr_bindings: forall Gamma,
    well_formed_ctx Gamma -> well_formed_ctx (erase_arr_bindings Gamma).
Admitted.

Definition appear_free_in_refinement (name: string) T (phi: refinement): Prop :=
  (exists st (c1 c2:constant),
      empty |- c1 \Vin T /\ empty |- c2 \Vin T /\
      phi (t_update st name c1) <> phi (t_update st name c2)
  ).

Definition appear_free_in_overbasety (name: string) T (oty: overbasety): Prop :=
  match oty with
  | {{v: _ | phi }} => appear_free_in_refinement name T phi
  end.

Fixpoint appear_free_in_underarrowty (name: string) T (uty: underarrowty): Prop :=
  match uty with
  | DependArrow x xty retty =>
      appear_free_in_overbasety name T xty \/ (x <> name /\ appear_free_in_underty name T retty)
  | IndependArrow t1 t2 =>
      appear_free_in_underarrowty name T t1 \/ appear_free_in_underty name T t2
  end
with appear_free_in_underty (name: string) T (uty: underty): Prop :=
       match uty with
       | [[v: _ | phi ]] => appear_free_in_refinement name T phi
       | ArrowUnder ty => appear_free_in_underarrowty name T ty
       end.

Definition appear_free_in_overunderty (name: string) T (overunderty: overunderty): Prop :=
  match overunderty with
  | Uty uty => appear_free_in_underty name T uty
  | Oty oty => appear_free_in_overbasety name T oty
  end.
Notation " x '\FVty' e " := (appear_free_in_overunderty x e) (at level 40).

Fixpoint appear_free_in_tvalue (id:string) (v:value) : Prop :=
  match v with
  | vconst _ => False
  | vvar x => x = id
  | vlam x xty e => x <> id /\ appear_free_in_ttm id e
  end
with appear_free_in_ttm (id:string) (e:tm) : Prop :=
       match e with
       | tvalue v => appear_free_in_tvalue id v
       | tlete x e_x e => appear_free_in_ttm id e_x \/ (x <> id /\ appear_free_in_ttm id e)
       | tletbiop x op v1 v2 e =>
           appear_free_in_tvalue id v1 \/ appear_free_in_tvalue id v2 \/ (x <> id /\ appear_free_in_ttm id e)
       | tletapp x v1 v2 e =>
           appear_free_in_tvalue id v1 \/ appear_free_in_tvalue id v2 \/ (x <> id /\ appear_free_in_ttm id e)
       | vexn => False
       end.

Notation " x '\FVtm' e " := (appear_free_in_ttm x e) (at level 40).
Notation " x '\FVvalue' e " := (appear_free_in_tvalue x e) (at level 40).

Definition refinement_subst_id (x1 x2: string) (phi: refinement): refinement :=
  (fun st c => phi (t_update st x1 (st x2)) c).

Definition over_subst_id (x1 x2: string) (oty: overbasety): overbasety :=
  match oty with
    | BaseOver T phi => BaseOver T (refinement_subst_id x1 x2 phi)
  end.

Fixpoint under_arr_subst_id (x1 x2: string) (uty: underarrowty): underarrowty :=
  match uty with
  | DependArrow x xty retty =>
      if String.eqb x1 x then
        DependArrow x (over_subst_id x1 x2 xty) retty
      else
        DependArrow x (over_subst_id x1 x2 xty) (under_subst_id x1 x2 retty)
  | IndependArrow t1 t2 =>
      IndependArrow (under_arr_subst_id x1 x2 t1) (under_subst_id x1 x2 t2)
  end
with under_subst_id (x1 x2: string) (uty: underty): underty :=
       match uty with
       | BaseUnder T phi => BaseUnder T (refinement_subst_id x1 x2 phi)
       | ArrowUnder ty => under_arr_subst_id x1 x2 ty
       end.

Notation " '<r[' x '|x->' y ']>' uty " := (refinement_subst_id x y uty) (at level 40).
Notation " '<u[' x '|x->' y ']>' uty " := (under_subst_id x y uty) (at level 40).
Notation " '<o[' x '|x->' y ']>' uty " := (over_subst_id x y uty) (at level 40).

Definition refinement_subst_c (x1: string) (c2: constant) (phi: refinement): refinement :=
  (fun st c => phi (t_update st x1 c2) c).

Lemma refinement_subst_c_closed_rf: forall x1 c2 T phi,
    is_closed_refinement T phi -> refinement_subst_c x1 c2 phi = phi.
Proof.
  intros.
  unfold is_closed_refinement in H. unfold refinement_subst_c.
  apply functional_extensionality. intros st.
  rewrite H with (st' := (x1 !-> c2; st)).
  auto.
Qed.

Definition over_subst_c (x1: string) (c2: constant) (oty: overbasety): overbasety :=
  match oty with
    | BaseOver T phi => BaseOver T (refinement_subst_c x1 c2 phi)
  end.

Fixpoint under_arr_subst_c (x1: string) (c2: constant) (uty: underarrowty): underarrowty :=
  match uty with
  | DependArrow x xty retty =>
      if String.eqb x1 x then
        DependArrow x (over_subst_c x1 c2 xty) retty
      else
        DependArrow x (over_subst_c x1 c2 xty) (under_subst_c x1 c2 retty)
  | IndependArrow t1 t2 =>
      IndependArrow (under_arr_subst_c x1 c2 t1) (under_subst_c x1 c2 t2)
  end
with under_subst_c (x1: string) (c2: constant) (uty: underty): underty :=
       match uty with
       | BaseUnder T phi => BaseUnder T (refinement_subst_c x1 c2 phi)
       | ArrowUnder ty => under_arr_subst_c x1 c2 ty
       end.

Definition overunder_subst_c (x1: string) (c2: constant) (outy: overunderty): overunderty :=
  match outy with
  | Uty uty => under_subst_c x1 c2 uty
  | Oty oty => over_subst_c x1 c2 oty
  end.

Notation " '<r[' x '|c->' y ']>' uty " := (refinement_subst_c x y uty) (at level 40).
Notation " '<u[' x '|c->' y ']>' uty " := (under_subst_c x y uty) (at level 40).
Notation " '<o[' x '|c->' y ']>' uty " := (over_subst_c x y uty) (at level 40).
Notation " '<ou[' x '|c->' y ']>' uty " := (overunder_subst_c x y uty) (at level 40).
