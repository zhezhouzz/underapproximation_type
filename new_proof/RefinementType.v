From stdpp Require Import mapset.
From CT Require Import CoreLangProp.
From CT Require Import OperationalSemanticsProp.

Import Atom.
Import CoreLang.
Import Tactics.
Import NamelessTactics.
Import OperationalSemantics.
Import OperationalSemanticsProp.

Definition refinement : Type := amap constant -> constant -> Prop.

Inductive overbasety : Type :=
| BaseOver (B: base_ty) (ϕ: refinement).

Global Hint Constructors overbasety: core.

Inductive underty : Type :=
| BaseUnder (B: base_ty) (ϕ: refinement)
| DependArrow (x: atom) (B: base_ty) (ϕ: refinement) (τ: underty)
| IndependArrow (τ1: underty) (τ2: underty).

Global Hint Constructors underty: core.

Notation "'[v:' B '|' ϕ ']'" := (BaseUnder B ϕ) (at level 80, B constr, ϕ constr).
Notation "'{v:' B '|' ϕ '}'" := (BaseOver B ϕ) (at level 80, B constr, ϕ constr).
Notation " x ':{v:' B '|' ϕ '}' '⤑' τ" := (DependArrow x B ϕ τ) (at level 80, right associativity, B constr, ϕ constr, τ constr).
Notation " τ1 '⤑' τ2 " := (IndependArrow τ1 τ2) (at level 80, right associativity, τ1 constr, τ2 constr).

Definition z := [v: TNat | fun _ _ => True ].
Definition y := {v: TNat | fun _ _ => True }.
Definition aa (x: atom) := x:{v: TNat | fun _ _ => True } ⤑ z.
Definition bb (x: atom) := (z ⤑ z).

Inductive overunderty : Type :=
| Uty: underty -> overunderty
| Oty: overbasety -> overunderty.

Global Hint Constructors overunderty: core.

Coercion Uty : underty >-> overunderty.
Coercion Oty : overbasety >-> overunderty.

Inductive wf_rty: overunderty -> Prop :=
| wf_rty_over_base: forall B ϕ, wf_rty ({v: B | ϕ })
| wf_rty_under_base: forall B ϕ, wf_rty ([v: B | ϕ ])
| wf_rty_oarr: forall x B ϕ (τ: underty),
    wf_rty τ -> wf_rty (x:{v: B | ϕ } ⤑ τ)
| wf_rty_arrarr1: forall x B ϕ (τ1 τ2: underty),
    wf_rty (x:{v: B | ϕ } ⤑ τ1) -> wf_rty τ2 ->
    wf_rty (x:{v: B | ϕ } ⤑ τ1 ⤑ τ2)
| wf_rty_arrarr2: forall (τ11 τ12 τ2: underty),
    wf_rty (τ11 ⤑ τ12) -> wf_rty τ2 ->
    wf_rty (τ11 ⤑ τ12 ⤑ τ2).

Global Hint Constructors wf_rty: core.

Definition over_to_under (ot: overbasety) :=
  match ot with
  | BaseOver t phi => BaseUnder t phi
  end.

Fixpoint underty_erase (ut: underty) : ty :=
  match ut with
  | [v: T | _ ] => T
  | _ :{v: T1 | _ } ⤑ retty => T1 ⤍ (underty_erase retty)
  | t1 ⤑ t2 => (underty_erase t1) ⤍ (underty_erase t2)
  end.

Definition overbasety_erase (aty: overbasety): base_ty :=
  match aty with
  | ({v: T | _ }) => T
  end.

Definition overunderty_erase (aty: overunderty): ty :=
  match aty with
  | Uty ty => underty_erase ty
  | Oty ty => overbasety_erase ty
  end.

Notation " 'u\_' ty '_/' " := (underty_erase ty) (at level 30).
Notation " 'o\_' ty '_/' " := (overbasety_erase ty) (at level 30).
Notation " 'ou\_' ty '_/' " := (overunderty_erase ty) (at level 30).

Inductive eval_cid: state -> cid -> constant -> Prop :=
| eval_cid_const: forall st c, eval_cid st (vconst c) c
| eval_cid_id: forall st id c, st id = Some c -> eval_cid st (vvar id) c.

Definition mk_op_retty_from_cids (op: biop) (cid1 cid2: cid): underty :=
  BaseUnder (ret_ty_of_op op) (fun st c =>
                                 (exists (c1 c2: constant),
                                     eval_cid st cid1 c1 /\ eval_cid st cid2 c2 /\ eval_op op c1 c2 c
                                 )).

(* subst id *)
Definition refinement_subst_id (x1 x2: string) (phi: refinement): refinement :=
  (fun st c => phi (t_update st x1 (st x2)) c).

Definition over_subst_id (x1 x2: string) (oty: overbasety): overbasety :=
  match oty with
  | BaseOver T phi => BaseOver T (refinement_subst_id x1 x2 phi)
  end.

Fixpoint under_subst_id (x1 x2: string) (uty: underty): underty :=
  match uty with
  | BaseUnder T phi => BaseUnder T (refinement_subst_id x1 x2 phi)
  | DependArrow x xty retty =>
      if String.eqb x1 x then
        DependArrow x (over_subst_id x1 x2 xty) retty
      else
        DependArrow x (over_subst_id x1 x2 xty) (under_subst_id x1 x2 retty)
  | IndependArrow t1 t2 =>
      IndependArrow (under_subst_id x1 x2 t1) (under_subst_id x1 x2 t2)
  end.

Definition overunder_subst_id (x1: string) (c2: string) (outy: overunderty): overunderty :=
  match outy with
  | Uty uty => under_subst_id x1 c2 uty
  | Oty oty => over_subst_id x1 c2 oty
  end.

Notation " '<r[' x '|id->' y ']>' uty " := (refinement_subst_id x y uty) (at level 40).
Notation " '<u[' x '|id->' y ']>' uty " := (under_subst_id x y uty) (at level 40).
Notation " '<o[' x '|id->' y ']>' uty " := (over_subst_id x y uty) (at level 40).
Notation " '<ou[' x '|id->' y ']>' uty " := (overunder_subst_id x y uty) (at level 40).

(* subst constant *)
Definition refinement_subst_c (x1: string) (c2: constant) (phi: refinement): refinement :=
  (fun st => phi (update st x1 c2)).

Definition over_subst_c (x1: string) (c2: constant) (oty: overbasety): overbasety :=
  match oty with
  | BaseOver T phi => BaseOver T (refinement_subst_c x1 c2 phi)
  end.

Fixpoint under_subst_c (x1: string) (c2: constant) (uty: underty): underty :=
  match uty with
  | BaseUnder T phi => BaseUnder T (refinement_subst_c x1 c2 phi)
  | DependArrow x xty retty =>
      if String.eqb x1 x then
        DependArrow x (over_subst_c x1 c2 xty) retty
      else
        DependArrow x (over_subst_c x1 c2 xty) (under_subst_c x1 c2 retty)
  | IndependArrow t1 t2 =>
      IndependArrow (under_subst_c x1 c2 t1) (under_subst_c x1 c2 t2)
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

Lemma subst_c_preserve_well_fromed_type:
  forall x c (tau: underty), wf_rty tau -> wf_rty (<u[ x |c-> c ]> tau).
Proof with eauto.
  intros.
  induction tau.
  - simpl...
  - simpl... destruct (eqb_spec x s)...
    constructor... inversion H...
    constructor... inversion H...
  - inversion H; subst.
    + assert (wf_rty (<u[ x |c-> c ]> tau2))...
      assert ( wf_rty (<u[ x |c-> c ]> (a o: tau_a ⤑ tau)))...
      simpl. simpl in H1. destruct (eqb_spec x a); subst; constructor...
    + destruct t1. inversion H2.
      assert (wf_rty (<u[ x |c-> c ]> tau2))...
      assert (wf_rty (<u[ x |c-> c ]> ((s o: o ⤑ t1) ⤑ t2)))...
      constructor...
      assert (wf_rty (<u[ x |c-> c ]> tau2))...
      constructor...
Qed.

Global Hint Resolve subst_c_preserve_well_fromed_type: core.


Definition under_subst_cid (x1: string) (cv: cid) (outy: underty): underty :=
  match cv with
  | vconst c => under_subst_c x1 c outy
  | vvar id => under_subst_id x1 id outy
  end.

Lemma under_subst_cid_preserve_ty: forall a c2 tau_x,
    u\_ under_subst_cid a c2 tau_x _/ = u\_ tau_x _/.
Proof with eauto.
  intros.
  induction tau_x...
  - simpl. destruct c2...
  - destruct o... simpl. destruct c2...
    + unfold under_subst_cid. unfold under_subst_cid in IHtau_x. simpl. simpl in IHtau_x.
      destruct (eqb_spec a s)... simpl... rewrite IHtau_x...
    + simpl. simpl in IHtau_x. destruct (eqb_spec a s)... simpl... rewrite IHtau_x...
  - simpl. destruct c2;simpl; simpl in IHtau_x1; simpl in IHtau_x2.
    + rewrite IHtau_x1. rewrite IHtau_x2...
    + rewrite IHtau_x1. rewrite IHtau_x2...
Qed.

Global Hint Rewrite under_subst_cid_preserve_ty: core.

(* appear free *)

Definition appear_free_in_refinement (name: string) (phi: refinement): Prop :=
  ~ (forall st c1, phi st = phi (update st name c1)).

Definition appear_free_in_overbasety (name: string) (oty: overbasety): Prop :=
  match oty with
  | {v: _ | phi } => appear_free_in_refinement name phi
  end.

Fixpoint appear_free_in_underty (name: string) (uty: underty): Prop :=
  match uty with
  | [v: _ | phi ] => appear_free_in_refinement name phi
  | DependArrow x xty retty =>
      appear_free_in_overbasety name xty \/ (x <> name /\ appear_free_in_underty name retty)
  | IndependArrow t1 t2 =>
      appear_free_in_underty name t1 \/ appear_free_in_underty name t2
  end.

Definition appear_free_in_overunderty (name: string) (overunderty: overunderty): Prop :=
  match overunderty with
  | Uty uty => appear_free_in_underty name uty
  | Oty oty => appear_free_in_overbasety name oty
  end.

Notation " x '\FVty' e " := (appear_free_in_overunderty x e) (at level 40).

(* \S{Ty}: *)

Definition mk_eq_constant c := [v: ty_of_const c | (fun _ v => v = c) ].
Definition mk_bot ty := [v: ty | (fun _ _ => False) ].
Definition mk_top ty := [v: ty | (fun _ _ => True) ].
Definition mk_over_top ty := {v: ty | (fun _ _ => True) }.
Definition mk_eq_var ty name := [v: ty | (fun state v => Some v = (state name)) ].
Definition mk_op_ret op a b:= [v: ret_ty_of_op op |
                              (fun st c => exists c_a c_b, st a = Some c_a /\ st b = Some c_b /\ eval_op op c_a c_b c ) ].
Definition mk_op op a b :=
  (a o: (mk_over_top (fst_ty_of_op op)) ⤑ (b o: (mk_over_top (snd_ty_of_op op)) ⤑ (mk_op_ret op a b))).

Lemma mk_op_has_type1: forall op a b, empty \N- vbiop op \Vin u\_ mk_op op a b _/.
Proof with eauto.
  intro op. destruct op; simpl; intros; constructor...
Qed.
Global Hint Resolve mk_op_has_type1: core.

Lemma mk_op_has_type: forall op Gamma a b, Gamma \N- vbiop op \Vin u\_ mk_op op a b _/.
Proof with eauto.
  intros. assert (empty \N- vbiop op \Tin u\_ mk_op op a b _/)...
  eapply weakening_empty with (Gamma:=Gamma) in H... inversion H...
Qed.

Global Hint Resolve mk_op_has_type: core.


Lemma destruct_tau: forall (tau: overunderty),
    ((exists T phi, tau = {v:T | phi}) \/ (exists T phi, tau = [v:T | phi]))
    \/ ((exists a Ta phia taub, tau = (a o: {v: Ta | phia} ⤑ taub))
    \/ (exists t1 t2, tau = (t1 ⤑ t2))).
Proof with eauto.
  intro tau.
  destruct tau. destruct u.
  - left. right. exists b. exists r...
  - right. destruct o. left. exists s, b, r, u...
  - right. right. exists u1, u2...
  - destruct o. left. left. exists b, r...
Qed.
