(** * MoreStlc: More on the Simply Typed Lambda-Calculus *)

Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From PLF Require Import CoreLangSimp.
From PLF Require Import NormalTypeSystemSimp.
From PLF Require Import LinearContext.
From PLF Require Import AppFreeSimp.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Lists.List.

Import CoreLangSimp.
Import LinearContext.
Import AppFreeSimp.
Import ListNotations.

Definition empstate: state := t_empty (cbool false).

Definition context := linear_context overunderty.

Definition empty_ctx: context := nil.

(* Need defined reversely *)
Fixpoint context_subst (id:string) (c: constant) (Gamma: context) : context :=
  match Gamma with
  | nil => Gamma
  | (x, xty) :: Gamma =>
      if String.eqb x id
      then
        (x, overunder_subst_c id c xty) :: Gamma
      else
        (x, overunder_subst_c id c xty) :: (context_subst id c Gamma)
  end.

Definition constant_basic_type_infer (c: constant) :=
  match c with
  | cbool _ => TBool
  | cnat _ => TNat
  end.

Inductive context_inv: context -> Prop :=
| context_inv_nil: context_inv nil
| context_inv_under_base: forall x (T:basic_ty) (phi: refinement) Gamma c_x,
    empty |- vconst c_x \Vin T ->
    phi empstate c_x ->
    context_inv (context_subst x c_x Gamma) ->
    context_inv ((x, Uty ([[v: T | phi]])) :: Gamma)
| context_inv_over_base: forall x (T: basic_ty) (phi: refinement) Gamma,
    (forall c_x, empty |- vconst c_x \Vin T -> phi empstate c_x -> context_inv (context_subst x c_x Gamma)) ->
    context_inv ((x, Oty ({{v: T | phi}})) :: Gamma)
| context_inv_dep_arrow: forall x a aty bty Gamma,
    l_find_right_most Gamma x = None ->
    context_inv Gamma -> context_inv ((x, Uty (a o: aty o--> bty)) :: Gamma)
| context_inv_indep_arrow: forall x aty bty Gamma,
    l_find_right_most Gamma x = None ->
    context_inv Gamma -> context_inv ((x, Uty (aty u--> bty)) :: Gamma).

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

(* Definition context_inv (Gamma: context) := *)
(*   forall x y z uty_x uty_y uty_z, *)
(*     l_find_right_most Gamma x = Some uty_x -> l_find_right_most Gamma y = Some uty_y -> l_find_right_most Gamma z = Some uty_z -> *)
(*     ~ ( x \InFV uty_y /\ x \InFV uty_z). *)

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

Lemma constant_eq_is_close: forall x c, ~ x \FVty mk_eq_constant c.
Proof.
  intros.
  simpl. unfold appear_free_in_refinement. unfold not_appear_free_in_refinement.
  assert (state -> state -> forall c0 : constant, c0 = c <-> c0 = c); auto. split; auto.
Qed.

Global Hint Resolve mk_eq_constant_spec: core.
Global Hint Resolve subst_constant_eq: core.
Global Hint Resolve constant_eq_is_close: core.

Definition phi_is_bot (phi: refinement):= forall st c, ~ phi st c.

(* logical relation *)

Definition closed_refinement (phi: refinement): Prop:= forall st st', phi st = phi st'.

Global Hint Unfold closed_refinement: core.

Definition is_constant (v: value) :=
  match v with
  | vconst _ => True
  | _ => False
  end.

Inductive is_application: tm -> tm -> tm -> Prop :=
| Is_application: forall (e1 e2: tm) (x f y: string),
    x <> f -> f <> y -> y <> x ->
    ~ f \FVtm e1 -> ~ f \FVtm e2 ->
    ~ x \FVtm e1 -> ~ x \FVtm e2 ->
    ~ y \FVtm e1 -> ~ y \FVtm e2 ->
    is_application e1 e2 (tlete f e1 (tlete x e2 (tletapp y f x y))).

(* type denotation *)
Definition over_tmR (ty:overty) (e:tm) : Prop :=
  empty |- e \in o\_ ty _/ /\
                  (match ty with
                   | {{v: T | phi }} =>
                       closed_refinement phi /\
                         (forall (v: value) (c: constant),
                             empty |- v \Vin T -> v = vconst c ->
                                     (e -->* v /\ phi empstate c))
                   end).

Fixpoint under_tmR_aux (ty:underty) (e:tm) : Prop :=
  empty |- e \in u\_ ty _/ /\
                  (match ty with
                   | [[v: T | phi ]] =>
                       closed_refinement phi /\
                         (forall (c: constant),
                             empty |- vconst c \Vin T ->
                                     (phi empstate c -> e -->* (vconst c))
                         )
                   | t1 u--> t2 =>
                       forall (e_x: tm),
                         under_tmR_aux t1 e_x ->
                         (forall e3, is_application e e_x e3 -> under_tmR_aux t2 e3)
                   | x o: t1 o--> t2 => False
                   end).

Definition under_tmR (ty:underty) (e:tm) : Prop :=
  empty |- e \in u\_ ty _/ /\
                  (match ty with
                   | [[v: _ | _ ]] => under_tmR_aux ty e
                   | _ u--> _ => under_tmR_aux ty e
                   | x o: t1 o--> t2 =>
                       forall (e_x: tm),
                         over_tmR t1 e_x ->
                         (forall (c: constant), e -->* (vconst c) ->
                                           (forall e3, is_application e (vconst c) e3 ->
                                                  under_tmR_aux (underty_subst x c t2) e3 ))
                   end).

Definition tmR (ty:overunderty) (e:tm) : Prop :=
  match ty with
  | Uty ty => under_tmR ty e
  | Oty ty => over_tmR ty e
  end.

Lemma tmR_constant (c: constant): tmR (mk_eq_constant c) (vconst c).
Proof with eauto.
  split...
  split... simpl...
  split...
  intros; subst...
Qed.

Global Hint Resolve tmR_constant: core.

Inductive tmR_in_ctx: context -> underty -> tm -> Prop :=
| tmR_in_ctx_nil: forall (ty: underty) e, tmR ty e -> tmR_in_ctx [] ty e
| tmR_in_ctx_cons_under: forall (x: string) (T:basic_ty) (phi: refinement) (Gamma: context) ty e,
    (forall e_x, under_tmR ([[v: T | phi]]) e_x ->
            exists (c_x: constant), empty |- (vconst c_x) \Vin T /\ e_x -->* (vconst c_x) /\
                                 tmR_in_ctx (context_subst x c_x Gamma)
                                            (<u[ x |c-> c_x ]> ty)
                                            (subst x (vconst c_x) e)) ->
    tmR_in_ctx ((x, (Uty ([[v: T | phi]]))) :: Gamma) ty e
| tmR_in_ctx_cons_over: forall (x: string) (T:basic_ty) (phi: refinement) (Gamma: context) ty e,
    (forall e_x, over_tmR ({{v: T | phi}}) e_x ->
            forall (c_x: constant), empty |- (vconst c_x) \Vin T -> e_x -->* (vconst c_x) ->
                               tmR_in_ctx (context_subst x c_x Gamma)
                                          (underty_subst x c_x ty)
                                          (subst x (vconst c_x) e)) ->
    tmR_in_ctx ((x, (Oty ({{v: T | phi}}))) :: Gamma) ty e
| tmR_in_ctx_cons_ind_arrow: forall (x: string) (t1 t2: underty) (Gamma: context) (ty: underty) e,
    l_find_right_most Gamma x = None -> ~ x \FVty ty ->
    (forall v, under_tmR (t1 u--> t2) (tvalue v) ->
            tmR_in_ctx Gamma ty (subst x v e)) ->
    tmR_in_ctx ((x, Uty (t1 u--> t2)) :: Gamma) ty e
| tmR_in_ctx_cons_d_arrow: forall (x: string) (y: string) (t1: overty) (t2: underty) (Gamma: context) (ty: underty) e,
    l_find_right_most Gamma x = None -> ~ x \FVty ty ->
    (forall v, under_tmR (y o: t1 o--> t2) (tvalue v) ->
          tmR_in_ctx Gamma ty (subst x v e)) ->
    tmR_in_ctx ((x, Uty (y o: t1 o--> t2)) :: Gamma) ty e
.

Global Hint Constructors tmR_in_ctx: core.

(* match Gamma with *)
(* | nil => True *)
(* | (x, ([[v: T | phi]])) :: Gamma => *)
(*     (forall e_x, under_tmR ([[v: T | phi]]) e_x -> *)
(*             exists (c_x: constant), e_x -->* (vconst c_x) /\ context_inv (context_subst x c_x Gamma)) *)
(* | (x, ({{v: T | phi}})) :: Gamma => *)
(*     (forall e_x, over_tmR ({{v: T | phi}}) e_x -> *)
(*             forall (c_x: constant), e_x -->* (vconst c_x) -> context_inv (context_subst x c_x Gamma)) *)
(* | _ :: Gamma => context_inv Gamma *)
(* end. *)


(* type operators *)

Definition close_ctx_in_refinement (id: string) (phi1 phi2: refinement) :=
  fun st v => exists id_c, phi1 (t_update st id id_c) v /\ phi2 (t_update st id id_c) v.

Inductive close_ctx: string -> underty -> underty -> underty -> Prop :=
| close_ctx_base_base: forall id T phi1 phi2,
    close_ctx id ([[v: T | phi1 ]]) ([[v: T | phi2 ]]) ([[v: T | close_ctx_in_refinement id phi1 phi2 ]])
| close_ctx_base_dep_arrow: forall id T phi1 x xty retty retty',
    close_ctx id ([[v: T | phi1 ]]) retty retty ->
    close_ctx id ([[v: T | phi1 ]]) (x o: xty o--> retty) (x o: xty o--> retty')
| close_ctx_base_indep_arrow: forall id T phi1 t1 t2 t1' t2',
    close_ctx id ([[v: T | phi1 ]]) t1 t1' ->
    close_ctx id ([[v: T | phi1 ]]) t2 t2' ->
    close_ctx id ([[v: T | phi1 ]]) (t1 u--> t2) (t1' u--> t2')
| close_ctx_dep_arr_any: forall id x xty retty t2, close_ctx id (x o: xty o--> retty) t2 t2
| close_ctx_indep_arr_any: forall id t1 t2 t3, close_ctx id (t1 u--> t2) t3 t3.

Notation " \Close x '\:' xty '->>' t1 'ty=' t2 " := (close_ctx x xty t1 t2) (at level 40).

Lemma close_ctx_spec: forall x (xty: underty) ty ty',
    close_ctx x xty ty ty' ->
    (forall (Gamma: context) (e: tm),
        tmR_in_ctx Gamma ty' e ->
        tmR_in_ctx (Gamma ++ ((x, Uty xty) :: nil)) ty e).
Admitted.

Definition disj_refinements (phi1 phi2: refinement) := fun st v => phi1 st v \/ phi2 st v.
Definition conj_refinements (phi1 phi2: refinement) := fun st v => phi1 st v /\ phi2 st v.

Inductive filter_by: underty -> overty -> underty -> Prop :=
  | filter_by_base_base: forall T phi1 phi2,
    filter_by ([[v: T | phi1 ]]) ({{v: T | phi2 }}) ([[v: T | conj_refinements phi1 phi2 ]]).

Inductive merge: underty -> underty -> underty -> Prop :=
| merge_base_base: forall T phi1 phi2,
    merge ([[v: T | phi1 ]]) ([[v: T | phi2 ]]) ([[v: T | disj_refinements phi1 phi2 ]])
| merge_dep_arrow_dep_arrow: forall T phi1 phi2 x1 (retty1:underty) x2 (retty2: underty) retty3 x3,
    ~ x3 \FVty retty1 -> ~ x3 \FVty retty2 ->
    merge (<u[ x1 |x-> x3 ]> retty1) (<u[ x2 |x-> x3 ]> retty2) retty3 ->
    merge (x1 o: {{v: T | phi1 }} o--> retty1) (x2 o: {{v: T | phi2 }} o--> retty2)
          (x3 o: {{v: T | conj_refinements phi1 phi2 }} o--> retty3)
| merge_dep_arrow_indep_arrow: forall t11 t12 t13 t21 t22 t23,
    merge t11 t12 t13 ->
    merge t21 t22 t23 ->
    merge (t11 u--> t21) (t12 u--> t22) (t13 u--> t23).

Notation " t1 '\Tyor' t2 'ty=' t3 " := (merge t1 t2 t3) (at level 40).

Lemma merge_spec: forall t1 t2 t3,
    merge t1 t2 t3 ->
    (forall Gamma e, tmR_in_ctx Gamma t3 e ->
                (tmR_in_ctx Gamma t1 e /\ tmR_in_ctx Gamma t2 e)).
Admitted.

Definition apply_op_over_refinements op (phi1 phi2: refinement): refinement :=
  (fun st v =>
     forall n1 n2, phi1 st (cnat n1) -> phi2 st (cnat n2) ->
              v = (apply_op op n1 n2)).

Definition mk_op_retty op (phi1 phi2: refinement) :=
  (BaseUnder (op_ret_ty op) (apply_op_over_refinements op phi1 phi2)).

Fixpoint phi_is_subtype_aux (Gamma: context) (phi1: refinement) (phi2: refinement) :=
  match Gamma with
  | nil => (phi1, phi2)
  | cons (_, Uty (DependArrow _ _ _)) Gamma => phi_is_subtype_aux Gamma phi1 phi2
  | cons (_, Uty (IndependArrow _ _)) Gamma => phi_is_subtype_aux Gamma phi1 phi2
  | cons (x, Uty (BaseUnder _ phi_x)) Gamma =>
      (let (phi1, phi2) := phi_is_subtype_aux Gamma phi1 phi2 in
       ((fun st v => exists v_x, phi_x (x !-> v_x; st) v /\ phi1 (x !-> v_x; st) v),
         (fun st v => exists v_x, phi_x (x !-> v_x; st) v /\ phi2 (x !-> v_x; st) v))
      )
  | cons (x, Oty (BaseOver _ phi_x)) Gamma =>
      (let (phi1, phi2) := phi_is_subtype_aux Gamma phi1 phi2 in
       ((fun st v => forall v_x, phi_x (x !-> v_x; st) v -> phi1 (x !-> v_x; st) v),
         (fun st v => forall v_x, phi_x (x !-> v_x; st) v -> phi2 (x !-> v_x; st) v))
      )
  end.

Definition phi_is_subtype (Gamma: context) (phi1: refinement) (phi2: refinement) :=
  let (phi1, phi2) := phi_is_subtype_aux Gamma phi1 phi2 in
  forall st v, phi1 st v -> phi2 st v.

Notation "Gamma '\C-' phi1 '===>' phi2" := (phi_is_subtype Gamma phi1 phi2)  (at level 40).

Reserved Notation "Gamma '\C-' t1 '\<:' t2"  (at level 40).

Inductive is_subtype : context -> underty -> underty -> Prop :=
| Sub_Base_Base: forall Gamma T phi1 phi2,
    Gamma \C- phi2 ===> phi1 ->
    Gamma \C- [[v: T | phi1 ]] \<: [[v: T | phi2 ]]
| Sub_IndependArrow_IndependArrow: forall Gamma tau11 tau12 tau21 tau22,
    Gamma \C- tau21 \<: tau11 ->
    Gamma \C- tau12 \<: tau22 ->
    Gamma \C- tau11 u--> tau12 \<: tau21 u--> tau22
| Sub_DependArrow_DependArrow: forall Gamma T x1 phi11 tau12 x2 phi21 tau22,
    Gamma \C- [[v: T | phi11 ]] \<: [[v: T | phi21 ]] ->
    (Gamma <l> x2 :l: ({{v: T | phi21 }})) \C- (under_subst_id x1 x2 tau12) \<: tau22 ->
    Gamma \C- x1 o: {{v: T | phi11 }} o--> tau12 \<: x2 o: {{v: T | phi21 }} o--> tau22

where "Gamma '\C-' t1 '\<:' t2" := (is_subtype Gamma t1 t2).

Lemma is_subtype_spec: forall Gamma t1 t2,
    is_subtype Gamma t1 t2 ->
    (forall e, tmR_in_ctx Gamma t1 e -> tmR_in_ctx Gamma t2 e).
Admitted.

Definition const_order: constant -> constant -> Prop.
Admitted.

Lemma const_order_is_well_founded: well_founded const_order.
Admitted.

Reserved Notation "Gamma '\C-' t '\T-->' T" (at level 40).
Reserved Notation "Gamma '\C-' t '\V-->' T" (at level 40).

Definition comsume (Gamma: context) (v: value) (Gamma': context): Prop.
Admitted.

Inductive term_under_type_infer : context -> tm -> underty -> Prop :=
| UT_Infer_Value: forall Gamma v uty, Gamma \C- v \V--> uty -> Gamma \C- (tvalue v) \T--> uty
| UT_Infer_Lete: forall (Gamma: context) y e_y (uty_y: underty) e uty uty',
    empty_ctx \C- e_y \T--> uty_y ->
    (Gamma <l> y :l: uty_y) \C- e \T--> uty ->
    \Close y \: uty_y ->> uty ty= uty' ->
                       Gamma \C- (tlete y e_y e) \T--> uty'
| UT_Infer_LetOp: forall Gamma y op v1 v2 phi1 phi2 uty_y e uty uty',
    Gamma \C- v1 \V--> [[v: TNat | phi1 ]] ->
    Gamma \C- v2 \V--> [[v: TNat | phi2 ]] ->
    (Gamma <l> y :l: (mk_op_retty op phi1 phi2) ) \C- e \T--> uty ->
    \Close y \: uty_y ->> uty ty= uty' ->
                       Gamma \C- (tletbiop y op v1 v2 e) \T--> uty'
| UT_Infer_LetAppIndepend: forall Gamma Gamma' y v_f v_x aty bty uty_x e uty uty',
    Gamma \C- v_f \V--> (aty u--> bty) ->
    Gamma \C- v_x \V--> uty_x ->
    Gamma \C- uty_x \<: aty ->
    comsume Gamma v_x Gamma' ->
    (Gamma' <l> y :l: bty) \C- e \T--> uty ->
    \Close y \: bty ->> uty ty= uty' ->
    Gamma \C- (tletapp y v_f v_x e) \T--> uty'

| UT_Infer_LetAppDepend: forall (Gamma: context) y a v_f v_x x aty aty' bty uty_x e uty uty' uty'',
    a <> x -> a <> y ->
    l_find_right_most Gamma a = None ->
    ~ a \FVtm (tletapp y v_f v_x e) ->
    Gamma \C- v_f \V--> (x o: aty o--> bty) ->
    Gamma \C- v_x \V--> uty_x ->
    filter_by uty_x aty aty' ->
    ((Gamma <l> a :l: aty') <l> y :l: (Uty (under_subst_id x a bty))) \C- e \T--> uty ->
    \Close y \: (under_subst_id x a bty) ->> uty ty= uty' ->
    \Close a \: aty' ->> uty' ty= uty'' ->
    Gamma \C- (tletapp y v_f v_x e) \T--> uty''

with value_under_type_infer : context -> value -> underty -> Prop :=
| UT_Infer_Contant: forall Gamma c, context_inv Gamma -> Gamma \C- (vconst c) \V--> (mk_eq_constant c)
| UT_Infer_VarUnderBase: forall Gamma x T phi,
    l_find_right_most Gamma x = Some (Uty (BaseUnder T phi)) ->
    Gamma \C- (vvar x) \V--> (mk_eq_var T x)
| UT_Infer_VarOverBase: forall Gamma x T phi,
    l_find_right_most Gamma x = Some (Oty (BaseOver T phi)) ->
    Gamma \C- (vvar x) \V--> (mk_eq_var T x)
| UT_Infer_VarDependArrow: forall Gamma x y oty uty,
    l_find_right_most Gamma x = Some (Uty (DependArrow y oty uty)) ->
    Gamma \C- (vvar x) \V--> (DependArrow y oty uty)
| UT_Infer_VarIndependArrow: forall Gamma x t1 t2,
    l_find_right_most Gamma x = Some (Uty (IndependArrow t1 t2)) ->
    Gamma \C- (vvar x) \V--> (IndependArrow t1 t2)
| UT_Infer_LamOver: forall Gamma x x' tau_x e retty,
    (Gamma <l> x :l: Oty tau_x) \C- e \T--> (under_subst_id x' x retty) ->
    Gamma \C- (vlam x tau_x e) \V--> (x' o: tau_x o--> retty)
| UT_Infer_LamUnder: forall Gamma x (tau_x: underty) e retty,
    (Gamma <l> x :l: Uty tau_x) \C- e \T--> retty ->
    Gamma \C- (vlam x tau_x e) \V--> tau_x u--> retty
| UT_Infer_Exn: forall Gamma T, Gamma \C- vexn \V--> (mk_bot T)
where
"Gamma '\C-' t '\T-->' T" := (term_under_type_infer Gamma t T) and "Gamma '\C-' t '\V-->' T" := (value_under_type_infer Gamma t T).

Reserved Notation "Gamma '\C-' t '\V<--' T" (at level 40).

Scheme value_under_type_infer_rec := Induction for value_under_type_infer Sort Prop
    with term_under_type_infer_rec := Induction for term_under_type_infer Sort Prop.

Inductive value_under_type_check : context -> value -> underty -> Prop :=
| UT_Check: forall Gamma e tau tau',
    Gamma \C- e \V--> tau -> Gamma \C- tau \<: tau' -> Gamma \C- e \V<-- tau'

where "Gamma '\C-' t '\V<--' T" := (value_under_type_check Gamma t T).

Lemma type_check_implies_denotation_constant: forall Gamma (c : constant),
    context_inv Gamma -> tmR_in_ctx Gamma (mk_eq_constant c) (vconst c).
Proof with eauto.
  intros Gamma c Hinv. generalize dependent c.
  induction Hinv; intros...
  - constructor... intros e_x He_x; subst.
    exists c_x. split... inversion He_x; subst... inversion H2; subst... destruct H4...
    simpl...
  - constructor... intros e_x He_x c_x HcT Hstep; subst.
    simpl... setoid_rewrite subst_constant_eq.
    inversion He_x; subst. simpl in H2. destruct H2.
    apply H0...
    apply H3 with (c:=c_x) in HcT... destruct HcT...
  - constructor... simpl...
  - constructor... simpl...
Qed.

Theorem type_check_implies_denotation: forall (Gamma: context) (e: tm) (uty:underty),
    Gamma \C- e \T--> uty -> tmR_in_ctx Gamma uty e.
Proof with eauto.
  apply (term_under_type_infer_rec
           (fun Gamma v uty H => tmR_in_ctx Gamma uty (tvalue v))
           (fun Gamma e uty H => tmR_in_ctx Gamma uty e)); intros Gamma.
  (* constant *)
  - apply type_check_implies_denotation_constant.
  (* var under base *)
  - induction Gamma.
    + admit.
    + intros x T phi Hvar Ha. destruct a as (a & aty).
      destruct (eqb_spec x a).
      { subst. simpl in Ha. }

    intros T phi Hvar. generalize Hvar. induction Gamma.
    + admit.
    + 

      inversion Hvar.
    + 
    admit.
  (* var dep arrow *)
  - admit.
    (* var indep arrow *)
  - admit.
  (* var over base *)
  - admit.
  - constructor.
Admitted.


(* Theorem type_check_implies_denotation: forall (uty:underty) (e: tm) (Gamma: context), *)
(*     Gamma \C- e \T--> uty -> tmR_in_ctx Gamma uty e. *)
(* Proof with eauto. *)
(*   intro uty. *)
(*   induction uty. *)
(*   (* base ty *) *)
(*   - intro e. *)
(*     apply (tm_mutual_rec *)
(*              (fun v => forall Gamma : context, Gamma \C- v \V--> [[v:b | r]] -> tmR_in_ctx Gamma ([[v:b | r]]) (tvalue v)) *)
(*              (fun e => forall Gamma : context, Gamma \C- e \T--> [[v:b | r]] -> tmR_in_ctx Gamma ([[v:b | r]]) e) *)
(*           ); intros. *)
(*     (* constant *) *)
(*     + admit. *)
(*     (* var *) *)
(*     + admit. *)
(*     (* lam *) *)
(*     + inversion H0. *)
(*     (* exn *) *)
(*     + admit. *)
(*     (* value *) *)
(*     + inversion H0; subst... *)
(*     + inversion H1; subst. *)
(*       apply H in H6. *)
(*       apply close_ctx_spec with (Gamma:=Gamma) (e:=e) in H9. apply H0 in H8. *)
    

(*   -  *)
(*   intro e. *)
(*   apply (tm_mutual_rec *)
(*            (fun v => forall (Gamma : context) (uty : underty), Gamma \C- v \V--> uty -> tmR_in_ctx Gamma uty (tvalue v)) *)
(*            (fun e => forall (Gamma : context) (uty : underty), Gamma \C- e \T--> uty -> tmR_in_ctx Gamma uty e) *)
(*         ); intros. *)
(*   (* constant *) *)
(*     - inversion H; subst. admit. *)
(*     (* var *) *)
(*     - inversion H; subst. *)
(*       (* under base var *) *)
(*       + admit. *)
(*       +  *)
(*       rewrite find_empty_ctx in H5. inversion H5. *)
(*       rewrite find_empty_ctx in H4. inversion H4. *)
(*     - inversion H0; subst. *)
(*     - inversion H; subst. *)
(*       rewrite feq_drop_first in H4. subst. inversion H0. *)
(*     - assert (v = vconst c); subst; eauto. *)
(*       apply H with (T:=T) (phi:=phi)... inversion H0; subst... *)
(*     - inversion H1; subst. *)
(*       eapply H with (T:= u\_ uty_y _/) in H7... *)

Lemma soundness: forall (e: tm) (T:basic_ty) (phi: constant -> Prop),
    l_empty \C- e \T--> [[v: T | fun _ c => phi c ]] ->
    forall (c: constant), empty |- (vconst c) \Vin T -> phi c -> e -->* vconst c .
Proof with eauto.
  intros.
  apply type_check_implies_denotation in H.
  inversion H; subst.
  inversion H2; subst. simpl in H3, H4.
  destruct H4 as (_ & _ & Hprop).
  apply Hprop; auto.
Qed.

(*   unfold closed_refinement in Hclosed. *)
(*   intro e. *)
(*   apply (tm_mutual_rec *)
(*            (fun e => forall (T : basic_ty) (phi : constant -> Prop), *)
(*                empty \C- e \V--> [[v:T | fun _ => phi]] -> forall c : constant, phi c -> e = vconst c) *)
(*            (fun e => forall (T : basic_ty) (phi : constant -> Prop), *)
(*                empty \C- e \T--> [[v:T | fun _ => phi]] -> forall c : constant, phi c -> e -->* vconst c)); intros. *)
(*   (* constant *) *)
(*   - inversion H; subst. rewrite feq_drop_first in H5; subst... subst... *)
(*   (* var *) *)
(*   - inversion H; subst. *)
(*     rewrite find_empty_ctx in H5. inversion H5. *)
(*     rewrite find_empty_ctx in H4. inversion H4. *)
(*   - inversion H0; subst. *)
(*   - inversion H; subst. *)
(*     rewrite feq_drop_first in H4. subst. inversion H0. *)
(*   - assert (v = vconst c); subst; eauto. *)
(*     apply H with (T:=T) (phi:=phi)... inversion H0; subst... *)
(*   - inversion H1; subst. *)
(*     eapply H with (T:= u\_ uty_y _/) in H7... *)

(*     assert (tlete s t t0 -->* vconst c) *)

(* Lemma feq_drop_first: forall (f g: constant -> Prop), (fun (_: state) => f) = (fun (_: state) => g) <-> (f = g). *)
(* Proof. *)
(*   split; intros. *)
(*   - assert (f = (fun _ : state => f) empst); auto. rewrite H in H0; auto. *)
(*   - subst. apply functional_extensionality. auto. *)
(* Qed. *)


