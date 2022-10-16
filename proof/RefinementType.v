(** * MoreStlc: More on the Simply Typed Lambda-Calculus *)

Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From PLF Require Import CoreLang.
From PLF Require Import LinearContext.
From Coq Require Import Lists.List.
Import ListNotations.

Ltac invert :=
  match goal with | H : ?T |- _ =>
                      match type of T with Prop => solve [exfalso; apply H; auto]
                      end
  end.


Import CoreLang.
Import LinearContext.

Definition context := linear_context outy.

Definition constant_basic_type_infer (c: constant) :=
  match c with
  | cbool _ => TBool
  | cnat _ => TNat
  | cnil T => TList T
  | cleaf T => TTree T
  | ccons T _ _ => TList T
  | cnode T _ _ _ => TTree T
  end.

Definition name_not_free_in_refinement (name: string) (phi: refinement): Prop :=
  forall state c cself, phi state cself -> phi (t_update state name c) cself.

Fixpoint name_not_free_in_uty (name: string) (uty: uty): Prop :=
  match uty with
  | BaseUnder _ phi =>
      name_not_free_in_refinement name phi
  | DependArrow x (BaseOver _ phi) retty =>
      name_not_free_in_refinement name phi /\
        x = name \/ name_not_free_in_uty name retty
  | IndependArrow t1 t2 =>
      name_not_free_in_uty name t1 /\ name_not_free_in_uty name t2
  end.

Definition name_not_free_in_oty (name: string) (oty: oty): Prop :=
  match oty with
  | (BaseOver _ phi) => name_not_free_in_refinement name phi
  end.

Definition name_not_free_in_outy (name: string) (outy: outy): Prop :=
  match outy with
  | Uty uty => name_not_free_in_uty name uty
  | Oty oty => name_not_free_in_oty name oty
  end.

Definition name_not_free_in_tm (name: string) (tm: tm): Prop.
Admitted.

Definition name_not_free_in_context (name: string) (Gamma: context): Prop.
Admitted.

Definition fresh_name_in_inference (name: string) (Gamma: context) (e: tm) : Prop.
Admitted.

Notation " x '\NewInTm' e " := (name_not_free_in_tm x e) (at level 40).
Notation " x '\NewInCtx' ctx " := (name_not_free_in_context x ctx) (at level 40).
Notation " x '\NewInI' ctx '\and' e " := (fresh_name_in_inference x ctx e) (at level 40).

Definition avaliable_in_context x (Gamma: context) :=
  forall y uty_y, find Gamma y = Some uty_y -> name_not_free_in_outy x uty_y.

Definition context_inv (Gamma: context) :=
  forall x y z uty_x uty_y uty_z,
    find Gamma x = Some uty_x -> find Gamma y = Some uty_y -> find Gamma z = Some uty_z ->
    ~ ( name_not_free_in_outy x uty_y /\ name_not_free_in_outy x uty_z).

Definition mk_eq_constant c := [[v: constant_basic_type_infer c | closed_r (fun v => v = c) ]].
Definition mk_bot ty := [[v: ty | closed_r (fun _ => False) ]].
Definition mk_top ty := [[v: ty | closed_r (fun _ => True) ]].
Definition mk_eq_var ty name := [[v: ty | (fun state v => v = (state name)) ]].

Fixpoint add_to (phi: refinement) (ty: uty): uty :=
  match ty with
  | BaseUnder T phi1 => BaseUnder T (fun st v => phi st v /\ phi1 st v)
  | DependArrow x (BaseOver T1 phi1) retty => DependArrow x (BaseOver T1 phi1) (add_to phi retty)
  | IndependArrow t1 t2 => IndependArrow t1 (add_to phi t2)
  end.

Notation " phi 'u-/\' uty " := (add_to phi uty) (at level 40).

Fixpoint exists_in name (uty: uty) :=
  match uty with
  | BaseUnder T phi => BaseUnder T (fun st v => exists c, phi (t_update st name c) v)
  | DependArrow x (BaseOver T phi) retty =>
      DependArrow x (BaseOver T (fun st v => forall c, phi (t_update st name c) v)) (exists_in name retty)
  | IndependArrow t1 t2 => IndependArrow (exists_in name t1) (exists_in name t2)
  end.

Notation " 'u-exists' x ';' uty " := (exists_in x uty) (at level 40).

Inductive exists_uty_in_uty: string -> uty -> uty -> uty -> Prop:=.

Notation " 'u-E-ty' x ';' xuty ';' uty 'u=' utyy " := (exists_uty_in_uty x xuty uty utyy) (at level 40).

Inductive merge: uty -> uty -> uty -> Prop :=.

Definition apply_op_over_refinements op (phi1 phi2: refinement): refinement :=
  (fun st v =>
     forall n1 n2, phi1 st (cnat n1) -> phi2 st (cnat n2) ->
              v = (apply_op op n1 n2)).

Definition mk_op_retty op (phi1 phi2: refinement) :=
  (BaseUnder (op_ret_ty op) (apply_op_over_refinements op phi1 phi2)).

Definition apply_cons_over_refinements T (phi1 phi2: refinement): refinement :=
  (fun st v =>  forall h t, phi1 st h -> phi2 st t -> v = ccons T h t).

Definition mk_cons_retty T (phi1 phi2: refinement) :=
  (BaseUnder T (apply_cons_over_refinements T phi1 phi2)).

Definition apply_node_over_refinements T (phi1 phi2 phi3: refinement): refinement :=
  (fun st v => forall root lt rt, phi1 st root -> phi2 st lt -> phi3 st rt -> v = cnode T root lt rt).

Definition mk_node_retty T (phi1 phi2 phi3: refinement) :=
  (BaseUnder T (apply_node_over_refinements T phi1 phi2 phi3)).

Definition uty_subst: string -> string -> uty -> uty.
Admitted.

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

Inductive is_subtype : context -> uty -> uty -> Prop :=
| Sub_Base_Base: forall Gamma T phi1 phi2,
    Gamma \C- phi2 ===> phi1 ->
    Gamma \C- [[v: T | phi1 ]] \<: [[v: T | phi2 ]]
| Sub_IndependArrow_IndependArrow: forall Gamma tau11 tau12 tau21 tau22,
    Gamma \C- tau21 \<: tau11 ->
    Gamma \C- tau12 \<: tau22 ->
    Gamma \C- tau11 u--> tau12 \<: tau21 u--> tau22
| Sub_DependArrow_DependArrow: forall Gamma T x1 phi11 tau12 x2 phi21 tau22,
    Gamma \C- [[v: T | phi11 ]] \<: [[v: T | phi21 ]] ->
    (update Gamma x2 ({{v: T | phi21 }})) \C- (uty_subst x1 x2 tau12) \<: tau22 ->
    Gamma \C- x1 o: {{v: T | phi11 }} o--> tau12 \<: x2 o: {{v: T | phi21 }} o--> tau22
| Sub_DependArrow_IndependArrow: forall Gamma T x1 phi11 tau12 phi21 tau22,
    Gamma \C- [[v: T | phi11 ]] \<: [[v: T | phi21 ]] ->
    (update Gamma x1 ([[v: T | phi11 ]])) \C- tau12 \<: tau22 ->
    Gamma \C- x1 o: {{v: T | phi11 }} o--> tau12 \<: [[v: T | phi21 ]] u--> tau22

where "Gamma '\C-' t1 '\<:' t2" := (is_subtype Gamma t1 t2).

Reserved Notation "Gamma '\C-' t '\T-->' T" (at level 40).
Reserved Notation "Gamma '\C-' t '\V-->' T" (at level 40).

Definition const_order: constant -> constant -> Prop.
Admitted.

Lemma const_order_is_well_founded: well_founded const_order.
Admitted.

Inductive term_under_type_infer : context -> tm -> uty -> Prop :=
| UT_Infer_Random: forall Gamma, Gamma \C- trandom \T--> (mk_top TNat)
| UT_Infer_Value: forall Gamma v uty, Gamma \C- v \V--> uty -> Gamma \C- (tvalue v) \T--> uty
| UT_Infer_Lete: forall Gamma y e_y uty_y e uty uty',
    Gamma \C- e_y \T--> uty_y ->
    (y |-c> uty_y ; Gamma) \C- e \T--> uty ->
    u-E-ty y ; uty_y ; uty u= uty' ->
                       Gamma \C- (tlete y e_y e) \T--> uty'
| UT_Infer_LetOp: forall Gamma y op v1 v2 phi1 phi2 uty_y e uty uty',
    Gamma \C- v1 \V--> [[v: TNat | phi1 ]] ->
    Gamma \C- v2 \V--> [[v: TNat | phi2 ]] ->
    ( y |-c> (mk_op_retty op phi1 phi2) ; Gamma) \C- e \T--> uty ->
    u-E-ty y ; uty_y ; uty u= uty' ->
                       Gamma \C- (tletbiop y op v1 v2 e) \T--> uty'
| UT_Infer_LetCons: forall Gamma T y v1 v2 phi1 phi2 uty_y e uty uty',
    Gamma \C- v1 \V--> [[v: T | phi1 ]] ->
    Gamma \C- v2 \V--> [[v: TList T | phi2 ]] ->
    (y |-c> (mk_cons_retty T phi1 phi2) ; Gamma) \C- e \T--> uty ->
    u-E-ty y ; uty_y ; uty u= uty' ->
                       Gamma \C- (tletcons y T v1 v2 e) \T--> uty'
| UT_Infer_LetNode: forall Gamma T y v1 v2 v3 phi1 phi2 phi3 uty_y e uty uty',
    Gamma \C- v1 \V--> [[v: T | phi1 ]] ->
    Gamma \C- v2 \V--> [[v: TList T | phi2 ]] ->
    Gamma \C- v3 \V--> [[v: TList T | phi3 ]] ->
    (y |-c> (mk_node_retty T phi1 phi2 phi3) ; Gamma) \C- e \T--> uty ->
    u-E-ty y ; uty_y ; uty u= uty' ->
                       Gamma \C- (tletnode y T v1 v2 v3 e) \T--> uty'
| UT_Infer_LetAppDepend: forall Gamma y x' v_f v_x x xargoty retty uty_x e uty uty' uty'',
    x' <> x ->
    x' \NewInI Gamma \and (tletapp y v_f v_x e) ->
    Gamma \C- v_f \V--> (x o: xargoty o--> retty) ->
    Gamma \C- v_x \V--> uty_x ->
    Gamma \C- (ot_to_ut xargoty) \<: uty_x ->
    (y |-c> (uty_subst x x' retty) ; (x' |-c> uty_x ; Gamma)) \C- e \T--> uty ->
    u-E-ty y ; (uty_subst x x' retty) ; uty u= uty' ->
                                        u-E-ty x' ;  uty_x ; uty' u= uty'' ->
                                                             Gamma \C- (tletapp y v_f v_x e) \T--> uty''

| UT_Infer_LetAppIndepend: forall Gamma y x' v_f v_x xarguty retty uty_x e uty uty' uty'',
    x' \NewInI Gamma \and (tletapp y v_f v_x e) ->
    Gamma \C- v_f \V--> (xarguty u--> retty) ->
    Gamma \C- v_x \V--> uty_x ->
    Gamma \C- uty_x \<: xarguty ->
    (y |-c> retty; (x' |-c> uty_x ; Gamma)) \C- e \T--> uty ->
    u-E-ty y ; retty ; uty u= uty' ->
                       u-E-ty x' ;  uty_x ; uty' u= uty'' ->
                                            Gamma \C- (tletapp y v_f v_x e) \T--> uty''

| UT_Infer_Ite: forall Gamma v e1 e2 uty_v x' uty1 uty2 uty,
    x' \NewInI Gamma \and (tite v e1 e2)->
    Gamma \C- v \V--> uty_v ->
    (x' |-c> ((fun _ v => v = true) u-/\ uty_v) ; Gamma) \C- e1 \T--> uty1 ->
    (x' |-c> ((fun _ v => v = false) u-/\ uty_v) ; Gamma) \C- e2 \T--> uty2 ->
    merge ( (fun st _ => (st x') = true) u-/\ uty1) ( (fun st _ => (st x') = false) u-/\ uty2) uty ->
    Gamma \C- (tite v e1 e2) \T--> uty

with value_under_type_infer : context -> value -> uty -> Prop :=
| UT_Infer_Contant: forall Gamma c, Gamma \C- (vconst c) \V--> (mk_eq_constant c)
| UT_Infer_VarUnderBase: forall Gamma x T phi,
    find Gamma x = Some (Uty (BaseUnder T phi)) ->
    avaliable_in_context x Gamma ->
    Gamma \C- (vvar x) \V--> (mk_eq_var T x)
| UT_Infer_VarDependArrow: forall Gamma x y oty uty,
    find Gamma x = Some (Uty (DependArrow y oty uty)) ->
    Gamma \C- (vvar x) \V--> (DependArrow y oty uty)
| UT_Infer_VarIndependArrow: forall Gamma x t1 t2,
    find Gamma x = Some (Uty (IndependArrow t1 t2)) ->
    Gamma \C- (vvar x) \V--> (IndependArrow t1 t2)
| UT_Infer_VarOverBase: forall Gamma x T phi,
    find Gamma x = Some (Oty (BaseOver T phi)) ->
    Gamma \C- (vvar x) \V--> (mk_eq_var T x)
| UT_Infer_LamOver: forall Gamma x tau_x e retty,
    ((x |-c> Oty tau_x ; Gamma)) \C- e \T--> retty ->
    Gamma \C- (vlam x tau_x e) \V--> (x o: tau_x o--> retty)
| UT_Infer_LamUnder: forall Gamma x tau_x e retty retty',
    ((x |-c> Uty tau_x ; Gamma)) \C- e \T--> retty ->
    (u-E-ty x ; tau_x ; retty u= retty') ->
    Gamma \C- (vlam x tau_x e) \V--> tau_x u--> retty'
| UT_Infer_Fix: forall Gamma f x' x phi_x e tau,
    (f |-c> Uty (x' o: {{v: TNat | fun st v => const_order v (st x) /\  phi_x st v }} o--> tau) ;
     (x |-c> Oty ({{v: TNat | phi_x }}) ;
      Gamma)) \C- e \T--> tau ->
    Gamma \C- (vfix f (x' o: {{v: TNat | phi_x }} o--> tau) x ({{v: TNat | phi_x }}) e)
                \V--> (x' o: {{v: TNat | phi_x }} o--> tau)
| UT_Infer_Exn: forall Gamma T, Gamma \C- vexn \V--> (mk_bot T)
where
"Gamma '\C-' t '\T-->' T" := (term_under_type_infer Gamma t T) and "Gamma '\C-' t '\V-->' T" := (value_under_type_infer Gamma t T).

Reserved Notation "Gamma '\C-' t '\V<--' T" (at level 40).

Inductive value_under_type_check : context -> value -> uty -> Prop :=
| UT_Check: forall Gamma e tau tau',
    Gamma \C- e \V--> tau -> Gamma \C- tau \<: tau' -> Gamma \C- e \V<-- tau'

where "Gamma '\C-' t '\V<--' T" := (value_under_type_check Gamma t T).
