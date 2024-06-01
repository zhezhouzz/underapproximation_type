From stdpp Require Import mapset.
From CT Require Import CoreLangClass.
From CT Require Import OperationalSemantics.
From CT Require Import BasicTypingClass.
From CT Require Import RefinementTypeClass.
From CT Require Import Denotation.
From CT Require Import DenotationCtx.
From CT Require Import InstantiationProp.

Import Atom.
Import CoreLang.
Import Tactics.
Import NamelessTactics.
Import ListCtx.
Import OperationalSemantics.
Import BasicTyping.
Import RefinementType.
Import Denotation.
Import DenotationCtx.
Import Instantiation.
Import Qualifier.

(** This file formalizes the type system of λᴱ and the main metatheoretic
  results. *)

Inductive wf_rty: listctx rty -> rty -> Prop :=
| WfBaseOver: forall Γ b ϕ,
    closed_rty (ctxdom Γ) {: b | ϕ } -> wf_rty Γ {: b | ϕ }
| WfBaseUnder: forall Γ b ϕ,
    closed_rty (ctxdom Γ) [: b | ϕ ] -> wf_rty Γ [: b | ϕ ]
| WfArr: forall Γ ρ τ (L: aset),
    wf_rty Γ ρ ->
    (forall x, x ∉ L -> wf_rty (Γ ++ [(x, ρ)]) (τ ^^ x)) ->
    wf_rty Γ (ρ ⇨ τ).

Notation " Γ '⊢WF' τ " := (wf_rty Γ τ) (at level 20, τ constr, Γ constr).

(** Semantic subtyping *)
(* Instead of the syntactic subtyping relation presented in the paper, we use
semantic subtyping in the mechanization. *)

Definition subtyping (Γ: listctx rty) (τ1 τ2: rty) : Prop :=
  (* Assume [τ1] and [τ2] are valid [rty]s. *)
  ⌊ τ1 ⌋ = ⌊ τ2 ⌋ /\
    forall e, ⟪ Γ ⟫ (fun Γv => ⟦m{ Γv } τ1⟧ e) →
         ⟪ Γ ⟫ (fun Γv => ⟦m{ Γv } τ2⟧ e).

Notation " Γ '⊢' τ1 '<⋮' τ2 " := (subtyping Γ τ1 τ2) (at level 20, τ1 constr, τ2 constr, Γ constr).

Reserved Notation "Γ '⊢' e '⋮t' τ" (at level 20).
Reserved Notation "Γ '⊢' e '⋮v' τ"  (at level 20).


(** Typing rules for values and terms. Values always have refinement types, while
  terms always have Hoare automata types. *)
Inductive term_type_check : listctx rty -> tm -> rty -> Prop :=
| TSub: forall Γ e (τ1 τ2: rty),
    Γ ⊢WF τ2 ->
    Γ ⊢ e ⋮t τ1 ->
    Γ ⊢ τ1 <⋮ τ2 ->
    Γ ⊢ e ⋮t τ2
(* | TLetE: forall Γ e_x e ρx ρ A A' B (L: aset), *)
(*     Γ ⊢WF (<[ A ] ρ [ B ]>) -> *)
(*     Γ ⊢ e_x ⋮t (<[ A ] ρx [ A' ]>) -> *)
(*     (forall x, x ∉ L -> *)
(*           (Γ ++ [(x, ρx)]) ⊢ (e ^t^ x) ⋮t (<[ A' ] ρ [ B ]>)) -> *)
(*     Γ ⊢ (tlete e_x e) ⋮t (<[ A ] ρ [ B ]>) *)
(* | TApp: forall Γ (v1 v2: value) e ρ ρx ρ2 A A' B (L: aset), *)
(*     Γ ⊢WF (<[ (A ^a^ v2) ] ρ [ B ]>) -> *)
(*     Γ ⊢ v2 ⋮v ρ2 -> *)
(*     Γ ⊢ v1 ⋮v (ρ2 ⇨ (<[ A ] ρx [ A' ]>)) -> *)
(*     (forall x, x ∉ L -> *)
(*           (Γ ++ [(x, ρx ^p^ v2)]) ⊢ (e ^t^ x) ⋮t (<[ A' ^a^ v2 ] ρ [ B ]>)) -> *)
(*     Γ ⊢ (tletapp v1 v2 e) ⋮t (<[ A ^a^ v2 ] ρ [ B ]>) *)
(* | TEOpApp: forall Γ (op: effop) (v2: value) e ρ ρx ρ2 A A' B (L: aset), *)
(*     Γ ⊢WF (<[ A ^a^ v2 ] ρ [ B ]>) -> *)
(*     Γ ⊢ v2 ⋮v ρ2 -> *)
(*     Γ ⊢ op ⋮o (ρ2 ⇨ (<[ A ] ρx [ A' ]>)) -> *)
(*     (forall x, x ∉ L -> *)
(*           (Γ ++ [(x, ρx ^p^ v2)]) ⊢ (e ^t^ x) ⋮t (<[ A' ^a^ v2 ] ρ [ B ]>)) -> *)
(*     Γ ⊢ (tleteffop op v2 e) ⋮t (<[ A ^a^ v2 ] ρ [ B ]>) *)
(* | TMatchb: forall Γ (v: value) e1 e2 ϕ τ (L : aset), *)
(*     Γ ⊢WF τ -> *)
(*     Γ ⊢ v ⋮v {:TBool | ϕ} -> *)
(*     (* We can also directly encode the path condition without mentioning [x]: *)
(*     {: TNat | (qual [# v] (fun V => V !!! 0 = (cbool true))%fin) & ϕ ^q^ v} *) *)
(*     (forall x, x ∉ L -> (Γ ++ [(x, {: TBool | b0:c=true & b0:v= v & ϕ})]) ⊢ e1 ⋮t τ) -> *)
(*     (forall x, x ∉ L -> (Γ ++ [(x, {: TBool | b0:c=false & b0:v= v & ϕ})]) ⊢ e2 ⋮t τ) -> *)
(*     Γ ⊢ (tmatchb v e1 e2) ⋮t τ *)
with value_type_check : listctx rty -> value -> rty -> Prop :=
| TConst: forall Γ (c: constant),
    Γ ⊢WF (mk_eq_constant c) ->
    Γ ⊢ c ⋮v (mk_eq_constant c)
| TBaseVarOver: forall Γ (x: atom) b ϕ,
    Γ ⊢WF (mk_eq_var b x) ->
    ctxfind Γ x = Some {: b | ϕ} ->
    Γ ⊢ x ⋮v (mk_eq_var b x)
| TBaseVarUnder: forall Γ (x: atom) b ϕ,
    Γ ⊢WF (mk_eq_var b x) ->
    ctxfind Γ x = Some [: b | ϕ] ->
    Γ ⊢ x ⋮v (mk_eq_var b x)
| TFuncVar: forall Γ (x: atom) ρ τ,
    Γ ⊢WF (ρ ⇨ τ) ->
    ctxfind Γ x = Some (ρ ⇨ τ) ->
    Γ ⊢ x ⋮v (ρ ⇨ τ)
| TFun: forall Γ Tx ρ e τ (L: aset),
    Γ ⊢WF (ρ ⇨ τ) ->
    (forall x, x ∉ L -> (Γ ++ [(x, ρ)]) ⊢ (e ^^ x) ⋮t (τ ^^ x)) ->
    Tx = ⌊ ρ ⌋ ->
    Γ ⊢ (vlam Tx e) ⋮v (ρ ⇨ τ)
(* | TFix: forall Γ (Tx : base_ty) ϕ e τ T (L: aset), *)
(*     Γ ⊢WF ({: Tx | ϕ} ⇨ τ) -> *)
(*     (* NOTE: should not open the whole type, because the first argument (bound *)
(*     variable 0) of the return type is not the fixed point function but [{: Tx | *)
(*     ϕ}]. The return type should be opened by [x]. *) *)
(*     (forall x, x ∉ L -> *)
(*           (Γ ++ [(x, {: Tx | ϕ})]) ⊢ (vlam (Tx ⤍ T) e) ^v^ x ⋮v (({: Tx | b0:x≺ x & ϕ} ⇨ τ) ⇨ (τ ^h^ x))) -> *)
(*     T = ⌊ τ ⌋ -> *)
(*     Γ ⊢ (vfix (Tx ⤍ T) (vlam (Tx ⤍ T) e)) ⋮v ({: Tx | ϕ} ⇨ τ) *)
where
"Γ '⊢' e '⋮t' τ" := (term_type_check Γ e τ) and "Γ '⊢' e '⋮v' ρ" := (value_type_check Γ e ρ).


Scheme value_term_type_check_ind := Minimality for value_type_check Sort Prop
    with term_value_type_check_ind := Minimality for term_type_check Sort Prop.
Combined Scheme value_term_type_check_mutind from
  value_term_type_check_ind, term_value_type_check_ind.

(** * Properties of the type system *)

Lemma subtyping_preserves_basic_typing Γ τ1 τ2 :
  Γ ⊢ τ1 <⋮ τ2 ->
  ⌊τ1⌋ = ⌊τ2⌋.
Proof.
  qauto.
Qed.

Lemma rty_subtyping_preserves_basic_typing Γ ρ1 ρ2 :
  Γ ⊢ ρ1 <⋮p ρ2 ->
  ⌊ρ1⌋ = ⌊ρ2⌋.
Proof.
  qauto.
Qed.

Lemma effop_typing_preserves_basic_typing Γ ρ op :
  Γ ⊢ op ⋮o ρ ->
  ⌊ρ⌋ = ty_of_op op.
Proof.
  inversion 1; subst. eauto.
Qed.

Lemma value_typing_regular_wf : forall (Γ: listctx rty) (v: value) (ρ: rty),
    Γ ⊢ v ⋮v ρ -> wf_rty Γ ρ
with tm_typing_regular_wf : forall (Γ: listctx rty) (e: tm) (τ: rty),
    Γ ⊢ e ⋮t τ -> wf_rty Γ τ.
Proof.
  all: destruct 1; eauto.
Qed.

Lemma value_tm_typing_regular_basic_typing:
  (forall (Γ: listctx rty) (v: value) (ρ: rty),
    Γ ⊢ v ⋮v ρ -> ⌊ Γ ⌋* ⊢t v ⋮v ⌊ ρ ⌋) /\
  (forall (Γ: listctx rty) (e: tm) (τ: rty),
    Γ ⊢ e ⋮t τ -> ⌊ Γ ⌋* ⊢t e ⋮t ⌊ τ ⌋).
Proof.
  apply value_term_type_check_mutind; intros; cbn; subst; eauto.
  - hauto using rty_subtyping_preserves_basic_typing.
  - auto_pose_fv x. repeat specialize_with x.
    rewrite ctx_erase_app_r in H1 by my_set_solver.
    rewrite <- rty_erase_open_eq in H1.
    eapply basic_typing_strengthen_value; eauto. my_set_solver.
  - econstructor.
    qauto using ctx_erase_lookup.
  - econstructor.
    qauto using ctx_erase_lookup.
  - econstructor.
    instantiate_atom_listctx.
    rewrite ctx_erase_app_r in H1 by my_set_solver.
    rewrite <- rty_erase_open_eq in H1.
    eauto.
  - econstructor.
    instantiate_atom_listctx.
    rewrite ctx_erase_app_r in H1 by my_set_solver.
    cbn in H1.
    rewrite <- rty_erase_open_eq in H1.
    eauto.
  - hauto using subtyping_preserves_basic_typing.
  - econstructor; eauto.
    instantiate_atom_listctx.
    rewrite ctx_erase_app_r in H3 by my_set_solver.
    eauto.
  - econstructor.
    cbn in H3. eauto. eauto.
    instantiate_atom_listctx.
    rewrite ctx_erase_app_r in H5 by my_set_solver.
    rewrite <- rty_erase_open_eq in H5.
    eauto.
  - apply effop_typing_preserves_basic_typing in H2. cbn in H2. sinvert H2.
    econstructor; eauto. qauto.
    instantiate_atom_listctx.
    rewrite ctx_erase_app_r in H4 by my_set_solver.
    rewrite <- rty_erase_open_eq in H4.
    rewrite <- H7. eauto.
  - auto_pose_fv x. repeat specialize_with x.
    rewrite ctx_erase_app_r in H3, H5 by my_set_solver.
    econstructor; eauto.
    eapply basic_typing_strengthen_tm; eauto. my_set_solver.
    eapply basic_typing_strengthen_tm; eauto. my_set_solver.
Qed.

Lemma value_typing_regular_basic_typing: forall (Γ: listctx rty) (v: value) (ρ: rty),
    Γ ⊢ v ⋮v ρ -> ⌊ Γ ⌋* ⊢t v ⋮v ⌊ ρ ⌋.
Proof.
  apply value_tm_typing_regular_basic_typing.
Qed.

Lemma tm_typing_regular_basic_typing: forall (Γ: listctx rty) (e: tm) (τ: rty),
    Γ ⊢ e ⋮t τ -> ⌊ Γ ⌋* ⊢t e ⋮t ⌊ τ ⌋.
Proof.
  apply value_tm_typing_regular_basic_typing.
Qed.

Lemma ctxRst_insert_easy Γ env (x: atom) ρ (v: value):
    ctxRst Γ env ->
    x ∉ ctxdom Γ ->
    p⟦ m{ env }p ρ ⟧ v ->
    ctxRst (Γ ++ [(x, ρ)]) (<[ x := v ]> env).
Proof.
  intros. econstructor; eauto.
  econstructor; eauto using ctxRst_ok_ctx.
  apply rtyR_typed_closed in H1. simp_hyps.
  (* This should be a lemma similar to [msubst_preserves_closed_rty_emrty], or
  we should strenghthen this lemma. But don't bother now as it is only used
  here. *)
  sinvert H2.
  econstructor. eauto using lc_msubst_rty, ctxRst_lc.
  rewrite fv_of_msubst_rty_closed in H4 by eauto using ctxRst_closed_env.
  rewrite ctxRst_dom in * by eauto.
  my_set_solver.
Qed.

Lemma ctxRst_ctxfind Γ Γv x ρ :
  ctxRst Γ Γv ->
  ctxfind Γ x = Some ρ ->
  exists (v : value), Γv !! x = Some v /\ p⟦ m{ Γv }p ρ ⟧ v.
Proof.
  induction 1.
  - easy.
  - intros.
    select (ctxfind (_ ++ _) _ = _)
      (fun H => apply ctxfind_app in H; eauto using ok_ctx_ok).

    assert (forall (v' : value), (p⟦(m{env}p) ρ⟧) v' ->
                            (p⟦(m{<[x0:=v]> env}p) ρ⟧) v'). {
      select (p⟦ _ ⟧ _) (fun H => apply rtyR_typed_closed in H). simp_hyps.
      intros.
      apply rtyR_msubst_insert_eq; eauto using ctxRst_closed_env.
      select (_ ⊢t _ ⋮t _)
        (fun H => apply basic_typing_contains_fv_tm in H; simpl in H).
      my_set_solver.
      select (ok_ctx _) (fun H => apply ok_ctx_ok in H; apply ok_post_destruct in H).
      srewrite ctxRst_dom.
      simp_hyps.
      apply not_elem_of_dom. eauto.
    }

    destruct_or!; simp_hyps.
    + eexists. split; eauto.
      assert (x <> x0). {
        select (ok_ctx _) (fun H => sinvert H); listctx_set_simpl.
        select (ctxfind _ _ = _) (fun H => apply ctxfind_some_implies_in_dom in H).
        my_set_solver.
      }
      by simplify_map_eq.
    + simpl in *.
      case_decide; try easy. simplify_eq.
      eexists. split; eauto. by simplify_map_eq.
Qed.

Lemma wf_rty_closed Γ ρ :
  wf_rty Γ ρ ->
  closed_rty (ctxdom Γ) ρ
with wf_rty_closed Γ τ :
  wf_rty Γ τ ->
  closed_rty (ctxdom Γ) τ.
Proof.
  all: destruct 1; eauto; split;
    let go :=
      repeat select (_ ⊢WF _) (fun H => apply wf_rty_closed in H; sinvert H);
      repeat select (_ ⊢WF _) (fun H => apply wf_rty_closed in H; sinvert H);
      repeat destruct select (_ ⊢WFa _);
      eauto in
    match goal with
    | |- lc_rty _ =>
        repeat econstructor; try instantiate_atom_listctx; go
    | |- lc_rty _ =>
        repeat econstructor; try instantiate_atom_listctx; go
    | |- _ =>
        simpl; auto_pose_fv x; repeat specialize_with x; go;
        rewrite <- ?open_fv_rty' in *;
        rewrite <- ?open_fv_rty' in *;
        rewrite ?ctxdom_app_union in *;
        my_set_solver
    end.
Qed.

Lemma denotation_application_lam Tx T ρ τ e :
  Tx ⤍ T = ⌊ ρ⇨τ ⌋ ->
  ∅ ⊢t vlam Tx e ⋮t Tx ⤍ T ->
  closed_rty ∅ (ρ⇨τ) ->
  (forall (v_x : value),
      p⟦ρ⟧ v_x ->
      ⟦τ ^h^ v_x⟧ (e ^t^ v_x)) ->
  (p⟦ρ⇨τ⟧) (vlam Tx e).
Proof.
  intros He Ht Hc H.
  split; [| split]; eauto. sinvert He; eauto.
  intros v_x HDv_x.
  repeat rewrite_measure_irrelevant.
  specialize (H v_x HDv_x).
  eapply rtyR_refine; cycle 1. eauto.

  apply rtyR_typed_closed in HDv_x. simp_hyps. sinvert H0.
  split; intros.
  - apply rtyR_typed_closed in H. destruct H as [H _].
    repeat esplit. eauto.
    rewrite <- rty_erase_open_eq.
    sinvert He.
    eapply mk_app_e_v_has_type; eauto.
  - eapply reduction_letapplam; eauto using basic_typing_regular_value.
Qed.

Lemma denotation_application_fixed (Tx : base_ty) T ϕ τ e :
  T = ⌊ τ ⌋ ->
  ∅ ⊢t vfix (Tx ⤍ T) (vlam (Tx ⤍ T) e) ⋮v Tx ⤍ T ->
  closed_rty ∅ ({:Tx|ϕ}⇨τ) ->
  (forall (v_x : value),
      p⟦{:Tx | ϕ}⟧ v_x ->
      p⟦({:Tx | (b0:v≺ v_x) & ϕ} ⇨ τ) ⇨ (τ ^h^ v_x)⟧
        ((vlam (Tx ⤍ T) e) ^v^ v_x)) ->
  p⟦{:Tx | ϕ}⇨τ⟧ (vfix (Tx ⤍ T) (vlam (Tx ⤍ T) e)).
Proof.
  intros He Ht Hc Hlam.
  split; [| split]; eauto. subst; eauto.
  intros v_x HDc.
  repeat rewrite_measure_irrelevant.
  assert (exists c, v_x = vconst c) as H. {
    apply rtyR_typed_closed in HDc.
    destruct HDc as [H _]. sinvert H.
    eauto using basic_typing_base_canonical_form.
  }
  destruct H as [c ->].
  induction c using (well_founded_induction constant_lt_well_founded).
  specialize (Hlam c HDc).
  destruct Hlam as (HTlam&HClam&HDlam).
  specialize (HDlam (vfix (Tx ⤍ T) (vlam (Tx ⤍ T) e))).
  repeat rewrite_measure_irrelevant.
  rewrite open_rty_idemp in HDlam by eauto using lc.
  eapply rtyR_refine; cycle 1.
  apply HDlam.
  split; [| split].
  simpl. cbn. unfold erase, rty_erase_ in He. rewrite <- He. eauto.
  sinvert HClam. econstructor. sinvert H0. eauto. my_set_solver.
  intros v_y HDv_y.
  repeat rewrite_measure_irrelevant.
  assert (exists y, v_y = vconst y). {
    apply rtyR_typed_closed in HDv_y.
    destruct HDv_y as [HTv_y _]. sinvert HTv_y.
    eauto using basic_typing_base_canonical_form.
  }
  destruct H0 as (y&->).
  destruct HDv_y as (HTy&_&Hy).
  ospecialize* (Hy _ []); eauto. destruct Hy as [_ Hy].
  rewrite qualifier_and_open in Hy.
  rewrite denote_qualifier_and in Hy.
  simp_hyps.
  apply H; eauto.
  (* lemma? *)
  apply rtyR_typed_closed in HDc. simp_hyps.
  split; [| split]; eauto.
  intros. apply value_reduction_refl in H4. simp_hyps.
  intuition.

  split; intros.
  - subst. cbn in HTlam.
    repeat esplit.
    eapply mk_app_e_v_has_type; eauto.
    rewrite <- rty_erase_open_eq.
    eapply mk_app_e_v_has_type; eauto.
    apply rtyR_typed_closed in HDc. destruct HDc as [HTc _].
    sinvert HTc. eauto.
  - apply reduction_mk_app_e_v in H0.
    sinvert H0. sinvert H1. simplify_list_eq.
    apply reduction_mk_app_e_v' in H2.
    eauto.
    eauto using basic_typing_regular_value.
Qed.


(** * Main metatheoretic results *)

(** Convert an event operator to a value:
  [op] is [fun x => leteffop y = op x in y] *)
Definition value_of_op op : value :=
  vlam TNat (tleteffop op (vbvar 0) (vbvar 0)).

(** Well-formed built-in operator typing context (Definition 4.7) *)
(* We simply treat the event operator as a value. This is equivalent to the
definition in the paper (if we expand the denotation of this value). *)
Definition well_formed_builtin_typing :=
  forall op ρ,
    builtin_typing_relation op ρ ->
    p⟦ ρ ⟧ (value_of_op op).

Lemma msubst_value_of_op Γv op :
  m{Γv}v (value_of_op op) = value_of_op op.
Proof.
  rewrite msubst_fresh_value. eauto.
  my_set_solver.
Qed.

Lemma value_of_op_regular_basic_typing op:
  ∅ ⊢t value_of_op op ⋮v ty_of_op op.
Proof.
  econstructor.
  simpl. instantiate (1:=∅). intros.
  econstructor. econstructor. simplify_map_eq. reflexivity. reflexivity.
  instantiate_atom_listctx.
  simpl. econstructor. econstructor. simplify_map_eq. reflexivity.
Qed.

(* At some point the proof is very slow without marking [msubst] opaque. *)
Opaque msubst.

Ltac simpl_fv :=
  do_hyps (fun H => try match type of H with
                    | _ ∉ _ =>
                        simpl in H; rewrite ?ctxRst_dom in H by eassumption
                    end).

(** Fundamental theorem for event operator typing *)
Lemma builtin_fundamental:
  well_formed_builtin_typing ->
  forall (Γ: listctx rty) (op: effop) (ρ : rty),
    Γ ⊢ op ⋮o ρ ->
    forall Γv, ctxRst Γ Γv -> p⟦ m{ Γv }p ρ ⟧ (value_of_op op).
Proof.
  intros HWF Γ op ρ Hop Γv HΓv. sinvert Hop.
  apply H1; eauto.
  apply HWF in H0.
  rewrite msubst_fresh_rty; eauto.
  apply rtyR_typed_closed in H0. simp_hyps.
  sinvert H3.
  my_set_solver.
Qed.

(** Combined fundamental theorem for value typing (refinemnet types) and term
  typing (Hoare automata types) *)
Theorem fundamental_combined:
  well_formed_builtin_typing ->
  (forall (Γ: listctx rty) (v: value) (ρ: rty),
    Γ ⊢ v ⋮v ρ ->
    forall Γv, ctxRst Γ Γv -> p⟦ m{Γv}p ρ ⟧ (m{Γv}v v)) /\
  (forall (Γ: listctx rty) (e: tm) (τ: rty),
    Γ ⊢ e ⋮t τ ->
    forall Γv, ctxRst Γ Γv -> ⟦ m{ Γv } τ ⟧ (m{ Γv }t e)).
Proof.
  intros HWFbuiltin.
  apply value_term_type_check_mutind.
  (* [TSubPP] *)
  - intros Γ v ρ1 ρ2 HWFρ2 _ HDρ1 Hsub Γv HΓv. specialize (HDρ1 _ HΓv).
    apply Hsub in HDρ1; auto.
  (* [TGhost] *)
  - intros Γ v b ρ L HWF Hv HDv Γv HΓv. repeat msubst_simp.
    split; [| split]. {
      assert (Γ ⊢ v ⋮v (b⇢ρ)) by eauto using value_type_check.
      eapply value_typing_regular_basic_typing in H.
      eapply msubst_preserves_basic_typing_value_emrty in H; eauto.
      econstructor. apply_eq H. cbn. apply rty_erase_msubst_eq.
    } {
      eapply_eq msubst_preserves_closed_rty_emrty; eauto using wf_rty_closed.
      msubst_simp.
    }
    auto_pose_fv x. repeat specialize_with x.
    intros.
    assert (ctxRst (Γ ++ [(x, mk_top b)]) (<[x:=v0]> Γv)) as HΓv'. {
      eapply ctxRst_insert_easy; eauto. my_set_solver.
      repeat msubst_simp. eauto using mk_top_denote_rty.
    }
    ospecialize* HDv; eauto.
    rewrite <- msubst_intro_rty in HDv by
      (eauto using ctxRst_closed_env, ctxRst_lc, basic_typing_closed_value;
       simpl_fv; my_set_solver).
    apply_eq HDv. by rewrite <- open_preserves_rty_measure.
    rewrite msubst_insert_fresh_value by
      (eauto using ctxRst_closed_env, basic_typing_closed_value;
       simpl_fv; my_set_solver).
    auto.
  (* [TConst] *)
  - intros Γ c HWF Γv HΓv. repeat msubst_simp. eauto using mk_eq_constant_denote_rty.
  (* [TBaseVar] *)
  - intros Γ x b ϕ Hwf Hfind Γv HΓv.
    dup_hyp HΓv (fun H => eapply ctxRst_ctxfind in H; eauto). simp_hyps.
    repeat msubst_simp. rewrite H0.
    destruct H1 as [H _].
    sinvert H. cbn in H3.
    dup_hyp H3 (fun H => apply basic_typing_base_canonical_form in H).
    simp_hyps. subst. sinvert H3.
    eauto using mk_eq_constant_denote_rty.
  (* [TFuncVar] *)
  - intros Γ x ρ τ Hwf Hfind Γv HΓv.
    dup_hyp HΓv (fun H => eapply ctxRst_ctxfind in H; eauto). simp_hyps.
    repeat msubst_simp.
    by rewrite H0.
  (* [TFun] *)
  - intros Γ Tx ρ e τ L HWF Ht HDe He Γv HΓv. repeat msubst_simp.
    eapply denotation_application_lam; eauto.
    cbn. rewrite <- rty_erase_msubst_eq, <- rty_erase_msubst_eq. subst. eauto.
    {
      assert (Γ ⊢ vlam Tx e ⋮v (ρ⇨τ)) by eauto using value_type_check.
      eapply value_typing_regular_basic_typing in H.
      eapply msubst_preserves_basic_typing_value_emrty in H; eauto.
      repeat msubst_simp.
      econstructor. apply_eq H. cbn. subst. reflexivity.
    } {
      eapply_eq msubst_preserves_closed_rty_emrty; eauto using wf_rty_closed.
      msubst_simp.
    }
    intros v_x Hv_x.
    auto_pose_fv x. repeat specialize_with x.
    assert (ctxRst (Γ ++ [(x, ρ)]) (<[x:=v_x]> Γv)) as HΓv'. {
      apply ctxRst_insert_easy; eauto. my_set_solver.
    }
    ospecialize* HDe; eauto.
    rewrite <- msubst_intro_tm in HDe by
        (eauto using ctxRst_closed_env, ctxRst_lc, rtyR_closed_value;
         simpl_fv; my_set_solver).
    rewrite <- msubst_intro_rty in HDe by
        (eauto using ctxRst_closed_env, ctxRst_lc, rtyR_closed_value;
         simpl_fv; my_set_solver).
    eauto.
  (* [TFix] *)
  - intros Γ Tx ϕ e τ T L HWF Hlam HDlam He Γv HΓv. repeat msubst_simp.
    eapply denotation_application_fixed; eauto.
    by rewrite <- rty_erase_msubst_eq.
    {
      assert (Γ ⊢ vfix (Tx ⤍ T) (vlam (Tx ⤍ T) e) ⋮v ({:Tx|ϕ}⇨τ))
        by eauto using value_type_check.
      eapply value_typing_regular_basic_typing in H.
      eapply msubst_preserves_basic_typing_value_emrty in H; eauto.
      repeat msubst_simp.
      apply_eq H. cbn. subst. eauto.
    } {
      eapply_eq msubst_preserves_closed_rty_emrty; eauto using wf_rty_closed.
      repeat msubst_simp.
    }
    intros v_x Hv_x.
    auto_pose_fv x. repeat specialize_with x.
    assert (ctxRst (Γ ++ [(x, {:Tx|ϕ})]) (<[x:=v_x]> Γv)) as HΓv'. {
      apply ctxRst_insert_easy; eauto. my_set_solver.
      msubst_simp.
    }
    ospecialize* HDlam; eauto.
    rewrite <- msubst_intro_value in HDlam by
        (eauto using ctxRst_closed_env, ctxRst_lc, rtyR_closed_value;
         simpl_fv; my_set_solver).
    repeat msubst_simp.
    rewrite <- msubst_intro_rty in HDlam by
        (eauto using ctxRst_closed_env, ctxRst_lc, rtyR_closed_value;
         simpl_fv; my_set_solver).
    rewrite msubst_insert_fresh_rty in HDlam by
      (eauto using ctxRst_closed_env, rtyR_closed_value; simpl_fv; my_set_solver).
    rewrite_msubst msubst_qualifier in HDlam.
    rewrite msubst_insert_fresh_qualifier in HDlam by
      (eauto using ctxRst_closed_env, rtyR_closed_value; simpl_fv; my_set_solver).
    apply_eq HDlam.
    simpl. repeat msubst_simp.
    clear. simplify_map_eq. eauto.
  (* [TEPur] *)
  - intros Γ v ρ A HWF Hv HDv Γv HΓv. specialize (HDv _ HΓv).
    repeat msubst_simp.
    split; [| split]. {
      eapply value_typing_regular_basic_typing in Hv.
      eapply msubst_preserves_basic_typing_value_emrty in Hv; eauto.
      cbn. rewrite <- rty_erase_msubst_eq. eauto.
    } {
      eapply_eq msubst_preserves_closed_rty_emrty; eauto using wf_rty_closed.
      repeat msubst_simp.
    }
    intros. rewrite_measure_irrelevant.
    apply value_reduction_refl in H0. simp_hyps. subst. simpl_list. eauto.
  (* [TSub] *)
  - intros Γ e τ1 τ2 HWFτ2 _ HDτ1 Hsub Γv HΓv. specialize (HDτ1 _ HΓv).
    apply Hsub in HDτ1; auto.
  (* [TInter] *)
  - intros Γ e τ1 τ2 HWF HT1 HD1 HT2 HD2 Γv HΓv. repeat msubst_simp.
    split; [| split]. {
      eapply tm_typing_regular_basic_typing in HT1.
      eapply msubst_preserves_basic_typing_tm_emrty in HT1; eauto.
      cbn. rewrite <- rty_erase_msubst_eq. eauto.
    } {
      eapply_eq msubst_preserves_closed_rty_emrty; eauto using wf_rty_closed.
      repeat msubst_simp.
    }
    repeat rewrite_measure_irrelevant. eauto.
  (* [TLetE] *)
  - intros Γ e_x e ρx ρ A A' B L HWF HTe_x HDe_x HTe HDe Γv HΓv.
    ospecialize* HDe_x; eauto. repeat msubst_simp.
    split; [| split]. {
      assert (Γ ⊢ tlete e_x e ⋮t (<[ A ] ρ [ B ]>)) by eauto using term_type_check.
      eapply tm_typing_regular_basic_typing in H.
      eapply msubst_preserves_basic_typing_tm_emrty in H; eauto.
      repeat msubst_simp.
      apply_eq H. cbn. eauto using rty_erase_msubst_eq.
    } {
      eapply_eq msubst_preserves_closed_rty_emrty; eauto using wf_rty_closed.
      repeat msubst_simp.
    }
    intros α β v HDα Hstepv.
    apply reduction_tlete in Hstepv.
    destruct Hstepv as (βx & βe & v_x & -> & Hstepv_x & Hstepv).
    auto_pose_fv x. repeat specialize_with x.
    destruct HDe_x as (_ & _ & HDe_x).
    ospecialize* HDe_x; eauto. destruct HDe_x as [HDv_x HDα_βx].
    assert (ctxRst (Γ ++ [(x, ρx)]) (<[x:=v_x]> Γv)) as HΓv'. {
      apply ctxRst_insert_easy; eauto. my_set_solver.
    }
    ospecialize* HDe; eauto.
    rewrite <- msubst_intro_tm in HDe by
        (eauto using ctxRst_closed_env, ctxRst_lc, rtyR_closed_value;
         simpl_fv; my_set_solver).
    repeat msubst_simp.
    destruct HDe as (_ & _ & HDe).
    rewrite msubst_insert_fresh_am in HDe by
      (eauto using ctxRst_closed_env, rtyR_closed_value;
       simpl_fv; my_set_solver).
    ospecialize* HDe; eauto.
    repeat rewrite_measure_irrelevant.
    rewrite msubst_insert_fresh_rty in HDe by
      (eauto using ctxRst_closed_env, rtyR_closed_value;
       simpl_fv; my_set_solver).
    rewrite msubst_insert_fresh_am in HDe by
      (eauto using ctxRst_closed_env, rtyR_closed_value;
       simpl_fv; my_set_solver).
    clear - HDe.
    by simplify_list_eq.
  (* [TApp] *)
  - intros Γ v1 v2 e ρ ρx ρ2 A A' B L HWF HTv2 HDv2 HTv1 HDv1 HTe HDe Γv HΓv.
    ospecialize* HDv1; eauto. ospecialize* HDv2; eauto. repeat msubst_simp.
    split; [| split]. {
      assert (Γ ⊢ tletapp v1 v2 e ⋮t (<[ A^a^v2 ] ρ [ B ]>)) by
        eauto using term_type_check.
      eapply tm_typing_regular_basic_typing in H.
      eapply msubst_preserves_basic_typing_tm_emrty in H; eauto.
      repeat msubst_simp.
      apply_eq H. cbn. eauto using rty_erase_msubst_eq.
    } {
      eapply_eq msubst_preserves_closed_rty_emrty; eauto using wf_rty_closed.
      repeat msubst_simp.
    }
    intros α β v HDα Hstepv.
    apply reduction_tletapp in Hstepv.
    destruct Hstepv as (βx & βe & v_x & -> & Hstepv_x & Hstepv).
    auto_pose_fv x. repeat specialize_with x.
    destruct HDv1 as (_ & _ & HDapp).
    repeat rewrite_measure_irrelevant.
    ospecialize* HDapp; eauto.
    destruct HDapp as (_ & _ & HDapp). simpl in HDapp.
    rewrite <- !msubst_open_am in HDapp by
        (eauto using ctxRst_closed_env, ctxRst_lc).
    rewrite <- !msubst_open_rty in HDapp by
        (eauto using ctxRst_closed_env, ctxRst_lc).
    ospecialize* HDapp; eauto.
    destruct HDapp as [HDv_x HDα_βx].
    assert (ctxRst (Γ ++ [(x, ρx ^p^ v2)]) (<[x:=v_x]> Γv)) as HΓv'. {
      apply ctxRst_insert_easy; eauto. my_set_solver.
    }
    ospecialize* HDe; eauto.
    rewrite <- msubst_intro_tm in HDe by
        (eauto using ctxRst_closed_env, ctxRst_lc, rtyR_closed_value;
         simpl_fv; my_set_solver).
    repeat msubst_simp.
    destruct HDe as (_ & _ & HDe).
    rewrite msubst_insert_fresh_am in HDe;
      eauto using ctxRst_closed_env, rtyR_closed_value.
    2 : {
      apply not_elem_of_union. split.
      simpl_fv; my_set_solver.
      eapply not_elem_of_weaken. 2: eauto using open_fv_am.
      my_set_solver.
    }
    ospecialize* HDe; eauto.
    repeat rewrite_measure_irrelevant.
    rewrite msubst_insert_fresh_rty in HDe by
      (eauto using ctxRst_closed_env, rtyR_closed_value;
       simpl_fv; my_set_solver).
    rewrite msubst_insert_fresh_am in HDe by
      (eauto using ctxRst_closed_env, rtyR_closed_value;
       simpl_fv; my_set_solver).
    clear - HDe.
    by simplify_list_eq.
  (* [TEOpApp] *)
  - intros Γ op v2 e ρ ρx ρ2 A A' B L HWF HTv2 HDv2 HTop HTe HDe Γv HΓv.
    assert (∅ ⊢t (m{Γv}t) (tleteffop op v2 e) ⋮t ⌊ρ⌋) as HT. {
      eapply msubst_preserves_basic_typing_tm_emrty; eauto.
      apply_eq tm_typing_regular_basic_typing; eauto using term_type_check.
    }
    assert (∅ ⊢t (m{Γv}t) (tletapp (value_of_op op) v2 e) ⋮t ⌊ρ⌋) as HTapp. {
      repeat msubst_simp. rewrite msubst_value_of_op.
      clear - HT.
      sinvert HT. sinvert H5.
      econstructor; eauto.
      econstructor. instantiate_atom_listctx.
      econstructor; eauto. econstructor. by simplify_map_eq.
      instantiate_atom_listctx. simpl.
      econstructor. econstructor. by simplify_map_eq.
    }
    eapply (rtyR_refine _ (m{Γv}t (tletapp (value_of_op op) v2 e))). {
      split; eauto.
      repeat msubst_simp. rewrite msubst_value_of_op.
      assert (body ((m{Γv}t) e)) as Hbody. {
        sinvert HT. eexists. eauto using basic_typing_regular_tm.
      }
      clear - Hbody. intros * Hstepv.
      apply reduction_tleteffop in Hstepv.
      destruct Hstepv as (c2 & c_x & β' & -> & -> & Hred & Hstepv).
      eapply_eq multistep_step. econstructor; eauto using lc.
      unshelve (repeat econstructor); exact ∅.
      econstructor. simpl. econstructor; eauto.
      econstructor; eauto. by simplify_list_eq.
      simpl. econstructor. econstructor; solve [eauto].
      by simplify_list_eq. by simplify_list_eq.
    }

    eapply builtin_fundamental in HTop; eauto.
    rewrite <- (msubst_value_of_op Γv) in HTop.
    revert HTapp HTop. generalize (value_of_op op) as v1.
    intros v1 HTapp HDv1.
    ospecialize* HDv2; eauto. repeat msubst_simp.
    split; [| split]. {
      cbn. by rewrite <- rty_erase_msubst_eq.
    } {
      eapply_eq msubst_preserves_closed_rty_emrty; eauto using wf_rty_closed.
      repeat msubst_simp.
    }
    (* The rest of the proof is exactly the same as [TApp]. Maybe find a way to
    abstract this and avoid copy-pasting. *)
    intros α β v HDα Hstepv.
    apply reduction_tletapp in Hstepv.
    destruct Hstepv as (βx & βe & v_x & -> & Hstepv_x & Hstepv).
    auto_pose_fv x. repeat specialize_with x.
    destruct HDv1 as (_ & _ & HDapp).
    repeat rewrite_measure_irrelevant.
    ospecialize* HDapp; eauto.
    destruct HDapp as (_ & _ & HDapp). simpl in HDapp.
    rewrite <- !msubst_open_am in HDapp by
        (eauto using ctxRst_closed_env, ctxRst_lc).
    rewrite <- !msubst_open_rty in HDapp by
        (eauto using ctxRst_closed_env, ctxRst_lc).
    ospecialize* HDapp; eauto.
    destruct HDapp as [HDv_x HDα_βx].
    assert (ctxRst (Γ ++ [(x, ρx ^p^ v2)]) (<[x:=v_x]> Γv)) as HΓv'. {
      apply ctxRst_insert_easy; eauto. my_set_solver.
    }
    ospecialize* HDe; eauto.
    rewrite <- msubst_intro_tm in HDe by
        (eauto using ctxRst_closed_env, ctxRst_lc, rtyR_closed_value;
         simpl_fv; my_set_solver).
    repeat msubst_simp.
    destruct HDe as (_ & _ & HDe).
    rewrite msubst_insert_fresh_am in HDe;
      eauto using ctxRst_closed_env, rtyR_closed_value.
    2 : {
      apply not_elem_of_union. split.
      simpl_fv; my_set_solver.
      eapply not_elem_of_weaken. 2: eauto using open_fv_am.
      my_set_solver.
    }
    ospecialize* HDe; eauto.
    repeat rewrite_measure_irrelevant.
    rewrite msubst_insert_fresh_rty in HDe by
      (eauto using ctxRst_closed_env, rtyR_closed_value;
       simpl_fv; my_set_solver).
    rewrite msubst_insert_fresh_am in HDe by
      (eauto using ctxRst_closed_env, rtyR_closed_value;
       simpl_fv; my_set_solver).
    clear - HDe.
    by simplify_list_eq.
  (* [TMatchb] *)
  - intros Γ v e1 e2 ϕ τ L HWF HTv HDv HTe1 HDe1 HTe2 HDe2 Γv HΓv.
    assert (∅ ⊢t (m{Γv}t) (tmatchb v e1 e2) ⋮t ⌊τ⌋) as HT by
      qauto using term_type_check,
                  tm_typing_regular_basic_typing,
                  msubst_preserves_basic_typing_tm_emrty.
    auto_pose_fv x. repeat specialize_with x.
    ospecialize* HDv; eauto.
    repeat msubst_simp.
    assert (exists (b : bool), m{Γv}v v = b) as [b He] by
        qauto using value_typing_regular_basic_typing,
                    msubst_preserves_basic_typing_value_emrty,
                    basic_typing_bool_canonical_form.
    rewrite He in *.
    assert (ctxRst
              (Γ ++ [(x, {:TBool|(b0:c=b) & ((b0:v=v) & ϕ)})])
              (<[x:=vconst b]>Γv)) as HΓv'. {
      apply ctxRst_insert_easy; eauto. my_set_solver.
      repeat msubst_simp.
      repeat apply denote_base_rty_qualifier_and; eauto.
      apply_eq mk_eq_constant_denote_rty. clear - HΓv.
      rewrite_msubst msubst_qualifier. simpl. repeat msubst_simp.
      apply_eq mk_eq_constant_denote_rty. clear - He HΓv.
      rewrite_msubst msubst_qualifier. simpl. rewrite He. repeat msubst_simp.
    }

    destruct b.
    + ospecialize* HDe1; eauto.
      rewrite msubst_insert_fresh_tm in HDe1 by
          (eauto using ctxRst_closed_env; simpl_fv; my_set_solver).
      rewrite msubst_insert_fresh_rty in HDe1 by
          (eauto using ctxRst_closed_env; simpl_fv; my_set_solver).
      eapply rtyR_refine; eauto.
      split; eauto using reduction_matchb_true.
      repeat esplit; eauto.
      apply rtyR_typed_closed in HDe1. destruct HDe1 as [HTe1' _].
      rewrite <- rty_erase_msubst_eq in HTe1'. eauto.
    + ospecialize* HDe2; eauto.
      rewrite msubst_insert_fresh_tm in HDe2 by
          (eauto using ctxRst_closed_env; simpl_fv; my_set_solver).
      rewrite msubst_insert_fresh_rty in HDe2 by
          (eauto using ctxRst_closed_env; simpl_fv; my_set_solver).
      eapply rtyR_refine; eauto.
      split; eauto using reduction_matchb_false.
      repeat esplit; eauto.
      apply rtyR_typed_closed in HDe2. destruct HDe2 as [HTe2' _].
      rewrite <- rty_erase_msubst_eq in HTe2'. eauto.
Qed.

(** Fundamental theorem for value typing *)
Theorem fundamental_value:
  well_formed_builtin_typing ->
  forall (Γ: listctx rty) (v: value) (ρ: rty),
    Γ ⊢ v ⋮v ρ ->
    forall Γv, ctxRst Γ Γv -> p⟦ m{Γv}p ρ ⟧ (m{Γv}v v).
Proof.
  qauto using fundamental_combined.
Qed.

(** Fundamental theorem (Theorem 4.8) *)
Theorem fundamental:
  well_formed_builtin_typing ->
  forall (Γ: listctx rty) (e: tm) (τ: rty),
    Γ ⊢ e ⋮t τ ->
    forall σ, ctxRst Γ σ -> ⟦ m{ σ } τ ⟧ (m{ σ }t e).
Proof.
  qauto using fundamental_combined.
Qed.

Transparent msubst.

(** A general type soundness theorem *)
Corollary soundness' :
  well_formed_builtin_typing ->
  forall (e : tm) (ρ : rty) (A : am),
    [] ⊢ e ⋮t (<[ A ] ρ [ A ]>) ->
    forall (v : value) α α',
      a⟦ A ⟧ α ->
      α ⊧ e ↪*{ α' } v ->
      p⟦ ρ ⟧ v /\ a⟦ A ⟧ (α ++ α').
Proof.
  intros HWF * HT * HDα Hstepv.
  eapply fundamental in HT; eauto using ctxRst.
  unfold msubst in HT. rewrite !map_fold_emrty in HT.
  qauto using HT.
Qed.

(** Type soundness (Corollary 4.9) *)
Corollary soundness :
  well_formed_builtin_typing ->
  forall (f: value) (b_x b_y: base_ty) (t: rty) (A: am),
    [] ⊢ f ⋮v (b_x⇢(mk_top b_y)⇨(<[ A ] t [ A ]>)) ->
    forall v_x v_y,
      ∅ ⊢t v_x ⋮v b_x ->
      ∅ ⊢t v_y ⋮v b_y ->
      forall (v : value) α α',
        a⟦ {0 ~a> v_y} ({1 ~a> v_x} A) ⟧ α ->
        α ⊧ (mk_app_e_v f v_y) ↪*{ α' } v ->
        a⟦ {0 ~a> v_y} ({1 ~a> v_x} A) ⟧ (α ++ α') /\
          p⟦ {0 ~p> v_y} ({1 ~p> v_x} t) ⟧ v.
Proof.
  intros HWF * HT * HTv_x HTv_y * HDα Hstepv.
  eapply fundamental_value in HT; eauto using ctxRst.
  unfold msubst in HT. rewrite !map_fold_emrty in HT.
  destruct HT as (_&_&HD); eauto.
  repeat rewrite_measure_irrelevant.
  specialize (HD _ HTv_x).
  simpl rty_open in HD.
  destruct HD as (_&_&HD).
  repeat rewrite_measure_irrelevant.
  apply and_comm.
  eapply HD; eauto using mk_top_denote_rty.
Qed.

Print Assumptions soundness.
