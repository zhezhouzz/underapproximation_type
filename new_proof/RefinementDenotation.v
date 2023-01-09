From stdpp Require Import mapset.
From stdpp Require Import natmap.
From CT Require Import CoreLangProp.
From CT Require Import OperationalSemanticsProp.
From CT Require Import BasicTypingProp.
From CT Require Import SyntaxSugar.
From CT Require Import Refinement.
From CT Require Import RefinementTac.

Import Atom.
Import CoreLang.
Import Tactics.
Import NamelessTactics.
Import ListCtx.
Import OperationalSemantics.
Import OperationalSemanticsProp.
Import BasicTyping.
Import SyntaxSugar.
Import Refinement.
Import RefinementTac.

(* Denotation: *)

Fixpoint rR (n: nat) (bst: bstate) (st: state) (τ: rty) (e: tm) : Prop :=
  [] ⊢t e ⋮t ⌊ τ ⌋ /\ closed_rty n (dom _ st) τ /\
    match τ with
    | {v: B | _ | _ | ϕ} => exists (c: constant), [] ⊢t c ⋮v B /\ ϕ bst st c /\ e = c
    | [v: B | _ | _ | ϕ] => forall (c: constant), [] ⊢t c ⋮v B -> ϕ bst st c -> e ↪* c
    | -:{v: B | _ | d | ϕ } ⤑ τ =>
        forall (c_x: constant),
          [] ⊢t c_x ⋮v B -> ϕ bst st c_x -> rR (S n) (<b[↦ c_x ]> bst) st τ (mk_app e c_x)
    | τ1 ⤑ τ2 => forall (e_x: tm), rR n bst st τ1 e_x -> rR n bst st τ2 (mk_app e e_x)
    end.

Lemma bst_eq_trans: forall n n' (bst1 bst2: bstate), n' <= n -> bst_eq n bst1 bst2 -> bst_eq n' bst1 bst2.
Proof.
  intros. intro i; intros. apply H0. lia.
Qed.

Lemma closed_rty_n_overbase: forall (n1 n2: nat) d1 d2 B ϕ, closed_rty n1 d1 {v:B|n2|d2|ϕ} -> n2 <= n1.
Proof.
  intros. invclear H; mydestr. invclear H0. invclear H; auto.
Qed.

Lemma closed_rty_n_underbase: forall (n1 n2: nat) d1 d2 B ϕ, closed_rty n1 d1 [v:B|n2|d2|ϕ] -> n2 <= n1.
Proof.
  intros. invclear H; mydestr. invclear H0. invclear H; auto.
Qed.

Lemma closed_rty_0_overbase: forall (n2: nat) d1 d2 B ϕ, closed_rty 0 d1 {v:B|n2|d2|ϕ} -> n2 = 0.
Proof.
  intros. apply closed_rty_n_overbase in H. lia.
Qed.

Lemma closed_rty_0_underbase: forall (n2: nat) d1 d2 B ϕ, closed_rty 0 d1 [v:B|n2|d2|ϕ] -> n2 = 0.
Proof.
  intros. apply closed_rty_n_underbase in H. lia.
Qed.

Lemma bst_eq_symmetry: forall n bst1 bst2, bst_eq n bst2 bst1 <-> bst_eq n bst1 bst2.
Proof.
  unfold bst_eq. split; intros; rewrite H; auto.
Qed.

Lemma bst_eq_push: forall n c_x bst1 bst2, bst_eq n bst1 bst2 -> bst_eq  (S n) (<b[↦c_x]> bst1) (<b[↦c_x]> bst2).
Proof.
  intros. intro i; intros.
  destruct i; simpl; auto. apply H. lia.
Qed.

Lemma rR_bst_bound_: forall τ (e: tm) st n (bst1 bst2: bstate), bst_eq n bst1 bst2 -> rR n bst1 st τ e -> rR n bst2 st τ e.
Proof.
  induction τ; intros; auto; invclear H0; mydestr; subst.
  - constructor; auto. constructor; auto. exists x. repeat split; auto.
    invclear H0; mydestr. invclear H0. invclear H4. invclear H6. eapply H4; eauto.
    rewrite bst_eq_symmetry in H. eapply bst_eq_trans; eauto.
  - constructor; auto. constructor; auto. intros. apply H2; auto.
    invclear H0; mydestr. invclear H0. invclear H5. invclear H7. eapply H5; eauto. eapply bst_eq_trans; eauto.
  - constructor; auto. constructor; auto. intros.
    apply IHτ with (bst1 := (<b[↦c_x]> bst1)). apply bst_eq_push; auto. apply H2; auto.
    invclear H0; mydestr. invclear H5. invclear H10. eapply H7; eauto. invclear H0. eapply bst_eq_trans; eauto.
  - constructor; auto. constructor; auto. intros. eapply IHτ2; eauto. apply H2; auto.
    eapply IHτ1; eauto. rewrite bst_eq_symmetry; auto.
Qed.

Lemma rR_bst_bound: forall τ (e: tm) st n (bst1 bst2: bstate), bst_eq n bst1 bst2 -> rR n bst1 st τ e <-> rR n bst2 st τ e.
Proof.
  split; intros; eapply rR_bst_bound_; eauto. rewrite bst_eq_symmetry; eauto.
Qed.

Lemma rR_0_empty_iff_forall: forall τ (e: tm) st, (forall bst1, rR 0 bst1 st τ e) <-> rR 0 b∅ st τ e.
Proof.
  split; intros; auto. rewrite rR_bst_bound; eauto. intro i; intros. invclear H0.
Qed.

Notation " '{' n ';' bst ';' st '}⟦' τ '⟧' " :=
  (rR n bst st τ) (at level 20, format "{ n ; bst ; st }⟦ τ ⟧", bst constr, st constr, τ constr).
Notation " '{' st '}⟦' τ '⟧' " := (fun e => rR 0 b∅ st τ e) (at level 20, format "{ st }⟦ τ ⟧", st constr, τ constr).
Notation " '⟦' τ '⟧' " := (fun e => rR 0 b∅ ∅ τ e) (at level 20, format "⟦ τ ⟧", τ constr).

(* regular of the denation *)

Lemma rR_regular1:
  forall τ n bst st e, { n; bst; st }⟦ τ ⟧ e -> closed_rty n (dom _ st) τ /\ [] ⊢t e ⋮t ⌊ τ ⌋.
Proof.
  induction τ; intros; invclear H; mydestr; subst; split; intros; auto.
Qed.

Lemma rR_regular2:
  forall τ st e, { st }⟦ τ ⟧ e -> closed_rty 0 (dom _ st) τ /\ [] ⊢t e ⋮t ⌊ τ ⌋.
Proof.
  intros. eapply rR_regular1; eauto.
Qed.

Lemma rR_regular3:
  forall τ e, ⟦ τ ⟧ e -> (closed_rty 0 ∅ τ) /\ [] ⊢t e ⋮t ⌊ τ ⌋.
Proof.
  intros. eapply rR_regular2 in H. mydestr; split; auto.
Qed.

Inductive ctxrR: state -> listctx rty -> rty -> tm -> Prop :=
| ctxrR_nil: forall st τ e, { st }⟦ τ ⟧ e -> ctxrR st [] τ e
| ctxrR_cons_over: forall st (x: atom) B n d ϕ Γ τ (e: tm),
    closed_rty 0 ({[x]} ∪ ctxdom Γ ∪ (dom _ st)) τ ->
    ok_dctx (dom _ st) ((x, {v: B | n | d | ϕ}) :: Γ) ->
    ((x, TBase B) :: (⌊Γ⌋*)) ⊢t e ⋮t ⌊τ⌋ ->
     (forall (c_x: constant), {st}⟦ {v: B | n | d | ϕ} ⟧ c_x ->
                         ctxrR (<[ x := c_x ]> st) Γ τ ({x := c_x}t e)) ->
     ctxrR st ((x, {v: B | n | d | ϕ}) :: Γ) τ e
| ctxrR_cons_under: forall st (x: atom) τ_x τ Γ e,
    not_overbasety τ_x ->
    closed_rty 0 ({[x]} ∪ ctxdom Γ ∪ (dom _ st)) τ ->
    ok_dctx (dom _ st) ((x, τ_x) :: Γ) ->
    ((x, ⌊τ_x⌋ ) :: (⌊Γ⌋*)) ⊢t e ⋮t ⌊τ⌋ ->
     (exists e_x_hat, {st}⟦ τ_x ⟧ e_x_hat /\
                   (forall e_x, {st}⟦ τ_x ⟧ e_x ->
                           (∀ (v_x: value), e_x_hat ↪* v_x ->
                                            ctxrR ({ x ↦ v_x } st) Γ τ (tlete e_x ({ 0 <t~ x} e))))) ->
     ctxrR st ((x, τ_x) :: Γ) τ e.

Notation " '{' st '}⟦' τ '⟧{' Γ '}' " := (ctxrR st Γ τ) (at level 20, format "{ st }⟦ τ ⟧{ Γ }", st constr, τ constr, Γ constr).
Notation " '⟦' τ '⟧{' Γ '}' " := (ctxrR ∅ Γ τ) (at level 20, format "⟦ τ ⟧{ Γ }", τ constr, Γ constr).

Lemma ctxrR_regular0:
  forall Γ τ st e, { st }⟦ τ ⟧{ Γ } e ->
              closed_rty 0 (ctxdom Γ ∪ (dom _ st)) τ /\ ok_dctx (dom _ st) Γ.
Proof.
  induction Γ; simpl; intros; invclear H; simpl; auto.
  - apply rR_regular2 in H0; mydestr.
    constructor; simpl; auto.
    + closed_rty_solver.
    + constructor.
Qed.

(* Lemma ctxrR_wf_regular: *)
(*   forall Γ τ d, ctxrR_wf d Γ τ -> (ok_dctx d Γ) /\ cl_dctx d Γ /\ closed_rty 0 (ctxdom Γ ∪ d) τ. *)
(* Proof. *)
(*   induction Γ; simpl; intros; invclear H; simpl. *)
(*   - split. repeat constructor; auto; fast_set_solver. *)
(*     split. constructor. *)
(*     closed_rty_solver. *)
(*   - mydestr. *)
(*     assert (ctxrR_wf ({[a]} ∪ d) Γ τ) by (split; try closed_rty_solver; invclear H1; auto). *)
(*     apply IHΓ in H. mydestr; listctx_set_simpl. invclear H1. *)
(*     split; auto. split; auto. constructor; auto. *)
(* Qed. *)

Lemma ctxrR_regular1:
  forall Γ τ st e, { st }⟦ τ ⟧{ Γ } e ->
              (ok_dctx (dom _ st) Γ) /\
                ctx_closed_rty (dom _ st) Γ /\
                closed_rty 0 (ctxdom Γ ∪ (dom _ st)) τ.
Proof.
  intros. apply ctxrR_regular0 in H. mydestr.
  do 2 (split; auto). apply ok_dctx_regular2 in H0; mydestr; auto.
Qed.

Lemma ctxrR_regular2:
  forall Γ τ st e, { st }⟦ τ ⟧{ Γ } e -> ⌊ Γ ⌋* ⊢t e ⋮t ⌊ τ ⌋.
Proof.
  induction Γ; simpl; intros; invclear H; simpl; auto.
  - apply rR_regular2 in H0; mydestr; auto.
Qed.

Lemma ctxrR_regular:
  forall Γ τ st e, { st }⟦ τ ⟧{ Γ } e ->
              ⌊ Γ ⌋* ⊢t e ⋮t ⌊ τ ⌋ /\
                (ok_dctx (dom _ st) Γ) /\
                ctx_closed_rty (dom _ st) Γ /\
                closed_rty 0 (ctxdom Γ ∪ (dom _ st)) τ.
Proof.
  intros. split.
  - eapply ctxrR_regular2; eauto.
  - eapply ctxrR_regular1; eauto.
Qed.

(* Lemma ctxrR_regular1: *)
(*   forall Γ τ st e, { st }⟦ τ ⟧{ Γ } e -> closed_rty 0 (ctxdom Γ ∪ dom _ st) τ /\ *)
(*                 ⌊ Γ ⌋* ⊢t e ⋮t ⌊ τ ⌋ /\ (ok_dctx (dom _ st) Γ). *)
(* Proof. *)
(*   induction Γ; simpl; intros; invclear H; simpl. *)
(*   - apply rR_regular2 in H0; mydestr; split; intros; my_simplify_map_eq. *)
(*     closed_rty_solver. *)
(*     split; auto. constructor; auto. fast_set_solver. *)
(*   - split; auto. closed_rty_solver. *)
(*   - mydestr. split; auto. apply rR_regular2 in H; mydestr. closed_rty_solver. *)
(* Qed. *)

Inductive ctxrR2: bstate -> state -> listctx rty -> rty -> rty -> Prop :=
| ctxrR2_nil: forall bst st τ1 τ2,
    closed_rty 0 (dom _ st) τ1 -> closed_rty 0 (dom _ st) τ2 ->
    (forall e, { st }⟦ τ1 ⟧ e -> { st }⟦ τ2 ⟧ e) ->
    ctxrR2 bst st [] τ1 τ2
| ctxrR2_cons_over: forall bst st (x: atom) B d ϕ Γ τ1 τ2,
    ok_dctx (dom _ st) ((x, {v: B | 0 | d | ϕ}) :: Γ) ->
    closed_rty 0 (ctxdom ((x, {v: B | 0 | d | ϕ}) :: Γ) ∪ dom _ st) τ1 ->
    closed_rty 0 (ctxdom ((x, {v: B | 0 | d | ϕ}) :: Γ) ∪ dom _ st) τ2 ->
    (forall (c_x: constant), {st}⟦ {v: B | 0 | d | ϕ} ⟧ c_x -> ctxrR2 bst (<[ x := c_x ]> st) Γ τ1 τ2) ->
    ctxrR2 bst st ((x, {v: B | 0 | d | ϕ}) :: Γ) τ1 τ2
| ctxrR2_cons_under: forall bst st (x: atom) τ_x τ1 τ2 Γ,
    ok_dctx (dom _ st) ((x, τ_x) :: Γ) ->
    closed_rty 0 (ctxdom ((x, τ_x) :: Γ) ∪ dom _ st) τ1 ->
    closed_rty 0 (ctxdom ((x, τ_x) :: Γ) ∪ dom _ st) τ2 ->
    not_overbasety τ_x ->
    (exists e_x_hat, {st}⟦ τ_x ⟧ e_x_hat /\
                  (forall e_x, {st}⟦ τ_x ⟧ e_x ->
                          (∀ (v_x: value), e_x_hat ↪* v_x -> ctxrR2 bst ({ x ↦ v_x } st) Γ τ1 τ2))) ->
    ctxrR2 bst st ((x, τ_x) :: Γ) τ1 τ2.

Notation " '{' st '}⟦' τ1 '⟧⊆⟦' τ2 '⟧{' Γ '}' " := (forall bst, ctxrR2 bst st Γ τ1 τ2) (at level 20, format "{ st }⟦ τ1 ⟧⊆⟦ τ2 ⟧{ Γ }", st constr, τ1 constr, τ2 constr, Γ constr).
Notation " '⟦' τ1 '⟧⊆⟦' τ2 '⟧{' Γ '}' " := (forall bst, ctxrR2 bst ∅ Γ τ1 τ2) (at level 20, format "⟦ τ1 ⟧⊆⟦ τ2 ⟧{ Γ }", τ1 constr, τ2 constr, Γ constr).
