From stdpp Require Import mapset.
From stdpp Require Import natmap.
From CT Require Import CoreLangClass.
From CT Require Import OperationalSemantics.
From CT Require Import BasicTypingClass.
From CT Require Import QualifierClass.
From CT Require Import ListCtx.

Import Atom.
Import CoreLang.
Import Tactics.
Import NamelessTactics.
Import OperationalSemantics.
Import BasicTyping.
Import Qualifier.
Import ListCtx.
Import List.

Inductive rty : Type :=
| overrty (b: base_ty) (ϕ: qualifier)
| underrty (b: base_ty) (ϕ: qualifier)
| arrrty (ρ: rty) (τ: rty).

Global Hint Constructors rty: core.

Notation "'{:' B '|' ϕ '}'" := (overrty B ϕ) (at level 5, format "{: B | ϕ }", B constr, ϕ constr).
Notation "'[:' B '|' ϕ ']'" := (underrty B ϕ) (at level 5, format "[: B | ϕ ]", B constr, ϕ constr).
Notation "ρ '⇨' τ " :=
 (arrrty ρ τ) (at level 80, format "ρ ⇨ τ", right associativity, ρ constr, τ constr).

(** Type erasure (Fig. 5) *)

Fixpoint rty_erase ρ : ty :=
  match ρ with
  | {: B | _} => B
  | [: B | _] => B
  | ρ ⇨ τ => (rty_erase ρ) ⤍ (rty_erase τ)
  end.

Definition ctx_erase (Γ: listctx rty) :=
  ⋃ ((List.map (fun e => {[e.1 := rty_erase e.2]}) Γ): list (amap ty)).

Notation " '⌊' ty '⌋' " := (rty_erase ty) (at level 5, format "⌊ ty ⌋", ty constr).
Notation " '⌊' ty '⌋*' " := (ctx_erase ty) (at level 5, format "⌊ ty ⌋*", ty constr).

(** * Naming related definitions *)

(** free variables *)
Fixpoint rty_fv ρ : aset :=
  match ρ with
  | {: _ | ϕ } => qualifier_fv ϕ
  | [: _ | ϕ ] => qualifier_fv ϕ
  | ρ ⇨ τ => rty_fv ρ ∪ rty_fv τ
  end.

#[global]
  Instance rty_stale : @Stale aset rty := rty_fv.
Arguments rty_stale /.

Fixpoint rty_open (k: nat) (s: value) (ρ: rty) : rty :=
  match ρ with
  | {: B | ϕ } => {: B | qualifier_open (S k) s ϕ }
  | [: B | ϕ ] => [: B | qualifier_open (S k) s ϕ ]
  | ρ ⇨ τ => (rty_open k s ρ) ⇨ (rty_open (S k) s τ)
  end.

(** never use *)
Definition _close_rty (x: atom) (k: nat) (ρ: rty) : rty := ρ.

Fixpoint rty_subst (k: atom) (s: value) (ρ: rty) : rty :=
  match ρ with
  | {: B | ϕ } => {: B | qualifier_subst k s ϕ }
  | [: B | ϕ ] => [: B | qualifier_subst k s ϕ ]
  | ρ ⇨ τ => (rty_subst k s ρ) ⇨ (rty_subst k s τ)
  end.

Inductive lc_rty : rty -> Prop :=
| lc_rty_over_base: forall B ϕ (L : aset),
    (forall x : atom, x ∉ L -> lc_qualifier (qualifier_open 0 x ϕ)) ->
    lc_rty {: B | ϕ }
| lc_rty_under_base: forall B ϕ (L : aset),
    (forall x : atom, x ∉ L -> lc_qualifier (qualifier_open 0 x ϕ)) ->
    lc_rty [: B | ϕ ]
| lc_rty_arr: forall ρ τ (L : aset),
    lc_rty ρ ->
    (forall x : atom, x ∉ L -> lc_rty (rty_open 0 x τ)) ->
    lc_rty (ρ ⇨ τ).

Definition body_rty (e: rty) := exists (L: aset), forall (x: atom), x ∉ L -> lc_rty (rty_open 0 (vfvar x) e).

#[export] Instance rty_substable : Substable rty :=
  {
    substitute := rty_subst;
    fv := rty_fv;
  }.

#[export] Instance rty_ast : Ast rty :=
  {
    open := rty_open;
    lc := lc_rty;
    body := body_rty;
    close := _close_rty;
  }.

(* Notation "'{' k '~>' s '}' e" := (rty_open k s e) (at level 20, k constr). *)
(* Notation "'{' k '~>' s '}' e" := (am_open k s e) (at level 20, k constr). *)
(* Notation "'{' k '~>' s '}' e" := (rty_open k s e) (at level 20, k constr). *)
(* Notation "e '^p^' s" := (rty_open 0 s e) (at level 20). *)
(* Notation "e '^a^' s" := (am_open 0 s e) (at level 20). *)
(* Notation "e '^h^' s" := (rty_open 0 s e) (at level 20). *)

(* Notation "'{' x ':=' s '}'" := (rty_subst x s) (at level 20, format "{ x := s }", x constr). *)
(* Notation "'{' x ':=' s '}a'" := (am_subst x s) (at level 20, format "{ x := s }a", x constr). *)
(* Notation "'{' x ':=' s '}'" := (rty_subst x s) (at level 20, format "{ x := s }", x constr). *)
(** Closed under free variable set *)
Inductive closed_rty (d : aset) (ρ: rty): Prop :=
| closed_rty_: lc_rty ρ -> rty_fv ρ ⊆ d -> closed_rty d ρ.

(** Well-formedness of type context. All terms and types are alpha-converted to
  have unique names. *)
Inductive ok_ctx: listctx rty -> Prop :=
| ok_ctx_nil: ok_ctx []
| ok_ctx_cons: forall (Γ: listctx rty)(x: atom) (ρ: rty),
    ok_ctx Γ ->
    closed_rty (ctxdom Γ) ρ ->
    x ∉ ctxdom Γ ->
    ok_ctx (Γ ++ [(x, ρ)]).

Lemma ok_ctx_ok: forall Γ, ok_ctx Γ -> ok Γ.
Proof.
  induction 1; eauto.
Qed.

(** Shorthands *)
Definition mk_eq_constant c := [: ty_of_const c | b0:c= c ].
Definition mk_bot ty := [: ty | mk_q_under_bot ].
Definition mk_top ty := [: ty | mk_q_under_top ].
Definition mk_eq_var ty (x: atom) := [: ty | b0:x= x ].

(** * Naming properties of refinement type syntax *)


Lemma rty_erase_open_eq (ρ: rty) k s : ⌊ρ⌋ = ⌊{k ~> s} ρ⌋.
Proof.
  generalize dependent k.
  unfold open.
  induction ρ; simpl in *; eauto.
  intros. rewrite <- IHρ1. rewrite <- IHρ2. auto.
Qed.

Lemma rty_erase_subst_eq (ρ: rty) x s : ⌊ρ⌋ = ⌊{x := s} ρ⌋.
Proof.
  unfold substitute.
  induction ρ; simpl in *; eauto.
  rewrite <- IHρ1. rewrite <- IHρ2. auto.
Qed.

Lemma ctx_erase_lookup Γ x ρ :
  ctxfind Γ x = Some ρ ->
  ⌊Γ⌋* !! x = Some ⌊ρ⌋.
Proof.
  induction Γ; simpl; intros; try easy.
  destruct a. case_decide. simplify_eq.
  cbn. simplify_map_eq. reflexivity.
  simp_hyps.
  cbn. rewrite insert_empty. rewrite <- insert_union_singleton_l.
  simplify_map_eq. reflexivity.
Qed.

Lemma ctx_erase_app Γ Γ':
  ⌊Γ ++ Γ'⌋* = ⌊Γ⌋* ∪ ⌊Γ'⌋*.
Proof.
  induction Γ; simpl.
  - cbn. by rewrite map_empty_union.
  - destruct a. cbn. unfold ctx_erase in *. rewrite IHΓ.
    by rewrite map_union_assoc.
Qed.

Lemma ctx_erase_dom Γ :
  dom ⌊Γ⌋* ≡ ctxdom Γ.
Proof.
  induction Γ; simpl.
  - cbn. apply dom_empty.
  - destruct a. cbn in *.
    rewrite insert_empty.
    setoid_rewrite dom_union.
    rewrite dom_singleton.
    f_equiv. eauto.
Qed.

Lemma ctx_erase_app_r Γ x ρ :
  x # Γ ->
  ⌊Γ ++ [(x, ρ)]⌋* = <[x:=⌊ρ⌋]> ⌊Γ⌋*.
Proof.
  intros H.
  rewrite ctx_erase_app.
  cbn. rewrite map_union_empty. rewrite insert_empty.
  rewrite <- insert_union_singleton_r. auto.
  simpl in H. rewrite <- ctx_erase_dom in H.
  by apply not_elem_of_dom.
Qed.

Lemma subst_commute_rty : forall x (u_x: value) y (u_y: value) (ρ: rty),
    x <> y -> x ∉ fv u_y -> y ∉ fv u_x ->
    {x := u_x } ({y := u_y } ρ) = {y := u_y } ({x := u_x } ρ).
Proof.
  unfold substitute.
  induction ρ; simpl; intros; f_equal;
    eauto using subst_commute_qualifier.
Qed.

Lemma subst_fresh_rty: forall (ρ: rty) (x:atom) (u: value),
    x # ρ -> {x := u} ρ = ρ.
Proof.
  unfold substitute.
  induction ρ; simpl; intros; f_equal; eauto using subst_fresh_qualifier;
    auto_apply; my_set_solver.
Qed.

Lemma open_fv_rty (ρ : rty) (v : value) k :
  rty_fv ({k ~> v} ρ) ⊆ fv v ∪ rty_fv ρ.
Proof.
  unfold open.
  revert k.
  induction ρ; simpl; intros; fold_nameless_qualifier; try apply open_fv.
  etrans. apply union_mono; eauto. my_set_solver.
Qed.

Lemma open_fv_rty' (ρ : rty) (v : value) k :
  rty_fv ρ ⊆ rty_fv ({k ~> v} ρ).
Proof.
  all: revert k.
  induction ρ; simpl; intros; eauto using open_fv_qualifier';
    repeat apply union_mono; eauto.
Qed.

Lemma not_in_union_list {A C} `{SemiSet A C} (x : A) (ss : list C):
  x ∉ ⋃ ss ->
  forall s, In s ss -> x ∉ s.
Proof.
  induction ss; cbn; intros; eauto.
  qsimpl.
Qed.

Lemma open_subst_same_rty: forall x y (ρ : rty) k,
    x # ρ ->
    {x := y } ({k ~> x} ρ) = {k ~> y} ρ.
Proof.
  unfold substitute. unfold open.
  induction ρ; simpl; intros; f_equal; eauto using open_subst_same_qualifier;
    auto_apply; my_set_solver.
Qed.

Lemma subst_open_rty: forall (ρ: rty) (x:atom) (u: value) (w: value) (k: nat),
    lc w -> {x := w} ({k ~> u} ρ) = ({k ~> {x := w} u} ({x := w} ρ)).
Proof.
  unfold substitute. unfold open.
  induction ρ; simpl; intros; f_equal; eauto using subst_open_qualifier.
Qed.

Lemma subst_open_rty_closed:
  ∀ (ρ : rty) (x : atom) (u w : value) (k : nat),
    closed u ->
    lc w → {x := w } ({k ~> u} ρ) = {k ~> u} ({x := w } ρ).
Proof.
  unfold substitute. unfold open.
  intros. simpl. rewrite subst_open_rty; auto.
  rewrite (subst_fresh); eauto. set_solver.
Qed.

Lemma subst_open_var_rty: forall x y (u: value) (ρ: rty) (k: nat),
    x <> y -> lc u -> {x := u} ({k ~> y} ρ) = ({k ~> y} ({x := u} ρ)).
Proof.
  intros.
  rewrite subst_open_rty; auto. unfold open. unfold substitute. simpl. rewrite decide_False; auto.
Qed.

Lemma subst_lc_rty : forall x (u: value) (ρ: rty),
    lc_rty ρ -> lc u -> lc_rty ({x := u} ρ).
Proof.
  unfold substitute.
  induction 1; intros; simpl in *; auto_exists_L; intros.
  - rewrite <- subst_open_var_qualifier by (eauto; my_set_solver);
      apply subst_lc_qualifier; eauto. apply H. my_set_solver.
  - rewrite <- subst_open_var_qualifier by (eauto; my_set_solver);
      apply subst_lc_qualifier; eauto. apply H. my_set_solver.
  - rewrite <- subst_open_var_rty by (eauto; my_set_solver); eauto.
    auto_apply; (eauto; my_set_solver).
Qed.

Lemma fv_of_subst_rty_closed:
  forall x (u : value) (ρ: rty),
    closed u ->
    rty_fv ({x := u } ρ) = (rty_fv ρ ∖ {[x]}).
Proof.
  unfold substitute.
  induction ρ; simpl in *; intros; eauto using fv_of_subst_qualifier_closed.
  rewrite IHρ1, IHρ2 by eauto.
  my_set_solver.
Qed.

Lemma open_not_in_eq_rty (x : atom) (ρ : rty) k :
  x # {k ~> x} ρ ->
  forall e, ρ = {k ~> e} ρ.
Proof.
  unfold open. unfold substitute.
  revert k.
  induction ρ; simpl in *; intros; f_equal; eauto using open_not_in_eq_qualifier;
    auto_apply; my_set_solver.
Qed.

Lemma subst_intro_rty: forall (ρ: rty) (x:atom) (w: value) (k: nat),
    x # ρ ->
    lc w -> {x := w} ({k ~> x} ρ) = ({k ~> w} ρ).
Proof.
  unfold open. unfold substitute.
  intros. simpl.
  specialize (subst_open_rty ρ x x w k) as J.
  unfold open in J. unfold substitute in J.
  simpl in J. rewrite decide_True in J; auto.
  rewrite J; auto. rewrite subst_fresh_rty; auto.
Qed.

Lemma lc_subst_rty: forall x (u: value) (ρ: rty), lc_rty ({x := u} ρ) -> lc u -> lc_rty ρ.
Proof.
  unfold substitute. simpl.
  intros.
  remember (rty_subst x u ρ).
  generalize dependent ρ.
  induction H; intros ρ' **; destruct ρ'; simpl in *; simplify_eq; auto_exists_L; intros.
  + specialize (H x0).
    rewrite <- subst_open_var_qualifier in * by (eauto; my_set_solver).
    eapply lc_subst_qualifier; eauto. apply H. my_set_solver.
  + specialize (H x0).
    rewrite <- subst_open_var_qualifier in * by (eauto; my_set_solver).
    eapply lc_subst_qualifier; eauto. apply H. my_set_solver.
  + repeat specialize_with x0.
    apply H2. rewrite subst_open_var_rty; eauto. my_set_solver.
Qed.

Lemma open_rty_idemp: forall u (v: value) (ρ: rty) (k: nat),
    lc v ->
    {k ~> u} ({k ~> v} ρ) = {k ~> v} ρ.
Proof.
  unfold open. unfold substitute.
  induction ρ; intros; simpl; f_equal; eauto using open_qualifier_idemp.
Qed.

Lemma closed_rty_subseteq_proper s1 s2 ρ :
  closed_rty s1 ρ ->
  s1 ⊆ s2 ->
  closed_rty s2 ρ.
Proof.
  intros. sinvert H. split. eauto.
  my_set_solver.
Qed.
