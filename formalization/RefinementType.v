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

Class Erase A B := erase : A -> B.
#[global] Instance rty_erase_ : Erase rty ty := rty_erase.
#[global] Instance ctx_erase_ : Erase (listctx rty) (amap ty) := ctx_erase.

Notation " '⌊' ty '⌋' " := (erase ty) (at level 5, format "⌊ ty ⌋", ty constr).

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
(* Notation "'{' x ':=' s '}h'" := (rty_subst x s) (at level 20, format "{ x := s }h", x constr). *)
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


Lemma rty_erase_open_eq (ρ: rty) k s : erase ρ = erase ({k ~> s} ρ).
Proof.
  generalize dependent k.
  unfold open. unfold erase.
  induction ρ; simpl in *; eauto.
  intros. rewrite <- IHρ1. rewrite <- IHρ2. auto.
Qed.

Lemma rty_erase_subst_eq (ρ: rty) x s : erase ρ = erase ({x := s} ρ).
Proof.
  unfold substitute. unfold erase.
  induction ρ; simpl in *; eauto.
  rewrite <- IHρ1. rewrite <- IHρ2. auto.
Qed.

Lemma ctx_erase_lookup Γ x ρ :
  ctxfind Γ x = Some ρ ->
  ⌊Γ⌋ !! x = Some ⌊ρ⌋.
Proof.
  induction Γ; simpl; intros; try easy.
  destruct a. case_decide. simplify_eq.
  cbn. simplify_map_eq. reflexivity.
  simp_hyps.
  cbn. rewrite insert_empty. rewrite <- insert_union_singleton_l.
  simplify_map_eq. reflexivity.
Qed.

Lemma ctx_erase_app Γ Γ':
  ⌊Γ ++ Γ'⌋ = ⌊Γ⌋ ∪ ⌊Γ'⌋.
Proof.
  unfold erase.
  induction Γ; simpl.
  - cbn. by rewrite map_empty_union.
  - destruct a. unfold ctx_erase_ in *. cbn. unfold ctx_erase in *. rewrite IHΓ.
    by rewrite map_union_assoc.
Qed.

Lemma ctx_erase_dom Γ :
  dom ⌊Γ⌋ ≡ ctxdom Γ.
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
  ⌊Γ ++ [(x, ρ)]⌋ = <[x:=⌊ρ⌋]> ⌊Γ⌋.
Proof.
  intros H.
  rewrite ctx_erase_app.
  cbn. rewrite map_union_empty. rewrite insert_empty.
  rewrite <- insert_union_singleton_r. auto.
  simpl in H. rewrite <- ctx_erase_dom in H.
  by apply not_elem_of_dom.
Qed.

Lemma subst_commute_am : forall x u_x y u_y a,
    x <> y -> x ∉ fv_value u_y -> y ∉ fv_value u_x ->
    {x := u_x }a ({y := u_y }a a) = {y := u_y }a ({x := u_x }a a).
Proof.
  intros.
  induction a; simpl; eauto; f_equal; eauto using subst_commute_qualifier.
Qed.

Lemma subst_commute_rty : forall x u_x y u_y ρ,
    x <> y -> x ∉ fv_value u_y -> y ∉ fv_value u_x ->
    {x := u_x } ({y := u_y } ρ) = {y := u_y } ({x := u_x } ρ)
with subst_commute_rty : forall x u_x y u_y τ,
    x <> y -> x ∉ fv_value u_y -> y ∉ fv_value u_x ->
    {x := u_x }h ({y := u_y }h τ) = {y := u_y }h ({x := u_x }h τ).
Proof.
  destruct ρ; simpl; intros; f_equal;
    eauto using subst_commute_qualifier, subst_commute_am.
  destruct τ; simpl; intros; f_equal;
    eauto using subst_commute_qualifier, subst_commute_am.
Qed.

Lemma subst_fresh_am: forall (a: am) (x:atom) (u: value),
    x # a -> {x := u}a a = a.
Proof.
  intros. induction a; simpl in *; eauto; repeat f_equal;
    eauto using subst_fresh_qualifier;
    auto_apply; try my_set_solver.
Qed.

Lemma subst_fresh_rty: forall (ρ: rty) (x:atom) (u: value),
    x # ρ -> {x := u} ρ = ρ
with subst_fresh_rty: forall (τ: rty) (x:atom) (u: value),
    x # τ -> {x := u}h τ = τ.
Proof.
  destruct ρ; simpl; intros; f_equal; eauto using subst_fresh_qualifier;
    auto_apply; my_set_solver.
  destruct τ; simpl; intros; f_equal;
    solve [ auto_apply; my_set_solver
          | apply subst_fresh_am; my_set_solver ].
Qed.

Lemma open_fv_am (a : am) (v : value) k :
  am_fv ({k ~> v} a) ⊆ am_fv a ∪ fv_value v.
Proof.
  induction a; simpl; eauto using open_fv_qualifier;
    repeat my_set_solver.
Qed.

Lemma open_fv_am' (a : am) (v : value) k :
  am_fv a ⊆ am_fv ({k ~> v} a).
Proof.
  induction a; simpl; eauto using open_fv_qualifier';
    my_set_solver.
Qed.

Lemma open_fv_rty (ρ : rty) (v : value) k :
  rty_fv ({k ~> v} ρ) ⊆ rty_fv ρ ∪ fv_value v
with open_fv_rty (τ : rty) (v : value) k :
  rty_fv ({k ~> v} τ) ⊆ rty_fv τ ∪ fv_value v.
Proof.
  all: revert k.
  destruct ρ; simpl; intros; eauto using open_fv_qualifier.
  etrans. apply union_mono; eauto. my_set_solver.
  destruct τ; simpl; intros.
  etrans. repeat apply union_mono; eauto using open_fv_am. my_set_solver.
  etrans. repeat apply union_mono; eauto. my_set_solver.
Qed.

Lemma open_fv_rty' (ρ : rty) (v : value) k :
  rty_fv ρ ⊆ rty_fv ({k ~> v} ρ)
with open_fv_rty' (τ : rty) (v : value) k :
  rty_fv τ ⊆ rty_fv ({k ~> v} τ).
Proof.
  all: revert k.
  destruct ρ; simpl; intros; eauto using open_fv_qualifier';
    repeat apply union_mono; eauto.
  destruct τ; simpl; intros;
    repeat apply union_mono; eauto using open_fv_am'.
Qed.

Lemma open_subst_same_am: forall x y (a : am) k,
    x # a ->
    {x := y }a ({k ~> x} a) = {k ~> y} a.
Proof.
  induction a; cbn; intros; eauto.
  f_equal. eauto using open_subst_same_qualifier.
  all:
  repeat
    match goal with
    | H : forall k, _ # _ -> _ =_ |- _ => rewrite H by my_set_solver; eauto
    end.
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
    {x := y } ({k ~> x} ρ) = {k ~> y} ρ
with open_subst_same_rty: forall x y (τ : rty) k,
    x # τ ->
    {x := y }h ({k ~> x} τ) = {k ~> y} τ.
Proof.
  destruct ρ; simpl; intros; f_equal; eauto using open_subst_same_qualifier;
    auto_apply; my_set_solver.
  destruct τ; simpl; intros; f_equal;
    solve [ auto_apply; my_set_solver
          | apply open_subst_same_am; my_set_solver ].
Qed.

Lemma subst_open_am: forall (a: am) (x:atom) (u: value) (w: value) (k: nat),
    lc w -> {x := w}a ({k ~> u} a) = ({k ~> {x := w}v u} ({x := w}a a)).
Proof.
  induction a; cbn; intros; eauto.
  f_equal. eauto using subst_open_qualifier.
  all:
  repeat
    match goal with
    | H : context [lc _ -> _] |- _ => rewrite H by my_set_solver; eauto
    end.
Qed.

Lemma subst_open_rty: forall (ρ: rty) (x:atom) (u: value) (w: value) (k: nat),
    lc w -> {x := w} ({k ~> u} ρ) = ({k ~> {x := w}v u} ({x := w} ρ))
with subst_open_rty: forall (τ: rty) (x:atom) (u: value) (w: value) (k: nat),
    lc w -> {x := w}h ({k ~> u} τ) = ({k ~> {x := w}v u} ({x := w}h τ)).
Proof.
  destruct ρ; simpl; intros; f_equal; eauto using subst_open_qualifier.
  destruct τ; simpl; intros; f_equal; eauto using subst_open_am.
Qed.

Lemma subst_open_rty_closed:
  ∀ (ρ : rty) (x : atom) (u w : value) (k : nat),
    closed_value u ->
    lc w → {x := w } ({k ~> u} ρ) = {k ~> u} ({x := w } ρ).
Proof.
  intros. rewrite subst_open_rty; auto.
  rewrite (subst_fresh_value); eauto. set_solver.
Qed.

Lemma subst_open_am_closed:
  ∀ (a : am) (x : atom) (u w : value) (k : nat),
    closed_value u ->
    lc w → {x := w }a ({k ~> u} a) = {k ~> u} ({x := w }a a).
Proof.
  intros. rewrite subst_open_am; auto.
  rewrite (subst_fresh_value); eauto. set_solver.
Qed.

Lemma subst_open_rty_closed:
  ∀ (τ : rty) (x : atom) (u w : value) (k : nat),
    closed_value u ->
    lc w → {x := w }h ({k ~> u} τ) = {k ~> u} ({x := w }h τ).
Proof.
  intros. rewrite subst_open_rty; auto.
  rewrite (subst_fresh_value); eauto. set_solver.
Qed.

Lemma subst_open_var_am: forall x y (u: value) (a: am) (k: nat),
    x <> y -> lc u -> {x := u}a ({k ~> y} a) = ({k ~> y} ({x := u}a a)).
Proof.
  intros.
  rewrite subst_open_am; auto. simpl. rewrite decide_False; auto.
Qed.

Lemma subst_open_var_rty: forall x y (u: value) (ρ: rty) (k: nat),
    x <> y -> lc u -> {x := u} ({k ~> y} ρ) = ({k ~> y} ({x := u} ρ)).
Proof.
  intros.
  rewrite subst_open_rty; auto. simpl. rewrite decide_False; auto.
Qed.

Lemma subst_open_var_rty: forall x y (u: value) (τ: rty) (k: nat),
    x <> y -> lc u -> {x := u}h ({k ~> y} τ) = ({k ~> y} ({x := u}h τ)).
Proof.
  intros.
  rewrite subst_open_rty; auto. simpl. rewrite decide_False; auto.
Qed.

Lemma subst_lc_am : forall x (u: value) (a: am),
    lc_am a -> lc u -> lc_am ({x := u}a a).
Proof.
  induction 1; intros Hu; eauto using lc_am.
  econstructor.
  auto_exists_L_intros.
  specialize_with x0.
  specialize_with y.
  rewrite <- !subst_open_var_qualifier by (eauto; my_set_solver).
  eauto using subst_lc_qualifier.
Qed.

Lemma subst_lc_rty : forall x (u: value) (ρ: rty),
    lc_rty ρ -> lc u -> lc_rty ({x := u} ρ)
with subst_lc_rty : forall x (u: value) (τ: rty),
    lc_rty τ -> lc u -> lc_rty ({x := u}h τ).
Proof.
  all: destruct 1; intros; simpl; econstructor; eauto using subst_lc_am;
    instantiate_atom_listctx.
  - rewrite <- subst_open_var_qualifier by (eauto; my_set_solver);
      eauto using subst_lc_qualifier.
  - rewrite <- subst_open_var_rty by (eauto; my_set_solver); eauto.
  - rewrite <- subst_open_var_rty by (eauto; my_set_solver); eauto.
Qed.

Lemma fv_of_subst_am_closed:
  forall x (u : value) (a: am),
    closed_value u ->
    am_fv ({x := u }a a) = (am_fv a ∖ {[x]}).
Proof.
  induction a; simpl; eauto using fv_of_subst_qualifier_closed; my_set_solver.
Qed.

Lemma fv_of_subst_rty_closed:
  forall x (u : value) (ρ: rty),
    closed_value u ->
    rty_fv ({x := u } ρ) = (rty_fv ρ ∖ {[x]})
with fv_of_subst_rty_closed:
  forall x (u : value) (τ: rty),
    closed_value u ->
    rty_fv ({x := u }h τ) = (rty_fv τ ∖ {[x]}).
Proof.
  destruct ρ; simpl; intros; eauto using fv_of_subst_qualifier_closed.
  rewrite !fv_of_subst_rty_closed, !fv_of_subst_rty_closed by eauto.
  my_set_solver.
  destruct τ; simpl; intros.
  rewrite !fv_of_subst_am_closed, !fv_of_subst_rty_closed by eauto.
  my_set_solver.
  rewrite !fv_of_subst_rty_closed by eauto.
  my_set_solver.
Qed.

Lemma open_not_in_eq_am (x : atom) (a : am) k :
  x # {k ~> x} a ->
  forall e, a = {k ~> e} a.
Proof.
  induction a; simpl; intros; eauto.
  f_equal. eapply open_not_in_eq_qualifier. my_set_solver.
  all: f_equal; auto_apply; my_set_solver.
Qed.

Lemma open_not_in_eq_rty (x : atom) (ρ : rty) k :
  x # {k ~> x} ρ ->
  forall e, ρ = {k ~> e} ρ
with open_not_in_eq_rty (x : atom) (τ : rty) k :
  x # {k ~> x} τ ->
  forall e, τ = {k ~> e} τ.
Proof.
  all: revert k; specialize (open_not_in_eq_rty x); specialize (open_not_in_eq_rty x).
  destruct ρ; simpl; intros; f_equal; eauto using open_not_in_eq_qualifier;
    auto_apply; my_set_solver.
  destruct τ; simpl; intros; f_equal;
    solve [ auto_apply; my_set_solver
          | apply (open_not_in_eq_am x); my_set_solver ].
Qed.

Lemma subst_intro_rty: forall (ρ: rty) (x:atom) (w: value) (k: nat),
    x # ρ ->
    lc w -> {x := w} ({k ~> x} ρ) = ({k ~> w} ρ).
Proof.
  intros.
  specialize (subst_open_rty ρ x x w k) as J.
  simpl in J. rewrite decide_True in J; auto.
  rewrite J; auto. rewrite subst_fresh_rty; auto.
Qed.

Lemma lc_subst_am:
  forall x (u: value) (a: am), lc_am ({x := u}a a) -> lc u -> lc_am a.
Proof.
  intros.
  remember (({x:=u}a) a).
  generalize dependent a.
  induction H; intros;
      match goal with
      | H : _ = {_:=_}a ?a |- _ => destruct a; simpl in *; simplify_eq
      end; eauto using lc_am.
  econstructor.
  auto_exists_L_intros. specialize_with x0. specialize_with y.
  rewrite <- !subst_open_var_qualifier in H by (eauto; my_set_solver).
  eauto using lc_subst_qualifier.
Qed.

Lemma lc_subst_rty: forall x (u: value) (ρ: rty), lc_rty ({x := u} ρ) -> lc u -> lc_rty ρ
with lc_subst_rty: forall x (u: value) (τ: rty), lc_rty ({x := u}h τ) -> lc u -> lc_rty τ.
Proof.
  intros.
  remember (({x:=u}) ρ).
  generalize dependent ρ.
  destruct H; intros ρ' **; destruct ρ'; simpl in *; simplify_eq;
    econstructor; eauto;
    instantiate_atom_listctx.
  rewrite <- subst_open_var_qualifier in * by (eauto; my_set_solver);
    eauto using lc_subst_qualifier.
  rewrite <- subst_open_var_rty in * by (eauto; my_set_solver); eauto.
  rewrite <- subst_open_var_rty in * by (eauto; my_set_solver); eauto.

  intros.
  remember (({x:=u}h) τ).
  generalize dependent τ.
  destruct H; intros τ' **; destruct τ'; simpl in *; simplify_eq;
    econstructor; eauto using lc_subst_am.
Qed.

Lemma open_am_idemp: forall u (v: value) (a: am) (k: nat),
    lc v ->
    {k ~> u} ({k ~> v} a) = ({k ~> v} a).
Proof.
  induction a; intros; simpl; f_equal; eauto using open_qualifier_idemp.
Qed.

Lemma open_rty_idemp: forall u (v: value) (ρ: rty) (k: nat),
    lc v ->
    {k ~> u} ({k ~> v} ρ) = {k ~> v} ρ
with open_rty_idemp: forall u (v: value)  (τ: rty) (k: nat),
    lc v ->
    {k ~> u} ({k ~> v} τ) = {k ~> v} τ.
Proof.
  destruct ρ; intros; simpl; f_equal; eauto using open_qualifier_idemp.
  destruct τ; intros; simpl; f_equal; eauto using open_am_idemp.
Qed.

Lemma closed_rty_subseteq_proper s1 s2 ρ :
  closed_rty s1 ρ ->
  s1 ⊆ s2 ->
  closed_rty s2 ρ.
Proof.
  intros. sinvert H. split. eauto.
  my_set_solver.
Qed.

Lemma closed_rty_hoare_congr d ρ a b :
  closed_rty d ρ ->
  closed_am d a ->
  closed_am d b ->
  closed_rty d (<[ a ] ρ [ b ]>).
Proof.
  inversion 1. inversion 1. inversion 1.
  econstructor.
  econstructor; eauto.
  simpl. my_set_solver.
Qed.
