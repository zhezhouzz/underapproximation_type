From stdpp Require Import mapset.
From CT Require Import Atom.
From CT Require Import RefinementType.
Import CoreLang.
Import CoreLangClass.

Lemma open_lc_rty: forall (u: value) (t: rty), body t -> lc u -> lc ({0 ~> u} t).
Admitted.
Lemma open_lc_respect_rty: forall (t: rty) (u v : value) k, lc ({k ~> u} t) -> lc u -> lc v -> lc ({k ~> v} t).
Admitted.
Lemma open_rec_lc_rty: forall (v: value) (u: rty) (k: nat), lc u -> {k ~> v} u = u.
Admitted.
Lemma open_swap_rty: forall (t: rty) i j (u v: value), lc u -> lc v -> i <> j -> {i ~> v} ({j ~> u} t) = {j ~> u} ({i ~> v} t).
Admitted.

#[export] Instance rty_astprop1 : AstProp1 rty :=
  {
    open_fv :=     open_fv_rty;
    open_fv' :=     open_fv_rty';
    subst_fresh :=     subst_fresh_rty;
    open_rec_lc :=     open_rec_lc_rty;
    subst_open :=     subst_open_rty;
    subst_lc :=     subst_lc_rty;
    subst_intro :=     subst_intro_rty;
    subst_open_var :=     subst_open_var_rty;
    open_lc :=     open_lc_rty;
    subst_commute :=     subst_commute_rty;
    fv_of_subst_closed :=     fv_of_subst_rty_closed;
    open_subst_same :=     open_subst_same_rty;
    lc_subst :=     lc_subst_rty;
    open_swap :=     open_swap_rty;
    open_lc_respect :=     open_lc_respect_rty;
    open_idemp :=     open_rty_idemp;
    subst_open_closed :=     subst_open_rty_closed;
  }.

Lemma fold_open_rty (k: nat) (f: value) (v_x: rty) : rty_open k f v_x = open k f v_x.
Proof. unfold open; auto. Qed.

Lemma fold_lc_rty (e: rty): lc_rty e = lc e.
Proof. unfold lc; auto. Qed.

Lemma fold_fv_rty (e: rty): rty_fv e = fv e.
Proof. unfold fv; auto. Qed.

Lemma fold_subst_rty (x: atom) (s: value) (v: rty) : rty_subst x s v = substitute x s v.
Proof. unfold substitute; auto. Qed.

Lemma fold_body_rty  (e: rty): body_rty e = body e.
Proof. unfold body; auto. Qed.

Ltac fold_nameless_rty :=
  repeat match goal with
    | [H: context [ rty_open _ _ _ ] |- _ ] => setoid_rewrite fold_open_rty in H
    | [H: _ |- context [ rty_open _ _ _ ]  ] => setoid_rewrite fold_open_rty
    | [H: context [ rty_subst _ _ _ ] |- _ ] => setoid_rewrite fold_subst_rty in H
    | [H: _ |- context [ rty_subst _ _ _ ]  ] => setoid_rewrite fold_subst_rty
    | [H: context [ rty_fv _ ] |- _ ] => setoid_rewrite fold_fv_rty in H
    | [H: _ |- context [ rty_fv _ ]  ] => setoid_rewrite fold_fv_rty
    | [H: context [ rty_open _ _ _ ] |- _ ] => rewrite fold_open_rty in H
    | [H: _ |- context [ rty_open _ _ _ ]  ] => rewrite fold_open_rty
    | [H: context [ rty_subst _ _ _ ] |- _ ] => rewrite fold_subst_rty in H
    | [H: _ |- context [ rty_subst _ _ _ ]  ] => rewrite fold_subst_rty
    | [H: context [ rty_fv _ ] |- _ ] => rewrite fold_fv_rty in H
    | [H: _ |- context [ rty_fv _ ]  ] => rewrite fold_fv_rty
    | [H: lc_rty _ |- _ ] => rewrite fold_lc_rty in H
    | [H: _ |- lc_rty _ ] => rewrite fold_lc_rty
    | [H: body_rty _ |- _ ] => rewrite fold_body_rty in H
    | [H: _ |- body_rty _ ] => rewrite fold_body_rty
    end.
