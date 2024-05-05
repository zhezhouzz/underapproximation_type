From stdpp Require Import mapset.
From CT Require Import Atom.
From CT Require Import Qualifier.
Import CoreLang.
Import CoreLangClass.

#[export] Instance qualifier_astprop1 : AstProp1 qualifier :=
  {
    open_fv :=     open_fv_qualifier;
    open_fv' :=     open_fv_qualifier';
    subst_fresh :=     subst_fresh_qualifier;
    open_rec_lc :=     open_rec_lc_qualifier;
    subst_open :=     subst_open_qualifier;
    subst_lc :=     subst_lc_qualifier;
    subst_intro :=     subst_intro_qualifier;
    subst_open_var :=     subst_open_var_qualifier;
    open_lc :=     open_lc_qualifier;
    subst_commute :=     subst_commute_qualifier;
    fv_of_subst_closed :=     fv_of_subst_qualifier_closed;
    open_subst_same :=     open_subst_same_qualifier;
    lc_subst :=     lc_subst_qualifier;
    open_swap :=     open_swap_qualifier;
    open_lc_respect :=     open_lc_respect_qualifier;
    open_idemp :=     open_qualifier_idemp;
    subst_open_closed :=     subst_open_qualifier_closed;
  }.

Lemma fold_open_qualifier (k: nat) (f: value) (v_x: qualifier) : qualifier_open k f v_x = open k f v_x.
Proof. unfold open; auto. Qed.

Lemma fold_lc_qualifier (e: qualifier): lc_qualifier e = lc e.
Proof. unfold lc; auto. Qed.

Lemma fold_fv_qualifier (e: qualifier): qualifier_fv e = fv e.
Proof. unfold fv; auto. Qed.

Lemma fold_subst_qualifier (x: atom) (s: value) (v: qualifier) : qualifier_subst x s v = substitute x s v.
Proof. unfold substitute; auto. Qed.

Lemma fold_body_qualifier  (e: qualifier): body_qualifier e = body e.
Proof. unfold body; auto. Qed.

Ltac fold_nameless_qualifier :=
  repeat match goal with
    | [H: context [ qualifier_open _ _ _ ] |- _ ] => setoid_rewrite fold_open_qualifier in H
    | [H: _ |- context [ qualifier_open _ _ _ ]  ] => setoid_rewrite fold_open_qualifier
    | [H: context [ qualifier_subst _ _ _ ] |- _ ] => setoid_rewrite fold_subst_qualifier in H
    | [H: _ |- context [ qualifier_subst _ _ _ ]  ] => setoid_rewrite fold_subst_qualifier
    | [H: context [ qualifier_fv _ ] |- _ ] => setoid_rewrite fold_fv_qualifier in H
    | [H: _ |- context [ qualifier_fv _ ]  ] => setoid_rewrite fold_fv_qualifier
    | [H: context [ qualifier_open _ _ _ ] |- _ ] => rewrite fold_open_qualifier in H
    | [H: _ |- context [ qualifier_open _ _ _ ]  ] => rewrite fold_open_qualifier
    | [H: context [ qualifier_subst _ _ _ ] |- _ ] => rewrite fold_subst_qualifier in H
    | [H: _ |- context [ qualifier_subst _ _ _ ]  ] => rewrite fold_subst_qualifier
    | [H: context [ qualifier_fv _ ] |- _ ] => rewrite fold_fv_qualifier in H
    | [H: _ |- context [ qualifier_fv _ ]  ] => rewrite fold_fv_qualifier
    | [H: lc_qualifier _ |- _ ] => rewrite fold_lc_qualifier in H
    | [H: _ |- lc_qualifier _ ] => rewrite fold_lc_qualifier
    | [H: body_qualifier _ |- _ ] => rewrite fold_body_qualifier in H
    | [H: _ |- body_qualifier _ ] => rewrite fold_body_qualifier
    end.
