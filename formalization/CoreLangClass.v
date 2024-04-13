From stdpp Require Import mapset.
From CT Require Import Atom.
From CT Require Import CoreLang.
From CT Require Import CoreLangProp.
Import CoreLang.

Class AstProp AST `{Stale aset AST} `{Ast AST} : Type :=
  {
    close_open_var: forall (e: AST) (x: atom) (k: nat), x ∉ (fv e) -> {k <~ x} ({k ~~> x} e) = e;
    open_fv: forall (v: AST) (u: value) (k: nat), fv ({k ~> u} v) ⊆ fv u ∪ fv v;
    open_fv': forall (v: AST) (u: value) (k: nat), fv v ⊆ fv ({k ~> u} v);
    close_var_fv: forall (v: AST) (x: atom) (k: nat), fv ({k <~ x} v) = (fv v) ∖ {[x]};
    subst_fresh: forall (e: AST) (x:atom) (u: value), x ∉ (fv e) -> {x := u} e = e;
    open_rec_lc: forall (v: value) (u: AST) (k: nat), ast_lc u -> {k ~> v} u = u;
    subst_open: forall (v: AST) (x:atom) (u: value) (w: value) (k: nat),
      ast_lc w -> {x := w} ({k ~> u} v) = ({k ~> {x := w} u} ({x := w} v));
    close_var_rename: forall x y (e: AST) k, y ∉ (fv e) -> {k <~ x} e = {k <~ y} ({x := (vfvar y)} e);
    subst_lc: forall x (u: value) (t: AST), ast_lc t -> ast_lc u -> ast_lc ({x := u} t);
    open_close_var: forall (x: atom) (t: AST), ast_lc t -> {0 ~~> x} ({0 <~ x} t) = t;
    subst_intro: forall (v: AST) (x:atom) (w: value) (k: nat),
      x # v -> ast_lc w -> {x := w} ({k ~~> x} v) = ({k ~> w} v);
    subst_open_var: forall x y (u: value) (t: AST) (k: nat),
      x <> y -> ast_lc u -> {x := u} ({k ~~> y} t) = ({k ~~> y} ({x := u} t));
    subst_body: forall x (u: value) (t: AST), ast_body t -> ast_lc u -> ast_body ({x := u} t);
    open_lc: forall (u: value) (t: AST), ast_body t -> ast_lc u -> ast_lc ({0 ~> u} t);
    open_with_fresh_include_fv: forall (x: atom) (e: AST) k,
      x ∉ fv e -> ({[x]} ∪ fv e) ⊆ ({[x]} ∪ fv ({k ~~> x} e));
    subst_as_close_open_: forall (x: atom) (u: value) (e: AST) (k: nat),
      {k ~> u} e = e -> {k ~> u} ({k <~ x} e) = {x := u} e;
    subst_as_close_open: forall (x: atom) (u: value) (e: AST),
      ast_lc e -> {0 ~> u} ({0 <~ x} e) = {x := u} e;
    close_fresh_rec: forall (x: atom) (e: AST) (k: nat), x ∉ fv e -> { k <~ x} e = e;
    subst_close: ∀ (x y: atom) u,
      x ∉ fv u -> x <> y -> forall (e: AST) k, {k <~ x} ({y := u } e) = {y := u } ({k <~ x} e);
    subst_commute: forall x u_x y u_y (e: AST),
      x <> y -> x ∉ fv u_y -> y ∉ fv u_x -> {x := u_x } ({y := u_y } e) = {y := u_y } ({x := u_x } e);
    subst_shadow: forall (x z: atom) (u: value) (e: AST), x # e -> {x := u } ({z := (vfvar x) } e) = {z := u } e;
    subst_subst: ∀ (x : atom) (u_x : value) (y : atom) (u_y: value) (e: AST),
      x ≠ y → y ∉ fv u_x → {x := u_x } ({y := u_y } e) = {y := {x := u_x } u_y } ({x := u_x } e);
    fv_of_subst: forall x (u : value) (e: AST), fv ({x := u } e) ⊆ (fv e ∖ {[x]}) ∪ fv u;
    fv_of_subst_closed: forall x (u: value) (e: AST), fv u ≡ ∅ -> fv ({x := u } e) = (fv e ∖ {[x]});
    open_subst_same: forall x (y: value) (e: AST) k, x # e -> {x := y } ({k ~~> x} e) = {k ~> y} e;
    close_rm_fv: forall x (e: AST) k, x ∉ fv ({k <~ x} e);
    close_then_subst_same: forall x v_x (e: AST), ({x := v_x } (x \ e)) = (x \ e);
    subst_open_closed: ∀ (v : AST) (x : atom) (u w : value) (k : nat),
      closed u -> ast_lc w → {x := w } ({k ~> u} v) = {k ~> u} ({x := w } v);
    body_lc_after_close: forall (x: atom) (e: AST), ast_lc e -> ast_body ({0 <~ x} e);
    lc_fresh_var_implies_body: forall (e: AST) (x: atom), x # e -> ast_lc (e ^^^ x) -> ast_body e;
    open_not_in_eq: forall (x : atom) (t : AST) k, x # {k ~~> x} t -> forall e, t = {k ~> e} t;
    lc_subst: forall x (u: value) (t: AST), ast_lc ({x := u} t) -> ast_lc u -> ast_lc t;
    open_swap: forall (t: AST) i j (u v: value), ast_lc u -> ast_lc v -> i <> j -> {i ~> v} ({j ~> u} t) = {j ~> u} ({i ~> v} t);
    open_lc_respect: forall (t: AST) (u v : value) k, ast_lc ({k ~> u} t) -> ast_lc u -> ast_lc v -> ast_lc ({k ~> v} t);
    open_idemp: forall u (v: value) (t: AST) (k: nat), ast_lc v -> {k ~> u} ({k ~> v} t) = ({k ~> v} t);
  }.

Import CoreLangProp.

#[export] Instance value_astprop : AstProp value :=
  {
    close_open_var :=     close_open_var_value;
    open_fv :=     open_fv_value;
    open_fv' :=     open_fv_value';
    close_var_fv :=     close_var_fv_value;
    subst_fresh :=     subst_fresh_value;
    open_rec_lc :=     open_rec_lc_value;
    subst_open :=     subst_open_value;
    close_var_rename :=     close_var_rename_value;
    subst_lc :=     subst_lc_value;
    open_close_var :=     open_close_var_value;
    subst_intro :=     subst_intro_value;
    subst_open_var :=     subst_open_var_value;
    subst_body :=     subst_body_value;
    open_lc :=     open_lc_value;
    open_with_fresh_include_fv :=     open_with_fresh_include_fv_value;
    subst_as_close_open_ :=     subst_as_close_open_value_;
    subst_as_close_open :=     subst_as_close_open_value;
    close_fresh_rec :=     close_fresh_rec_value;
    subst_close :=     subst_close_value;
    subst_commute :=     subst_commute_value;
    subst_shadow :=     subst_shadow_value;
    subst_subst :=     subst_subst_value;
    fv_of_subst :=     fv_of_subst_value;
    fv_of_subst_closed :=     fv_of_subst_value_closed;
    open_subst_same :=     open_subst_same_value;
    close_rm_fv :=     close_rm_fv_value;
    close_then_subst_same :=     close_then_subst_same_value;
    subst_open_closed :=     subst_open_value_closed;
    body_lc_after_close :=     body_lc_after_close_value;
    lc_fresh_var_implies_body :=     lc_fresh_var_implies_body_value;
    open_not_in_eq :=     open_not_in_eq_value;
    lc_subst :=     lc_subst_value;
    open_swap :=     open_swap_value;
    open_lc_respect :=     open_lc_respect_value;
    open_idemp :=     open_value_idemp;
  }.

#[export] Instance tm_astprop : AstProp tm :=
  {
    close_open_var :=     close_open_var_tm;
    open_fv :=     open_fv_tm;
    open_fv' :=     open_fv_tm';
    close_var_fv :=     close_var_fv_tm;
    subst_fresh :=     subst_fresh_tm;
    open_rec_lc :=     open_rec_lc_tm;
    subst_open :=     subst_open_tm;
    close_var_rename :=     close_var_rename_tm;
    subst_lc :=     subst_lc_tm;
    open_close_var :=     open_close_var_tm;
    subst_intro :=     subst_intro_tm;
    subst_open_var :=     subst_open_var_tm;
    subst_body :=     subst_body_tm;
    open_lc :=     open_lc_tm;
    open_with_fresh_include_fv :=     open_with_fresh_include_fv_tm;
    subst_as_close_open_ :=     subst_as_close_open_tm_;
    subst_as_close_open :=     subst_as_close_open_tm;
    close_fresh_rec :=     close_fresh_rec_tm;
    subst_close :=     subst_close_tm;
    subst_commute :=     subst_commute_tm;
    subst_shadow :=     subst_shadow_tm;
    subst_subst :=     subst_subst_tm;
    fv_of_subst :=     fv_of_subst_tm;
    fv_of_subst_closed :=     fv_of_subst_tm_closed;
    open_subst_same :=     open_subst_same_tm;
    close_rm_fv :=     close_rm_fv_tm;
    close_then_subst_same :=     close_then_subst_same_tm;
    subst_open_closed :=     subst_open_tm_closed;
    body_lc_after_close :=     body_lc_after_close_tm;
    lc_fresh_var_implies_body := lc_fresh_var_implies_body;
    open_not_in_eq :=     open_not_in_eq_tm;
    lc_subst :=     lc_subst_tm;
    open_swap :=     open_swap_tm;
    open_lc_respect :=     open_lc_respect_tm;
    open_idemp :=     open_tm_idemp;
  }.
