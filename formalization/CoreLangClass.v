From stdpp Require Import mapset.
From CT Require Import Atom.
From CT Require Import CoreLang.
From CT Require Import CoreLangProp.
Import CoreLang.

Class AstProp1 AST `{Stale aset AST} `{Ast AST} : Type :=
  {
    fv_of_subst_closed: forall x (u: value) (e: AST), fv u ≡ ∅ -> fv ({x := u } e) = (fv e ∖ {[x]});
    lc_subst: forall x (u: value) (t: AST), lc ({x := u} t) -> lc u -> lc t;
    open_fv: forall (v: AST) (u: value) (k: nat), fv ({k ~> u} v) ⊆ fv u ∪ fv v;
    open_fv': forall (v: AST) (u: value) (k: nat), fv v ⊆ fv ({k ~> u} v);
    open_idemp: forall u (v: value) (t: AST) (k: nat), lc v -> {k ~> u} ({k ~> v} t) = ({k ~> v} t);
    open_lc: forall (u: value) (t: AST), body t -> lc u -> lc ({0 ~> u} t);
    open_lc_respect: forall (t: AST) (u v : value) k, lc ({k ~> u} t) -> lc u -> lc v -> lc ({k ~> v} t);
    open_rec_lc: forall (v: value) (u: AST) (k: nat), lc u -> {k ~> v} u = u;
    open_subst_same: forall x (y: value) (e: AST) k, x # e -> {x := y } ({k ~~> x} e) = {k ~> y} e;
    open_swap: forall (t: AST) i j (u v: value), lc u -> lc v -> i <> j -> {i ~> v} ({j ~> u} t) = {j ~> u} ({i ~> v} t);
    subst_commute: forall x u_x y u_y (e: AST),
      x <> y -> x ∉ fv u_y -> y ∉ fv u_x -> {x := u_x } ({y := u_y } e) = {y := u_y } ({x := u_x } e);
    subst_fresh: forall (e: AST) (x:atom) (u: value), x ∉ (fv e) -> {x := u} e = e;
    subst_intro: forall (v: AST) (x:atom) (w: value) (k: nat),
      x # v -> lc w -> {x := w} ({k ~~> x} v) = ({k ~> w} v);
    subst_lc: forall x (u: value) (t: AST), lc t -> lc u -> lc ({x := u} t);
    subst_open: forall (v: AST) (x:atom) (u: value) (w: value) (k: nat),
      lc w -> {x := w} ({k ~> u} v) = ({k ~> {x := w} u} ({x := w} v));
    subst_open_closed: ∀ (v : AST) (x : atom) (u w : value) (k : nat),
      closed u -> lc w → {x := w } ({k ~> u} v) = {k ~> u} ({x := w } v);
    subst_open_var: forall x y (u: value) (t: AST) (k: nat),
      x <> y -> lc u -> {x := u} ({k ~~> y} t) = ({k ~~> y} ({x := u} t));
  }.

Class AstProp AST `{Stale aset AST} `{Ast AST} `{AstProp1 AST} : Type :=
  {
    close_open_var: forall (e: AST) (x: atom) (k: nat), x ∉ (fv e) -> {k <~ x} ({k ~~> x} e) = e;
    close_var_fv: forall (v: AST) (x: atom) (k: nat), fv ({k <~ x} v) = (fv v) ∖ {[x]};
    close_var_rename: forall x y (e: AST) k, y ∉ (fv e) -> {k <~ x} e = {k <~ y} ({x := (vfvar y)} e);
    open_close_var: forall (x: atom) (t: AST), lc t -> {0 ~~> x} ({0 <~ x} t) = t;
    subst_body: forall x (u: value) (t: AST), body t -> lc u -> body ({x := u} t);
    open_with_fresh_include_fv: forall (x: atom) (e: AST) k,
      x ∉ fv e -> ({[x]} ∪ fv e) ⊆ ({[x]} ∪ fv ({k ~~> x} e));
    subst_as_close_open_: forall (x: atom) (u: value) (e: AST) (k: nat),
      {k ~> u} e = e -> {k ~> u} ({k <~ x} e) = {x := u} e;
    subst_as_close_open: forall (x: atom) (u: value) (e: AST),
      lc e -> {0 ~> u} ({0 <~ x} e) = {x := u} e;
    close_fresh_rec: forall (x: atom) (e: AST) (k: nat), x ∉ fv e -> { k <~ x} e = e;
    subst_close: ∀ (x y: atom) u,
      x ∉ fv u -> x <> y -> forall (e: AST) k, {k <~ x} ({y := u } e) = {y := u } ({k <~ x} e);
    subst_shadow: forall (x z: atom) (u: value) (e: AST), x # e -> {x := u } ({z := (vfvar x) } e) = {z := u } e;
    subst_subst: ∀ (x : atom) (u_x : value) (y : atom) (u_y: value) (e: AST),
      x ≠ y → y ∉ fv u_x → {x := u_x } ({y := u_y } e) = {y := {x := u_x } u_y } ({x := u_x } e);
    fv_of_subst: forall x (u : value) (e: AST), fv ({x := u } e) ⊆ (fv e ∖ {[x]}) ∪ fv u;
    close_rm_fv: forall x (e: AST) k, x ∉ fv ({k <~ x} e);
    close_then_subst_same: forall x v_x (e: AST), ({x := v_x } (x \ e)) = (x \ e);
    body_lc_after_close: forall (x: atom) (e: AST), lc e -> body ({0 <~ x} e);
    lc_fresh_var_implies_body: forall (e: AST) (x: atom), x # e -> lc (e ^^^ x) -> body e;
    open_not_in_eq: forall (x : atom) (t : AST) k, x # {k ~~> x} t -> forall e, t = {k ~> e} t;
    lc_implies_body: forall (e: AST), lc e -> body e;
  }.

Import CoreLangProp.

#[export] Instance tm_astprop1 : AstProp1 tm :=
  {
    open_fv :=     open_fv_tm;
    open_fv' :=     open_fv_tm';
    subst_fresh :=     subst_fresh_tm;
    open_rec_lc :=     open_rec_lc_tm;
    subst_open :=     subst_open_tm;
    subst_lc :=     subst_lc_tm;
    subst_intro :=     subst_intro_tm;
    subst_open_var :=     subst_open_var_tm;
    open_lc :=     open_lc_tm;
    subst_commute :=     subst_commute_tm;
    fv_of_subst_closed :=     fv_of_subst_tm_closed;
    open_subst_same :=     open_subst_same_tm;
    subst_open_closed :=     subst_open_tm_closed;
    lc_subst :=     lc_subst_tm;
    open_idemp :=     open_tm_idemp;
    open_lc_respect :=     open_lc_respect_tm;
    open_swap :=     open_swap_tm;
  }.

#[export] Instance tm_astprop : AstProp tm :=
  {
    close_open_var :=     close_open_var_tm;
    close_var_fv :=     close_var_fv_tm;
    close_var_rename :=     close_var_rename_tm;
    open_close_var :=     open_close_var_tm;
    subst_body :=     subst_body_tm;
    open_with_fresh_include_fv :=     open_with_fresh_include_fv_tm;
    subst_as_close_open_ :=     subst_as_close_open_tm_;
    subst_as_close_open :=     subst_as_close_open_tm;
    close_fresh_rec :=     close_fresh_rec_tm;
    subst_close :=     subst_close_tm;
    subst_shadow :=     subst_shadow_tm;
    subst_subst :=     subst_subst_tm;
    fv_of_subst :=     fv_of_subst_tm;
    close_rm_fv :=     close_rm_fv_tm;
    close_then_subst_same :=     close_then_subst_same_tm;
    body_lc_after_close :=     body_lc_after_close_tm;
    lc_fresh_var_implies_body := lc_fresh_var_implies_body;
    open_not_in_eq :=     open_not_in_eq_tm;
    lc_implies_body := lc_implies_body_tm;
  }.

#[export] Instance value_astprop1 : AstProp1 value :=
  {
    open_fv :=     open_fv_value;
    open_fv' :=     open_fv_value';
    subst_fresh :=     subst_fresh_value;
    open_rec_lc :=     open_rec_lc_value;
    subst_open :=     subst_open_value;
    subst_lc :=     subst_lc_value;
    subst_intro :=     subst_intro_value;
    subst_open_var :=     subst_open_var_value;
    open_lc :=     open_lc_value;
    subst_commute :=     subst_commute_value;
    fv_of_subst_closed :=     fv_of_subst_value_closed;
    open_subst_same :=     open_subst_same_value;
    subst_open_closed :=     subst_open_value_closed;
    lc_subst :=     lc_subst_value;
    open_idemp :=     open_value_idemp;
    open_lc_respect :=     open_lc_respect_value;
    open_swap :=     open_swap_value;
  }.

#[export] Instance value_astprop : AstProp value :=
  {
    close_open_var :=     close_open_var_value;
    close_var_fv :=     close_var_fv_value;
    close_var_rename :=     close_var_rename_value;
    open_close_var :=     open_close_var_value;
    subst_body :=     subst_body_value;
    open_with_fresh_include_fv :=     open_with_fresh_include_fv_value;
    subst_as_close_open_ :=     subst_as_close_open_value_;
    subst_as_close_open :=     subst_as_close_open_value;
    close_fresh_rec :=     close_fresh_rec_value;
    subst_close :=     subst_close_value;
    subst_shadow :=     subst_shadow_value;
    subst_subst :=     subst_subst_value;
    fv_of_subst :=     fv_of_subst_value;
    close_rm_fv :=     close_rm_fv_value;
    close_then_subst_same :=     close_then_subst_same_value;
    body_lc_after_close :=     body_lc_after_close_value;
    lc_fresh_var_implies_body := lc_fresh_var_implies_body;
    open_not_in_eq :=     open_not_in_eq_value;
    lc_implies_body := lc_implies_body_value;
  }.

(** We will not simpl the type class interfaces; unless we explictly unfold it. *)

Arguments substitute: simpl never.
Arguments fv: simpl never.
Arguments open: simpl never.
Arguments close: simpl never.
Arguments lc: simpl never.
Arguments body: simpl never.

(** MNF *)
Definition lete_lc_body: forall e1 e, lc (tlete e1 e) <-> lc e1 /\ body e := lete_lc_body.
Definition letapp_lc_body: forall (v1 v2: value) e, lc (tletapp v1 v2 e) <-> lc v1 /\ lc v2 /\ body e := letapp_lc_body.
Definition leteffop_lc_body: forall op (v1: value) e, lc (tletop op v1 e) <-> lc v1 /\ body e := leteffop_lc_body.
Definition lc_abs_iff_body: forall T (e: tm), lc (vlam T e) <-> body e := lc_abs_iff_body.
Definition lc_fix_iff_body: forall T e, lc (vfix T e) <-> body e := lc_fix_iff_body.
Definition body_vbvar_0: body (vbvar 0) := body_vbvar_0.

Import NamelessTactics.
Import Tactics.

Ltac lc_normalize_one :=
  match goal with
  | [H: lc ?e, H': context [ open _ _ ?e ]  |- _ ] => rewrite open_rec_lc in H'; auto
  | [H: lc ?e |- context [ open _ _ ?e ] ] => rewrite open_rec_lc; auto
  end.

Ltac lc_solver_ast :=
  simp_hyps;
  repeat (match goal with
          | [ |- body (treturn (vbvar 0))] => apply body_vbvar_0
          | [ |- body (vbvar 0)] => apply body_vbvar_0
          | [ |- lc (vfvar _)] => constructor
          | [ |- lc (vconst _)] => constructor
          | [ |- lc (tmatchb _ _ _)] => apply lc_tmatchb; intuition
          | [ |- lc (tletapp _ _ _)] => rewrite letapp_lc_body; intuition
          | [ |- lc (tletop _ _ _)] => rewrite leteffop_lc_body; intuition
          | [ |- lc (tlete _ _)] => rewrite lete_lc_body; split; auto
          | [ |- lc (vfix _ _)] => rewrite lc_fix_iff_body; auto
          | [ |- lc (vlam _ _)] => rewrite lc_abs_iff_body; auto
          | [H: lc (tlete _ ?e) |- body ?e ] => rewrite lete_lc_body in H; simp_hyps; auto
          | [H: lc (tletapp _ _ ?e) |- body ?e ] => rewrite letapp_lc_body in H; simp_hyps; auto
          | [H: lc (treturn (vlam _ ?e)) |- body ?e ] => rewrite lc_abs_iff_body in H; simp_hyps; auto
          | [H: lc (treturn (vfix _ ?e)) |- body ?e ] => rewrite lc_fix_iff_body in H; simp_hyps; auto
          | [H: lc (tletapp ?e _ _) |- lc ?e ] => rewrite letapp_lc_body in H; simp_hyps; auto
          | [H: lc (tletapp _ ?e _) |- lc ?e ] => rewrite letapp_lc_body in H; simp_hyps; auto
          | [H: lc ?e |- body ?e] => apply lc_implies_body; auto
          | [H: lc ?e |- lc ( open 0 _ ?e)] => rewrite open_rec_lc; auto
          | [H: body ?e |- lc ( open 0 _ ?e)] => apply open_lc; auto
          end; try lc_normalize_one); auto.

(** Annoying, because we don't define lc mutual recursively. *)
Lemma normalize_lc_value (e: value): lc (treturn e) = lc e.
Proof. unfold lc; auto. Qed.

Lemma normalize_body_value (e: value): body (treturn e) = body e.
Proof. unfold lc; auto. Qed.

Ltac normalize_lc :=
  repeat match goal with
    | [H: context [ lc (treturn _) ] |- _ ] => rewrite normalize_lc_value in H
    | [H: _ |- context [ lc (treturn _) ]  ] => rewrite normalize_lc_value
    | [H: context [ body (treturn _) ] |- _ ] => rewrite normalize_body_value in H
    | [H: _ |- context [ body (treturn _) ]  ] => rewrite normalize_body_value
    end.

Lemma fold_open_value (k: nat) (f: value) (v_x: value) : open_value k f v_x = open k f v_x.
Proof. unfold open; auto. Qed.

Lemma fold_open_tm (k: nat) (f: value) (v_x: tm) : open_tm k f v_x = open k f v_x.
Proof. unfold open; auto. Qed.

Lemma fold_lc_tm (e: tm): tm_lc e = lc e.
Proof. unfold lc; auto. Qed.

Lemma fold_lc_value (e: value): tm_lc (treturn e) = lc e.
Proof. unfold lc; auto. Qed.

Lemma fold_fv_tm (e: tm): fv_tm e = fv e.
Proof. unfold fv; auto. Qed.

Lemma fold_fv_value (e: value): fv_value e = fv e.
Proof. unfold fv; auto. Qed.

Lemma fold_close_value (x: atom) (s: nat) (v: value) : close_value x s v = close x s v.
Proof. unfold close; auto. Qed.

Lemma fold_close_tm (x: atom) (s: nat) (v: tm) : close_tm x s v = close x s v.
Proof. unfold close; auto. Qed.

Lemma fold_subst_value (x: atom) (s: value) (v: value) : value_subst x s v = substitute x s v.
Proof. unfold substitute; auto. Qed.

Lemma fold_subst_tm (x: atom) (s: value) (v: tm) : tm_subst x s v = substitute x s v.
Proof. unfold substitute; auto. Qed.

Lemma fold_body_value  (e: value): tm_body (treturn e) = body e.
Proof. unfold body; auto. Qed.

Lemma fold_body_tm  (e: tm): tm_body e = body e.
Proof. unfold body; auto. Qed.

Ltac fold_nameless :=
  (repeat match goal with
     | [H: context [ open_tm _ _ _ ] |- _ ] => setoid_rewrite fold_open_tm in H
     | [H: _ |- context [ open_tm _ _ _ ]  ] => setoid_rewrite fold_open_tm
     | [H: context [ open_value _ _ _ ] |- _ ] => setoid_rewrite fold_open_value in H
     | [H: _ |- context [ open_value _ _ _ ]  ] => setoid_rewrite fold_open_value
     | [H: context [ close_tm _ _ _ ] |- _ ] => setoid_rewrite fold_close_tm in H
     | [H: _ |- context [ close_tm _ _ _ ]  ] => setoid_rewrite fold_close_tm
     | [H: context [ close_value _ _ _ ] |- _ ] => setoid_rewrite fold_close_value in H
     | [H: _ |- context [ close_value _ _ _ ]  ] => setoid_rewrite fold_close_value
     | [H: context [ tm_subst _ _ _ ] |- _ ] => setoid_rewrite fold_subst_tm in H
     | [H: _ |- context [ tm_subst _ _ _ ]  ] => setoid_rewrite fold_subst_tm
     | [H: context [ value_subst _ _ _ ] |- _ ] => setoid_rewrite fold_subst_value in H
     | [H: _ |- context [ value_subst _ _ _ ]  ] => setoid_rewrite fold_subst_value
     | [H: context [ fv_tm _ ] |- _ ] => setoid_rewrite fold_fv_tm in H
     | [H: _ |- context [ fv_tm _ ]  ] => setoid_rewrite fold_fv_tm
     | [H: context [ fv_value _ ] |- _ ] => setoid_rewrite fold_fv_value in H
     | [H: _ |- context [ fv_value _ ]  ] => setoid_rewrite fold_fv_value
     | [H: context [ open_tm _ _ _ ] |- _ ] => rewrite fold_open_tm in H
     | [H: _ |- context [ open_tm _ _ _ ]  ] => rewrite fold_open_tm
     | [H: context [ open_value _ _ _ ] |- _ ] => rewrite fold_open_value in H
     | [H: _ |- context [ open_value _ _ _ ]  ] => rewrite fold_open_value
     | [H: context [ close_tm _ _ _ ] |- _ ] => rewrite fold_close_tm in H
     | [H: _ |- context [ close_tm _ _ _ ]  ] => rewrite fold_close_tm
     | [H: context [ close_value _ _ _ ] |- _ ] => rewrite fold_close_value in H
     | [H: _ |- context [ close_value _ _ _ ]  ] => rewrite fold_close_value
     | [H: context [ tm_subst _ _ _ ] |- _ ] => rewrite fold_subst_tm in H
     | [H: _ |- context [ tm_subst _ _ _ ]  ] => rewrite fold_subst_tm
     | [H: context [ value_subst _ _ _ ] |- _ ] => rewrite fold_subst_value in H
     | [H: _ |- context [ value_subst _ _ _ ]  ] => rewrite fold_subst_value
     | [H: context [ fv_tm _ ] |- _ ] => rewrite fold_fv_tm in H
     | [H: _ |- context [ fv_tm _ ]  ] => rewrite fold_fv_tm
     | [H: context [ fv_value _ ] |- _ ] => rewrite fold_fv_value in H
     | [H: _ |- context [ fv_value _ ]  ] => rewrite fold_fv_value
     | [H: tm_lc (treturn _) |- _ ] => rewrite fold_lc_value in H
     | [H: _ |- tm_lc (treturn _) ] => rewrite fold_lc_value
     | [H: tm_body (treturn _) |- _ ] => rewrite fold_body_value in H
     | [H: _ |- tm_body (treturn _) ] => rewrite fold_body_value
     | [H: tm_lc _ |- _ ] => rewrite fold_lc_tm in H
     | [H: _ |- tm_lc _ ] => rewrite fold_lc_tm
     | [H: tm_body _ |- _ ] => rewrite fold_body_tm in H
     | [H: _ |- tm_body _ ] => rewrite fold_body_tm
     end); normalize_lc.

Ltac nameless_eval:=
  repeat lc_normalize_one;
  simpl in *;
  repeat match goal with
    | [H: context [ open _ _ _ ] |- _ ] => unfold open in H; simpl in H
    | [H: _ |- context [ open _ _ _ ]  ] => unfold open; simpl
    | [H: context [ close _ _ _ ] |- _ ] => unfold close in H; simpl in H
    | [H: _ |- context [ close _ _ _ ]  ] => unfold close; simpl
    | [H: context [ substitute _ _ _ ] |- _ ] => unfold substitute in H; simpl in H
    | [H: _ |- context [ substitute _ _ _ ]  ] => unfold substitute; simpl
    | [H: context [ fv _ ] |- _ ] => unfold fv in H; simpl in H
    | [H: _ |- context [ fv _ ]  ] => unfold fv; simpl
    end; fold_nameless;
  normalize_lc;
  repeat lc_normalize_one.

Ltac nameless_set_simpl :=
  nameless_eval;
  (repeat match goal with
     | H : fv ({_ ~> _} _) ⊆ _ |- _ =>
         setoid_rewrite <- open_fv' in H
     | H : _ ⊆ dom (<[_:=_]>_) |- _ =>
         setoid_rewrite dom_insert in H
     | H : _ !! _ = _ |- _ =>
         apply elem_of_dom_2 in H
     end).
