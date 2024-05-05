From stdpp Require Import mapset.
From stdpp Require Import natmap.
From stdpp Require Import fin vector.
From CT Require Import CoreLangClass.

Import Atom.
Import CoreLang.
Import Tactics.
Import NamelessTactics.

(** This file defines the qualifiers used in refinement types, corresponding to
  ϕ in Fig. 4. In contrast to the syntactic presentation in paper, propositions
  are encoded as shallowly embedded Coq propositions. *)

(** A qualifier is defined as a Coq proposition [prop], with closed arguments
  corresponding to the possibly open [vals]. [vals] captures the variables that
  are referred to in [prop]. We use length-indexed list [vec] to make sure
  [prop] arguments and [vals] match exactly. *)
Inductive qualifier : Type :=
| qual {n : nat} (vals : vec value n) (prop : vec constant n -> Prop).

(** For example, a qualifier may be: (with notation from stdpp)
<<
qual [# vbvar 0; vbvar 1; vfvar "x"]
   (fun v => v !!! 0 = v !!! 1 /\ v !!! 2 = cnat 1)%fin
>>

It means the first and second bound variables are equal, and the free variable
[x] is [1].
*)

(** Conjunction of two qualifiers *)
Definition qualifier_and (q1 q2 : qualifier) : qualifier :=
  match q1, q2 with
  | qual vals1 prop1, qual vals2 prop2 =>
      qual (vals1 +++ vals2)
        (fun v =>
           let (v1, v2) := Vector.splitat _ v
           in prop1 v1 /\ prop2 v2)
  end.

(** * Qualifier denotation *)

Fixpoint denote_vals {n} (vals : vec value n) : option (vec constant n) :=
  match vals with
  | [#] => Some [#]
  | x ::: v =>
      match x with
      | vconst c =>
          match denote_vals v with
          | Some v' => Some (c ::: v')
          | None => None
          end
      | _ => None
      end
  end.

(** Denote a _closed_ qualifier to a Coq proposition. The result is [False] if the
qualifier mentions functions. *)
Definition denote_qualifier (ϕ : qualifier) : Prop :=
  match ϕ with
  | qual vals prop =>
      match denote_vals vals with
      | Some v => prop v
      | None => False
      end
  end.

(** * Notations *)

Definition mk_q_bvar_eq_val n v :=
  qual [# vbvar n; v] (fun v => v !!! 0 = v !!! 1)%fin.
Definition mk_q_under_bot := qual [#] (fun _ => False).
Definition mk_q_under_top := qual [#] (fun _ => True).

Notation " 'b0:v=' v " := (mk_q_bvar_eq_val 0 v)
                            (at level 5, format "b0:v= v", v constr).
Notation " 'b0:x=' y " := (mk_q_bvar_eq_val 0 (vfvar y))
                            (at level 5, format "b0:x= y", y constr).
Notation " 'b0:c=' c " := (mk_q_bvar_eq_val 0 (vconst c))
                            (at level 5, format "b0:c= c", c constr).
Notation " 'b1:v=' v " := (mk_q_bvar_eq_val 1 v)
                            (at level 5, format "b1:v= v", v constr).
Notation " 'b1:x=' y " := (mk_q_bvar_eq_val 1 (vfvar y))
                            (at level 5, format "b1:x= y", y constr).
Notation " 'b1:c=' c " := (mk_q_bvar_eq_val 1 (vconst c))
                            (at level 5, format "b1:c= c", c constr).
Notation " ϕ1 '&' ϕ2 " := (qualifier_and ϕ1 ϕ2)
                             (at level 5, format "ϕ1  &  ϕ2").

(** * Naming related definitions and lemmas *)

(** free variables *)
Definition qualifier_fv ϕ : aset :=
  match ϕ with
  | qual vals _ => Vector.fold_right (fun v s => fv_value v ∪ s) vals ∅
  end.

#[global]
Instance qualifier_stale : @Stale aset qualifier := qualifier_fv.
Arguments qualifier_stale /.

Definition qualifier_open (k: nat) (s: value) (ϕ: qualifier) : qualifier :=
  match ϕ with
  | qual vals prop =>
      qual (vmap (open_value k s) vals) prop
  end.

(* Notation "'{' k '~>' s '}' e" := (qualifier_open k s e) (at level 20, k constr). *)
(* Notation "e '^^' s" := (qualifier_open 0 s e) (at level 20). *)

Definition qualifier_subst (x: atom) (v: value) (ϕ: qualifier) : qualifier :=
  match ϕ with
  | qual vals prop =>
      qual (vmap (value_subst x v) vals) prop
  end.

(* Notation "'{' x ':=' s '}'" := (qualifier_subst x s) (at level 20, format "{ x := s }", x constr). *)

Inductive lc_qualifier : qualifier -> Prop :=
| lc_qual n vals prop :
  Vector.Forall (fun v => lc (treturn v)) vals ->
  lc_qualifier (@qual n vals prop).

Definition body_qualifier := fun (ϕ: qualifier) => (exists L : aset, forall x : atom, x ∉ L -> lc_qualifier (qualifier_open 0 x ϕ)).

(** never used *)
Definition _close_qualifier (x: atom) (n: nat) (ϕ: qualifier) := ϕ.

#[export] Instance qualifier_substable : Substable qualifier :=
  {
    substitute := qualifier_subst;
    fv := qualifier_fv;
  }.

#[export] Instance qualifier_ast : Ast qualifier :=
  {
    open := qualifier_open;
    lc := lc_qualifier;
    body := body_qualifier;
    close := _close_qualifier;
  }.

Lemma subst_commute_qualifier : forall x (u_x: value) y (u_y: value) ϕ,
    x <> y -> x ∉ fv_value u_y -> y ∉ fv_value u_x ->
    {x := u_x } ({y := u_y } ϕ) = {y := u_y } ({x := u_x } ϕ).
Proof.
  intros.
  destruct ϕ. unfold substitute.
  simpl.
  f_equal.
  rewrite !Vector.map_map.
  apply Vector.map_ext. fold_nameless.
  intros. rewrite subst_commute; eauto.
Qed.

Lemma subst_fresh_qualifier: forall (ϕ: qualifier) (x:atom) (u: value),
    x ∉ (qualifier_fv ϕ) -> {x := u} ϕ = ϕ.
Proof.
  intros. unfold substitute.
  destruct ϕ.
  simpl in *.
  f_equal.
  clear prop.
  induction vals; simpl in *; eauto.
  f_equal.
  fold_nameless. apply subst_fresh. my_set_solver.
  auto_apply. my_set_solver.
Qed.

Lemma open_fv_qualifier (ϕ : qualifier) (v : value) k :
  qualifier_fv ({k ~> v} ϕ) ⊆ fv_value v ∪ qualifier_fv ϕ.
Proof.
  destruct ϕ.
  simpl. clear. induction vals; simpl. easy.
  etrans. apply union_mono. fold_nameless. apply open_fv.
  reflexivity. my_set_solver.
Qed.

Lemma open_fv_qualifier' (ϕ : qualifier) (v : value) k :
  qualifier_fv ϕ ⊆ qualifier_fv ({k ~> v} ϕ).
Proof.
  intros. destruct ϕ.
  simpl. clear. induction vals; simpl. easy.
  apply union_mono; eauto. fold_nameless. apply open_fv'.
Qed.

Lemma lc_qualifier_and q1 q2 :
  lc_qualifier q1 -> lc_qualifier q2 ->
  lc_qualifier (q1 & q2).
Proof.
  inversion 1. inversion 1. subst.
  simpl. constructor.
  rewrite Vector.to_list_Forall in *.
  rewrite Vector.to_list_append.
  apply Forall_app. eauto.
Qed.

Lemma qualifier_and_open k v q1 q2 :
  {k ~> v} (q1 & q2) = ({k ~> v} q1) & ({k ~> v} q2).
Proof.
  unfold open.
  destruct q1, q2. simpl. f_equal.
  (* Need a lemma [map_app] for vector. *)
  clear.
  induction vals; eauto.
  simpl. f_equal. eauto.
Qed.

Lemma qualifier_and_subst x v q1 q2 :
  {x := v} (q1 & q2) = ({x := v} q1) & ({x := v} q2).
Proof.
  unfold open. unfold substitute.
  destruct q1, q2. simpl. f_equal.
  (* Need a lemma [map_app] for vector. *)
  clear.
  induction vals; eauto.
  simpl. f_equal. eauto.
Qed.

Lemma qualifier_and_fv q1 q2 :
  qualifier_fv (q1 & q2) = qualifier_fv q1 ∪ qualifier_fv q2.
Proof.
  destruct q1, q2. simpl.
  clear.
  induction vals; simpl. my_set_solver.
  rewrite IHvals. my_set_solver.
Qed.

Lemma denote_vals_app {n1 n2} (vals1 : vec value n1) (vals2 : vec value n2) :
  denote_vals (vals1 +++ vals2) =
    match denote_vals vals1, denote_vals vals2 with
    | Some v1, Some v2 => Some (v1 +++ v2)
    | _, _ => None
    end.
Proof.
  induction vals1; simpl; qauto.
Qed.

Lemma denote_qualifier_and q1 q2 :
  denote_qualifier (q1 & q2) <-> denote_qualifier q1 /\ denote_qualifier q2.
Proof.
  destruct q1, q2. simpl.
  rewrite denote_vals_app.
  case_split; try qauto.
  case_split; try qauto.
  rewrite Vector.splitat_append. eauto.
Qed.

Lemma open_subst_same_qualifier: forall x y (ϕ : qualifier) k,
    x # ϕ ->
    {x := y } ({k ~> x} ϕ) = {k ~> y} ϕ.
Proof.
  unfold substitute. unfold open.
  destruct ϕ. cbn. intros.
  f_equal. clear - H.
  (* A better proof should simply reduce to vector facts. Don't bother yet. *)
  induction vals; cbn; eauto.
  cbn in H.
  f_equal. fold_nameless. apply open_subst_same. my_set_solver.
  apply IHvals. my_set_solver.
Qed.

Lemma subst_open_qualifier: forall (ϕ: qualifier) (x:atom) (u: value) (w: value) (k: nat),
    lc w -> {x := w} ({k ~> u} ϕ) = ({k ~> {x := w} u} ({x := w} ϕ)).
Proof.
  unfold open. unfold substitute.
  destruct ϕ. cbn. intros.
  f_equal.
  rewrite !Vector.map_map.
  apply Vector.map_ext. fold_nameless.
  intros. rewrite subst_open; eauto.
Qed.

Lemma subst_open_qualifier_closed:
  ∀ (ϕ : qualifier) (x : atom) (u w : value) (k : nat),
    closed u ->
    lc w → {x := w } ({k ~> u} ϕ) = {k ~> u} ({x := w } ϕ).
Proof.
  intros. rewrite subst_open_qualifier; auto.
  rewrite (subst_fresh); eauto. set_solver.
Qed.

Lemma subst_lc_qualifier : forall x (u: value) (ϕ: qualifier),
    lc_qualifier ϕ -> lc u -> lc_qualifier ({x := u} ϕ).
Proof.
  destruct 1. intros Hu.
  econstructor.
  srewrite Vector.to_list_Forall.
  rewrite Vector.to_list_map.
  rewrite Forall_map.
  eapply Forall_impl; eauto.
  simpl. intros. fold_nameless. eapply subst_lc; eauto.
Qed.

Lemma subst_open_var_qualifier: forall x y (u: value) (ϕ: qualifier) (k: nat),
    x <> y -> lc u -> {x := u} ({k ~> y} ϕ) = ({k ~> y} ({x := u} ϕ)).
Proof.
  intros.
  rewrite subst_open_qualifier; auto. nameless_eval. rewrite decide_False; auto.
Qed.

Lemma fv_of_subst_qualifier_closed:
  forall x (u : value) (ϕ: qualifier),
    closed u ->
    qualifier_fv ({x := u } ϕ) = qualifier_fv ϕ ∖ {[x]}.
Proof.
  destruct ϕ; simpl. clear. induction vals; simpl; intros.
  my_set_solver.
  fold_nameless.
  rewrite fv_of_subst_closed by eauto.
  my_set_solver.
Qed.

Lemma open_not_in_eq_qualifier (x : atom) (ϕ : qualifier) k :
  x # {k ~> x} ϕ ->
  forall e, ϕ = {k ~> e} ϕ.
Proof.
  unfold open.
  destruct ϕ. simpl. intros.
  f_equal.
  clear - H.
  induction vals; simpl; eauto.
  f_equal. fold_nameless. apply open_not_in_eq with x. my_set_solver.
  auto_apply. my_set_solver.
Qed.

Lemma lc_subst_qualifier:
  forall x (u: value) (ϕ: qualifier), lc_qualifier ({x := u} ϕ) -> lc u -> lc_qualifier ϕ.
Proof.
  unfold substitute.
  intros.
  sinvert H.
  destruct ϕ. simpl in *. simplify_eq.
  econstructor.
  srewrite Vector.to_list_Forall.
  srewrite Vector.to_list_map.
  srewrite Forall_map.
  eapply Forall_impl; eauto.
  nameless_eval. intros. eapply lc_subst; eauto. apply H.
Qed.

Lemma open_rec_lc_qualifier: forall (v: value) (ϕ: qualifier) (k: nat),
    lc_qualifier ϕ -> {k ~> v} ϕ = ϕ.
Proof.
  unfold open.
  destruct 1. simpl. f_equal.
  rewrite <- Vector.map_id.
  apply Vector.map_ext_in.
  rewrite Vector.Forall_forall in H.
  intros. fold_nameless. apply open_rec_lc; eauto. apply H; auto.
Qed.

Lemma open_qualifier_idemp: forall u (v: value) (ϕ: qualifier) (k: nat),
    lc v ->
    {k ~> u} ({k ~> v} ϕ) = ({k ~> v} ϕ).
Proof.
  unfold open.
  destruct ϕ; intros. simpl.
  f_equal.
  rewrite Vector.map_map.
  apply Vector.map_ext_in.
  intros. fold_nameless.
  rewrite open_idemp; eauto.
Qed.

Lemma subst_intro_qualifier: forall (ϕ: qualifier) (x:atom) (w: value) (k: nat),
    x # ϕ ->
    lc w -> {x := w} ({k ~> x} ϕ) = ({k ~> w} ϕ).
Proof.
  unfold open. unfold substitute.
  intros.
  specialize (subst_open_qualifier ϕ x x w k) as J.
  nameless_eval. rewrite decide_True in J; auto.
  rewrite J; auto. rewrite subst_fresh_qualifier; auto.
Qed.

Lemma open_lc_qualifier: forall (u: value) (ϕ: qualifier),
    (* don't body defining body yet. *)
    body_qualifier ϕ ->
    lc u ->
    lc_qualifier ({0 ~> u} ϕ).
Proof.
  unfold open.
  intros. destruct H.
  let acc := collect_stales tt in pose acc.
  pose (Atom.fv_of_set a).
  assert (a0 ∉ a). apply Atom.fv_of_set_fresh.
  erewrite <- subst_intro_qualifier; auto. instantiate (1:= a0).
  apply subst_lc_qualifier; eauto. apply H.
  my_set_solver. my_set_solver.
Qed.

Lemma open_swap_qualifier: forall (ϕ: qualifier) i j (u v: value),
    lc u ->
    lc v ->
    i <> j ->
    {i ~> v} ({j ~> u} ϕ) = {j ~> u} ({i ~> v} ϕ).
Proof.
  unfold open.
  destruct ϕ. intros. simpl.
  f_equal. rewrite !Vector.map_map.
  apply Vector.map_ext.
  intros. fold_nameless. rewrite open_swap; eauto.
Qed.

Lemma open_lc_respect_qualifier: forall (ϕ: qualifier) (u v : value) k,
    lc_qualifier ({k ~> u} ϕ) ->
    lc u ->
    lc v ->
    lc_qualifier ({k ~> v} ϕ).
Proof.
  unfold open.
  intros. sinvert H.
  destruct ϕ. simpl in *. simplify_eq.
  econstructor.
  srewrite Vector.to_list_Forall.
  rewrite Vector.to_list_map in *.
  rewrite Forall_map in *.
  eapply Forall_impl; eauto.
  nameless_eval; eauto. intros.
  fold_nameless. eapply open_lc_respect; eauto.
Qed.

Arguments qualifier_and : simpl never.

(** * Well-founded relation in qualifiers *)

(* Well-founded constraint of base type for fixed point. *)
Definition constant_measure (c : constant) :=
  match c with
  | cnat n => n
  | cbool b => Nat.b2n b
  end.

Definition constant_lt := ltof _ constant_measure.

Notation " a '≺' b " := (constant_lt a b) (at level 20, a constr, b constr).

Lemma constant_lt_well_founded : well_founded constant_lt.
Proof.
  apply well_founded_ltof.
Qed.

Notation " 'b0≺b1'" :=
  (qual [# vbvar 0; vbvar 1] (fun v => (v !!! 0) ≺ (v !!! 1))%fin)
    (at level 5).

Notation " 'b0:x≺' x " :=
  (qual [# vbvar 0; vfvar x] (fun v => (v !!! 0) ≺ (v !!! 1))%fin)
    (at level 5, x constr).

Notation " 'b0:v≺' v " :=
  (qual [# vbvar 0; v] (fun v => (v !!! 0) ≺ (v !!! 1))%fin)
    (at level 5).

Notation " 'b0:c≺' c " :=
  (qual [# vbvar 0; vconst c] (fun v => (v !!! 0) ≺ (v !!! 1))%fin)
    (at level 5).
