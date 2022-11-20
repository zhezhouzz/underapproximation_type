
From Coq Require Import Arith.Arith.
From Coq Require Import Bool.Bool.
Require Export Coq.Strings.String.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Lists.List.
Import ListNotations.

(** Documentation for the standard library can be found at
    https://coq.inria.fr/library/.

    The [Search] command is a good way to look for theorems involving
    objects of specific types. See [Lists] for a reminder of how
    to use it. *)

(** If you want to find out how or where a notation is defined, the
    [Locate] command is useful.  For example, where is the natural
    addition operation defined in the standard library? *)

(** (There are several uses of the [+] notation, but only one for
    naturals.) *)


Definition total_map (A : Type) := string -> A.


Definition t_empty {A : Type} (v : A) : total_map A :=
  (fun _ => v).

Definition t_update {A : Type} (m : total_map A)
           (x : string) (v : A) :=
  fun x' => if String.eqb x x' then v else m x'.

Definition examplemap :=
  t_update (t_update (t_empty false) "foo" true)
           "bar" true.

Notation "'_' '!->' v" := (t_empty v)
                            (at level 100, right associativity).

Example example_empty := (_ !-> false).

Notation "x '!->' v ';' m" := (t_update m x v)
                                (at level 100, v at next level, right associativity).

Lemma t_apply_empty : forall (A : Type) (x : string) (v : A),
    (_ !-> v) x = v.
Proof with eauto.
  intros. unfold t_empty...
Qed.

Lemma t_update_eq : forall (A : Type) (m : total_map A) x v,
    (x !-> v ; m) x = v.
Proof with eauto.
  intros.
  unfold t_update.
  destruct (eqb_spec x x); subst... exfalso...
Qed.

Theorem t_update_neq : forall (A : Type) (m : total_map A) x1 x2 v,
    x1 <> x2 ->
    (x1 !-> v ; m) x2 = m x2.
Proof with eauto.
  intros.
  unfold t_update.
  destruct (eqb_spec x1 x2); subst... exfalso...
Qed.

Lemma t_update_shadow : forall (A : Type) (m : total_map A) x v1 v2,
    (x !-> v2 ; x !-> v1 ; m) = (x !-> v2 ; m).
Proof with eauto.
  intros.
  unfold t_update.
  apply functional_extensionality. intro x'.
  destruct (eqb_spec x x'); subst...
Qed.

Theorem t_update_same : forall (A : Type) (m : total_map A) x,
    (x !-> m x ; m) = m.
Proof with eauto.
  intros.
  unfold t_update.
  apply functional_extensionality. intro x'.
  destruct (eqb_spec x x'); subst...
Qed.

Theorem t_update_permute : forall (A : Type) (m : total_map A)
                             v1 v2 x1 x2,
    x2 <> x1 ->
    (x1 !-> v1 ; x2 !-> v2 ; m)
    =
      (x2 !-> v2 ; x1 !-> v1 ; m).
Proof with eauto.
  intros.
  unfold t_update.
  apply functional_extensionality. intro x'.
  destruct (eqb_spec x1 x'); subst...
  destruct (eqb_spec x2 x'); subst... exfalso...
Qed.

Definition partial_map (A : Type) := total_map (option A).

Definition empty {A : Type} : partial_map A :=
  t_empty None.

Definition update {A : Type} (m : partial_map A)
           (x : string) (v : A) :=
  (x !-> Some v ; m).

(** We introduce a similar notation for partial maps: *)
Notation "x '|->' v ';' m" := (update m x v)
                                (at level 100, v at next level, right associativity).

(** We can also hide the last case when it is empty. *)
Notation "x '|->' v" := (update empty x v)
                          (at level 100).

Definition examplepmap :=
  ("Church" |-> true ; "Turing" |-> false).

(** We now straightforwardly lift all of the basic lemmas about total
    maps to partial maps.  *)

Lemma apply_empty : forall (A : Type) (x : string),
    @empty A x = None.
Proof.
  intros. unfold empty. rewrite t_apply_empty.
  reflexivity.
Qed.

Lemma update_eq : forall (A : Type) (m : partial_map A) x v,
    (x |-> v ; m) x = Some v.
Proof.
  intros. unfold update. rewrite t_update_eq.
  reflexivity.
Qed.

Theorem update_neq : forall (A : Type) (m : partial_map A) x1 x2 v,
    x2 <> x1 ->
    (x2 |-> v ; m) x1 = m x1.
Proof.
  intros A m x1 x2 v H.
  unfold update. rewrite t_update_neq. reflexivity.
  apply H. Qed.

Lemma update_shadow : forall (A : Type) (m : partial_map A) x v1 v2,
    (x |-> v2 ; x |-> v1 ; m) = (x |-> v2 ; m).
Proof.
  intros A m x v1 v2. unfold update. rewrite t_update_shadow.
  reflexivity.
Qed.

Theorem update_same : forall (A : Type) (m : partial_map A) x v,
    m x = Some v ->
    (x |-> v ; m) = m.
Proof.
  intros A m x v H. unfold update. rewrite <- H.
  apply t_update_same.
Qed.

Theorem update_permute : forall (A : Type) (m : partial_map A)
                           x1 x2 v1 v2,
    x2 <> x1 ->
    (x1 |-> v1 ; x2 |-> v2 ; m) = (x2 |-> v2 ; x1 |-> v1 ; m).
Proof.
  intros A m x1 x2 v1 v2. unfold update.
  apply t_update_permute.
Qed.

Definition includedin {A : Type} (m m' : partial_map A) :=
  forall x v, m x = Some v -> m' x = Some v.


Lemma includedin_update : forall (A : Type) (m m' : partial_map A)
                            (x : string) (vx : A),
    includedin m m' ->
    includedin (x |-> vx ; m) (x |-> vx ; m').
Proof.
  unfold includedin.
  intros A m m' x vx H.
  intros y vy.
  destruct (eqb_spec x y) as [Hxy | Hxy].
  - rewrite Hxy.
    rewrite update_eq. rewrite update_eq. intro H1. apply H1.
  - rewrite update_neq. rewrite update_neq.
    + apply H.
    + apply Hxy.
    + apply Hxy.
Qed.
