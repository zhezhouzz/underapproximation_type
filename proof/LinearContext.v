Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From Coq Require Import Lists.List.
Import ListNotations.

Definition linear_context (A : Type) := list (string * A).

Definition l_empty {A:Type} : linear_context A := nil.

(* Implemenetd as find the right most one *)

Fixpoint l_find_right_most {A:Type} (ctx: linear_context A) (name: string) :=
  match ctx with
  | [] => None
  | (x, xty)::ctx =>
      match l_find_right_most ctx name with
      | None => if String.eqb x name then Some xty else None
      | Some xty => Some xty
      end
  end.

Lemma l_find_right_most_some_spec {A:Type}: forall (ctx: linear_context A) (name: string) (tau: A),
  l_find_right_most ctx name = Some tau ->
  (exists ctx1 ctx2,
      ctx = ctx1 ++ ((name, tau)::nil) ++ ctx2 /\
        l_find_right_most ctx2 name = None
  ).
Admitted.

(* update the to left most *)
Definition l_append {A : Type} (m : linear_context A) (x : string) (v : A) : linear_context A := m ++ [(x, v)].

(* Not same name *)
Definition linear_context_inv {A:Type} (ctx: linear_context A) := True.

Notation " m '<l>' x ':l:' ty " := (l_append m x ty) (at level 80).

Global Hint Rewrite app_nil_l: core.
Global Hint Rewrite app_nil_r: core.

(* Some lemmas. *)

Lemma app_one_eq_nil {A: Type}: forall (x: string) (tau:A) Gamma, ~ ([] = Gamma ++ [(x, tau)]).
Proof.
  intros. intro H. symmetry in H. apply app_eq_nil in H. destruct H. inversion H0.
Qed.

Lemma app_one_is_cons {A: Type}: forall (x: A) l, (x::nil) ++ l = x :: l.
Proof. simpl. reflexivity. Qed.

Lemma l_find_right_most_weak {A: Type}: forall (Gamma1  Gamma2: linear_context A) a,
    l_find_right_most (Gamma1 ++ Gamma2) a = None ->
    (l_find_right_most Gamma1 a = None /\ l_find_right_most Gamma2 a = None).
Admitted.

Global Hint Resolve l_find_right_most_weak: core.

(* Declare Scope linear_context_scope. *)
(* Notation "G[ ]" := nil (format "G[ ]") : linear_context_scope. *)
(* Notation " context 'G::' x " := (cons x context) (at level 80) : linear_context_scope. *)
(* Notation "G[ x ]" := (cons x nil) : linear_context_scope. *)
(* Notation "G[ x ; y ]" := (cons y (cons x nil)) : linear_context_scope. *)
(* Notation "G[ x ; y ; z ]" := (cons z (cons y (cons x nil))) : linear_context_scope. *)
(* Notation "G[ x ; y ; z ; a ]" := (cons a (cons z (cons y (cons x nil)))) : linear_context_scope. *)
