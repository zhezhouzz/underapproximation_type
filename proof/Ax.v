Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
(* From PLF Require Import Types. *)
(* From PLF Require Import Smallstep. *)
From PLF Require Import CoreLangSimp.
From PLF Require Import NormalTypeSystemSimp.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.

From Coq Require Import Lists.List.
Import ListNotations.
Import NormalTypeSystemSimp.

Definition state_permute {A:Type}: forall (st: string -> option A) x y (a a': A),
    (x |-> a; y |-> a'; st) = (y |-> a'; x |-> a; st).
Admitted.

Definition exists_not_free_var_in_tm: forall e, exists x, ~ x \FVtm e.
Admitted.

Lemma closed_term_has_no_free_var: forall e x T, empty \N- e \Tin T -> ~ x \FVtm e.
Admitted.

Lemma lete_preserve_not_free: forall e x a e_a, ~ x \FVtm e_a -> ~ x \FVtm e -> ~ x \FVtm tlete a e_a e.
Proof with eauto.
Admitted.

Global Hint Resolve lete_preserve_not_free: core.

Definition const_order (c1 c2: constant) : Prop :=
  match c1 with
  | cbool c1 =>
      match c2 with
      | cbool c2 =>
          (match c1 with
           | true => False
           | false => c2 = true
           end)
      | cnat _ => True
      end
  | cnat c1 =>
      match c2 with
      | cbool _ => False
      | cnat c2 => c1 < c2
      end
  end.

(* for vfix Axiom *)
Lemma const_order_is_well_founded: well_founded const_order.
Admitted.


(* Facts *)
Theorem preservation_value : forall t (v: value) T,
    empty \N- t \Tin T  -> t -->* v  ->
                  empty \N- v \Vin T.
Proof with eauto.
Admitted.

Lemma ty_unique: forall Gamma e T1 T2,
    Gamma \N- e \Tin T1  -> Gamma \N- e \Tin T2 -> T1 = T2.
Proof with eauto.
Admitted.

Lemma empty_has_type_implies_closed: forall e T, empty \N- e \Tin T -> (forall x, ~ x \FVtm e).
Admitted.

Global Hint Resolve empty_has_type_implies_closed: core.
