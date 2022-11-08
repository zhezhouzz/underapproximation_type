Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From Coq Require Import Lists.List.
From Coq Require Import Strings.String.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.
From PLF Require Import RfTypeDef.
From PLF Require Import LinearContext.

Import ListNotations.
Import CoreLangSimp.
Import NormalTypeSystemSimp.

Definition tystate := string -> option base_ty.

Definition state_to_tystate (nst: state) :=
  fun x => (match nst x with
         | None => None
         | Some c => Some (ty_of_const c)
         end).

Notation " 'st\_' st '_/' " := (state_to_tystate st) (at level 40).

Global Hint Unfold state_to_tystate: core.

Definition state_in_tystate (st: state) (tyst: tystate): Prop :=
  forall x Tx, tyst x = Some Tx -> (exists c, st x = Some c).

Notation " st '\TYSTin' nst " := (state_in_tystate st nst) (at level 40).

Definition nstate_to_tystate_hd: forall nst x e_x,
    (st\_ x |-> e_x; nst _/) = (x |-> (ty_of_const e_x); (st\_ nst _/)).
Proof with eauto.
  intros. apply functional_extensionality. intros x'. unfold state_to_tystate.
  destruct (eqb_spec x x'); subst...
  - rewrite update_eq. unfold state_to_tystate. rewrite update_eq. reflexivity.
  - rewrite update_neq... unfold state_to_tystate. rewrite update_neq...
Qed.

Global Hint Rewrite nstate_to_tystate_hd: core.

Definition nstate_to_tystate_empty: (st\_ empty _/) = empty.
Proof with eauto.
  apply functional_extensionality...
Qed.

Global Hint Rewrite nstate_to_tystate_empty: core.

Definition lcontxt := linear_context overunderty.

Inductive type_ctx_no_dup: tystate -> lcontxt -> Prop :=
| type_ctx_no_dup_nil: forall tyst, type_ctx_no_dup tyst nil
| type_ctx_no_dup_cons: forall tyst x tau_x Gamma,
    tyst x = None ->
    l_find_right_most Gamma x = None ->
    type_ctx_no_dup tyst ((x, tau_x) :: Gamma).

Global Hint Constructors type_ctx_no_dup: core.

Lemma type_ctx_no_dup_cannot_find_fst: forall tyst Gamma b tau_b,
    type_ctx_no_dup tyst ((b, tau_b)::Gamma) -> l_find_right_most Gamma b = None.
Proof. intros. inversion H. auto. Qed.

Global Hint Resolve type_ctx_no_dup_cannot_find_fst: core.

Lemma type_ctx_no_dup_cannot_find_fst_in_nst: forall tyst Gamma b tau_b,
    type_ctx_no_dup tyst ((b, tau_b)::Gamma) -> tyst b = None.
Proof. intros. inversion H. auto. Qed.

Global Hint Resolve type_ctx_no_dup_cannot_find_fst_in_nst: core.

Lemma type_ctx_no_dup_cannot_find_last: forall tyst Gamma b tau_b,
    type_ctx_no_dup tyst (Gamma ++ ((b, tau_b)::nil)) -> l_find_right_most Gamma b = None.
Admitted.

Global Hint Resolve type_ctx_no_dup_cannot_find_last: core.

Lemma type_ctx_no_dup_cannot_find_last_in_nst: forall tyst Gamma b tau_b,
    type_ctx_no_dup tyst (Gamma ++ ((b, tau_b)::nil)) -> tyst b = None.
Admitted.

Global Hint Resolve type_ctx_no_dup_cannot_find_last_in_nst: core.

Lemma type_ctx_no_dup_fst_last_diff_name: forall tyst a tau_a Gamma b tau_b,
    type_ctx_no_dup tyst ((a, tau_a)::Gamma ++ ((b, tau_b)::nil)) -> a <> b.
Admitted.

Global Hint Resolve type_ctx_no_dup_fst_last_diff_name: core.

Lemma type_ctx_no_dup_ctx_pre: forall tyst Gamma1 Gamma2,
    type_ctx_no_dup tyst (Gamma1 ++ Gamma2) -> type_ctx_no_dup tyst Gamma1.
Admitted.

Global Hint Resolve type_ctx_no_dup_ctx_pre: core.

Lemma type_ctx_no_dup_ctx_post: forall tyst Gamma1 Gamma2,
    type_ctx_no_dup tyst (Gamma1 ++ Gamma2) -> type_ctx_no_dup tyst Gamma2.
Admitted.

Global Hint Resolve type_ctx_no_dup_ctx_post: core.
