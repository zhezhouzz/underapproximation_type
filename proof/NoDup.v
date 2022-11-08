Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From Coq Require Import Lists.List.
From Coq Require Import Strings.String.
From PLF Require Import RfTypeDef.
From PLF Require Import LinearContext.
From PLF Require Import Nstate.

Import ListNotations.
Import Nstate.

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
