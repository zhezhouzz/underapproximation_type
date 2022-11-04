Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From Coq Require Import Lists.List.
From Coq Require Import Strings.String.
From PLF Require Import RfTypeDef.
From PLF Require Import LinearContext.
Import ListNotations.

Definition lcontxt := linear_context overunderty.

Inductive type_ctx_no_dup: lcontxt -> Prop :=
| type_ctx_no_dup_nil: type_ctx_no_dup nil
| type_ctx_no_dup_cons: forall x tau_x Gamma,
    l_find_right_most Gamma x = None ->
    type_ctx_no_dup ((x, tau_x) :: Gamma).

Global Hint Constructors type_ctx_no_dup: core.

Lemma type_ctx_no_dup_cannot_find_last: forall Gamma b tau_b,
    type_ctx_no_dup (Gamma ++ ((b, tau_b)::nil)) -> l_find_right_most Gamma b = None.
Admitted.

Global Hint Resolve type_ctx_no_dup_cannot_find_last: core.

Lemma type_ctx_no_dup_fst_last_diff_name: forall a tau_a Gamma b tau_b,
    type_ctx_no_dup ((a, tau_a)::Gamma ++ ((b, tau_b)::nil)) -> a <> b.
Admitted.

Global Hint Resolve type_ctx_no_dup_fst_last_diff_name: core.

Lemma type_ctx_no_dup_ctx_sub: forall Gamma1 Gamma2,
    type_ctx_no_dup (Gamma1 ++ Gamma2) -> type_ctx_no_dup Gamma1 /\ type_ctx_no_dup Gamma2.
Admitted.

Global Hint Resolve type_ctx_no_dup_ctx_sub: core.
