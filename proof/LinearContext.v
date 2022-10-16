Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From Coq Require Import Lists.List.
Import ListNotations.

Definition linear_context (A : Type) := list (string * A).

Definition empty {A:Type} : linear_context A := [].

Definition find {A:Type} (ctx: linear_context A) (name: string) : option A.
Admitted.

Definition update {A : Type} (m : linear_context A) (x : string) (v : A) : linear_context A.
Admitted.

Definition rev_ctx {A : Type} (m : linear_context A): linear_context A.
Admitted.

Notation "x '|-c>' v ';' m" := (update m x v)
                                (at level 100, v at next level, right associativity).
Notation "x '|-c>' v" := (update empty x v)
                          (at level 100).

