From Coq Require Import Classes.DecidableClass.
From Coq Require Import Lia.
From Coq Require Import ZArith.
From Coq Require Import Logic.Classical.

Definition int := Z.
Variable IL: Type.
(* Variable len: IL -> int -> Prop. *)
Variable emp: IL -> Prop.
Variable hd: IL -> int -> Prop.
Variable tl: IL -> IL -> Prop.
Variable list_mem: IL -> int -> Prop.
Variable uniq: IL -> Prop.

Lemma list_emp_ex: exists l, emp l.
Admitted.

Lemma list_emp_is_uniq (l: IL) : (emp l) -> (uniq l).
Admitted.
#[export] Hint Resolve list_emp_is_uniq : core.

Lemma test1: (
forall v, (exists x, (exists s, (uniq s/\ (exists r_0, ((0 <= r_0/\ r_0 < 2/\ (r_0 = 0 -> (emp s/\ (exists x_0, (emp x_0/\ hd v x/\ tl v x_0))))/\ (r_0 = 1 -> False)) -> (~uniq v/\ list_mem v x)))))))%Z.
Proof.
  intros.
  destruct (classic (emp v)).
  - destruct list_emp_ex as (l0 & Hl0). exists 0%Z, l0. intuition.
    exists 1%Z. intuition.
  - destruct list_emp_ex as (l0 & Hl0). exists 0%Z, l0. intuition.
    exists 1%Z. intuition.
