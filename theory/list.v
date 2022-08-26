From Coq Require Import Bool.Bool.
From Coq Require Import Arith.Arith.
From Coq Require Import Init.Nat.
From Coq Require Import Arith.PeanoNat. Import Nat.
From Coq Require Import Arith.EqNat.
From Coq Require Import Lists.List.
From Coq Require Import Logic.FunctionalExtensionality.
Import ListNotations.

Definition mk_singleton {A:Type} (x: A) := fun v => v = x.

Definition in_type_eq_nat (n: nat) := fun (V: nat -> Prop) => forall v, v = n -> V v.

Lemma one_has_type_eq0: in_type_eq_nat 0 (mk_singleton 0).
Proof.
  unfold in_type_eq_nat 0, mk_singleton. auto.
Qed.

Definition in_type_top:= fun (V: nat -> Prop) => forall v, V v.

Lemma one_has_type_top: in_type_top (mk_singleton 0).
Proof.
  unfold in_type_top, mk_singleton.
Abort.

Definition func_id := fun (x: nat) => x.

(* x:[v | v = 0] --> [v | v = 0] *)
Definition in_type_0_to_0 := fun (F: (nat -> nat) -> Prop) =>
                               forall (V_x: nat -> Prop),
                                 in_type_eq_nat 0 V_x ->
                                 in_type_eq_nat 0 (fun v => exists f v_x, V_x v_x /\ F f /\ v = f v_x).

Lemma func_id_has_type_0_to_0: in_type_0_to_0 (mk_singleton func_id).
Proof.
  unfold in_type_0_to_0, func_id, in_type_eq_nat 0, mk_singleton.
  intros V_x Hx. intros.
  exists (fun x => x). exists v.
  split; auto.
Qed.

(* x:[v | top] --> [v | v = 0] *)
Definition in_type_top_to_0 := fun (F: (nat -> nat) -> Prop) =>
                               forall (V_x: nat -> Prop),
                                 in_type_top V_x ->
                                 in_type_eq_nat 0 (fun v => exists f v_x, V_x v_x /\ F f /\ v = f v_x).

Lemma func_id_has_type_top_to_0: in_type_top_to_0 (mk_singleton func_id).
Proof.
  unfold in_type_top_to_0, func_id, in_type_eq_nat 0, mk_singleton.
  intros V_x Hx. intros.
  exists (fun x => x). exists v.
  split; auto.
Qed.

Definition in_type_eq_u_to_eq_x (u: nat) :=
  fun (F: (nat -> nat) -> Prop) =>
    forall (V_x: nat -> Prop),
      (fun (V: nat -> Prop) => forall v, V v.) ->
      in_type_eq_nat 0 (fun v => exists f v_x, V_x v_x /\ F f /\ v = f v_x).

(* exists u, x:[v | v = u] --> [v | v = x] *)
Definition in_type_top_to_0 :=
  fun (F: (nat -> nat) -> Prop) =>
    forall (V_u: nat -> Prop),
      in_type_top V_u ->
      exists u, V_u u /\
             (F )
                               forall (V_x: nat -> Prop),
                                 in_type_top V_x ->
                                 in_type_eq_nat 0 (fun v => exists f v_x, V_x v_x /\ F f /\ v = f v_x).


Inductive mem {A: Type}: (list A) -> A -> Prop :=
| I_mem_hd: forall x l, mem (x :: l) x
| I_mem_tail: forall x y l, mem l x -> mem (y::l) x.

Lemma nil_spec : forall v, (forall x: nat, ~ mem v x) -> v = [].
Proof.
  intros v.
  induction v.
  - auto.
  - intros.
    exfalso. apply (H a). apply I_mem_hd.
Qed.

Definition in_argty1 (u w: nat) := fun (V: nat -> Prop) => forall v, (w = v) -> V v.

Definition in_argty2 (u w: nat) (h: nat) := fun (V: list nat -> Prop) => forall v, (mem v u -> u = h) -> V v.

Definition in_retty (u w: nat) (h: nat) := fun (V: list nat -> Prop) => forall v, ((mem v u -> u = h) /\ (mem v h)) -> V v.

Definition in_t_to_ret (u w: nat) (h: nat) :=
  fun (F: (list nat -> list nat) -> Prop) =>
    forall (V_t: list nat -> Prop),
      in_argty2 u w h V_t ->
      in_retty u w h (fun v => exists f v_t, F f /\ V_t v_t /\ v = f v_t).


Lemma cons_spec :
  forall u, exists w,
     forall (V_h: nat -> Prop),
       in_argty1 u w V_h ->
       exists h, V_h h /\ in_t_to_ret u w h (fun v => forall t, v t = h :: t).
Proof.
  intros.
  unfold in_argty1.
  unfold in_t_to_ret.
  unfold in_argty2.
  unfold in_retty.
  exists 0. intros. exists 0. split; auto.
  intros.
  destruct H1. destruct v.
  inversion H2.
  exists (fun t => 0::t).
  exists v.
  split; auto.
  split. apply H0. intros. apply H1. apply I_mem_tail. auto.

  - ayto.


(* forall f, (forall u, exists w, f \in TYPE) -> forall v, exists h t, v = h::t *)
(* induction with quantified variables? *)

Lemma cons_spec : forall v, exists (h: nat) t,
    (forall u, exists w,
        ((mem v u -> u = h) /\ (mem v h)) ->
        (h = w) /\ (mem t u -> u = h) /\ v = h::t).
Proof.
  intros.
  destruct v.
  exists 0. exists []. intro. exists 0. intros. destruct H. inversion H0.
  exists n. exists v. intros. exists n. intros. split; auto. destruct H. split.
  - intros. apply H. apply I_mem_tail. auto.
  - auto.
Qed.
