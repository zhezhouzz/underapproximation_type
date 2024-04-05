From Coq Require Import Classes.DecidableClass.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Classical.


Variable term : Type.
Variable value : Type.
Variable reduce: term -> value -> Prop.

Definition denotation (phi: value -> Prop) (e: term): Prop := exists v, reduce e v /\ phi v.

Definition denotation_ghost (phi: value -> value -> Prop) (e: term) : Prop := forall v1: value, exists v: value, reduce e v /\ phi v1 v.

Lemma reduce_dec: forall e v v', reduce e v -> reduce e v' -> v = v'.
Admitted.

Lemma reduce_terminate: forall e, exists v: value, reduce e v.
Admitted.

Lemma intersection (phi1 phi2: value -> Prop): forall e v1 v2, not (v1 = v2) ->
    denotation_ghost (fun vx v => (vx = v1 /\ phi1 v) \/ (vx = v2 /\ phi2 v) \/ (not (vx = v1) /\ not (vx = v2))) e <->
      denotation phi1 e /\ denotation phi2 e.
Proof.
  split; unfold denotation_ghost; unfold denotation; intros.
  - destruct (H0 v1) as (v & Hv & HH). destruct (H0 v2) as (v' & Hv' & HH').
    assert (v = v'). eapply reduce_dec; eauto. subst.
    intuition; subst; try contradiction.
    + exists v'. intuition.
    + exists v'. intuition.
  - destruct H0 as ((v & Hr & H1) & (v' & Hr' & H2)).
    assert (v = v'). eapply reduce_dec; eauto. subst.
    exists v'. intuition.
    destruct (classic (v0 = v1)); destruct (classic (v0 = v2)); intuition; try contradiction.
Qed.

Lemma definition_equal (phi_x: value -> Prop) (phi: value -> value -> Prop) :
  forall e, (forall v_x, phi_x v_x -> denotation (phi v_x) e) <-> (forall v_x, denotation (fun v => phi_x v_x -> phi v_x v) e).
Proof.
  split; unfold denotation; intros.
  - destruct (classic (phi_x v_x)).
    + destruct (H v_x H0) as (v & Hr & Hphi).
      exists v. intuition.
    + destruct (reduce_terminate e) as (v & Hr).
      exists v. intuition.
  - + destruct (H v_x) as (v & Hr & HH).
      exists v. intuition.
Qed.

(* Lemma subtyping_ghost (phi1 phi2: value -> value -> Prop) : *)
(*   (forall v, (exists v1, phi1 v1 v) -> (exists v2, phi2 v2 v)) -> *)
(*   forall e, denotation_ghost phi1 e -> denotation_ghost phi2 e. *)
(* Proof. *)
(*   unfold denotation_ghost. *)
(*   intro. intro. intro. intro v2. *)
(*   destruct (H0 v2) as (v & Hr & Himp). *)
(*   exists v. intuition. specialize (H v). *)
(*   assert (exists v1 : value, phi1 v1 v). *)
(*   { intros. destruct (H0 v1) as (v' & Hr' & Himp'). *)
(*     assert (v = v'). eapply reduce_dec; eauto. subst. auto. } *)
(*   auto. *)
(* Qed. *)

Lemma subtyping_ghost (phi1 phi2: value -> value -> Prop) :
  (forall v, (forall v1, phi1 v1 v) -> (forall v2, phi2 v2 v)) ->
  forall e, denotation_ghost phi1 e -> denotation_ghost phi2 e.
Proof.
  unfold denotation_ghost.
  intro. intro. intro. intro v2.
  destruct (H0 v2) as (v & Hr & Himp).
  exists v. intuition. specialize (H v).
  assert (forall v1 : value, phi1 v1 v).
  { intros. destruct (H0 v1) as (v' & Hr' & Himp').
    assert (v = v'). eapply reduce_dec; eauto. subst. auto. }
  auto.
Qed.

Lemma subtyping (phi1 phi2: value -> Prop) :
  (forall v, phi2 v -> phi1 v) ->
  forall e, denotation_ghost (fun v1 v => phi1 v1 -> v = v1) e -> denotation_ghost (fun v2 v => phi2 v2 -> v = v2) e.
Proof.
  unfold denotation_ghost.
  intro. intro. intro. intro v2.
  intros. destruct (H0 v1) as (v2 & Hr & Himp).
  exists v2. intuition.

  exists v1.
