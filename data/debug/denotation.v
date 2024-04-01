From Coq Require Import Classes.DecidableClass.

Variable term : Type.
Variable value : Type.
Variable reduce: term -> value -> Prop.

Definition denotation (phi: value -> Prop) (e: term): Prop := exists v, reduce e v /\ phi v.

Definition denotation_ghost (phi: value -> value -> Prop) (e: term) : Prop := forall v1: value, exists v: value, reduce e v /\ phi v1 v.

Lemma reduce_dec: forall e v v', reduce e v -> reduce e v' -> v = v'.
Admitted.

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
