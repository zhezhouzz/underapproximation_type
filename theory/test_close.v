From Coq Require Import Bool.Bool.
From Coq Require Import Arith.Arith.
From Coq Require Import Init.Nat.
From Coq Require Import Arith.PeanoNat. Import Nat.
From Coq Require Import Arith.EqNat.

Lemma subtyping (Q1 Q2: nat -> nat -> Prop) (P: nat -> Prop):
  (forall x y z, Q1 x y /\ Q1 x z -> y = z) ->
  (forall x y z, Q2 x y /\ Q2 x z -> y = z) ->
  (forall x, P x -> forall nu, (Q1 x nu -> Q2 x nu)) <->
  (forall (E:nat -> nat -> Prop), (forall x y z, E x y /\ E x z -> y = z) ->
      (exists (x: nat), P x /\ (forall nu, Q1 x nu -> E x nu)) -> (exists (x: nat), P x /\ (forall nu, Q2 x nu -> E x nu))).
Proof.
  intros.
  split.
  2: {
    intros.
    remember (H1 (fun x' nu' => x = x' /\ nu = nu')) as X. clear HeqX.
    assert (exists x0 : nat, P x0 /\ (forall nu0 : nat, Q1 x0 nu0 -> (fun x' nu' : nat => x = x' /\ nu = nu') x0 nu0)).
    exists x. split; auto. intros. split; auto. eapply H. split; eauto.
    apply X in H4. destruct H4.
    apply X in H2. destruct H2.
    assert (exists x : nat, P x -> forall nu : nat, Q1 x nu -> E x nu).
    { destruct H0. exists x. intros. apply
    
  }

                                  - intros.
    destruct H0.
    exists x. intros. apply H0; auto.

Lemma test2:
  forall (p_x: nat -> Prop),
  forall (p_y: nat -> nat -> Prop),
  forall (p_body: nat -> nat -> nat -> bool -> bool -> bool -> Prop),
    (exists x, p_x x /\
            forall y, p_y x y ->
                 forall (f: nat -> bool), forall (u: nat), p_body x y u (f x) (f y) (f u)) <->
      (exists x, p_x x /\
              forall y, p_y x y ->
                   forall (fx: bool) (fy:bool), (x = y -> fx = fy) ->
                                     forall (u: nat) (fu: bool),
                                       (u = x -> fu = fx) ->
                                       (u = y -> fu = fy) ->
                                       p_body x y u fx fy fu).
Proof.
  intros.
  split.
  + intros.
    destruct H as (x & PX & H).
    exists x. split; auto. clear PX.
    intros.
    apply H with (f := fun w =>
                         if w =? x then fx
                         else if w =? y then fy
                              else fu
                 ) (u:=u) in H0.
    rewrite eqb_refl in H0.
    destruct (y =? x) eqn:Hyx; destruct (u =? x) eqn:Hux; auto.
    - apply eqb_eq in Hyx. apply eqb_eq in Hux. subst.
      rewrite <- H1; auto.
      rewrite H2; auto.
    - destruct (u =? y) eqn:Huy; auto.
      apply eqb_eq in Hyx. apply eqb_eq in Huy. apply eqb_neq in Hux; subst. exfalso. apply Hux. auto.
      apply eqb_eq in Hyx. apply eqb_neq in Huy. apply eqb_neq in Hux; subst. rewrite <- H1; auto.
    - rewrite eqb_refl in H0. apply eqb_eq in Hux; subst.
      rewrite H2; auto.
    - rewrite eqb_refl in H0.
      destruct (u =? y) eqn:Huy; auto.
      apply eqb_eq in Huy. subst. rewrite H3; auto.
 + intros.
    destruct H as (x & PX & H).
    exists x. split; auto.
Qed.

Lemma test3:
  forall (p_x: nat -> Prop),
  forall (p_y: nat -> nat -> Prop),
  forall (p_body: nat -> nat -> nat -> bool -> bool -> bool -> Prop),
    (exists x, p_x x /\
            forall y, p_y x y ->
                 forall (f: nat -> bool), exists (u: nat), p_body x y u (f x) (f y) (f u)) <->
      (exists x, p_x x /\
              forall y, p_y x y ->
                   forall (fx: bool) (fy:bool), (x = y -> fx = fy) ->
                                     exists (u: nat) (fu: bool),
                                       (u = x -> fu = fx) ->
                                       (u = y -> fu = fy) ->
                                       p_body x y u fx fy fu).
Proof.
  intros.
  split.
  + intros.
    destruct H as (x & PX & H).
    exists x. split; auto. clear PX.
    intros.
    apply H in H0.
    apply H with (f := fun w =>
                         if w =? x then fx
                         else if w =? y then fy
                              else fu
                 ) in H0.
    rewrite eqb_refl in H0.
    destruct (y =? x) eqn:Hyx; destruct (u =? x) eqn:Hux; auto.
  - apply eqb_eq in Hyx. apply eqb_eq in Hux. subst.
    rewrite <- H1; auto.
    rewrite H2; auto.
  - destruct (u =? y) eqn:Huy; auto.
    apply eqb_eq in Hyx. apply eqb_eq in Huy. apply eqb_neq in Hux; subst. exfalso. apply Hux. auto.
    apply eqb_eq in Hyx. apply eqb_neq in Huy. apply eqb_neq in Hux; subst. rewrite <- H1; auto.
  - rewrite eqb_refl in H0. apply eqb_eq in Hux; subst.
    rewrite H2; auto.
  - rewrite eqb_refl in H0.
    destruct (u =? y) eqn:Huy; auto.
    apply eqb_eq in Huy. subst. rewrite H3; auto.
    + intros.
      destruct H as (x & PX & H).
      exists x. split; auto.
Qed.
