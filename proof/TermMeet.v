Set Warnings "-notation-overridden,-parsing".

From PLF Require Import Maps.
From PLF Require Import CoreLangSimp.
From PLF Require Import NormalTypeSystemSimp.
From PLF Require Import LinearContext.
From PLF Require Import RfTypeDef.
From PLF Require Import TypeClosedSimp.
From PLF Require Import TermOrdering.
From PLF Require Import DenotationSimp.
From PLF Require Import WellFormedSimp.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Lists.List.
From Coq Require Import FunInd.
From Coq Require Import Recdef.

Import CoreLangSimp.
Import NormalTypeSystemSimp.
Import LinearContext.
Import Nstate.
Import NoDup.
Import RfTypeDef.
Import TypeClosedSimp.
Import DenotationSimp.
Import WellFormedSimp.
Import ListNotations.

(* meet operation, the trick here is encode the conjunction into the target language, via a poly equal operator.

  let x1 = e1 in
  let x2 = e2 in
  let x3 = x1 ==T x2 in
  match x3 with
  | true -> x2
  | false -> err

 *)
Lemma meet_of_two_terms_exists: forall e1 e2 T,
    empty \N- e1 \Tin T -> empty \N- e2 \Tin T -> (exists e3, (empty \N- e3 \Tin T) /\ (forall c, e3 -->* c <-> e1 -->* c /\ e2 -->* c)).
Admitted.

Lemma meet_of_two_terms_implies_denotation: forall st tau e1 e2 e3,
    tmR_aux st tau e1 -> tmR_aux st tau e2 ->
    (forall c, e3 -->* c <-> e1 -->* c /\ e2 -->* c) -> tmR_aux st tau e3.
Admitted.

Lemma meet_of_two_terms_term_order: forall e1 e2 e3,
    (forall c, e3 -->* c <-> e1 -->* c /\ e2 -->* c) -> (e3 <-< e1) /\ (e3 <-< e2).
Admitted.

Lemma meet_of_three_terms_exists: forall e1 e2 e3 T,
    empty \N- e1 \Tin T -> empty \N- e2 \Tin T -> empty \N- e3 \Tin T ->
    (exists e, (empty \N- e \Tin T) /\
            (forall c, e -->* c <-> e1 -->* c /\ e2 -->* c /\ e3 -->* c)).
Admitted.

Lemma meet_of_three_terms_implies_denotation: forall st tau e1 e2 e3 e,
    tmR_aux st tau e1 -> tmR_aux st tau e2 -> tmR_aux st tau e3 ->
    (forall c, e -->* c <-> e1 -->* c /\ e2 -->* c /\ e3 -->* c) -> tmR_aux st tau e.
Admitted.

Lemma meet_of_three_terms_term_order: forall e1 e2 e3 e,
    (forall c, e -->* c <-> e1 -->* c /\ e2 -->* c /\ e3 -->* c) -> (e <-< e1) /\ (e <-< e2) /\ (e <-< e3).
Admitted.

Lemma meet_of_four_terms_exists: forall e1 e2 e3 e4 T,
    empty \N- e1 \Tin T -> empty \N- e2 \Tin T -> empty \N- e3 \Tin T -> empty \N- e4 \Tin T ->
    (exists e, (empty \N- e \Tin T) /\
            (forall c, e -->* c <-> e1 -->* c /\ e2 -->* c /\ e3 -->* c /\ e4 -->* c)).
Admitted.

Lemma meet_of_four_terms_implies_denotation: forall st tau e1 e2 e3 e4 e,
    tmR_aux st tau e1 -> tmR_aux st tau e2 -> tmR_aux st tau e3 ->
    (forall c, e -->* c <-> e1 -->* c /\ e2 -->* c /\ e3 -->* c /\ e4 -->* c) -> tmR_aux st tau e.
Admitted.

Lemma meet_of_four_terms_term_order: forall e1 e2 e3 e4 e,
    (forall c, e -->* c <-> e1 -->* c /\ e2 -->* c /\ e3 -->* c /\ e4 -->* c) -> (e <-< e1) /\ (e <-< e2) /\ (e <-< e3) /\ (e <-< e4).
Admitted.
