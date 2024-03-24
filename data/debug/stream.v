From Coq Require Import Classes.DecidableClass.
From Coq Require Import Lia.
From Coq Require Import ZArith.

Variable IS: Type.
Variable IQ: Type.
Variable stream_len: IS -> Z -> Prop.
Variable stream_emp: IS -> Prop.
Variable stream_hd: IS -> Z -> Prop.
Variable stream_tl: IS -> IS -> Prop.
Variable bankersq1: IQ -> Z -> Prop.
Variable bankersq2: IQ -> IS -> Prop.
Variable bankersq3: IQ -> Z -> Prop.
Variable bankersq4: IQ -> IS -> Prop.
Variable bankersq_len: IQ -> Z -> Prop.
(* Variable stream_mem: IS -> Z -> Prop. *)
(* Variable sorted: IS -> Prop. *)

Definition forc (x: IS) := x.

(* Lemma stream_stream_len_0_stream_emp (l : IS) :(stream_len l 0) -> (stream_emp l) . *)
(* Admitted. *)
(* #[export] Hint Resolve stream_stream_len_0_stream_emp : core. *)

Lemma stream_stream_len_leq_0_emp_stream (l : IS) (n: Z) :(stream_len l n) -> (n <= 0)%Z -> (stream_emp l).
Admitted.
#[export] Hint Resolve stream_stream_len_leq_0_emp_stream : core.

Lemma stream_stream_len_not_0_not_stream_emp (l : IS) (n: Z) : not (n = 0)%Z -> (stream_len l n) -> not (stream_emp l).
Admitted.
#[export] Hint Resolve stream_stream_len_not_0_not_stream_emp : core.

Lemma stream_stream_emp_no_stream_hd (l: IS) (x: Z): (stream_emp l) -> not (stream_hd l x).
Admitted.
#[export] Hint Resolve stream_stream_emp_no_stream_hd: core.

Lemma stream_stream_emp_no_stream_tl (l: IS) (l': IS): (stream_emp l) -> not (stream_tl l l').
Admitted.
#[export] Hint Resolve stream_stream_emp_no_stream_tl: core.

Lemma stream_stream_hd_no_stream_emp (l: IS) (x: Z): (stream_hd l x) -> not (stream_emp l).
Admitted.
#[export] Hint Resolve stream_stream_hd_no_stream_emp: core.

Lemma stream_stream_tl_no_stream_emp (l: IS) (l': IS): (stream_tl l l') -> not (stream_emp l).
Admitted.
#[export] Hint Resolve stream_stream_tl_no_stream_emp: core.

Lemma decide_stream_emp: forall v, stream_emp v \/ not (stream_emp v).
Admitted.

Lemma stream_not_stream_emp_ex_stream (l : IS): not (stream_emp l) -> exists x l', stream_hd l x /\ stream_tl l l'.
Admitted.
#[export] Hint Resolve stream_not_stream_emp_ex_stream : core.

Lemma steam_tl_len_plus_1 (l : IS) (l1 : IS) (n : Z): (stream_tl l l1) -> ((stream_len l1 n) <-> (stream_len l (n + 1)))%Z.
Admitted.

Lemma sized_stream_check: (forall size, (0 <= size -> (forall v, ((exists u, (stream_len v u /\ u <= size)) -> ((size = 0 /\ stream_emp v) \/ (~size = 0 /\ (exists x_1, ((x_1 /\ stream_emp v) \/ (~x_1 /\ (exists size_1, (0 <= size_1 /\ size_1 < size /\ size_1 = (size - 1) /\ (exists l, ((exists u, (stream_len l u /\ u <= size_1)) /\ (exists x_3, ((forc x_3) = l /\ (exists x_4, (stream_hd v x_4 /\ stream_tl v (forc x_3))))))))))))))))))%Z.
Proof.
  intros. destruct (Z.eqb_spec size 0); destruct H0 as (m & Hm & Hms).
  - left. subst. intuition; eauto.
  - right. intuition.
    destruct (decide_stream_emp v).
    + exists True. left. intuition.
    + exists False. right. intuition.
    destruct (stream_not_stream_emp_ex_stream v) as (x & l' & Hx & Hl'); eauto.
    exists (size - 1)%Z. intuition.
    exists l'. intuition. exists (m - 1)%Z. intuition. eapply steam_tl_len_plus_1 in Hl'. rewrite Hl'.
    assert (m - 1 + 1 = m)%Z. lia. rewrite H1. auto.
    exists l'. intuition.
    exists x. intuition.
Qed.

Lemma bankersq_destruct (q : IQ): exists lenf f lenr r,
    (lenr >= 0 /\ lenr <= lenf /\ stream_len f lenf /\ stream_len r lenr /\
    bankersq1 q lenf /\ bankersq2 q f /\ bankersq3 q lenr /\ bankersq4 q r)%Z.
Admitted.

Lemma bankersq1_len (q : IQ) (n: Z) (m: Z): bankersq1 q n -> bankersq_len q m -> n = m.
Admitted.

Ltac destr :=
  match goal with
  | [ H: _ /\ _ |- _]  => destruct H
  | [ H: exists _, _|- _]  => destruct H
  end.

Lemma bankersq_check: (forall lenf, (lenf >= 0 -> (forall v, ((exists b_0, (0 <= b_0 /\ b_0 = lenf /\ (exists lenr, (0 <= lenr /\ lenr <= b_0 /\ (exists s_0, (0 <= s_0 /\ s_0 = lenf /\ (exists f, (stream_len f s_0 /\ (exists s_1, (0 <= s_1 /\ s_1 = lenr /\ (exists r, (stream_len r s_1 /\ bankersq_len v lenf)))))))))))) -> (exists b_0, (0 <= b_0 /\ b_0 = lenf /\ (exists lenr, (0 <= lenr /\ lenr <= b_0 /\ (exists s_0, (0 <= s_0 /\ s_0 = lenf /\ (exists f, (stream_len f s_0 /\ (exists s_1, (0 <= s_1 /\ s_1 = lenr /\ (exists r, (stream_len r s_1 /\ (exists lenf_0, (lenf_0 >= 0 /\ lenf_0 = lenf /\ (exists f_0, (stream_len f_0 lenf_0 /\ f_0 = f /\ (exists lenr_0, (lenr_0 >= 0 /\ lenr_0 <= lenf_0 /\ lenr_0 = lenr /\ (exists r_0, (stream_len r_0 lenr_0 /\ r_0 = r /\ bankersq1 v lenf_0 /\ bankersq2 v f_0 /\ bankersq3 v lenr_0 /\ bankersq4 v r_0))))))))))))))))))))))))%Z.
Proof.
  intros. repeat destr. subst.
  destruct (bankersq_destruct v) as (lenf' & f' & lenr' & r' & H11 & H22 & H33 & H44 & Hlenf & Hf & Hlenr & Hr).
  eapply bankersq1_len in H10; eauto. subst.
  exists lenf. intuition.
  exists lenr'. intuition.
  exists lenf. intuition.
  exists f'. intuition. exists lenr'. intuition. exists r'. intuition.
  exists lenf. intuition. exists f'. intuition. exists lenr'. intuition.
  exists r'. intuition.
Qed.

Lemma check : (forall size, (0 <= size -> (forall v, ((stream_len v size/\ sorted v) -> ((size = 0/\ stream_emp v) \/ (~size = 0/\ (exists size_1, ((0 <= size_1/\ size_1 = (size - 1))/\ (exists l, (((size_1 < size/\ size_1 >= 0)/\ stream_len l size_1/\ sorted l)/\ (exists y, (stream_hd v y/\ stream_tl v l))))))))))))%Z.
Proof.
  intros. destruct (Z.eqb_spec size 0).
  - left. subst. intuition.
  - right. intuition.
    destruct (decide_stream_emp v).
    + apply stream_stream_len_not_0_not_stream_emp in H1; auto. intuition.
    + exists (size - 1)%Z. intuition.
      destruct (stream_not_stream_emp_ex_stream_hd _ H0) as (x & Hx).
      destruct (stream_not_stream_emp_ex_stream_tl _ H0) as (l & Hl).
      exists l. intuition; eauto.
Qed.

Lemma subtyping : forall (v: IS) (size: Z), (size >= 0)%Z -> prop2 v size -> prop1 v size.
Proof.
  unfold prop1, prop2. intros.
  destruct (Z.eqb_spec size 0).
  - left. intuition.
  - right. intuition. subst.
    assert ((stream_lenF v > 0)%Z) as Htmp; intuition.
    apply stream_nonstream_empty_de_consF in Htmp. destruct Htmp as (x & v' & Hxv'). subst. clear n. clear H.
    exists x, (stream_lenF v'). intuition. rewrite stream_consF_stream_len. intuition.
    exists v'. intuition. rewrite stream_consF_stream_len. intuition.
    apply H2. rewrite stream_consF_ord. right. auto.
Qed.
