From Coq Require Import Classes.DecidableClass.
From Coq Require Import Lia.
From Coq Require Import ZArith.

Variable IS: Type.
Variable IQ: Type.
Variable len: IS -> Z -> Prop.
Variable emp: IS -> Prop.
Variable hd: IS -> Z -> Prop.
Variable tl: IS -> IS -> Prop.
Variable batchedq1: IQ -> IS -> Prop.
Variable batchedq2: IQ -> IS -> Prop.
Variable batchedq_len: IQ -> Z -> Prop.


(* Lemma len_0_emp (l : IS) :(len l 0) -> (emp l) . *)
(* Admitted. *)
(* #[export] Hint Resolve len_0_emp : core. *)

Lemma len_leq_0_emp_stream (l : IS) (n: Z) :(len l n) -> (n <= 0)%Z -> (emp l).
Admitted.
#[export] Hint Resolve len_leq_0_emp_stream : core.

Lemma len_not_0_not_emp (l : IS) (n: Z) : not (n = 0)%Z -> (len l n) -> not (emp l).
Admitted.
#[export] Hint Resolve len_not_0_not_emp : core.

Lemma emp_no_hd (l: IS) (x: Z): (emp l) -> not (hd l x).
Admitted.
#[export] Hint Resolve emp_no_hd: core.

Lemma emp_no_tl (l: IS) (l': IS): (emp l) -> not (tl l l').
Admitted.
#[export] Hint Resolve emp_no_tl: core.

Lemma hd_no_emp (l: IS) (x: Z): (hd l x) -> not (emp l).
Admitted.
#[export] Hint Resolve hd_no_emp: core.

Lemma tl_no_emp (l: IS) (l': IS): (tl l l') -> not (emp l).
Admitted.
#[export] Hint Resolve tl_no_emp: core.

Lemma decide_emp: forall v, emp v \/ not (emp v).
Admitted.

Lemma not_emp_ex_stream (l : IS): not (emp l) -> exists x l', hd l x /\ tl l l'.
Admitted.
#[export] Hint Resolve not_emp_ex_stream : core.

Lemma steam_tl_len_plus_1 (l : IS) (l1 : IS) (n : Z): (tl l l1) -> ((len l1 n) <-> (len l (n + 1)))%Z.
Admitted.

Ltac destr :=
  match goal with
  | [ H: _ /\ _ |- _]  => destruct H
  | [ H: exists _, _|- _]  => destruct H
  end.

Lemma batchedq_destruct (q : IQ): exists f r n m, (batchedq1 q f /\ batchedq2 q r /\ len r n /\ 0 <= n /\ len f m /\ 0 <= m /\ n <= m )%Z.
Admitted.

Lemma batchedq1_len (q : IQ) (f: IS) (n: Z) (m: Z): batchedq1 q f -> batchedq_len q n -> len f m -> n = m.
Admitted.

Lemma sized_check: (forall sizel, (sizel >= 0 -> (forall v, ((exists b_0, (0 <= b_0 /\ b_0 = sizel /\ (exists sizer, (0 <= sizer /\ sizer <= b_0 /\ (exists l1, (len l1 sizel /\ (exists l2, (len l2 sizer /\ batchedq_len v sizel)))))))) -> (exists b_0, (0 <= b_0 /\ b_0 = sizel /\ (exists sizer, (0 <= sizer /\ sizer <= b_0 /\ (exists l1, (len l1 sizel /\ (exists l2, (len l2 sizer /\ batchedq1 v l1 /\ batchedq2 v l2))))))))))))%Z.
Proof.
  intros. repeat destr. subst.
  destruct (batchedq_destruct v) as (f' & r' & n & m & Hf & Hr & Hn & HH & HHH & HHHH).
  eapply batchedq1_len in H6; eauto. subst.
  exists m. intuition. exists n. intuition.
  exists f'. intuition. exists r'. intuition.
Qed.


Lemma batchedq1_len (q : IQ) (n: Z) (m: Z): batchedq1 q n -> batchedq_len q m -> n = m.
Admitted.



Lemma batchedq_check: (forall lenf, (lenf >= 0 -> (forall v, ((exists b_0, (0 <= b_0 /\ b_0 = lenf /\ (exists lenr, (0 <= lenr /\ lenr <= b_0 /\ (exists s_0, (0 <= s_0 /\ s_0 = lenf /\ (exists f, (len f s_0 /\ (exists s_1, (0 <= s_1 /\ s_1 = lenr /\ (exists r, (len r s_1 /\ batchedq_len v lenf)))))))))))) -> (exists b_0, (0 <= b_0 /\ b_0 = lenf /\ (exists lenr, (0 <= lenr /\ lenr <= b_0 /\ (exists s_0, (0 <= s_0 /\ s_0 = lenf /\ (exists f, (len f s_0 /\ (exists s_1, (0 <= s_1 /\ s_1 = lenr /\ (exists r, (len r s_1 /\ (exists lenf_0, (lenf_0 >= 0 /\ lenf_0 = lenf /\ (exists f_0, (len f_0 lenf_0 /\ f_0 = f /\ (exists lenr_0, (lenr_0 >= 0 /\ lenr_0 <= lenf_0 /\ lenr_0 = lenr /\ (exists r_0, (len r_0 lenr_0 /\ r_0 = r /\ batchedq1 v lenf_0 /\ batchedq2 v f_0 /\ batchedq3 v lenr_0 /\ batchedq4 v r_0))))))))))))))))))))))))%Z.
Proof.
  intros. repeat destr. subst.
  destruct (batchedq_destruct v) as (lenf' & f' & lenr' & r' & H11 & H22 & H33 & H44 & Hlenf & Hf & Hlenr & Hr).
  eapply batchedq1_len in H10; eauto. subst.
  exists lenf. intuition.
  exists lenr'. intuition.
  exists lenf. intuition.
  exists f'. intuition. exists lenr'. intuition. exists r'. intuition.
  exists lenf. intuition. exists f'. intuition. exists lenr'. intuition.
  exists r'. intuition.
Qed.

Lemma check : (forall size, (0 <= size -> (forall v, ((len v size/\ sorted v) -> ((size = 0/\ emp v) \/ (~size = 0/\ (exists size_1, ((0 <= size_1/\ size_1 = (size - 1))/\ (exists l, (((size_1 < size/\ size_1 >= 0)/\ len l size_1/\ sorted l)/\ (exists y, (hd v y/\ tl v l))))))))))))%Z.
Proof.
  intros. destruct (Z.eqb_spec size 0).
  - left. subst. intuition.
  - right. intuition.
    destruct (decide_emp v).
    + apply len_not_0_not_emp in H1; auto. intuition.
    + exists (size - 1)%Z. intuition.
      destruct (not_emp_ex_hd _ H0) as (x & Hx).
      destruct (not_emp_ex_tl _ H0) as (l & Hl).
      exists l. intuition; eauto.
Qed.

Lemma subtyping : forall (v: IS) (size: Z), (size >= 0)%Z -> prop2 v size -> prop1 v size.
Proof.
  unfold prop1, prop2. intros.
  destruct (Z.eqb_spec size 0).
  - left. intuition.
  - right. intuition. subst.
    assert ((lenF v > 0)%Z) as Htmp; intuition.
    apply nonempty_de_consF in Htmp. destruct Htmp as (x & v' & Hxv'). subst. clear n. clear H.
    exists x, (lenF v'). intuition. rewrite consF_len. intuition.
    exists v'. intuition. rewrite consF_len. intuition.
    apply H2. rewrite consF_ord. right. auto.
Qed.
