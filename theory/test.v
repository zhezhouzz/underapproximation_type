Lemma L (mem: nat -> nat -> Prop) (hd: nat -> nat -> Prop) (empty: nat -> Prop):
  forall v,
(exists l1, (exists hd1, ((forall u, ((mem l1 u) -> (hd l1 u)))/\ (forall u, (((mem l1 u) -> (hd l1 u))/\ (~ (empty l1))))/\ (forall u, ((hd1 = u) <-> (hd l1 u)))/\ (forall u, (((hd1 = u) <-> (hd v u))/\ ((mem v u) -> (hd1 = u))))))) ->
(exists l1, (exists l2, (exists hd1, (exists tl1, (exists hd2, (exists tl2, (exists b1, (exists tmp0, (exists b2, ((forall u, ((mem l1 u) -> (hd l1 u)))/\ (forall u, (((mem l1 u) -> (hd l1 u))/\ (~ (empty l1))))/\ (forall u, ((mem l2 u) -> (hd l1 u)))/\ (forall u, (((mem l2 u) -> (hd l2 u))/\ (~ (empty l2))))/\ (forall u, ((hd1 = u) <-> (hd l1 u)))/\ (forall u, ((mem tl1 u) -> (mem l1 u)))/\ (forall u, ((hd2 = u) <-> (hd l2 u)))/\ (forall u, ((mem tl2 u) -> (mem l2 u)))/\ (b1 <-> (hd1 < hd2))/\ (~ b1)/\ (forall u, ((mem tmp0 u) -> (hd tl1 u)))/\ (b2 <-> (hd2 < hd1))/\ b2/\ (forall u, ((mem v u) -> (hd tl1 u))))))))))))).
Proof.
  intros.
  destruct H as (l1 & hd1 & H1 & H2 & H3 & H4).
  exists l1, 0, hd1, 1, 0, 3.
  exists False. exists 6. exists True.
  assert (forall u : nat, (mem l1 u -> hd l1 u) /\ ~ empty l1). admit.
  split. auto.
  split; auto.
  split; auto.
  split. admit.
  split. admit.
  split. admit.
  split. simpl. admit.
  right. split; auto.


  destruct H as (f & H1 & H2).
  assert (hd f 0 \/ (forall u, ~ hd f u)). admit.
  destruct H as [H | H].
  -
    exists 0, f, 0, 1, 0, 2, True.
    assert (forall u, mem 1 u -> hd f u). admit.
    assert (forall u, mem 2 u <-> hd f u). admit.
    split; auto.
    split; auto.
    split; auto.
    split; auto.
    split; auto.
    split; auto.
    split. split; auto. intros. admit. admit.
    

  
  exists (fun a b => a = b).
  intros.
  destruct H as (lenf & lenr & H1 & H2 & H3 & H4).
  exists lenf, lenf, lenr, lenr.
  exists (lenr + 1), True.
  exists 4.
  repeat (split; auto).
  intros. destruct H. subst. admit.
  left.
  split; auto.
  intros. destruct H. subst. auto.
  split. auto.
  split. auto.
  split. auto.
  split. auto.
  split. auto.
  split. intros. destruct H. subst. admit.
  split. split. intros. auto. auto.
  left. split. auto. auto.
  - auto.
  - auto.
  - auto.
  - intros. destruct H. subst. admit.
  - auto.
  
  
  admit.
  admit.
  exists 3.
  exists 
