Lemma L (mem: nat -> nat -> Prop) (hd: nat -> nat -> Prop):
  forall v,
(exists f, ((forall u, ((mem f u) -> (hd f u)))/\ (forall u, ((mem v u) -> (hd f u))))) ->
(exists lenf, (exists f, (exists lenr, (exists r, (exists x, (exists r1, (exists b, ((lenf >= 0)/\ (forall u, ((mem f u) -> (hd f u)))/\ (lenr >= 0)/\ (lenr <= lenf)/\ (forall u, ((mem r u) -> (hd f u)))/\ (hd f x)/\ (forall u, (((mem r1 u) -> (u = x))/\ ((u = x) <-> (hd r1 u))))/\ (b <-> ((1 + lenr) <= lenf))/\ ((b/\ (forall u, (((mem v u) -> (u = x))/\ ((u = x) <-> (hd v u))))) \/ ((~ b)/\ (forall u, (~ (mem v u))))))))))))).
Proof.
  intros.
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
