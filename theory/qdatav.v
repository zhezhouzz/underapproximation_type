Definition zmem : nat -> nat -> Prop := fun _ _ => False.

Lemma x : exists (mem: nat -> nat -> Prop),
    (exists (x:nat), (True/\ (exists (v:nat), ((forall (u:nat), ((mem (v:nat) (u:nat)) -> ((u:nat) = (x:nat))))/\ (forall (l':nat), (((forall (u:nat), (~ (mem (l':nat) (u:nat)))) \/ (exists (u:nat), ((mem (l':nat) (u:nat))/\ (~ ((u:nat) = (x:nat)))))) \/ (((exists (u:nat), (mem (l':nat) (u:nat))) \/ (exists (u:nat), (mem (v:nat) (u:nat))))/\ ((forall (u:nat), ((~ (mem (l':nat) (u:nat))) \/ (exists (w:nat), ((mem (l':nat) (w:nat))/\ (~ ((w:nat) = (u:nat))))))) \/ (exists (u:nat), ((mem (v:nat) (u:nat))/\ (~ (mem (l':nat) (u:nat))))))))))))).
Proof.
  exists zmem.
  (* x is 0 *)
  exists 0.
  split; auto.
  (* nu is 3 *)
  exists 3.
  split. intros. inversion H.
  intros.
  left. left. intros. intro. inversion H.
  right.
