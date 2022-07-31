Lemma local_v : forall nu,
    (nu > 0) -> exists x, (x > 0) /\
                      (exists x0,
                          (x0 <-> (0 = x)) /\
                            ((x0 -> nu = 0) \/ (~ x0 -> (exists y, y = x + 1 /\ nu = y - 1)))
                      ).
Proof.
  intros.
  exists nu.
  split; auto.
  exists False.
  split.
  + split. intros. inversion H0. intros. subst. inversion H.
  + right. intros. exists (nu + 1). split; auto. 
