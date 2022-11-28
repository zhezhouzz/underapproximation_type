From stdpp Require Import mapset.
From CT Require Import Atom.
From CT Require Import Tactics.
From CT Require Import CoreLang.

Import CoreLang.

Ltac my_set_solver := fast_set_solver!! || set_solver.

Ltac rewrite_by_set_solver :=
  match goal with
  | H: context [ _ ->  _ = _ ]  |- _ => rewrite H by fast_set_solver!!
  end.

Ltac rewrite_by_fol :=
  match goal with
  | H: context [ _ ->  _ = _ ]  |- _ => rewrite H by firstorder
  end.

Ltac eempty_aset :=
  match goal with
  | [ |- context [_ -> True] ] => exists ∅; simpl; auto
  end.

Ltac bi_iff_rewrite :=
  match goal with
  | [ H: context [ _ <-> _ ] |- _ ] =>
      (rewrite <- H; firstorder; auto) || (rewrite H; firstorder; auto)
  end.

Ltac destruct_hyp_conj :=
  match goal with
  | [H: ?P /\ ?Q |- _ ] =>
      destruct H; repeat match goal with
                         | [ H' : P /\ Q |- _ ] => clear H'
                         end
  | [ H: ex _ |- _ ] => destruct H
  end.

#[global]
Instance atom_stale : @Stale aset atom := singleton.
Arguments atom_stale /.
#[global]
Instance aset_stale : Stale aset := id.
Arguments aset_stale /.
#[global]
Instance value_stale : Stale value := fv_value.
Arguments value_stale /.
#[global]
Instance tm_stale : Stale tm := fv_tm.
Arguments tm_stale /.
Arguments stale /.

Ltac collect_one_stale e acc :=
  match goal with
  | _ =>
      lazymatch acc with
      | tt => constr:(stale e)
      | _ => constr:(acc ∪ stale e)
      end
  | _ => acc
  end.

(** Return all stales in the context. *)
Ltac collect_stales S :=
  let stales := fold_hyps S collect_one_stale in
  lazymatch stales with
  | tt => fail "no stale available"
  | _ => stales
  end.

Ltac lc_at_tm_rewrite :=
  repeat destruct_hyp_conj;
  match goal with
  | [H: forall k, _ _ ?e <-> ?Q, H': _ _ ?e |- _ ] =>
      rewrite H in H'; repeat destruct_hyp_conj; auto
  | [H: forall k, _ _ ?e <-> ?Q |- context [_ _ ?e] ] =>
      rewrite H; auto
  end.

Ltac apply_by_set_solver :=
  match goal with
  | [H: forall k, _ -> _ _ (_ _ _ ?e) |- _ _ (_ _ _ ?e) ] => apply H; auto; set_solver
  | [H: forall k, _ -> _ _ (_ _ ?e) |- _ _ (_ _ _ ?e) ] => simpl in H; apply H; auto; set_solver
  | [H: forall k, _ -> _ /\ _ |- _ _ (_ _ (vfvar ?x) ?e) ] =>
      specialize (H x); destruct H; auto; my_set_solver; auto
  end.

Ltac auto_exists_L :=
  let acc := collect_stales tt in econstructor; auto; instantiate (1 := acc).

Ltac auto_exists_L_and_solve :=
  match goal with
  | [ |- ?P /\ _ ] => constructor; auto; auto_exists_L_and_solve
  | _ => auto_exists_L; intros; repeat split; apply_by_set_solver
  end.

Ltac auto_exfalso :=
  match goal with
  | [H: ?a <> ?a |- _ ] => exfalso; auto
  | [H: False |- _] => inversion H
  | [H: Some _ = None |- _ ] => inversion H
  | [H: None = Some _ |- _ ] => inversion H
  | [H1: [] = _ ++ _ |- _ ] => symmetry in H1; apply app_eq_nil in H1; destruct H1 as (_ & H1); inversion H1
  end.

Ltac var_dec_solver :=
  try auto_exfalso;
  match goal with
  | [H: Some ?a = Some ?b |- _] => inversion H; subst; clear H; simpl; auto
  (* | [H: ?a <> ?a |- _ ] => exfalso; lia *)
  | [H: context [ decide (?a = ?a) ] |- _ ] => rewrite decide_True in H; auto
  | [H: context [ decide (?a = ?b) ] |- _ ] =>
      match goal with
      | [H': a = b |- _ ] => rewrite decide_True in H; auto
      | [H': a <> b |- _ ] => rewrite decide_False in H; auto
      | _ => destruct (Nat.eq_dec a b); subst; simpl in H; simpl
      | _ => destruct (Atom.atom_dec a b); subst; simpl in H; simpl
      end
  | [ |- context [ decide (?a = ?a) ] ] => rewrite decide_True; auto
  | [ |- context [ decide (?a = ?b) ] ] =>
      match goal with
      | [H: a = b |- _ ] => rewrite decide_True; auto
      | [H: a <> b |- _ ] => rewrite decide_False; auto
      | _ => destruct (Nat.eq_dec a b); subst; simpl; var_dec_solver
      | _ => destruct (Atom.atom_dec a b); subst; simpl; var_dec_solver
      end
  | _ => progress simpl
  end.

Ltac auto_eq_post :=
  repeat match goal with
         | [ |- ?a ?e1 = ?a ?e2 ] =>
             assert (e1 = e2) as HH; try (rewrite HH; auto)
         | [|- ?a ?b ?e1 = ?a ?b ?e2 ] =>
             assert (e1 = e2) as HH; try (rewrite HH; auto)
         | [|- ?a ?b ?c ?e1 = ?a ?b ?c ?e2 ] =>
             assert (e1 = e2) as HH; try (rewrite HH; auto)
         | [ |- ?a ?b ?c1 = ?a ?b ?c2 ] =>
             assert (c1 = c2); auto
         | [ H: ?a _ = ?a _ |- _ ] =>
             inversion H; subst; clear H; auto
         | [ H: ?a _ _ = ?a _ _ |- _ ] =>
             inversion H; subst; clear H; auto
         | [ H: ?a _ _ _ = ?a _ _ _ |- _ ] =>
             inversion H; subst; clear H; auto
         end.

Ltac equate x y :=
  idtac x;
  let dummy := constr:(eq_refl x : x = y) in idtac.

