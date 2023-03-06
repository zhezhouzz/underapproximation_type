From stdpp Require Import mapset.
From CT Require Import CoreLangProp.
From CT Require Import ListCtx.

Import Atom.
Import CoreLang.
Import Tactics.
Import NamelessTactics.
Import ListCtx.

Definition ty_of_const (c: constant): base_ty :=
  match c with
  | cnat _ => TNat
  | cbool _ => TBool
  end.

Definition fst_ty_of_op (op: biop): base_ty :=
  match op with
  | op_plus => TNat
  | op_eq => TNat
  | op_lt => TNat
  | op_rannat => TNat
  end.

Definition snd_ty_of_op (op: biop): base_ty :=
  match op with
  | op_plus => TNat
  | op_eq => TNat
  | op_lt => TNat
  | op_rannat => TNat
  end.

Definition ret_ty_of_op (op: biop): base_ty :=
  match op with
  | op_plus => TNat
  | op_eq => TBool
  | op_lt => TBool
  | op_rannat => TNat
  end.

Definition ty_of_op (op: biop): ty :=
  (fst_ty_of_op op) ⤍ (snd_ty_of_op op) ⤍ (ret_ty_of_op op).

Lemma op_ty_spec: forall op, ty_of_op op = fst_ty_of_op op ⤍ snd_ty_of_op op ⤍ ret_ty_of_op op.
Proof with eauto.
  intros.
  destruct op...
Qed.

Definition context := listctx ty.

Reserved Notation "Γ '⊢t' t '⋮t' T" (at level 40).
Reserved Notation "Γ '⊢t' t '⋮v' T" (at level 40).

(** Basic typing rules  *)
Inductive tm_has_type : context -> tm -> ty -> Prop :=
| T_Err : forall Γ T, ok Γ -> Γ ⊢t terr ⋮t T
| T_Value : forall Γ v T, Γ ⊢t v ⋮v T -> Γ ⊢t v ⋮t T
| T_Lete : forall Γ e1 e2 T1 T2 (L: aset),
    Γ ⊢t e1 ⋮t T1 ->
    (forall (x: atom), x ∉ L -> (Γ ++ [(x, T1)]) ⊢t e2 ^t^ x ⋮t T2) ->
    Γ ⊢t (tlete e1 e2) ⋮t T2
| T_LetOp : forall Γ (op: biop) v1 v2 e (T1 T2 Tx: base_ty) T (L: aset),
    Γ ⊢t v1 ⋮v T1 ->
    Γ ⊢t v2 ⋮v T2 ->
    (ty_of_op op) = T1 ⤍ T2 ⤍ Tx ->
    (forall (x: atom), x ∉ L -> (Γ ++ [(x, TBase Tx)]) ⊢t e ^t^ x ⋮t T) ->
    Γ ⊢t tletbiop op v1 v2 e ⋮t T
| T_LetApp : forall Γ v1 v2 e T1 Tx T (L: aset),
    Γ ⊢t v1 ⋮v T1 ⤍ Tx ->
    Γ ⊢t v2 ⋮v T1 ->
    (forall (x: atom), x ∉ L -> (Γ ++ [(x, Tx)]) ⊢t e ^t^ x ⋮t T) ->
    Γ ⊢t tletapp v1 v2 e ⋮t T
| T_Matchb: forall Γ v e1 e2 T,
    Γ ⊢t v ⋮v TBool ->
    Γ ⊢t e1 ⋮t T ->
    Γ ⊢t e2 ⋮t T ->
    Γ ⊢t (tmatchb v e1 e2) ⋮t T
with value_has_type : context -> value -> ty -> Prop :=
| T_Const : forall Γ (c: constant), ok Γ -> Γ ⊢t c ⋮v (ty_of_const c)
| T_Var : forall Γ (x: atom) T,
    ok Γ ->
    ctxfind Γ x = Some T -> Γ ⊢t x ⋮v T
| T_Lam : forall Γ Tx T e (L: aset),
    (forall (x: atom), x ∉ L -> (Γ ++ [(x, Tx)]) ⊢t e ^t^ x ⋮t T) ->
    Γ ⊢t vlam Tx e ⋮v Tx ⤍ T
| T_Fix : forall Γ (Tx: base_ty) T e (L: aset),
    (forall (f: atom), f ∉ L -> (Γ ++ [(f, TBase Tx)]) ⊢t (vlam (Tx ⤍ T) e) ^v^ f ⋮v ((Tx ⤍ T) ⤍ T)) ->
    Γ ⊢t vfix (Tx ⤍ T) (vlam Tx e) ⋮v Tx ⤍ T
where "Γ '⊢t' t '⋮t' T" := (tm_has_type Γ t T) and "Γ '⊢t' t '⋮v' T" := (value_has_type Γ t T).

Scheme value_has_type_mutual_rec := Induction for value_has_type Sort Prop
    with tm_has_type_mutual_rec := Induction for tm_has_type Sort Prop.

Global Hint Constructors tm_has_type: core.
Global Hint Constructors value_has_type: core.

Lemma tricky_closed_value_exists: forall (T: ty), exists v, forall Γ, ok Γ -> Γ ⊢t v ⋮v T.
Proof.
  destruct T.
  - destruct b. exists 0. constructor; auto. exists true. constructor; auto.
  - exists (vlam T1 terr). econstructor; eauto.
Qed.

Lemma subseteq_trans_cons: forall x (s1 s2 s3: aset), {[x]} ∪ s1 ⊆ {[x]} ∪ s2 -> s2 ⊆ {[x]} ∪ s3 -> {[x]} ∪ s1 ⊆ {[x]} ∪ s3.
Proof.
  intros.
  assert ({[x]} ∪ s2 ⊆ {[x]} ∪ s3). fast_set_solver.
  fast_set_solver.
Qed.

Ltac auto_pose_fv a :=
  let acc := collect_stales tt in
  pose (fv_of_set acc) as a;
  assert (a ∉ acc) by (apply fv_of_set_fresh; auto).

Lemma basic_typing_contains_fv_tm: forall Γ e T, Γ ⊢t e ⋮t T -> fv_tm e ⊆ ctxdom Γ.
Proof.
   apply (tm_has_type_mutual_rec
           (fun Γ v T P => fv_value v ⊆ ctxdom Γ)
           (fun Γ e T P => fv_tm e ⊆ ctxdom Γ)
        ); simpl; intros; subst; listctx_set_simpl;
     try (fast_set_solver);
     (auto_pose_fv x; repeat specialize_with x; listctx_set_simpl';
      match goal with
      | [ H: fv_tm ({?k ~t> _} ?e) ⊆ _ |- context [ctxdom ?Γ] ] =>
          assert (({[x]} ∪ fv_tm e) ⊆ ({[x]} ∪ fv_tm ({k ~t> x} e))) by (apply open_with_fresh_include_fv_tm; fast_set_solver);
          mmy_set_simpl1; eapply subseteq_trans_cons in H; eauto;
          assert (fv_tm e ⊆ ctxdom Γ) by mmy_set_solver1; auto; fast_set_solver
      end).
Qed.

Lemma basic_typing_contains_fv_value: forall Γ e T, Γ ⊢t e ⋮v T -> fv_value e ⊆ ctxdom Γ.
Proof.
   apply (value_has_type_mutual_rec
           (fun Γ v T P => fv_value v ⊆ ctxdom Γ)
           (fun Γ e T P => fv_tm e ⊆ ctxdom Γ)
        ); simpl; intros; subst; listctx_set_simpl;
     try (fast_set_solver);
     (auto_pose_fv x; repeat specialize_with x; listctx_set_simpl';
      match goal with
      | [ H: fv_tm ({?k ~t> _} ?e) ⊆ _ |- context [ctxdom ?Γ] ] =>
          assert (({[x]} ∪ fv_tm e) ⊆ ({[x]} ∪ fv_tm ({k ~t> x} e))) by (apply open_with_fresh_include_fv_tm; fast_set_solver);
          mmy_set_simpl1; eapply subseteq_trans_cons in H; eauto;
          assert (fv_tm e ⊆ ctxdom Γ) by mmy_set_solver1; auto; fast_set_solver
      end).
Qed.

Ltac instantiate_atom_listctx :=
  let acc := collect_stales tt in
  instantiate (1 := acc); intros; listctx_set_simpl;
  repeat (match goal with
          | [H: forall (x: atom), x ∉ ?L -> _, H': ?a ∉ _ ∪ (stale _) |- _ ] =>
              assert (a ∉ L) as Htmp by fast_set_solver;
              specialize (H a Htmp); clear Htmp; repeat destruct_hyp_conj; auto
          end; simpl).

Lemma basic_typing_regular_value: forall Γ v t, Γ ⊢t v ⋮v t -> ok Γ /\ lc v.
Proof.
  apply (value_has_type_mutual_rec
           (fun Γ v t P => ok Γ /\ lc v)
           (fun Γ e t P => ok Γ /\ lc e)
        ); split; intros; repeat destruct_hyp_conj; auto;
    try (match goal with
         | [H: forall (x: atom), x ∉ ?L -> ok (?Γ ++ _) /\ lc ?e |- _ ] =>
             specialize (H (fv_of_set L) (fv_of_set_fresh L));
             destruct H; repeat destruct_hyp_conj
         end; lc_solver; listctx_set_solver);
  try (econstructor; auto; instantiate_atom_listctx).
  invclear H1. econstructor; eauto.
Qed.

Lemma basic_typing_regular_tm: forall Γ e t, Γ ⊢t e ⋮t t -> ok Γ /\ lc e.
Proof.
  apply (tm_has_type_mutual_rec
           (fun Γ v t P => ok Γ /\ lc v)
           (fun Γ e t P => ok Γ /\ lc e)
        ); split; intros; repeat destruct_hyp_conj; auto;
    try (match goal with
         | [H: forall (x: atom), x ∉ ?L -> ok (?Γ ++ _) /\ lc ?e |- _ ] =>
             specialize (H (fv_of_set L) (fv_of_set_fresh L));
             destruct H; repeat destruct_hyp_conj
         end; lc_solver; listctx_set_solver);
    try (econstructor; auto; instantiate_atom_listctx).
  invclear H1. econstructor; eauto.
Qed.

Ltac basic_typing_regular_simp :=
  repeat match goal with
    | [H: _ ⊢t _ ⋮v _ |- lc _] => apply basic_typing_regular_value in H; destruct H; auto
    | [H: _ ⊢t _ ⋮t _ |- lc _] => apply basic_typing_regular_tm in H; destruct H; auto
    | [H: _ ⊢t _ ⋮v _ |- body _] => apply basic_typing_regular_value in H; destruct H; auto
    | [H: _ ⊢t _ ⋮t _ |- body _] => apply basic_typing_regular_tm in H; destruct H; auto
    | [H: _ ⊢t _ ⋮v _ |- ok _] => apply basic_typing_regular_value in H; destruct H; auto
    | [H: _ ⊢t _ ⋮t _ |- ok _] => apply basic_typing_regular_tm in H; destruct H; auto
    end.

Lemma empty_basic_typing_base_const_exists: forall (v: value) (B: base_ty), [] ⊢t v ⋮v B -> (exists (c: constant), v = c).
Proof.
  intros. inversion H; subst.
  - exists c; auto.
  - inversion H1.
Qed.

Lemma empty_basic_typing_bool_value_exists: forall (v: value), [] ⊢t v ⋮v TBool -> v = true \/ v = false.
Proof.
  intros. inversion H; subst.
  - destruct c; inversion H0. destruct b; inversion H0. left; auto. right; auto.
  - inversion H1.
Qed.

Lemma empty_basic_typing_nat_value_exists: forall (v: value), [] ⊢t v ⋮v TNat -> (exists (i: nat), v = i).
Proof.
  intros. inversion H; subst.
  - destruct c; inversion H0. exists n; auto.
  - inversion H1.
Qed.

Lemma empty_basic_typing_arrow_value_lam_exists:
  forall (v: value) T1 T2, [] ⊢t v ⋮v T1 ⤍ T2 ->
                        (exists e, v = vlam T1 e) \/ (exists e, v = vfix (T1 ⤍ T2) (vlam T1 e)).
Proof.
  intros. inversion H; subst.
  - inversion H1.
  - left. exists e; auto.
  - right; exists e; auto.
Qed.
