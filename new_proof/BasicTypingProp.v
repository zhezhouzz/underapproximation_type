From stdpp Require Import mapset.
From CT Require Import CoreLangProp.
From CT Require Import ListCtx.
From CT Require Import BasicTyping.
From CT Require Import OperationalSemantics.


Import Atom.
Import CoreLang.
Import Tactics.
Import NamelessTactics.
Import ListCtx.
Import BasicTyping.
Import OperationalSemantics.

(* weakening *)

Ltac sv :=
  match goal with
  | [H: ctxfind (?Γ1 ++ ?Γ3) ?x = Some ?T |- ctxfind (?Γ1 ++ ?Γ2 ++ ?Γ3) ?x = Some ?T ] =>
      apply ctxfind_app_weaken; auto
  end.

Lemma basic_typing_weaken_value_aux: forall Γ (v: value) T,
    Γ ⊢t v ⋮v T ->
    (forall Γ1 Γ2 Γ3, Γ = Γ1 ++ Γ3 -> ok (Γ1 ++ Γ2 ++ Γ3) → (Γ1 ++ Γ2 ++ Γ3) ⊢t v ⋮v T).
Proof.
  apply (value_has_type_mutual_rec
           (fun Γ v T P => ∀ (Γ1 Γ2 Γ3 : list (atom * ty)),
                (Γ = Γ1 ++ Γ3) -> ok (Γ1 ++ Γ2 ++ Γ3) → (Γ1 ++ Γ2 ++ Γ3) ⊢t v ⋮v T)
           (fun Γ e T P => ∀ (Γ1 Γ2 Γ3 : list (atom * ty)),
                (Γ = Γ1 ++ Γ3) -> ok (Γ1 ++ Γ2 ++ Γ3) → (Γ1 ++ Γ2 ++ Γ3) ⊢t e ⋮t T)
        ); simpl; intros; subst; econstructor; eauto;
    try (instantiate_atom_listctx; auto_apply; listctx_set_solver);
    try listctx_set_solver.
Qed.

Lemma basic_typing_weaken_tm_aux: forall Γ (v: tm) T,
    Γ ⊢t v ⋮t T ->
    (forall Γ1 Γ2 Γ3, Γ = Γ1 ++ Γ3 -> ok (Γ1 ++ Γ2 ++ Γ3) → (Γ1 ++ Γ2 ++ Γ3) ⊢t v ⋮t T).
Proof.
  apply (tm_has_type_mutual_rec
           (fun Γ v T P => ∀ (Γ1 Γ2 Γ3 : list (atom * ty)),
                (Γ = Γ1 ++ Γ3) -> ok (Γ1 ++ Γ2 ++ Γ3) → (Γ1 ++ Γ2 ++ Γ3) ⊢t v ⋮v T)
           (fun Γ e T P => ∀ (Γ1 Γ2 Γ3 : list (atom * ty)),
                (Γ = Γ1 ++ Γ3) -> ok (Γ1 ++ Γ2 ++ Γ3) → (Γ1 ++ Γ2 ++ Γ3) ⊢t e ⋮t T)
        ); simpl; intros; subst; econstructor; eauto;
    try (instantiate_atom_listctx; auto_apply; listctx_set_solver);
    try listctx_set_solver.
Qed.

Lemma basic_typing_weaken_value: forall Γ1 Γ3 Γ2 (v: value) T,
    (Γ1 ++ Γ3) ⊢t v ⋮v T -> ok (Γ1 ++ Γ2 ++ Γ3) -> (Γ1 ++ Γ2 ++ Γ3) ⊢t v ⋮v T.
Proof.
  intros. eapply basic_typing_weaken_value_aux; eauto.
Qed.

Lemma basic_typing_weaken_tm: forall Γ1 Γ3 Γ2 (v: tm) T,
    (Γ1 ++ Γ3) ⊢t v ⋮t T -> ok (Γ1 ++ Γ2 ++ Γ3) -> (Γ1 ++ Γ2 ++ Γ3) ⊢t v ⋮t T.
Proof.
  intros. eapply basic_typing_weaken_tm_aux; eauto.
Qed.

Lemma basic_typing_weaken_value_pre: forall Γ1 Γ3 u T,
    ok (Γ1 ++ Γ3) -> Γ1 ⊢t u ⋮v T -> (Γ1 ++ Γ3) ⊢t u ⋮v T.
Proof.
  intros. rewrite <- (app_nil_r Γ1) in H0. rewrite <- (app_nil_r (Γ1 ++ Γ3)).
  rewrite <- app_assoc. apply basic_typing_weaken_value; auto. listctx_set_solver.
Qed.

Lemma basic_typing_weaken_tm_pre: forall Γ1 Γ3 u T,
    ok (Γ1 ++ Γ3) -> Γ1 ⊢t u ⋮t T -> (Γ1 ++ Γ3) ⊢t u ⋮t T.
Proof.
  intros. rewrite <- (app_nil_r Γ1) in H0. rewrite <- (app_nil_r (Γ1 ++ Γ3)).
  rewrite <- app_assoc. apply basic_typing_weaken_tm; auto. listctx_set_solver.
Qed.

Lemma basic_typing_weaken_value_post: forall Γ1 Γ3 u T,
    ok (Γ1 ++ Γ3) -> Γ3 ⊢t u ⋮v T -> (Γ1 ++ Γ3) ⊢t u ⋮v T.
Proof.
  intros. rewrite <- (app_nil_l Γ3) in H0. rewrite <- (app_nil_l (Γ1 ++ Γ3)).
  apply basic_typing_weaken_value; auto.
Qed.

Lemma basic_typing_weaken_tm_post: forall Γ1 Γ3 u T,
    ok (Γ1 ++ Γ3) -> Γ3 ⊢t u ⋮t T -> (Γ1 ++ Γ3) ⊢t u ⋮t T.
Proof.
  intros. rewrite <- (app_nil_l Γ3) in H0. rewrite <- (app_nil_l (Γ1 ++ Γ3)).
  apply basic_typing_weaken_tm; auto.
Qed.

(* subst *)

Ltac basic_typing_solver1 :=
  step_regular_simp;
  basic_typing_regular_simp;
  listctx_set_simpl;
  (match goal with
   | [H: ?Γ1 ⊢t ?u ⋮v ?T |- (?Γ1 ++ ?Γ2) ⊢t ?u ⋮v ?T ] =>
       apply basic_typing_weaken_value_pre; auto; try listctx_set_solver
   | [H: ctxfind _ ?x = Some ?T |- _ ⊢t (vfvar ?x) ⋮v ?T ] => constructor; listctx_set_solver
   | [H: ?Γ2 ⊢t ?u ⋮v ?T |- (?Γ1 ++ ?Γ2) ⊢t ?u ⋮v ?T ] => apply basic_typing_weaken_value_post; auto; try listctx_set_solver
   | [H: _ ⊢t ?u ⋮v _ |- lc (tvalue ?u)] => apply basic_typing_regular_value in H; destruct H; auto
   | [H: _ ⊢t ?u ⋮t _ |- lc ?u] => apply basic_typing_regular_tm in H; destruct H; auto
   end || lc_solver ||
     listctx_set_solver || eauto).

Lemma basic_typing_subst_value_aux: forall Γ (v: value) T,
    Γ ⊢t v ⋮v T ->
    (forall Γ1 z u U Γ3, Γ = Γ1 ++ [(z, U)] ++ Γ3 -> Γ1 ⊢t u ⋮v U -> (Γ1 ++ Γ3) ⊢t {z := u}v v ⋮v T).
Proof.
  apply (value_has_type_mutual_rec
           (fun Γ v T P => forall Γ1 z u U Γ3,
                Γ = Γ1 ++ [(z, U)] ++ Γ3 -> Γ1 ⊢t u ⋮v U -> (Γ1 ++ Γ3) ⊢t {z := u}v v ⋮v T)
           (fun Γ e T P => forall Γ1 z u U Γ3,
                Γ = Γ1 ++ [(z, U)] ++ Γ3 -> Γ1 ⊢t u ⋮v U -> (Γ1 ++ Γ3) ⊢t {z := u}t e ⋮t T)
        ); simpl; intros; subst; listctx_set_simpl;
    try (var_dec_solver; basic_typing_solver1);
    try (econstructor; listctx_set_solver1).
  - econstructor. instantiate_atom_listctx.
    rewrite <- subst_open_var_tm; basic_typing_solver1.
  - econstructor. instantiate_atom_listctx.
    rewrite <- subst_open_var_tm; basic_typing_solver1.
  - econstructor; basic_typing_solver1.
  - econstructor. instantiate (1:= T1). basic_typing_solver1.
    instantiate_atom_listctx; rewrite <- subst_open_var_tm; basic_typing_solver1.
  - econstructor.
    instantiate (1:= T1). basic_typing_solver1.
    instantiate (1:= T2). basic_typing_solver1.
    instantiate (1:= Tx). basic_typing_solver1.
    instantiate_atom_listctx; rewrite <- subst_open_var_tm; basic_typing_solver1.
  - econstructor.
    instantiate (1:= Tx). instantiate (1:= T1). basic_typing_solver1. basic_typing_solver1.
    instantiate_atom_listctx; rewrite <- subst_open_var_tm; basic_typing_solver1.
  - econstructor; basic_typing_solver1.
Qed.

Lemma basic_typing_subst_tm_aux: forall Γ (v: tm) T,
    Γ ⊢t v ⋮t T ->
    (forall Γ1 z u U Γ3, Γ = Γ1 ++ [(z, U)] ++ Γ3 -> Γ1 ⊢t u ⋮v U -> (Γ1 ++ Γ3) ⊢t {z := u}t v ⋮t T).
Proof.
  apply (tm_has_type_mutual_rec
           (fun Γ v T P => forall Γ1 z u U Γ3,
                Γ = Γ1 ++ [(z, U)] ++ Γ3 -> Γ1 ⊢t u ⋮v U -> (Γ1 ++ Γ3) ⊢t {z := u}v v ⋮v T)
           (fun Γ e T P => forall Γ1 z u U Γ3,
                Γ = Γ1 ++ [(z, U)] ++ Γ3 -> Γ1 ⊢t u ⋮v U -> (Γ1 ++ Γ3) ⊢t {z := u}t e ⋮t T)
        ); simpl; intros; subst; listctx_set_simpl;
    try (var_dec_solver; basic_typing_solver1);
    try (econstructor; listctx_set_solver).
  - econstructor. instantiate_atom_listctx.
    rewrite <- subst_open_var_tm; basic_typing_solver1.
  - econstructor. instantiate_atom_listctx.
    rewrite <- subst_open_var_tm; basic_typing_solver1.
  - econstructor; basic_typing_solver1.
  - econstructor. instantiate (1:= T1). basic_typing_solver1.
    instantiate_atom_listctx; rewrite <- subst_open_var_tm; basic_typing_solver1.
  - econstructor.
    instantiate (1:= T1). basic_typing_solver1.
    instantiate (1:= T2). basic_typing_solver1.
    instantiate (1:= Tx). basic_typing_solver1.
    instantiate_atom_listctx; rewrite <- subst_open_var_tm; basic_typing_solver1.
  - econstructor.
    instantiate (1:= Tx). instantiate (1:= T1). basic_typing_solver1. basic_typing_solver1.
    instantiate_atom_listctx; rewrite <- subst_open_var_tm; basic_typing_solver1.
  - econstructor; basic_typing_solver1.
Qed.

Lemma basic_typing_subst_value: forall Γ1 z u U Γ3 (v: value) T,
    (Γ1 ++ [(z, U)] ++ Γ3) ⊢t v ⋮v T -> Γ1 ⊢t u ⋮v U -> (Γ1 ++ Γ3) ⊢t {z := u}v v ⋮v T.
Proof.
  intros. eapply basic_typing_subst_value_aux; eauto.
Qed.

Lemma basic_typing_subst_tm: forall Γ1 z u U Γ3 (v: tm) T,
    (Γ1 ++ [(z, U)] ++ Γ3) ⊢t v ⋮t T -> Γ1 ⊢t u ⋮v U -> (Γ1 ++ Γ3) ⊢t {z := u}t v ⋮t T.
Proof.
  intros. eapply basic_typing_subst_tm_aux; eauto.
Qed.

Lemma basic_typing_subst_value_pre: forall Γ1 z u U (v: value) T,
    (Γ1 ++ [(z, U)]) ⊢t v ⋮v T -> Γ1 ⊢t u ⋮v U -> Γ1 ⊢t {z := u}v v ⋮v T.
Proof.
  intros. rewrite <- (app_nil_r (Γ1 ++ [(z, U)])) in H. rewrite <- app_assoc in H.
  rewrite <- (app_nil_r Γ1).
  eapply basic_typing_subst_value; eauto.
Qed.

Lemma basic_typing_subst_tm_pre: forall Γ1 z u U (v: tm) T,
    (Γ1 ++ [(z, U)]) ⊢t v ⋮t T -> Γ1 ⊢t u ⋮v U -> Γ1 ⊢t {z := u}t v ⋮t T.
Proof.
  intros. rewrite <- (app_nil_r (Γ1 ++ [(z, U)])) in H. rewrite <- app_assoc in H.
  rewrite <- (app_nil_r Γ1).
  eapply basic_typing_subst_tm; eauto.
Qed.

Lemma basic_typing_subst_value_post: forall Γ1 z u U (v: value) T,
    ((z, U) :: Γ1) ⊢t v ⋮v T -> [] ⊢t u ⋮v U -> Γ1 ⊢t {z := u}v v ⋮v T.
Proof.
  intros.
  assert (([] ++ [(z, U)] ++ Γ1) ⊢t v ⋮v T) by listctx_set_simpl.
  apply (basic_typing_subst_value [] z u) in H1; listctx_set_simpl.
Qed.

Lemma basic_typing_subst_tm_post: forall Γ1 z u U (v: tm) T,
    ((z, U) :: Γ1) ⊢t v ⋮t T -> [] ⊢t u ⋮v U -> Γ1 ⊢t {z := u}t v ⋮t T.
Proof.
  intros.
  assert (([] ++ [(z, U)] ++ Γ1) ⊢t v ⋮t T) by listctx_set_simpl.
  apply (basic_typing_subst_tm [] z u) in H1; listctx_set_simpl.
Qed.

(* Lemma basic_typing_subst_value_post *)

Lemma eval_op_type_safe: forall op c1 c2 c3 (T1 T2 T3: base_ty),
  eval_op op c1 c2 c3 ->
  ty_of_op op = T1 ⤍ T2 ⤍ T3 ->
  ty_of_const c1 = T1 /\ ty_of_const c2 = T2 /\ ty_of_const c3 = T3.
Proof.
  repeat split; intros; destruct op; inversion H; subst; rewrite op_ty_spec in H0; inversion H0; subst; auto.
Qed.

Lemma eval_op_preservation: forall Γ op c1 c2 c3 (T1 T2 T3: base_ty),
    eval_op op c1 c2 c3 ->
    ty_of_op op = T1 ⤍ T2 ⤍ T3 ->
    ok Γ ->
    Γ ⊢t c1 ⋮v T1 /\ Γ ⊢t c2 ⋮v T2 /\ Γ ⊢t c3 ⋮v T3.
Proof.
  intros.
  assert (ty_of_const c1 = T1 /\ ty_of_const c2 = T2 /\ ty_of_const c3 = T3) by (eapply eval_op_type_safe; eauto).
  repeat destruct_hyp_conj; subst. repeat split; auto.
Qed.

(* Lemma eval_op_preservation: forall Γ op c1 c2 c3 (T1 T2 T3: base_ty), *)
(*     eval_op op c1 c2 c3 -> *)
(*     ty_of_op op = T1 ⤍ T2 ⤍ T3 -> *)
(*     Γ ⊢t c1 ⋮v T1 /\ Γ ⊢t c2 ⋮v T2 /\ Γ ⊢t c3 ⋮v T3. *)
(* Proof. *)
(*   intros. *)
(*   inversion H0; subst. *)
(*   eapply eval_op_preservation_aux; eauto. *)
(* Qed. *)

Ltac basic_typing_simpl1 :=
  step_regular_simp;
  basic_typing_regular_simp;
  listctx_set_simpl;
  repeat match goal with
    | [H: ?Γ ⊢t (tvalue ?v) ⋮t ?T |- _ ] => inversion H; subst; clear H; auto
    end.

Lemma basic_typing_weaken_tm_empty: forall Γ (e: tm) (T: ty), ok Γ -> [] ⊢t e ⋮t T -> Γ ⊢t e ⋮t T.
Proof.
  intros.
  assert (Γ = [] ++ Γ) by auto. rewrite H1.
  apply basic_typing_weaken_tm_pre; basic_typing_solver1.
Qed.

Lemma basic_typing_weaken_value_empty: forall Γ (e: value) (T: ty), ok Γ -> [] ⊢t e ⋮v T -> Γ ⊢t e ⋮v T.
Proof.
  intros.
  assert (Γ = [] ++ Γ) by auto. rewrite H1.
  apply basic_typing_weaken_value_pre; basic_typing_solver1.
Qed.

Lemma ty_tlete_dummy: forall Γ Tx T (e_x e: tm), Γ ⊢t e_x ⋮t Tx -> Γ ⊢t e ⋮t T -> Γ ⊢t (tlete e_x e) ⋮t T.
Proof.
  intros.
  auto_exists_L; intros. rewrite open_rec_lc_tm; basic_typing_solver1.
  apply basic_typing_weaken_tm_pre; auto; basic_typing_solver1.
Qed.

Ltac basic_typing_solver2 :=
  basic_typing_simpl1;
  try match goal with
    | [H: ?Γ ⊢t ?e ⋮v ?T |- _ ⊢t (tvalue ?e) ⋮t _ ] => constructor; auto
    | [H: ty_of_op _ =  _ ⤍ _ ⤍ ?T |- ?Γ ⊢t (vconst _) ⋮v ?T ] => eapply eval_op_preservation; eauto
    | [H: [] ⊢t ?e ⋮t ?T |- _ ⊢t ?e ⋮t ?T ] => apply basic_typing_weaken_tm_empty; eauto
    | [H: [] ⊢t ?e ⋮v ?T |- _ ⊢t ?e ⋮v ?T ] => apply basic_typing_weaken_value_empty; eauto
    | [H: ?Γ ⊢t ?e ⋮t ?T |- (?Γ ++ _) ⊢t ?e ⋮t ?T ] =>
        apply basic_typing_weaken_tm_pre; auto; basic_typing_solver2
    | [H: ?Γ ⊢t ?e1 ⋮t _, H': ?Γ ⊢t ?e2 ⋮t ?T |- ?Γ ⊢t tlete ?e1 ?e2 ⋮t ?T ] =>
        eapply ty_tlete_dummy; eauto
    end;
  (basic_typing_solver1).

Ltac perservation_aux a Ha Tx :=
  match goal with
  | [H: forall (x:atom), _ -> _ ⊢t ?e ^t^ (vfvar x) ⋮t ?T |- _ ⊢t ?e ^t^ ?v ⋮t ?T ] =>
      auto_pose_fv a; rewrite <- (subst_intro_tm _ a);
      try (eapply basic_typing_subst_tm_pre; try basic_typing_solver2; apply H; basic_typing_solver2);
      try (apply basic_typing_subst_tm_pre with (U := Tx); basic_typing_solver2);
      try basic_typing_solver2
  end.

(* perservation *)
Lemma preservation: forall Γ T (e e': tm), e ↪ e' -> Γ ⊢t e ⋮t T -> Γ ⊢t e' ⋮t T.
Proof.
  intros. generalize dependent e'.
  induction H0; intros; auto;
    match goal with
    | [H: _ ↪ _ |- _ ] => inversion H; subst
    end; auto.
  - econstructor; auto; instantiate_atom_listctx; auto_apply; listctx_set_solver.
  - perservation_aux a Ha Tx.
  - perservation_aux a Ha Tx. apply H2; basic_typing_solver2.
  - inversion H; subst; clear H. econstructor. instantiate (1:= Tx).
    + perservation_aux a Ha Tx.
    + instantiate_atom_listctx; auto_apply; listctx_set_solver.
  - inversion H; subst; clear H. auto_pose_fv a. eapply T_LetApp; eauto.
    + rewrite <- (subst_intro_value _ a);
        try (eapply basic_typing_subst_value_pre; try (apply H6; basic_typing_solver2); basic_typing_solver2);
        basic_typing_solver2.
Qed.

(* multi preservation *)

Lemma multi_preservation: forall Γ T (e e': tm), e ↪* e' -> Γ ⊢t e ⋮t T -> Γ ⊢t e' ⋮t T.
Proof.
  intros.
  induction H; auto.
  - apply IHmultistep. eapply preservation; eauto.
Qed.

Lemma multi_preservation_value: forall Γ T (e: tm) (v: value), e ↪* v -> Γ ⊢t e ⋮t T -> Γ ⊢t v ⋮v T.
Proof.
  intros.
  eapply multi_preservation in H0; eauto. inversion H0; subst; auto.
Qed.

(* Facts *)

Lemma empty_basic_typing_eval_op_result_exists: forall (op:biop) (v1 v2: value) (T1 T2 Tx: base_ty),
    ty_of_op op = T1 ⤍ T2 ⤍ Tx -> [] ⊢t v2 ⋮v T2 -> [] ⊢t v1 ⋮v T1 ->
    exists (c1 c2 c3: constant), v1 = c1 /\ v2 = c2 /\ eval_op op c1 c2 c3 /\ [] ⊢t c3 ⋮v Tx.
Proof.
  intros.
  inversion H; subst.
  assert (exists (c1: constant), v1 = c1) as Htmp. eapply empty_basic_typing_base_const_exists; eauto. destruct Htmp; subst.
  assert (exists (c2: constant), v2 = c2) as Htmp. eapply empty_basic_typing_base_const_exists; eauto. destruct Htmp; subst.
  destruct op;
    simpl in H0; apply empty_basic_typing_nat_value_exists in H0; destruct H0; subst;
    simpl in H1; apply empty_basic_typing_nat_value_exists in H1; destruct H1; subst.
  - exists x2, x1, (x2 + x1). repeat split; auto. econstructor; auto.
  - exists x2, x1, (Nat.eqb x2 x1). repeat split; auto. econstructor; auto.
  - exists x2, x1, (Nat.ltb x2 x1). repeat split; auto. econstructor; auto.
  - exists x2, x1, 0. repeat split; auto. econstructor; auto.
Qed.

(* Ltacs *)

Ltac op_simpl :=
  basic_typing_simpl1;
  repeat match goal with
  | [H: _ ^t^ _ = terr |- _ ] => apply open_eq_terr in H; subst
  | [H: tlete terr _ ↪* (tvalue _) |- _ ] => apply tlete_terr_exfalso in H; inversion H
  | [H: tlete (tvalue _) _ ↪* (tvalue _) |- _ ] => rewrite tlete_value_exists in H; repeat destruct_hyp_conj; auto
  | [ |- ex (fun v_x => tvalue ?v ↪* tvalue v_x /\ _)] => exists v; split; auto
  end.

Ltac op_solver :=
  op_simpl;
  auto;
  repeat match goal with
    | [H: _ ⊢t ?e ⋮t _ |- lc ?e ] => apply basic_typing_regular_tm in H; destruct H; auto
    (* | [H: basicR _ ?e |- lc ?e ] => apply basicR_typable_empty in H; auto *)
    (* | [H: basicR _ ?e |- [] ⊢t ?e ⋮t _ ] => apply basicR_typable_empty in H; eauto *)
    | [H: _ ↪ ?e |- lc ?e] => apply multi_step_regular2 in H; auto
    | [H: ?Γ ⊢t ?e ⋮t ?T |- (?Γ ++ _) ⊢t ?e ⋮t _] => apply basic_typing_weaken_tm_pre; eauto
    | [H: ?Γ ⊢t _ ⋮t _ |- ok _ ] => apply basic_typing_regular_tm in H; destruct H
    end; listctx_set_solver.

Ltac basic_typing_vfavr_solver_slow :=
  op_simpl;
  match goal with
  | [|- _ ∉ _ ] => listctx_set_solver
  | [|- ok _ ] => op_solver
  | [|- ctxfind _ _ = Some _ ] => repeat var_dec_solver
  | [|- [(_, _); (_, _)] ⊢t (vfvar ?x) ⋮v _ ] => constructor; basic_typing_vfavr_solver_slow
  | [|- [(_, _); (_, _); (_, _)] ⊢t (vfvar ?x) ⋮v _ ] => constructor
                                                       (* ; basic_typing_vfavr_solver *)
  | [|- [(_, _)] ⊢t (vfvar ?x) ⋮v _ ] => constructor; basic_typing_vfavr_solver_slow
  | [|- (_ ++ _) ⊢t (tvalue (vfvar ?x)) ⋮t _] => econstructor; op_simpl
  | [|- (_ ++ _) ⊢t (vfvar ?x) ⋮v _] =>
      apply basic_typing_weaken_value_post; eauto; basic_typing_vfavr_solver_slow
  end.

Lemma ok_cache_l3: forall Γ (x1 x2 x3: atom) (T1 T2 T3: ty),
  ok Γ ->
  x1 ∉ stale Γ ->
  x2 ∉ stale Γ ∪ {[x1]} ->
  x3 ∉ stale Γ ∪ {[x1; x2]} ->
  ok (Γ ++ [(x1, T1); (x2, T2); (x3, T3)]).
Proof.
  intros. repeat basic_typing_vfavr_solver_slow.
Qed.

Lemma ok_cache_l2: forall Γ (x1 x2: atom) (T1 T2: ty),
  ok Γ ->
  x1 ∉ stale Γ ->
  x2 ∉ stale Γ ∪ {[x1]} ->
  ok (Γ ++ [(x1, T1); (x2, T2)]).
Proof.
  intros. repeat basic_typing_vfavr_solver_slow.
Qed.

Lemma vfvar_cache_l33: forall Γ (x1 x2 x3: atom) (T1 T2 T3: ty) e T,
  Γ ⊢t e ⋮t T ->
  x1 ∉ stale Γ ->
  x2 ∉ stale Γ ∪ {[x1]} ->
  x3 ∉ stale Γ ∪ {[x1; x2]} ->
  (Γ ++ [(x1, T1); (x2, T2); (x3, T3)]) ⊢t x3 ⋮v T3.
Proof.
  intros. repeat basic_typing_vfavr_solver_slow.
Qed.

Lemma vfvar_cache_l22: forall Γ (x1 x2: atom) (T1 T2: ty) e T,
  Γ ⊢t e ⋮t T ->
  x1 ∉ stale Γ ->
  x2 ∉ stale Γ ∪ {[x1]} ->
  (Γ ++ [(x1, T1); (x2, T2)]) ⊢t x2 ⋮v T2.
Proof.
  intros. repeat basic_typing_vfavr_solver_slow.
Qed.

Lemma vfvar_cache_l21: forall Γ (x1 x2: atom) (T1 T2: ty) e T,
  Γ ⊢t e ⋮t T ->
  x1 ∉ stale Γ ->
  x2 ∉ stale Γ ∪ {[x1]} ->
  (Γ ++ [(x1, T1); (x2, T2)]) ⊢t x1 ⋮v T1.
Proof.
  intros. repeat basic_typing_vfavr_solver_slow.
Qed.

Ltac basic_typing_vfavr_solver :=
  op_simpl;
  match goal with
  | [H: ?Γ ⊢t _ ⋮t _ |- (?Γ ++ [(_, _); (_, _); (?x, _)]) ⊢t (vfvar ?x) ⋮v _ ] => eapply vfvar_cache_l33; eauto; listctx_set_solver
  | [H: ?Γ ⊢t _ ⋮t _ |- (?Γ ++ [(_, _); (?x, _)]) ⊢t (vfvar ?x) ⋮v _ ] => eapply vfvar_cache_l22; eauto; listctx_set_solver
  | [H: ?Γ ⊢t _ ⋮t _ |- (?Γ ++ [(?x, _); (_, _)]) ⊢t (vfvar ?x) ⋮v _ ] => eapply vfvar_cache_l21; eauto; listctx_set_solver
  | [ |- ok (?Γ ++ [(_, _); (_, _)]) ] => eapply ok_cache_l2; eauto; listctx_set_solver
  | [ |- ok (?Γ ++ [(_, _); (_, _); (_, _)]) ] => eapply ok_cache_l3; eauto; listctx_set_solver
  | [|- _ ∉ _ ] => listctx_set_solver
  | [|- ok _ ] => op_solver
  | [|- ctxfind _ _ = Some _ ] => repeat var_dec_solver
  | [|- [(_, _); (_, _)] ⊢t (vfvar ?x) ⋮v _ ] => constructor; basic_typing_vfavr_solver
  | [|- [(_, _); (_, _); (_, _)] ⊢t (vfvar ?x) ⋮v _ ] => constructor; basic_typing_vfavr_solver
  | [|- [(_, _)] ⊢t (vfvar ?x) ⋮v _ ] => constructor; basic_typing_vfavr_solver
  | [|- (_ ++ _) ⊢t (tvalue (vfvar ?x)) ⋮t _] => econstructor; eauto; basic_typing_vfavr_solver
  | [|- (_ ++ _) ⊢t (vfvar ?x) ⋮v _] =>
      apply basic_typing_weaken_value_post; eauto; basic_typing_vfavr_solver
  end.

(* Lemma basic_typing_subst_value_post: forall Γ1 z u U (v: value) T, *)
(*     ((z, U) :: Γ1) ⊢t v ⋮v T -> [] ⊢t u ⋮v U -> Γ1 ⊢t {z := u}v v ⋮v T. *)
(* Proof. *)
(*   intros. *)
(*   assert (([] ++ [(z, U)] ++ Γ1) ⊢t v ⋮v T) by listctx_set_simpl. *)
(*   apply (basic_typing_subst_value [] z u) in H1; listctx_set_simpl. *)
(* Qed. *)

(* Lemma basic_typing_subst_tm_post: forall Γ1 z u U (v: tm) T, *)
(*     ((z, U) :: Γ1) ⊢t v ⋮t T -> [] ⊢t u ⋮v U -> Γ1 ⊢t {z := u}t v ⋮t T. *)
(* Proof. *)
(*   intros. *)
(*   assert (([] ++ [(z, U)] ++ Γ1) ⊢t v ⋮t T) by listctx_set_simpl. *)
(*   apply (basic_typing_subst_tm [] z u) in H1; listctx_set_simpl. *)
(* Qed. *)

Ltac basic_typing_solver3 :=
  match goal with
  | [H: [] ⊢t ?v ⋮v _ |- closed_value ?v ] => apply basic_typing_contains_fv_value in H; basic_typing_solver2
  | [H: [] ⊢t ?e ⋮t _ |- closed_tm ?e ] => apply basic_typing_contains_fv_tm in H; basic_typing_solver2
  | [H: ((?z, ?U) :: ?Γ1) ⊢t ?v ⋮v ?T , H': [] ⊢t ?u ⋮v ?U |- ?Γ1 ⊢t {?z := ?u}v ?v ⋮v ?T] => eapply basic_typing_subst_value_post; eauto
  | [H: ((?z, ?U) :: ?Γ1) ⊢t ?v ⋮t ?T , H': [] ⊢t ?u ⋮v ?U |- ?Γ1 ⊢t {?z := ?u}t ?v ⋮t ?T] => eapply basic_typing_subst_tm_post; eauto
  | [H: [] ⊢t ?u ⋮v _ |- context [ fv_value ?u] ] =>
      assert (fv_value u = ∅) as Htmp by
        (apply basic_typing_contains_fv_value in H; set_solver);
      rewrite Htmp; auto; try clear Htmp; try fast_set_solver
  | [H: ?Γ ⊢t ?e ⋮t ?T, H': ?e ↪* (tvalue ?v) |- ?Γ ⊢t ?v ⋮v ?T ] =>
      eapply multi_preservation_value; eauto
  end || basic_typing_solver2.

Lemma closed_has_type_under_empty_value: forall Γ (v: value) T, Γ ⊢t v ⋮v T -> closed_value v -> [] ⊢t v ⋮v T.
Proof.
  apply (rev_ind (fun Γ => forall (v: value) T, Γ ⊢t v ⋮v T -> closed_value v -> [] ⊢t v ⋮v T)); intros; auto.
  auto_destruct_pair.
  destruct (tricky_closed_value_exists t) as (vv & Hvv).
  apply (basic_typing_subst_value_pre _ a vv) in H0. rewrite subst_fresh_value in H0. auto. my_set_solver.
  apply Hvv. op_simpl.
Qed.

Lemma closed_has_type_under_empty_tm: forall Γ (v: tm) T, Γ ⊢t v ⋮t T -> closed_tm v -> [] ⊢t v ⋮t T.
Proof.
  apply (rev_ind (fun Γ => forall (v: tm) T, Γ ⊢t v ⋮t T -> closed_tm v -> [] ⊢t v ⋮t T)); intros; auto.
  auto_destruct_pair.
  destruct (tricky_closed_value_exists t) as (vv & Hvv).
  apply (basic_typing_subst_tm_pre _ a vv) in H0. rewrite subst_fresh_tm in H0. auto. my_set_solver.
  apply Hvv. op_simpl.
Qed.

Lemma basic_has_type_swap_tm_aux: forall Γ e T,
    Γ ⊢t e ⋮t T -> (forall Γ1 Γ2 Γ3, Γ = Γ1 ++ Γ2 ++ Γ3 -> (Γ2 ++ Γ1 ++ Γ3) ⊢t e ⋮t T).
Proof.
 apply (tm_has_type_mutual_rec
           (fun Γ e T P => forall Γ1 Γ2 Γ3, Γ = Γ1 ++ Γ2 ++ Γ3 -> (Γ2 ++ Γ1 ++ Γ3) ⊢t e ⋮v T)
           (fun Γ e T P => forall Γ1 Γ2 Γ3, Γ = Γ1 ++ Γ2 ++ Γ3 -> (Γ2 ++ Γ1 ++ Γ3) ⊢t e ⋮t T)
        ); simpl; intros; subst; listctx_set_simpl.
 - constructor. listctx_set_solver5.
 - constructor. listctx_set_solver5.
   rewrite ctxfind_app in e; try listctx_set_solver5.
   rewrite ctxfind_app in e; try listctx_set_solver5.
   destruct_hyp_disj; rewrite ctxfind_app; try listctx_set_solver5.
   + right. rewrite ctxfind_app; listctx_set_solver5.
   + right. rewrite ctxfind_app; listctx_set_solver5.
 - auto_exists_L; intros.
   assert ((Γ2 ++ Γ1 ++ (Γ3 ++ [(x, Tx)])) ⊢t e ^t^ x ⋮t T).
   { apply H; listctx_set_solver5. }
   listctx_set_solver5.
 - auto_exists_L; intros.
   assert ((Γ2 ++ Γ1 ++ (Γ3 ++ [(f, Tx ⤍ T)])) ⊢t vlam Tx e ^v^ f ⋮v Tx ⤍ T).
   { apply H; listctx_set_solver5. }
   listctx_set_solver5.
 - constructor. listctx_set_solver5.
 - auto_exists_L; intros.
   assert ((Γ2 ++ Γ1 ++ (Γ3 ++ [(x, T1)])) ⊢t e2 ^t^ x ⋮t T2).
   { apply H0; listctx_set_solver5. }
   listctx_set_solver5.
 - auto_exists_L; intros.
   assert ((Γ2 ++ Γ1 ++ (Γ3 ++ [(x, TBase Tx)])) ⊢t e ^t^ x ⋮t T).
   { apply H1; listctx_set_solver5. }
   listctx_set_solver5.
 - auto_exists_L; intros.
   assert ((Γ2 ++ Γ1 ++ (Γ3 ++ [(x, Tx)])) ⊢t e ^t^ x ⋮t T).
   { apply H1; listctx_set_solver5. }
   listctx_set_solver5.
Qed.

Lemma basic_has_type_swap_tm: forall Γ1 Γ2 Γ3 e T,
    (Γ1 ++ Γ2 ++ Γ3) ⊢t e ⋮t T <-> (Γ2 ++ Γ1 ++ Γ3) ⊢t e ⋮t T.
Proof.
  split; intros; eapply basic_has_type_swap_tm_aux in H; eauto.
Qed.

Lemma basic_has_type_swap_value_aux: forall Γ e T,
    Γ ⊢t e ⋮v T -> (forall Γ1 Γ2 Γ3, Γ = Γ1 ++ Γ2 ++ Γ3 -> (Γ2 ++ Γ1 ++ Γ3) ⊢t e ⋮v T).
Proof.
 apply (value_has_type_mutual_rec
           (fun Γ e T P => forall Γ1 Γ2 Γ3, Γ = Γ1 ++ Γ2 ++ Γ3 -> (Γ2 ++ Γ1 ++ Γ3) ⊢t e ⋮v T)
           (fun Γ e T P => forall Γ1 Γ2 Γ3, Γ = Γ1 ++ Γ2 ++ Γ3 -> (Γ2 ++ Γ1 ++ Γ3) ⊢t e ⋮t T)
        ); simpl; intros; subst; listctx_set_simpl.
 - constructor. listctx_set_solver5.
 - constructor. listctx_set_solver5.
   rewrite ctxfind_app in e; try listctx_set_solver5.
   rewrite ctxfind_app in e; try listctx_set_solver5.
   destruct_hyp_disj; rewrite ctxfind_app; try listctx_set_solver5.
   + right. rewrite ctxfind_app; listctx_set_solver5.
   + right. rewrite ctxfind_app; listctx_set_solver5.
 - auto_exists_L; intros.
   assert ((Γ2 ++ Γ1 ++ (Γ3 ++ [(x, Tx)])) ⊢t e ^t^ x ⋮t T).
   { apply H; listctx_set_solver5. }
   listctx_set_solver5.
 - auto_exists_L; intros.
   assert ((Γ2 ++ Γ1 ++ (Γ3 ++ [(f, Tx ⤍ T)])) ⊢t vlam Tx e ^v^ f ⋮v Tx ⤍ T).
   { apply H; listctx_set_solver5. }
   listctx_set_solver5.
 - constructor. listctx_set_solver5.
 - auto_exists_L; intros.
   assert ((Γ2 ++ Γ1 ++ (Γ3 ++ [(x, T1)])) ⊢t e2 ^t^ x ⋮t T2).
   { apply H0; listctx_set_solver5. }
   listctx_set_solver5.
 - auto_exists_L; intros.
   assert ((Γ2 ++ Γ1 ++ (Γ3 ++ [(x, TBase Tx)])) ⊢t e ^t^ x ⋮t T).
   { apply H1; listctx_set_solver5. }
   listctx_set_solver5.
 - auto_exists_L; intros.
   assert ((Γ2 ++ Γ1 ++ (Γ3 ++ [(x, Tx)])) ⊢t e ^t^ x ⋮t T).
   { apply H1; listctx_set_solver5. }
   listctx_set_solver5.
Qed.

Lemma basic_has_type_swap_value: forall Γ1 Γ2 Γ3 e T,
    (Γ1 ++ Γ2 ++ Γ3) ⊢t e ⋮v T <-> (Γ2 ++ Γ1 ++ Γ3) ⊢t e ⋮v T.
Proof.
  split; intros; eapply basic_has_type_swap_value_aux in H; eauto.
Qed.

Lemma basic_has_type_swap2_tm: forall Γ1 Γ2 e T,
    (Γ1 ++ Γ2) ⊢t e ⋮t T <-> (Γ2 ++ Γ1) ⊢t e ⋮t T.
Proof.
  intros.
  assert ((Γ1 ++ Γ2 ++ []) ⊢t e ⋮t T <-> (Γ2 ++ Γ1 ++ []) ⊢t e ⋮t T).
  eapply basic_has_type_swap_tm. listctx_set_simpl.
Qed.

Lemma basic_has_type_swap2_value: forall Γ1 Γ2 e T,
    (Γ1 ++ Γ2) ⊢t e ⋮v T <-> (Γ2 ++ Γ1) ⊢t e ⋮v T.
Proof.
  intros.
  assert ((Γ1 ++ Γ2 ++ []) ⊢t e ⋮v T <-> (Γ2 ++ Γ1 ++ []) ⊢t e ⋮v T).
  eapply basic_has_type_swap_value. listctx_set_simpl.
Qed.

Lemma basic_has_type_head_to_tail_tm: forall x Tx Γ e T, ((x, Tx) :: Γ) ⊢t e ⋮t T <-> (Γ ++ [(x, Tx)]) ⊢t e ⋮t T.
Proof.
  split; intros.
  - assert (([(x, Tx)] ++ Γ ++ []) ⊢t e ⋮t T) by listctx_set_simpl.
    rewrite basic_has_type_swap_tm in H0. listctx_set_simpl.
  - assert ((Γ ++ [(x, Tx)] ++ []) ⊢t e ⋮t T) by listctx_set_simpl.
    rewrite basic_has_type_swap_tm in H0. listctx_set_simpl.
Qed.

Lemma basic_has_type_head_to_tail_value: forall x Tx Γ e T, ((x, Tx) :: Γ) ⊢t e ⋮v T <-> (Γ ++ [(x, Tx)]) ⊢t e ⋮v T.
Proof.
  split; intros.
  - assert (([(x, Tx)] ++ Γ ++ []) ⊢t e ⋮v T) by listctx_set_simpl.
    rewrite basic_has_type_swap_value in H0. listctx_set_simpl.
  - assert ((Γ ++ [(x, Tx)] ++ []) ⊢t e ⋮v T) by listctx_set_simpl.
    rewrite basic_has_type_swap_value in H0. listctx_set_simpl.
Qed.

Lemma basic_has_type_renaming: forall e γ x Tx x0 T,
    x <> x0 ->
    (γ ++ [(x, Tx)]) ⊢t e ⋮t T -> ok (γ ++ [(x0, Tx)]) ->
    (γ ++ [(x0, Tx)]) ⊢t {x := x0 }t e ⋮t T.
Proof.
  intros.
  assert (([(x0, Tx)] ++ (γ ++ [(x, Tx)])) ⊢t e ⋮t T).
  { apply basic_typing_weaken_tm_post; auto.
  listctx_set_simpl. rewrite ok_pre_destruct. split; basic_typing_solver3. }
  assert ((((x0, Tx) :: γ) ++ [(x, Tx)]) ⊢t e ⋮t T) by listctx_set_simpl.
  apply basic_typing_subst_tm with (u := x0) in H3; auto.
  - listctx_set_simpl. rewrite <- basic_has_type_head_to_tail_tm; auto.
  - econstructor; eauto; basic_typing_solver3. repeat var_dec_solver.
Qed.


Ltac basic_typing_solver4 :=
  match goal with
  | [H: (?Γ1 ++ ?Γ2) ⊢t ?e ⋮t ?T |- (?Γ2 ++ ?Γ1) ⊢t ?e ⋮t ?T ] => rewrite basic_has_type_swap2_tm; auto
  | [H: (?Γ1 ++ ?Γ2) ⊢t ?e ⋮v ?T |- (?Γ2 ++ ?Γ1) ⊢t ?e ⋮v ?T ] => rewrite basic_has_type_swap2_value; auto
  end || basic_typing_solver3.

Ltac basic_typing_solver5 :=
  repeat (simpl; (basic_typing_solver4 ||
                    match goal with
                    | [H: _ ⊢t (tvalue _) ⋮t _  |- _ ] => invclear H; eauto
                    | [H: ?Γ ⊢t ?e ⋮t _, H': ?z ∉ ctxdom ?Γ  |- ?z ∉ fv_tm ?e ] =>
                        apply basic_typing_contains_fv_tm in H; simpl in H; fast_set_solver
                    | [H: ?Γ ⊢t ?e ⋮v _, H': ?z ∉ ctxdom ?Γ  |- ?z ∉ fv_value ?e ] =>
                        apply basic_typing_contains_fv_value in H; fast_set_solver
                    | [H: ?Γ ⊢t tlete ?u _ ⋮t _ |- ?Γ ⊢t ?u ⋮t _ ] => invclear H; eauto
                    | [H: ?Γ ⊢t tlete (tvalue ?u) _ ⋮t _ |- ?Γ ⊢t ?u ⋮v _ ] => invclear H; eauto
                    end)).

Lemma vlam_tyable_dummy: forall Γ e Tx T,
  Γ ⊢t e ⋮t T -> Γ ⊢t vlam Tx e ⋮v Tx ⤍ T.
Proof.
  intros. auto_exists_L; intros.
  rewrite open_rec_lc_tm; basic_typing_solver5.
Qed.

Lemma vlam_implies_open_tyable: forall Γ e1 v2 Tx T,
  Γ ⊢t v2 ⋮v Tx -> Γ ⊢t vlam Tx e1 ⋮v Tx ⤍ T -> Γ ⊢t e1 ^t^ v2 ⋮t T.
Proof.
  intros. invclear H0. auto_pose_fv x. repeat specialize_with x.
  eapply basic_typing_subst_tm_pre in H3; eauto.
  rewrite subst_open_tm in H3; basic_typing_solver5.
  simpl in H3. var_dec_solver.
  rewrite subst_fresh_tm in H3; basic_typing_solver5.
Qed.

Ltac basic_typing_solver6 :=
  repeat (basic_typing_solver5 ||
            (match goal with
             | [H: ?Γ ⊢t vlam ?Tx ?e1 ⋮v ?Tx ⤍ ?T |- ?Γ ⊢t ?e1 ^t^ ?v2 ⋮t ?T] =>
                 apply vlam_implies_open_tyable with (Tx := Tx); eauto
             | [ |- _ ⊢t tvalue _ ⋮t _ ⤍ _ ] => constructor; auto
             | [ |- _ ⊢t vlam _ _ ⋮v _ ⤍ _ ] => apply vlam_tyable_dummy; eauto

             | [H: _ ⊢t (tvalue ?v) ⋮t _ |- _ ⊢t ?v ⋮v _ ] => invclear H; eauto
             end)).

Lemma cons_basic_typing_drop_last_tm: forall x Tx Γ1 Γ2 e T,
    ok (((x, Tx) :: Γ1) ++ Γ2) ->
    ((x, Tx) :: Γ1) ⊢t e ⋮t T ->
    ((x, Tx) :: Γ1 ++ Γ2) ⊢t e ⋮t T.
Proof.
  intros. rewrite app_comm_cons.
  apply basic_typing_weaken_tm_pre; auto.
Qed.

Lemma cons_basic_typing_drop_last_value: forall x Tx Γ1 Γ2 e T,
    ok (((x, Tx) :: Γ1) ++ Γ2) ->
    ((x, Tx) :: Γ1) ⊢t e ⋮v T ->
    ((x, Tx) :: Γ1 ++ Γ2) ⊢t e ⋮v T.
Proof.
  intros. rewrite app_comm_cons.
  apply basic_typing_weaken_value_pre; auto.
Qed.

Ltac basic_typing_solver7 :=
  match goal with
  | [H: _ ⊢t ?e ⋮t _ |- _ ∉ fv_tm ?e] =>
      apply basic_typing_contains_fv_tm in H; simpl in H
  | [H: ((?x, ?Tx) :: ?Γ1) ⊢t ?e ⋮t ?T |- ((?x, ?Tx) :: ?Γ1 ++ _) ⊢t ?e ⋮t ?T ] =>
      eapply cons_basic_typing_drop_last_tm; eauto
  | [H: ((?x, ?Tx) :: ?Γ1) ⊢t ?e ⋮v ?T |- ((?x, ?Tx) :: ?Γ1 ++ _) ⊢t ?e ⋮v ?T ] =>
      eapply cons_basic_typing_drop_last_value; eauto
  end || basic_typing_solver6.

Ltac basic_typing_solver := basic_typing_solver7.

Ltac lc_simpl :=
  simpl;
  repeat match goal with
    | [H: context [(?e ^t^ ?x)] |- _ ] =>
        simpl;
        assert (lc e) as Htmp by (auto; (lc_solver || fast_set_solver || basic_typing_solver))
        ;rewrite open_rec_lc_tm in H; auto; try clear Htmp
    | [ |- context [(?e ^t^ ?x)] ] =>
        simpl;
        assert (lc e) as Htmp by (auto; (lc_solver || fast_set_solver || basic_typing_solver))
        ;rewrite open_rec_lc_tm; auto; try clear Htmp
    | [H: context [(?x \t\ ?e)] |- _ ] =>
        simpl;
        assert (x # e) as Htmp by (auto; (lc_solver || fast_set_solver || basic_typing_solver))
        ;rewrite close_fresh_rec_tm in H; auto; try clear Htmp
    | [ |- context [(?x \t\ ?e)] ] =>
        simpl;
        assert (x # e) as Htmp by (auto; (lc_solver || fast_set_solver || basic_typing_solver))
        ;rewrite close_fresh_rec_tm; auto; try clear Htmp
    end.
