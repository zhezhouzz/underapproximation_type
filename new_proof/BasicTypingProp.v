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
        ); simpl; intros; subst; econstructor; auto;
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
        ); simpl; intros; subst; econstructor; auto;
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

(* Lemma basic_typing_subst_value_post *)

Lemma eval_op_type_safe: forall op c1 c2 c3 (T1 T2 T3: base_ty),
  eval_op op c1 c2 c3 ->
  ty_of_op op = T1 ⤍ T2 ⤍ T3 ->
  ty_of_const c1 = T1 /\ ty_of_const c2 = T2 /\ ty_of_const c3 = T3.
Proof.
  repeat split; intros; destruct op; inversion H; subst; rewrite op_ty_spec in H0; inversion H0; subst; auto.
Qed.

Lemma eval_op_preservation_aux: forall Γ op c1 c2 c3 (T1 T2 T3: base_ty),
    eval_op op c1 c2 c3 ->
    ty_of_op op = T1 ⤍ T2 ⤍ T3 ->
    ok Γ ->
    Γ ⊢t c1 ⋮v T1 /\ Γ ⊢t c2 ⋮v T2 /\ Γ ⊢t c3 ⋮v T3.
Proof.
  intros.
  assert (ty_of_const c1 = T1 /\ ty_of_const c2 = T2 /\ ty_of_const c3 = T3) by (eapply eval_op_type_safe; eauto).
  repeat destruct_hyp_conj; subst. repeat split; auto.
Qed.

Lemma eval_op_preservation: forall Γ op c1 c2 c3 (T1 T2 T3: base_ty),
    eval_op op c1 c2 c3 ->
    Γ ⊢t op ⋮v T1 ⤍ T2 ⤍ T3 ->
    Γ ⊢t c1 ⋮v T1 /\ Γ ⊢t c2 ⋮v T2 /\ Γ ⊢t c3 ⋮v T3.
Proof.
  intros.
  inversion H0; subst.
  eapply eval_op_preservation_aux; eauto.
Qed.

Ltac basic_typing_simpl1 :=
  step_regular_simp;
  basic_typing_regular_simp;
  listctx_set_simpl;
  repeat match goal with
    | [H: ?Γ ⊢t (tvalue ?v) ⋮t ?T |- _ ] => inversion H; subst; clear H; auto
    end.


Ltac basic_typing_solver2 :=
  basic_typing_simpl1;
  try match goal with
    | [H: ?Γ ⊢t (vbiop _) ⋮v _ ⤍ _ ⤍ ?T |- ?Γ ⊢t (vconst _) ⋮v ?T ] => eapply eval_op_preservation; eauto
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
