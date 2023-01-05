From stdpp Require Import mapset.
From stdpp Require Import natmap.
From stdpp Require Import fin_map_dom.
From CT Require Import CoreLangProp.
From CT Require Import OperationalSemanticsProp.
From CT Require Import BasicTypingProp.
From CT Require Import SyntaxSugar.
From CT Require Import Refinement.
From CT Require Import RefinementTac.
From CT Require Import RefinementDenotation.
From CT Require Import TermOrdering.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Classical.
From CT Require Import RefinementDenotationTac.

Import Atom.
Import CoreLang.
Import Tactics.
Import NamelessTactics.
Import ListCtx.
Import OperationalSemantics.
Import OperationalSemanticsProp.
Import BasicTyping.
Import SyntaxSugar.
Import Refinement.
Import RefinementTac.
Import RefinementDenotation.
Import TermOrdering.

Fixpoint ctxdom_base (Γ: listctx rty) : aset :=
  match Γ with
  | [] => ∅
  | (x, {v: _ | _ | _ | _ }) :: Γ => {[x]} ∪ ctxdom_base Γ
  | (x, [v: _ | _ | _ | _ ]) :: Γ => {[x]} ∪ ctxdom_base Γ
  | (x, _) :: Γ => ctxdom_base Γ
  end.

Lemma ctxrR_empty_iff_rR: forall st τ e, ({st}⟦τ⟧{ [] }) e <-> {st}⟦τ⟧ e.
Proof.
  split; intros; auto.
  - invclear H; auto.
  - constructor; auto.
Qed.

Lemma not_ctxrR_empty_iff_not_rR: forall st τ e, ~ ({st}⟦τ⟧{ [] }) e <-> ~ {st}⟦τ⟧ e.
Proof.
  split; intros; auto.
  - intros HF. apply H. constructor; auto.
  - intros HF. apply H. invclear HF; auto.
Qed.

Definition base_type_max_term (b: base_ty) :=
  match b with
  | TNat => nat-gen
  | TBool => bool-gen
  end.

Lemma base_type_max_term_typable: forall b, [] ⊢t base_type_max_term b ⋮t b.
Proof.
  destruct b; simpl; eauto.
Qed.

Global Hint Resolve base_type_max_term_typable: core.

Lemma base_type_max_term_spec: forall (b: base_ty) (c: constant), [] ⊢t c ⋮v b -> base_type_max_term b ↪* c.
Proof.
  destruct b; simpl; intros; eauto; destruct c; invclear H; subst; eauto.
Qed.

Global Hint Resolve base_type_max_term_spec: core.

Lemma base_type_reduce_to_constant: forall e (b: base_ty) (v: value), [] ⊢t e ⋮t b -> e ↪* v -> (∃ c, v = vconst c).
Proof.
  intros.
  eapply multi_preservation in H; eauto. denotation_simp. eapply empty_basic_typing_base_const_exists; eauto.
Qed.

Lemma base_type_max_term_reduce_to_constant: forall b (v: value), base_type_max_term b ↪* v -> (∃ c, v = vconst c).
Proof.
  intros. assert ([] ⊢t base_type_max_term b ⋮t b); eauto.
  eapply multi_preservation in H; eauto. denotation_simp. eapply empty_basic_typing_base_const_exists; eauto.
Qed.

Global Hint Resolve base_type_max_term_reduce_to_constant: core.

Ltac denotation_simp1 :=
  repeat (denotation_simp;
   mydestr;
   repeat match goal with
   | [H: ¬ is_arr _ |- _ ] => apply not_is_arr in H; destruct H; mydestr; subst
   | [H: ¬ not_underbasety _ |- _ ] => apply not_not_underbasety in H; mydestr; subst
   | [H: ¬ ({_}⟦ _ ⟧{ [] }) _ |- _ ] => rewrite not_ctxrR_empty_iff_not_rR in H
   | [ |- ¬ ({_}⟦ _ ⟧{ [] }) _ ] => rewrite not_ctxrR_empty_iff_not_rR
   (* | [H: {_}⟦ _ ⟧{ [] } _ |- _ ] => invclear H *)
   | [H: closed_rty 0 _ [v:_|?n|_|_] |- _ ] =>
       match n with
       | 0 => fail 1
       | _ => assert (n = 0) by (apply closed_rty_0_underbase in H; auto); subst
       end
   | [H: closed_rty 0 _ {v:_|?n|_|_} |- _ ] =>
       match n with
       | 0 => fail 1
       | _ => assert (n = 0) by (apply closed_rty_0_overbase in H; auto); subst
       end
   | [H: ?e ↪* (tvalue ?v), H': [] ⊢t ?e ⋮t (TBase _)  |- _ ] =>
       match v with
       | vconst _ => fail 1
       | _ => assert (exists c, v = vconst c) as Htmp by (eapply base_type_reduce_to_constant in H; eauto);
             destruct Htmp; subst
       end
   | [H: base_type_max_term _ ↪* (tvalue ?v) |- _ ] =>
       match v with
       | vconst _ => fail 1
       | _ => assert (exists c, v = vconst c) as Htmp by (eapply base_type_max_term_reduce_to_constant in H; eauto);
             destruct Htmp; subst
       end
   end).

Lemma ok_dctx_post_destruct: forall Γ d x τ,
    ok_dctx d (Γ ++ [(x, τ)]) <->
      ok_dctx d Γ /\ x ∉ d /\ x ∉ ctxdom Γ /\ closed_rty 0 (ctxdom_base Γ ∪ d) τ.
Proof.
  induction Γ; simpl; split; intros; mydestr.
  - invclear H; do 3 (constructor; auto); closed_rty_solver.
  - destruct (classic (is_arr τ)).
    + apply ok_dctx_cons_arr; auto; closed_rty_solver.
    + apply ok_dctx_cons_base; auto; closed_rty_solver. constructor.
  - invclear H.
    + apply IHΓ in H8; mydestr. do 3 (constructor; auto; try listctx_set_solver).
      denotation_simp1; closed_rty_solver.
    + apply IHΓ in H8; mydestr. do 3 (constructor; auto; try listctx_set_solver).
      destruct r; invclear H3; closed_rty_solver.
  - invclear H.
    + apply ok_dctx_cons_base; auto; try listctx_set_solver.
      rewrite IHΓ. do 3 (split; auto; try fast_set_solver).
      denotation_simp1; closed_rty_solver.
    + apply ok_dctx_cons_arr; auto; try listctx_set_solver.
      rewrite IHΓ. do 3 (split; auto; try fast_set_solver).
      destruct r; invclear H6; closed_rty_solver.
Qed.

Ltac listctx_set_simpl4 :=
  match goal with
  | [H: _ ++ [_] = [_] |- _ ] => apply elt_eq_unit in H; mydestr; subst
  | [H: _ ++ [_] = _ :: _ ++ [_] |- _ ] => rewrite app_comm_cons in H; listctx_set_simpl
  end; listctx_set_simpl.


Lemma terr_inhabitant_iff_false_phi': forall n1 n2 bst st (d: aset) b (ϕ: refinement),
    {n1; bst; st}⟦ [v: b | n2 | d | ϕ ] ⟧ terr <->
      (closed_rty n1 (dom aset st) [v:b|n2|d|ϕ] /\ forall c: constant, [] ⊢t c ⋮v b → ~ ϕ bst st c).
Proof.
  split; intros.
  - invclear H. mydestr; subst. rewrite terr_reduction_iff_false_phi in H1; auto.
  - mydestr. constructor; auto. split; auto. rewrite terr_reduction_iff_false_phi; auto.
Qed.

Ltac ok_dctx_solver2 :=
  match goal with
      | [H: ok_dctx ?d ((?a, ?b) :: ?l ++ [_]) |- ok_dctx ?d ((?a, ?b) :: ?l)] =>
          rewrite app_comm_cons in H; rewrite ok_dctx_post_destruct in H; mydestr; auto
  end || ok_dctx_solver.

Lemma ok_dctx_single_implies_closed_rty: forall d x τ, ok_dctx d [(x, τ)] -> closed_rty 0 d τ.
Proof.
  intros. invclear H; auto.
Qed.

Ltac closed_rty_solver2 :=
  match goal with
      | [H: ok_dctx ?d [(_, ?τ)] |- closed_rty 0 ?d ?τ ] => apply ok_dctx_single_implies_closed_rty in H; auto
  end || closed_rty_solver.

Lemma exists_forall_aux1: forall st b (ϕ: refinement),
    (∀ c : constant, [] ⊢t c ⋮v b → ¬ ϕ b∅ st c) <->
      ~ (exists c : constant, [] ⊢t c ⋮v b /\ ϕ b∅ st c).
Proof.
  split; intros.
  - intro HF; mydestr. eapply H; eauto.
  - intro HF. apply H. eexists; split; eauto.
Qed.

Ltac pbc :=
  match goal with
  | [ |- ?ret] => destruct (classic ret); auto; exfalso
  end.

Lemma exists_forall_aux2 {A: Type}: forall P Q, ~ (∃ (a: A), P a /\ Q a) <-> (∀ (a: A), ~ P a \/ ~ Q a).
Proof.
  split; intros.
  - destruct (classic (P a)); destruct (classic (Q a)); auto.
    exfalso. apply H. exists a. split; auto.
  - intro Hf. mydestr. destruct (H x).
    + apply H2; auto.
    + apply H2; auto.
Qed.

Lemma nexists {A: Type}: forall P, ~ (∃ (a: A), P a) <-> (forall (a: A), ~ P a).
Proof.
  split; intros.
  - intro Hf. apply H; eexists; eauto.
  - intro Hf; mydestr. eapply H; eauto.
Qed.

Lemma nand: forall P Q, ~ (P /\ Q) <-> (~ P \/ ~ Q).
Proof.
  split; intros.
  - destruct (classic P); destruct (classic Q); auto.
  - intro Hf; mydestr. destruct H; apply H; auto.
Qed.

Lemma nor_to_implies: forall P Q, (~ P \/ Q) <-> (P -> Q).
Proof.
  split; intros.
  - destruct H; auto. exfalso; apply H; auto.
  - destruct (classic P); auto.
Qed.

Lemma nneg: forall P, ~ ~ P <-> P.
Proof.
  split; intros.
  - destruct (classic P); auto. exfalso. apply H; auto.
  - intro Hf. apply Hf; auto.
Qed.

Ltac neg_simpl :=
  repeat match goal with
  | [H: ~ (exists _, _) |- _ ] => setoid_rewrite nexists in H
  | [H: context [ ~ (_ /\ _)] |- _ ] => setoid_rewrite nand in H
  | [H: context [ ~ ~ _ ] |- _ ] => setoid_rewrite nneg in H
  | [H: context [ ~ _ \/ _ ]  |- _ ] => setoid_rewrite nor_to_implies in H
  end.

Ltac refinement_solver2 :=
  repeat (
      try auto_reduction_exfalso;
      match goal with
      | [H: {?st}⟦ ?τ ⟧{ [] } ?e |- {?st}⟦ ?τ ⟧ ?e ] => invclear H
      | [H: closed_rty _ _ ?τ |- valid_rty ?τ] => destruct H; auto
      | [ |- _ ⊢t terr ⋮t _ ] => constructor; auto
      | [H: ok_dctx ?d ((_, ?τ) :: _) |- closed_rty 0 ?d ?τ ] => invclear H; auto
      | [H: ok_dctx _ _ |- ok _ ] => apply ok_dctx_regular1 in H; mydestr
      | [H: ok (_ :: ?Γ) |- ok ⌊?Γ⌋* ] => rewrite ok_pre_destruct in H; mydestr; ctx_erase_simp
      | [ |- context [ ⌊ _ ⌋* ] ] => ctx_erase_simp; fast_set_solver
      | [H: ?e ↪* (tvalue ?v), H': [] ⊢t ?e ⋮t ?T |- [] ⊢t ?v ⋮v ?T ] =>
          eapply multi_preservation_value in H'; eauto
      | [H: [] ⊢t ?e ⋮t ?T |- _ ⊢t ?e ⋮t _ ] => apply basic_typing_weaken_tm_empty; eauto
      end || refinement_solver1).

Lemma termR_perserve_ctxrR: forall Γ τ (e e': tm),
    not_overbasety τ ->
    valid_rty τ ->
    e <-<{ ⌊ Γ ⌋* ;  ⌊ τ ⌋ } e' -> (forall st, {st}⟦τ⟧{Γ} e -> {st}⟦τ⟧{Γ} e').
Proof.
  induction Γ; intros; invclear H2.
  - constructor. intros. eapply termR_perserve_rR; eauto.
  - constructor; auto. termR_solver.
    intros. apply IHΓ with (e:= (tlete c_x (x \t\ e))); eauto.
    apply termR_elete with (Tx := B); auto. constructor; auto; refinement_solver1; basic_typing_solver2.
  - constructor; auto. termR_solver.
    destruct H12 as (e_x_hat & He_x_hat & HH). exists e_x_hat. split; auto.
    intros. apply IHΓ with (e:= (tlete e_x (x \t\ e))); eauto.
    apply termR_elete with (Tx := ⌊ τ_x ⌋ ); auto. constructor; auto; refinement_solver1.
Qed.

Lemma tyable_implies_terr_termR: forall (e: tm) Γ T, Γ ⊢t e ⋮t T -> terr <-<{ Γ; T} e.
Proof.
  intros. repeat split; auto.
  - constructor; basic_typing_solver2.
  - intros. auto_reduction_exfalso.
Qed.

Lemma tlete_terr_not_reduce_to_value: forall e (v: value), ~ tlete e terr ↪* v.
Proof.
  intros. intro Hf. rewrite lete_step_spec in Hf; mydestr. simpl in H1. auto_reduction_exfalso.
Qed.

Ltac auto_reduction_exfalso2 :=
  match goal with
  | [H: tlete _ terr ↪* (tvalue _) |- _ ] => apply tlete_terr_not_reduce_to_value in H; invclear H
  end || auto_reduction_exfalso.

Lemma tyable_implies_terr_termR_terr: forall (e: tm) Γ T1 T, Γ ⊢t e ⋮t T1 -> tlete e terr <-<{ Γ; T} terr.
Proof.
  intros. repeat split; auto.
  - auto_exists_L. intros. simpl. constructor. basic_typing_solver2.
  - econstructor; basic_typing_solver2.
  - intros. auto_reduction_exfalso2.
Qed.

Lemma terr_is_not_inhabitant_of_overbase: forall τ n bst st,
    {n; bst; st}⟦τ⟧ terr -> not_overbasety τ.
Proof.
  destruct τ; intros; invclear H; mydestr; simpl; auto.
  invclear H3.
Qed.

Lemma ctxrR_err_overbase_forall_forward: forall Γ st τ x b n d ϕ,
    ¬ ({st}⟦τ⟧{(x, {v:b|n|d|ϕ}) :: Γ}) terr ->
    ok_dctx (dom aset st) ((x, {v:b|n|d|ϕ}) :: Γ) ->
    closed_rty 0 ({[x]} ∪ ctxdom Γ ∪ dom aset st) τ ->
    not_overbasety τ ->
    (exists (c: constant), {st}⟦{v:b|n|d|ϕ}⟧ c /\ ¬ ({<[x:=c]> st}⟦τ⟧{Γ}) terr).
Proof.
  intros. pbc. apply H. clear H. neg_simpl.
  constructor; auto; refinement_solver2.
  intros.
  assert (({<[x:=c_x]> st}⟦τ⟧{Γ}) terr); auto.
  apply termR_perserve_ctxrR with (e:= terr); auto; refinement_solver2.
  apply tyable_implies_terr_termR.
  econstructor; eauto; refinement_solver2; mydestr.
  apply basic_typing_weaken_value_empty; eauto. refinement_solver2.
  auto_exists_L_intros. do 2 constructor; refinement_solver2.
Qed.

Lemma ctxrR_err_overbase_forall_backward: forall Γ st τ x b n d ϕ,
    ok_dctx (dom aset st) ((x, {v:b|n|d|ϕ}) :: Γ) ->
    closed_rty 0 ({[x]} ∪ ctxdom Γ ∪ dom aset st) τ ->
    not_overbasety τ ->
    (exists (c: constant), {st}⟦{v:b|n|d|ϕ}⟧ c /\ ¬ ({<[x:=c]> st}⟦τ⟧{Γ}) terr) ->
    ¬ ({st}⟦τ⟧{(x, {v:b|n|d|ϕ}) :: Γ}) terr.
Proof.
  intros. mydestr. intro Hf. apply H3. clear H3.
  invclear Hf; try invclear H6.
  apply termR_perserve_ctxrR with (e:= (tlete x0 (x \t\ terr))); auto; refinement_solver2.
  eapply tyable_implies_terr_termR_terr; refinement_solver2.
  apply basic_typing_weaken_value_empty; eauto. refinement_solver2.
Qed.

Lemma ctxrR_err_overbase_forall: forall Γ st τ x b n d ϕ,
    ok_dctx (dom aset st) ((x, {v:b|n|d|ϕ}) :: Γ) ->
    closed_rty 0 ({[x]} ∪ ctxdom Γ ∪ dom aset st) τ ->
    not_overbasety τ ->
    ¬ ({st}⟦τ⟧{(x, {v:b|n|d|ϕ}) :: Γ}) terr <->
    (exists (c: constant), {st}⟦{v:b|n|d|ϕ}⟧ c /\ ¬ ({<[x:=c]> st}⟦τ⟧{Γ}) terr).
Proof.
  split; intros.
  - eapply ctxrR_err_overbase_forall_forward; eauto.
  - eapply ctxrR_err_overbase_forall_backward; eauto.
Qed.

Lemma ctxrR_err_underbase_forall_forward: forall Γ st τ x b n d ϕ,
    ¬ ({st}⟦τ⟧{(x, [v:b|n|d|ϕ]) :: Γ}) terr ->
    ok_dctx (dom aset st) ((x, [v:b|n|d|ϕ]) :: Γ) ->
    closed_rty 0 ({[x]} ∪ ctxdom Γ ∪ dom aset st) τ ->
    not_overbasety τ ->
    (forall e_x_hat, {st}⟦[v:b|n|d|ϕ]⟧ e_x_hat ->
                (exists (c: constant), [] ⊢t c ⋮v b /\ e_x_hat ↪* c /\ ¬ ({<[x:=c]> st}⟦τ⟧{Γ}) terr)
    ).
Proof.
  intros. pbc. apply H. clear H. neg_simpl.
  constructor; auto; refinement_solver2.
  exists (e_x_hat). split; auto. intros. denotation_simp1.
  apply termR_perserve_ctxrR with (e:= terr); auto; refinement_solver2.
  - apply tyable_implies_terr_termR. econstructor; eauto; refinement_solver2; mydestr.
    auto_exists_L_intros. do 2 constructor; refinement_solver2.
  - apply H4; auto; refinement_solver2.
Qed.

Lemma ctxrR_err_underbase_forall_backward: forall Γ st τ x b n d ϕ,
    ok_dctx (dom aset st) ((x, [v:b|n|d|ϕ]) :: Γ) ->
    closed_rty 0 ({[x]} ∪ ctxdom Γ ∪ dom aset st) τ ->
    not_overbasety τ ->
    (forall e_x_hat, {st}⟦[v:b|n|d|ϕ]⟧ e_x_hat ->
                (exists (c: constant), [] ⊢t c ⋮v b /\ e_x_hat ↪* c /\ ¬ ({<[x:=c]> st}⟦τ⟧{Γ}) terr)
    ) ->
    ¬ ({st}⟦τ⟧{(x, [v:b|n|d|ϕ]) :: Γ}) terr.
Proof.
  intros. intro Hf. invclear Hf; mydestr.
  assert ([] ⊢t x0 ⋮t b) by refinement_solver2.
  assert (∃ c : constant, [] ⊢t c ⋮v b ∧ x0 ↪* c ∧ ¬ ({<[x:=c]> st}⟦τ⟧{Γ}) terr); auto.
  mydestr. apply H11. clear H11. eapply H4 in H3; eauto.
  apply termR_perserve_ctxrR with (e:= (tlete x0 (x \t\ terr))); auto; refinement_solver2; simpl.
  eapply tyable_implies_terr_termR_terr; refinement_solver2.
Qed.

Lemma ctxrR_err_underbase_forall: forall Γ st τ x b n d ϕ,
    ok_dctx (dom aset st) ((x, [v:b|n|d|ϕ]) :: Γ) ->
    closed_rty 0 ({[x]} ∪ ctxdom Γ ∪ dom aset st) τ ->
    not_overbasety τ ->
    ¬ ({st}⟦τ⟧{(x, [v:b|n|d|ϕ]) :: Γ}) terr <->
    (forall e_x_hat, {st}⟦[v:b|n|d|ϕ]⟧ e_x_hat ->
                (exists (c: constant), [] ⊢t c ⋮v b /\ e_x_hat ↪* c /\ ¬ ({<[x:=c]> st}⟦τ⟧{Γ}) terr)
    ).
Proof.
  split; intros.
  - eapply ctxrR_err_underbase_forall_forward; eauto.
  - eapply ctxrR_err_underbase_forall_backward; eauto.
Qed.

Lemma terr_inhabitant_implies_halt: forall τ n bst st,
    not_overbasety τ -> ~ {n; bst; st}⟦ τ ⟧ terr -> forall e, {n; bst; st}⟦τ⟧ e -> (exists (v: value), e ↪* v).
Proof.
  destruct τ; intros.
  - invclear H.
  - pbc. apply H0; clear H0. rewrite terr_inhabitant_iff_false_phi'. split; refinement_solver2.
    intros. intro Ff. apply H2. exists c. invclear H1; mydestr; auto.
  - pbc. apply H0; clear H0. constructor; refinement_solver2. split; refinement_solver2.
    intros.
    apply (termR_perserve_rR _ (mk_app e c_x)); auto; try refinement_solver2. admit. admit.
    admit.
  - pbc. apply H0; clear H0. do 2 constructor; refinement_solver2.
    intros.
    apply (termR_perserve_rR _ (mk_app e e_x)); auto; try refinement_solver2. admit. admit.
Admitted.

Lemma terr_ctxrR_inhabitant_implies_halt: forall Γ st τ,
    not_overbasety τ ->
    ~ {st}⟦τ⟧{Γ} terr ->
    forall e, {st}⟦τ⟧{Γ} e -> (exists (v: value), e ↪* v).
Proof.
  induction Γ; intros; mydestr.
  - denotation_simp1. invclear H1. eapply terr_inhabitant_implies_halt; eauto.
  - destruct r.
    + rewrite ctxrR_err_overbase_forall in H0; refinement_solver2.
      mydestr. invclear H1.
      apply IHΓ with (e:= (tlete x (a \t\ e))) in H2; refinement_solver2.
      invclear H1.

    pbc. apply H0; clear H0. constructor.
    rewrite terr_inhabitant_iff_false_phi'. in H0.
  intros.
  split; intros.
  - invclear H. mydestr; subst. rewrite terr_reduction_iff_false_phi in H1; auto.
  - mydestr. constructor; auto. split; auto. rewrite terr_reduction_iff_false_phi; auto.
Qed.

Inductive halt_ctx: state -> listctx rty -> Prop :=
| halt_ctx_nil: ∀ st, halt_ctx st []
| halt_ctx_cons_obase: forall st Γ x b n d ϕ,
    ok_dctx (dom _ st) (Γ ++ [(x, {v: b | n | d | ϕ })]) ->
    halt_ctx st Γ ->
    (∃ c : constant, {st}⟦{v:b|n|d|ϕ}⟧ c) ->
    halt_ctx st (Γ ++ [(x, {v: b | n | d | ϕ })])
| halt_ctx_cons: forall st Γ x τ,
    ok_dctx (dom _ st) (Γ ++ [(x, τ)]) ->
    not_overbasety τ ->
    halt_ctx st Γ ->
    ~ ({st}⟦ τ ⟧{ Γ } terr) ->
    halt_ctx st (Γ ++ [(x, τ)]).

Global Hint Constructors halt_ctx: core.

Lemma halt_ctx_post_destruct: forall Γ x st τ,
    halt_ctx st (Γ ++ [(x, τ)]) <->
      ok_dctx (dom _ st) (Γ ++ [(x, τ)]) /\
      ((∃ b n d ϕ, τ = {v: b | n | d | ϕ } /\ (∃ c : constant, {st}⟦{v:b|n|d|ϕ}⟧ c) /\ halt_ctx st Γ) \/
         (not_overbasety τ /\ ~ ({st}⟦ τ ⟧{ Γ } terr) /\ halt_ctx st Γ)).
Proof.
  split; intros.
  - invclear H; listctx_set_simpl. split; auto.
    left. do 4 eexists; eauto.
  - mydestr. destruct H0; mydestr; subst.
    + econstructor; eauto.
    + apply halt_ctx_cons; auto.
Qed.

Lemma halt_ctx_pre_destruct: forall Γ x st b n d ϕ,
    halt_ctx st ((x, {v: b | n | d | ϕ }) :: Γ) <->
      (ok_dctx (dom _ st) ((x, {v: b | n | d | ϕ }) :: Γ) /\
         (forall Γ1 y τ_y Γ2, Γ = Γ1 ++ (y, τ_y) :: Γ2 ->
                         ∃ c : constant, ({0;b∅;st}⟦{v:b|n|d|ϕ}⟧) c ∧
                         (not_overbasety τ_y -> ¬ ({<[x:=c]> st}⟦ τ_y ⟧{Γ1} terr)) /\
                           (~ not_overbasety τ_y -> (∃ c_y : constant, {<[x:=c]> st}⟦ τ_y ⟧{Γ1} c_y))
         )
      ).
Proof.
  apply (rev_ind (fun Γ => ∀ x st b n d ϕ,
                      halt_ctx st ((x, {v: b | n | d | ϕ }) :: Γ) <->
                        (ok_dctx (dom _ st) ((x, {v: b | n | d | ϕ }) :: Γ) /\
                           (forall Γ1 y τ_y Γ2, Γ = Γ1 ++ (y, τ_y) :: Γ2 ->
                         ∃ c : constant, ({0;b∅;st}⟦{v:b|n|d|ϕ}⟧) c ∧
                         (not_overbasety τ_y -> ¬ ({<[x:=c]> st}⟦ τ_y ⟧{Γ1} terr)) /\
                           (~ not_overbasety τ_y -> (∃ c_y : constant, {<[x:=c]> st}⟦ τ_y ⟧{Γ1} c_y))
         )
        ))); split; intros.
  - admit.
  - admit.
  - mydestr. rewrite app_comm_cons in H0; rewrite halt_ctx_post_destruct in H0; mydestr.
    split; auto.
    destruct H1; mydestr; subst; intros.
    + destruct (decide (a = y)); subst.
      {
        apply ok_find_same in H1; mydestr; refinement_solver2; subst.
        rewrite H in H3; mydestr.
        eexists; split; auto.
      }
    + intros.
      assert (Γ1 = l /\ y = a /\ τ_y = {v:x|x1|x2|x3} /\ Γ2 = []). admit. mydestr; subst.
      admit.
    + intros.
      assert (Γ1 = l /\ y = a /\ τ_y = [v:x|x1|x2|x3] /\ Γ2 = []). admit. mydestr; subst.
      rewrite ctxrR_err_overbase_forall in H3; auto; refinement_solver2. admit. admit.
    + intros.
      assert (Γ1 = l /\ y = a /\ τ_y = r /\ Γ2 = []). admit. mydestr; subst.
      rewrite H in H2; mydestr. apply H4.
      apply H.
      listctx_set_solver.
      refinement_simp1. eauto. || listctx_set_solver || fast_set_solver.
      refinement_solver.
      refinement_solver2.

      invclear H3.

         (
           ((∃ b n d ϕ, τ = {v: b | n | d | ϕ } /\
                          (∃ (c: constant), [] ⊢t c ⋮v b /\ ϕ b∅ st c /\ halt_ctx ({ x ↦ c } st) Γ)
            ) \/
              (∃ b n d ϕ, τ = [v: b | n | d | ϕ ] /\
                            (∃ e_x_hat, {st}⟦[v:b|n|d|ϕ]⟧ e_x_hat /\
                                        (∀ (c: constant), [] ⊢t c ⋮v b -> e_x_hat ↪* c -> halt_ctx ({ x ↦ c } st) Γ)
                            )
           )) \/
             (is_arr τ /\ halt_ctx st Γ)
         )
      ).

Lemma halt_ctx_pre_destruct: forall Γ x st τ,
    halt_ctx st ((x, τ) :: Γ) <->
      (ok_dctx (dom _ st) ((x, τ) :: Γ) /\
         (
           ((∃ b n d ϕ, τ = {v: b | n | d | ϕ } /\
                          (∃ (c: constant), [] ⊢t c ⋮v b /\ ϕ b∅ st c /\ halt_ctx ({ x ↦ c } st) Γ)
            ) \/
              (∃ b n d ϕ, τ = [v: b | n | d | ϕ ] /\
                            (∃ e_x_hat, {st}⟦[v:b|n|d|ϕ]⟧ e_x_hat /\
                                        (∀ (c: constant), [] ⊢t c ⋮v b -> e_x_hat ↪* c -> halt_ctx ({ x ↦ c } st) Γ)
                            )
           )) \/
             (is_arr τ /\ halt_ctx st Γ)
         )
      ).
Proof.
  apply (rev_ind (fun Γ => forall x st τ,
                      halt_ctx st ((x, τ) :: Γ) <->
                        (ok_dctx (dom _ st) ((x, τ) :: Γ) /\
                           (
                             ((∃ b n d ϕ, τ = {v: b | n | d | ϕ } /\
                                            (∃ (c: constant), [] ⊢t c ⋮v b /\ ϕ b∅ st c /\ halt_ctx ({ x ↦ c } st) Γ)
                             ) \/
                               (∃ b n d ϕ, τ = [v: b | n | d | ϕ ] /\
                                             (exists e_x_hat, {st}⟦[v:b|n|d|ϕ]⟧ e_x_hat /\
                                                         (∀ (c: constant), [] ⊢t c ⋮v b -> e_x_hat ↪* c -> halt_ctx ({ x ↦ c } st) Γ)
                                             )
                               )
                             ) \/
                               (is_arr τ /\ halt_ctx st Γ)
                           )
                        )
        )); split; intros.
  - rewrite <- app_nil_l in H. rewrite halt_ctx_post_destruct in H.
    do 2 destruct H; listctx_set_simpl; mydestr; subst; split; auto; left.
    + left. do 4 eexists. split; auto. admit.
      (* exists x4; split; refinement_solver2. invclear H2; auto. *)
    + right. do 4 eexists. split; auto. admit.
  - admit.
  - mydestr.
    rewrite app_comm_cons in H0; rewrite halt_ctx_post_destruct in H0.
    do 2 destruct H0; mydestr; subst; split; auto.
    { admit. }
    { rewrite H in H2; mydestr. do 2 destruct H2; mydestr; subst; simpl.
      - left. left. do 4 eexists. split; auto.
        rewrite ctxrR_err_overbase_forall in H3; auto; refinement_solver2.
        mydestr. exists x8. split; refinement_solver2. split; auto.
        constructor; auto. admit.
        rewrite ctxrR_err_overbase_forall in H3; auto; refinement_solver2.


        admit. admit.
      - left. right. do 4 eexists. split; auto.
        rewrite ctxrR_err_underbase_forall in H3; auto; refinement_solver2.
        admit. admit.
      - admit.
    }



    assert (halt_ctx st ((x0, τ) :: l)) as Hpre. admit.
    rewrite H in Hpre; mydestr. split; auto. admit.
    do 2 destruct H2; mydestr; subst; simpl.
    + left. left. do 4 eexists. split; auto.
      (* exists x4. split; auto. split; auto. constructor; auto. admit. *)
      (* exists x4; split; refinement_solver2. *)
      rewrite app_comm_cons in H0; rewrite halt_ctx_post_destruct in H0.
      do 2 destruct H0; mydestr; subst.
      { admit.
        (* intros. constructor; auto. admit. admit. *)
        (* exists x8. *)
        (* invclear H5; mydestr. admit. *)
        (*  invclear H9. *)
      }
      { rewrite ctxrR_err_overbase_forall in H7; simpl; auto. mydestr.
        exists x9. split. admit. split. admit. constructor; auto. admit.
        intros; mydestr. constructor; auto. admit. admit.
        rewrite ctxrR_err_overbase_forall in H5; simpl; auto. mydestr. intro Hf. apply H10.
        
      }

      rewrite halt_ctx_post_destruct.

      constructor. admit.


       rewrite app_comm_cons in H0. rewrite halt_ctx_post_destruct in H0.
    do 2 destruct H0; mydestr; subst; simpl; split; auto.
    rewrite H in Hpre; mydestr. split; auto.



    rewrite app_comm_cons in H0. rewrite halt_ctx_post_destruct in H0.
    do 2 destruct H0; mydestr; subst.
    + split; auto. rewrite H in H2.

      match goal with
      | [H: {?st}⟦ ?τ ⟧{ [] } ?e |- {?st}⟦ ?τ ⟧ ?e ] => invclear H
      end.
      admit. auto.
      constructor.

      denotation_simp1. invclear H2.

    invclear H; listctx_set_simpl4; denotation_simp1; split; auto.
    { left. do 4 eexists; split; auto. admit. }
    {
      right. left. exists b, n, d, ϕ. split; auto. rewrite terr_inhabitant_iff_false_phi' in H4; mydestr.
      assert (closed_rty 0 (dom aset st) [v:b|n|d|ϕ]) by closed_rty_solver2.
      repeat split; auto.
      pbc. apply H4. split; auto. rewrite exists_forall_aux1; auto. intros; constructor.
    }
    {
      destruct (classic (is_arr τ)).
      - right. right. split; auto.
      - denotation_simp1. invclear H2. left. do 4 eexists. split; auto. admit.
    }
  - admit.
  - invclear H0; listctx_set_simpl4; denotation_simp1; split; auto.
    { rewrite H in H3; mydestr. do 2 destruct H1; mydestr; subst.
      - left. do 4 eexists. split; auto. repeat eexists; eauto. apply halt_ctx_cons_ubase; auto. admit. apply ctxrR_err_underbase_forall in H5.

        eapply ok_dctx_trans; eauto.
        ok_dctx_solver2.
        ok_dctx_solver2.
      destruct (classic (is_arr τ)).
      - right. right. split; auto. admit.
      - denotation_simp1.
        + right. left. do 4 eexists. split; auto. split.

          exists x, x1, x2, x3. repeat eexists; eauto.
      rewrite H in H3; mydestr. repeat destruct H1.
      - right. left. eex
    }
    listctx_set_simpl.
    match goal with
    | [H: _ ++ [_] = _ :: _ ++ [_] |- _ ] => rewrite app_comm_cons in H; listctx_set_simpl
    end.
    rewrite app_comm_cons in H1. listctx_set_simpl.
  induction Γ; split; intros; invclear H; listctx_set_simpl.
  - listctx_set_simpl4. denotation_simp1. split; auto.
    right. left. exists b, n, d, ϕ. rewrite terr_inhabitant_iff_false_phi' in H4; mydestr.
    repeat split; auto.
    + destruct (classic (∃ c : constant, [] ⊢t c ⋮v b ∧ ϕ b∅ st c)); auto; exfalso.
      assert (closed_rty 0 (dom aset st) [v:b|n|d|ϕ]) by closed_rty_solver2.
      denotation_simp1. apply H4. split; auto. intros. intro HF. apply H. exists c. split; auto.
    + intros. constructor.
  - listctx_set_simpl4. denotation_simp1. split; auto. destruct (classic (is_arr τ)).
    right. right. split; auto.
    denotation_simp1. invclear H2.
    left. repeat eexists. intros. constructor.
  - admit.
  - admit.
  - admit.
  - 

      apply H4. split; closed_rty_solver2. intros. intro HF. apply H. exists c. split; auto.
      rewrite terr_inhabitant_iff_false_phi' in H4; mydestr.
  - destruct H; mydestr; subst.
    + apply halt_ctx_cons_ubase; auto.
    + apply halt_ctx_cons; auto.
Qed.

Inductive halt_ctx_rev: state -> listctx rty -> Prop :=
| halt_ctx_rev_nil: ∀ st, halt_ctx_rev st []
| halt_ctx_rev_cons_obase: forall st Γ x (b: base_ty) n d (ϕ: refinement),
    (forall (c: constant), [] ⊢t c ⋮v b -> ϕ b∅ ∅ c -> halt_ctx_rev ({ x ↦ c } st) Γ) ->
    halt_ctx_rev st ((x, {v: b | n | d | ϕ }) :: Γ)
| halt_ctx_rev_cons_ubase: forall st Γ x (b: base_ty) n d (ϕ: refinement),
    (exists (c: constant), [] ⊢t c ⋮v b /\ ϕ b∅ ∅ c) ->
    (forall (c: constant), [] ⊢t c ⋮v b -> halt_ctx_rev ({ x ↦ c } st) Γ) ->
    halt_ctx_rev st ((x, [v: b | n | d | ϕ ]) :: Γ)
| halt_ctx_rev_cons_arr: forall st Γ x τ,
    is_arr τ -> halt_ctx_rev st Γ -> halt_ctx_rev st ((x, τ) :: Γ).

Lemma halt_ctx_weak_last: forall Γ x st τ,
    halt_ctx st (Γ ++ [(x, τ)]) -> halt_ctx st Γ.
Proof.
  intros. rewrite halt_ctx_post_destruct in H. destruct H; mydestr; subst; auto.
Qed.



Lemma neg_terr_inhabitant_implies_c_exists: forall n1 n2 bst st (d: aset) b (ϕ: refinement),
    closed_rty n1 (dom aset st) [v:b|n2|d|ϕ] ->
    ~ {n1; bst; st}⟦ [v: b | n2 | d | ϕ ] ⟧ terr <-> exists c: constant, [] ⊢t c ⋮v b /\ ϕ bst st c.
Proof.
  split; intros.
  - destruct (classic (exists c: constant, [] ⊢t c ⋮v b /\ ϕ bst st c)); auto.
    exfalso. apply H0. rewrite terr_inhabitant_iff_false_phi'. split; auto.
    intros. intro HF. apply H1. exists c. split; auto.
  - intros HF. rewrite terr_inhabitant_iff_false_phi' in HF; mydestr.
    apply H2 in H0. apply H0; auto.
Qed.

Ltac auto_exfalso3 :=
  match goal with
    | [H: _ ++ [_] = [] |- _ ] => apply app_eq_nil in H; destruct H as (_ & Htmp); invclear Htmp
    end || auto_exfalso.

Lemma halt_ctx_regular: forall Γ st, halt_ctx st Γ -> ok_dctx (dom _ st) Γ.
Proof.
  induction Γ; intros; invclear H; auto.
  - constructor.
  - auto_exfalso3.
  - auto_exfalso3.
Qed.



Lemma halt_ctx_pre_destruct: forall Γ x b n d ϕ st,
    halt_ctx st ((x, [v: b | n | d | ϕ ]) :: Γ) <->
      ok_dctx (dom _ st) ((x, [v: b | n | d | ϕ ]) :: Γ) /\
      (exists (c: constant), [] ⊢t c ⋮v b ∧ ϕ b∅ st c) /\ (forall (c: constant), [] ⊢t c ⋮v b -> halt_ctx ({ x ↦ c } st) Γ).
Proof.
  apply (rev_ind (fun Γ => forall x b n d ϕ st,
                      halt_ctx st ((x, [v: b | n | d | ϕ ]) :: Γ) <->
                        ok_dctx (dom _ st) ((x, [v: b | n | d | ϕ ]) :: Γ) /\
                        (exists (c: constant), [] ⊢t c ⋮v b ∧ ϕ b∅ st c) /\ (forall (c: constant), [] ⊢t c ⋮v b -> halt_ctx ({ x ↦ c } st) Γ)));
  repeat split; simpl; intros; mydestr.
  - apply halt_ctx_regular; auto.
  - invclear H; listctx_set_simpl4; denotation_simp1.
    apply neg_terr_inhabitant_implies_c_exists in H4; auto. closed_rty_solver2.
  - constructor.
  - rewrite <- app_nil_l. constructor; auto. constructor. denotation_simp1.
    rewrite neg_terr_inhabitant_implies_c_exists. eexists; split; eauto. closed_rty_solver2.
  - apply halt_ctx_regular; auto.
  - rewrite app_comm_cons in H0. rewrite halt_ctx_post_destruct in H0.
    destruct H0; mydestr; subst; apply H in H2; mydestr; eexists; eauto.
  - rewrite app_comm_cons in H0. rewrite halt_ctx_post_destruct in H0.
    destruct H0; mydestr; subst.
    + apply H in H3; mydestr. apply halt_ctx_cons_ubase; denotation_simp. invclear H2; auto; ok_dctx_solver.



      admit.
    + apply H in H3; mydestr. apply halt_ctx_cons; denotation_simp. invclear H0; auto; ok_dctx_solver.
  - rewrite app_comm_cons. destruct (classic (not_underbasety r)).
    + apply halt_ctx_cons; denotation_simp. rewrite H. split. ok_dctx_solver2.
      eexists; eauto. intros. eapply halt_ctx_weak_last; eauto.
    + denotation_simp1. apply halt_ctx_cons_ubase; auto. rewrite H. split. ok_dctx_solver2.
      eexists; eauto. intros. eapply halt_ctx_weak_last; eauto.
      admit.

      eapply halt_ctx_cons_ubase. denotation_simp. rewrite H. split. eexists; eauto. intros. eapply halt_ctx_weak_last; eauto.

  - constructor.
    + apply H in H1; mydestr.
    invclear H0.
    + rewrite app_comm_cons in H1. apply app_inj_tail in H1. mydestr. invclear H1.
      apply H in H2. mydestr. exists x. auto.
    + rewrite app_comm_cons in H1. apply app_inj_tail in H1. mydestr. invclear H1.
      apply H in H4. mydestr. exists x. auto.
  - admit.
  - mydestr. rewrite app_comm_cons. destruct (classic (not_underbasety r)).
    + apply halt_ctx_cons; auto. rewrite H. split. exists x; auto. intros. apply H1 in H3. simpl. invclear H3.

Inductive ctxrR_halt: state -> listctx rty -> rty -> tm -> Prop :=
| ctxrR_halt_nil: forall st τ e, { st }⟦ τ ⟧ e -> ctxrR_halt st [] τ e
| ctxrR_halt_cons_over: forall st (x: atom) B n d ϕ Γ τ (e: tm),
    closed_rty 0 ({[x]} ∪ ctxdom Γ ∪ (dom _ st)) τ ->
    ok_dctx (dom _ st) ((x, {v: B | n | d | ϕ}) :: Γ) ->
    ((x, TBase B) :: (⌊Γ⌋*)) ⊢t e ⋮t ⌊τ⌋ ->
     (forall (c_x: constant), {st}⟦ {v: B | n | d | ϕ} ⟧ c_x ->
                         ctxrR_halt  (<[ x := c_x ]> st) Γ τ (tlete c_x ({ 0 <t~ x} e))) ->
     ctxrR_halt st ((x, {v: B | n | d | ϕ}) :: Γ) τ e
| ctxrR_halt_cons_under: forall st (x: atom) τ_x τ Γ e,
    not_overbasety τ_x ->
    closed_rty 0 ({[x]} ∪ ctxdom Γ ∪ (dom _ st)) τ ->
    ok_dctx (dom _ st) ((x, τ_x) :: Γ) ->
    ((x, ⌊τ_x⌋ ) :: (⌊Γ⌋*)) ⊢t e ⋮t ⌊τ⌋ ->
     (exists e_x_hat, {st}⟦ τ_x ⟧ e_x_hat /\
                   (∃ (v_x_hat: value), e_x_hat ↪* v_x_hat) /\
                   (forall e_x, {st}⟦ τ_x ⟧ e_x ->
                           (∀ (v_x: value), e_x_hat ↪* v_x ->
                                            ctxrR_halt ({ x ↦ v_x } st) Γ τ (tlete e_x ({ 0 <t~ x} e))))) ->
     ctxrR_halt  st ((x, τ_x) :: Γ) τ e.

Fixpoint random_inhabitant (τ: rty) :=
  match τ with
  | {v: _ | _ | _ | _ } => terr
  | [v: TNat | _ | _ | _ ] => nat-gen
  | [v: TBool | _ | _ | _ ] => bool-gen
  | -:{v: T1 | _ | _ | _ } ⤑ τ => vlam T1 (random_inhabitant τ)
  | τ1 ⤑ τ2 => vlam (rty_erase τ1) (random_inhabitant τ2)
  end.

Lemma halt_ctx_implies_halt_ctx_rev: forall Γ, halt_ctx Γ -> halt_ctx_rev ∅ Γ.
Proof.
  induction Γ; intros.
  - constructor.
  - mydestr. invclear H.

Lemma halt_ctx_plus_ctxrR_implies_ctxrR_halt: forall Γ τ e, halt_ctx Γ -> ⟦ τ ⟧{ Γ } e -> ctxrR_halt ∅ Γ τ e.
Proof.
  induction Γ; intros.
  - invclear H0. constructor; auto.
  - mydestr. invclear H.

Lemma random_inhabitant_in_any_under: ∀ (τ: rty) (bst : bstate) n st,
    not_overbasety τ ->
    closed_rty n (dom _ st) τ ->
    ({n;bst;st}⟦τ⟧) (random_inhabitant τ).
Proof.
  induction τ; intros; invclear H; mydestr.
  - destruct B.
    + split; simpl; auto. admit. split; auto. intros. admit.
    + split; simpl; auto. apply mk_bool_gen_typable; auto. split; auto. intros. destruct c; invclear H.
      apply mk_bool_gen_reduce_to_all_bool.
  - denotation_simp. constructor; auto. admit. constructor; auto.
    intros.
    apply (termR_perserve_rR _ (random_inhabitant τ)); auto; try refinement_solver1. admit.
    apply IHτ; refinement_solver1.
  - denotation_simp. constructor; auto. admit. constructor; auto.
    intros.
    assert (exists (v_x: value), e_x ↪* v_x). admit. destruct H1.
    apply (termR_perserve_rR _ (random_inhabitant τ2)); auto; try refinement_solver1. admit.
    apply IHτ2; refinement_solver1.
Admitted.

Lemma denotation_weaken_empty: forall Γ (e: tm) τ (st: state),
    not_overbasety τ ->
    valid_rty τ ->
    ok_dctx (dom _ st) Γ -> { st }⟦ τ ⟧ e -> {st}⟦ τ ⟧{ Γ } e.
Proof.
  induction Γ; intros; denotation_simp; mydestr.
  constructor; auto.
  destruct (classic (not_overbasety r)).
  - constructor; auto; try refinement_solver1.
    exists (random_inhabitant r); invclear H1;
    assert (({0;b∅;st}⟦r⟧) (random_inhabitant r)) by
      (intros; apply random_inhabitant_in_any_under; auto);
    split; try (apply random_inhabitant_in_any_under; auto); intros.
    + assert ([] ⊢t v_x ⋮v ⌊r⌋) as Hv_x by refinement_solver1.
      destruct v_x; mydestr; simpl; try auto_ty_exfalso.
      apply (termR_perserve_ctxrR _ _ e); auto. admit.
      apply IHΓ; auto. denotation_simp. intros. rewrite rR_shadow_update_st; auto; refinement_solver1.
      apply rR_regular1 in H2; mydestr. refinement_solver1.
    + apply (termR_perserve_ctxrR _ _ e); auto. admit.
      apply IHΓ; auto. denotation_simp. admit. admit.
  - refinement_solver1. constructor; auto; try refinement_solver1.
    intros. apply (termR_perserve_ctxrR _ _ e); auto. admit.
    invclear H1; apply IHΓ; auto; try auto_ty_exfalso; try ok_dctx_solver_slow.
    intros. rewrite rR_shadow_update_st; auto; refinement_solver1.
    apply rR_regular1 in H2; mydestr. refinement_solver1.
Admitted.

Lemma denotation_weaken: forall Γ1 Γ3 (e: tm) τ (st: state) Γ2,
    ctxrR_wf (dom _ st) (Γ1 ++ Γ2 ++ Γ3) τ -> {st}⟦ τ ⟧{ Γ1 ++ Γ3 } e -> {st}⟦ τ ⟧{ Γ1 ++ Γ2 ++ Γ3 } e.
Proof.
  intros. generalize dependent Γ2.
  induction H0; intros; listctx_set_simpl.
  - specialize (H b∅). invclear H; mydestr. denotation_simp.
  induction Γ1; intros; listctx_set_simpl.
  - admit.
  - 

Lemma err_exists_denotation_excluded: forall Γ b n (d: aset) (ϕ: refinement),
    closed_rty 0 (dom aset (∅: state)) [v:b|n|d|ϕ] ->
    ~ (⟦ [v: b | n | d | ϕ ] ⟧{ Γ } terr) -> (∃ (v: value), ⟦ {v: b | n | d | ϕ } ⟧{ Γ } v).
Proof.
  induction Γ; intros.
  - destruct (classic (∃ v : value, (⟦{v:b|n|d|ϕ}⟧{ [] }) v)); auto; exfalso; apply H0; clear H0.
    rewrite forall_iff_not_exists1 in H1.
    constructor. intros.
    do 2 constructor; auto. rewrite terr_reduction_iff_false_phi.
    intros. intros Hf.
    specialize (H1 c). apply H1.
    constructor; auto. intros. constructor; auto. constructor; auto.
    rewrite closed_rty_under_iff_over; auto.
    exists c. constructor; auto. constructor; auto.
    rewrite bound_in_refinement_0. apply Hf. refinement_solver1.
  - mydestr. refinement_simp1.
    destruct (classic (∃ v : value, (⟦{v:b|n|d|ϕ}⟧{ a :: Γ }) v)); auto; exfalso; apply H0; clear H0.
    rewrite forall_iff_not_exists1 in H1.
    mydestr.
    constructor.


    repeat match goal with
           | [H: closed_rty _ _ _ |- bound_in_refinement _ _ ] => destruct H; mydestr
           | [H: valid_rty _ |- bound_in_refinement _ _ ] => invclear H
           | [H: wf_r _ _ ?ϕ |- bound_in_refinement _ ?ϕ ] =>  destruct H
           end.
    refinement_simp1; auto.
    repeat match goal with
    | [H: lc_rty_idx 0 [v:_|?n|_|_] |- _ ] =>
        match n with
        | 0 => fail 1
        | _ =>  assert (n = 0) by (apply lc_rty_idx_under_0_is_0 in H; auto); subst
        end
    end.
    repeat match goal with
           | [H: closed_rty _ _ _ |- bound_in_refinement _ _ ] => destruct H; mydestr
           | [H: valid_rty _ |- bound_in_refinement _ _ ] => invclear H
           | [H: wf_r _ _ ?ϕ |- bound_in_refinement _ ?ϕ ] =>  destruct H
           end.
    assert (n = 0) by (apply lc_rty_idx_under_0_is_0 in H2; auto); subst.
    invclear H2.
    destruct H5. 
    invclear H.
 mydestr. invclear H. invclear H6. unfold bound_in_refinement in H5.
    assert (closed_rty 0 ∅ [v:b|n|d|ϕ]). setoid_rewrite <- H2; auto.

    setoid_rewrite H2 in H.

    admit. auto.
    intros. apply multistep_R.

    constructor.
    exfalso. apply H. constructor; intros. rewrite terr_inhabitant_iff_false_phi.


  intros. destruct (classic (∃ v : value, (⟦[v:b|n|d|ϕ]⟧{Γ}) v)); auto.
  exfalso. apply H. rewrite terr_inhabitant_iff_false_phi.

  intro Hf. mydestr. invclear Hf.
    + invclear H. denotation_simp.
      match goal with
      | [H: ∀ c, [] ⊢t (vconst c) ⋮v _ → _ _ ∅ c → terr ↪* (tvalue (vconst c)) |- _ ] => idtac H
      end.
      rewrite terr_inhabitant_implies_false_phi in H4.
      apply H4 in H7.
      repeat match goal with
      | [H: _ ⊢t ?v ⋮v (TBase _) |- _ ] =>
          match v with
          | vconst _ => fail 1
          | _ => set H7 as Htmp; apply empty_basic_typing_base_const_exists in Htmp; mydestr; subst
          end
      end.
      set H7 as Htmp; apply empty_basic_typing_base_const_exists in Htmp; mydestr; subst.
      match goal with
      | [H: _ ⊢t (tvalue ?v) ⋮t _ |- _ ] => invclear H
      end.

      apply empty_basic_typing_base_const_exists in H.

       specialize (H1 b∅).
      invclear H0. invclear H1. mydestr.
Admitted.

(* Denotation: *)

Fixpoint rR (n: nat) (bst: bstate) (st: state) (τ: rty) (e: tm) : Prop :=
  [] ⊢t e ⋮t ⌊ τ ⌋ /\ closed_rty n (dom _ st) τ /\
    match τ with
    | {v: B | _ | _ | ϕ} => exists (c: constant), [] ⊢t c ⋮v B /\ ϕ bst st c /\ e = c
    | [v: B | _ | _ | ϕ] => forall (c: constant), [] ⊢t c ⋮v B -> ϕ bst st c -> e ↪* c
    | -:{v: B | _ | _ | ϕ } ⤑ τ =>
        forall (c_x: constant), [] ⊢t c_x ⋮v B -> ϕ bst st c_x -> rR (S n) (<b[↦ c_x ]> bst) st τ (mk_app e c_x)
    | τ1 ⤑ τ2 => forall (e_x: tm), rR n bst st τ1 e_x -> rR n bst st τ2 (mk_app e e_x)
    end.

Notation " '{' n ';' bst ';' st '}⟦' τ '⟧' " :=
  (rR n bst st τ) (at level 20, format "{ n ; bst ; st }⟦ τ ⟧", bst constr, st constr, τ constr).
Notation " '{' st '}⟦' τ '⟧' " := (fun e => forall bst, rR 0 bst st τ e) (at level 20, format "{ st }⟦ τ ⟧", st constr, τ constr).
Notation " '⟦' τ '⟧' " := (fun e => forall bst, rR 0 bst ∅ τ e) (at level 20, format "⟦ τ ⟧", τ constr).

(* regular of the denation *)

Lemma rR_regular1:
  forall τ n bst st e, { n; bst; st }⟦ τ ⟧ e -> closed_rty n (dom _ st) τ /\ [] ⊢t e ⋮t ⌊ τ ⌋.
Proof.
  induction τ; intros; invclear H; mydestr; subst; split; intros; auto.
Qed.

Lemma rR_regular2:
  forall τ st e, { st }⟦ τ ⟧ e -> closed_rty 0 (dom _ st) τ /\ [] ⊢t e ⋮t ⌊ τ ⌋.
Proof.
  intros. specialize (H b∅). eapply rR_regular1; eauto.
Qed.

Lemma rR_regular3:
  forall τ e, ⟦ τ ⟧ e -> (closed_rty 0 ∅ τ) /\ [] ⊢t e ⋮t ⌊ τ ⌋.
Proof.
  intros. eapply rR_regular2 in H. mydestr; split; auto.
  intros. apply H. my_set_solver.
Qed.

Inductive ctxrR_wf: aset -> listctx rty -> rty -> Prop :=
| ctxrR_wf_nil: forall d τ, closed_rty 0 d τ -> ctxrR_wf d [] τ
| ctxrR_wf_cons_over: forall d (x: atom) τ_x Γ τ,
    ok_dctx d ((x, τ_x) :: Γ) ->
    closed_rty 0 d τ_x ->
    ctxrR_wf ({[x]} ∪ d) Γ τ ->
    ctxrR_wf d ((x, τ_x) :: Γ) τ.

Inductive ctxrR: state -> listctx rty -> rty -> tm -> Prop :=
| ctxrR_nil: forall st τ e, { st }⟦ τ ⟧ e -> ctxrR st [] τ e
| ctxrR_cons_over: forall st (x: atom) B d ϕ Γ τ (e: tm),
    ctxrR_wf (dom _ st) ((x, {v: B | 0 | d | ϕ}) :: Γ) τ ->
    ((x, TBase B) :: (⌊Γ⌋*)) ⊢t e ⋮t ⌊τ⌋ ->
     (forall (c_x: constant), {st}⟦ {v: B | 0 | d | ϕ} ⟧ c_x ->
                         ctxrR (<[ x := c_x ]> st) Γ τ (tlete c_x ({ 0 <t~ x} e))) ->
     ctxrR st ((x, {v: B | 0 | d | ϕ}) :: Γ) τ e
| ctxrR_cons_under: forall st (x: atom) τ_x τ Γ e,
        ctxrR_wf (dom _ st) ((x, τ_x) :: Γ) τ ->
        ((x, ⌊τ_x⌋ ) :: (⌊Γ⌋*)) ⊢t e ⋮t ⌊τ⌋ ->
         not_overbasety τ_x ->
         (exists e_x_hat, {st}⟦ τ_x ⟧ e_x_hat /\
                       (forall e_x, {st}⟦ τ_x ⟧ e_x ->
                               (∀ (v_x: value), e_x_hat ↪* v_x ->
                                                ctxrR ({ x ↦ v_x } st) Γ τ (tlete e_x ({ 0 <t~ x} e))))) ->
         ctxrR st ((x, τ_x) :: Γ) τ e.

Notation " '{' st '}⟦' τ '⟧{' Γ '}' " := (ctxrR st Γ τ) (at level 20, format "{ st }⟦ τ ⟧{ Γ }", st constr, τ constr, Γ constr).
Notation " '⟦' τ '⟧{' Γ '}' " := (ctxrR ∅ Γ τ) (at level 20, format "⟦ τ ⟧{ Γ }", τ constr, Γ constr).

Lemma ctxrR_implies_ctxrR_wf:
  forall Γ τ st e, { st }⟦ τ ⟧{ Γ } e -> ctxrR_wf (dom _ st) Γ τ.
Proof.
  induction Γ; simpl; intros; invclear H; simpl; auto.
  - apply rR_regular2 in H0; mydestr. constructor; auto.
Qed.

Lemma ctxrR_wf_regular:
  forall Γ τ d, ctxrR_wf d Γ τ -> (ok_dctx d Γ) /\ cl_dctx d Γ /\ closed_rty 0 (ctxdom Γ ∪ d) τ.
Proof.
  induction Γ; simpl; intros; invclear H; simpl.
  - split. repeat constructor; auto; fast_set_solver.
    split. constructor.
    closed_rty_solver.
  - apply IHΓ in H6; mydestr2.
    split; auto.
    split. constructor; listctx_set_simpl. closed_rty_solver.
Qed.

Lemma ctxrR_regular0:
  forall Γ τ st e, { st }⟦ τ ⟧{ Γ } e ->
              (ok_dctx (dom _ st) Γ) /\ cl_dctx (dom _ st) Γ /\ closed_rty 0 (ctxdom Γ ∪ (dom _ st)) τ.
Proof.
  intros. apply ctxrR_wf_regular. eapply ctxrR_implies_ctxrR_wf; eauto.
Qed.

Lemma ctxrR_regular1:
  forall Γ τ st e, { st }⟦ τ ⟧{ Γ } e -> ⌊ Γ ⌋* ⊢t e ⋮t ⌊ τ ⌋.
Proof.
  induction Γ; simpl; intros; invclear H; simpl; auto.
  - apply rR_regular2 in H0; mydestr; auto.
Qed.

Lemma ctxrR_regular:
  forall Γ τ st e, { st }⟦ τ ⟧{ Γ } e ->
              ⌊ Γ ⌋* ⊢t e ⋮t ⌊ τ ⌋ /\
                (ok_dctx (dom _ st) Γ) /\
                cl_dctx (dom _ st) Γ /\
                closed_rty 0 (ctxdom Γ ∪ (dom _ st)) τ.
Proof.
  intros. split.
  - eapply ctxrR_regular1; eauto.
  - eapply ctxrR_regular0; eauto.
Qed.

(* Lemma ctxrR_regular1: *)
(*   forall Γ τ st e, { st }⟦ τ ⟧{ Γ } e -> closed_rty 0 (ctxdom Γ ∪ dom _ st) τ /\ *)
(*                 ⌊ Γ ⌋* ⊢t e ⋮t ⌊ τ ⌋ /\ (ok_dctx (dom _ st) Γ). *)
(* Proof. *)
(*   induction Γ; simpl; intros; invclear H; simpl. *)
(*   - apply rR_regular2 in H0; mydestr; split; intros; my_simplify_map_eq. *)
(*     closed_rty_solver. *)
(*     split; auto. constructor; auto. fast_set_solver. *)
(*   - split; auto. closed_rty_solver. *)
(*   - mydestr. split; auto. apply rR_regular2 in H; mydestr. closed_rty_solver. *)
(* Qed. *)

Inductive ctxrR2: bstate -> state -> listctx rty -> rty -> rty -> Prop :=
| ctxrR2_nil: forall bst st τ1 τ2,
    closed_rty 0 (dom _ st) τ1 -> closed_rty 0 (dom _ st) τ2 ->
    (forall e, { st }⟦ τ1 ⟧ e -> { st }⟦ τ2 ⟧ e) ->
    ctxrR2 bst st [] τ1 τ2
| ctxrR2_cons_over: forall bst st (x: atom) B d ϕ Γ τ1 τ2,
    ctxrR_wf (dom _ st) ((x, {v: B | 0 | d | ϕ}) :: Γ) τ1 ->
    ctxrR_wf (dom _ st) ((x, {v: B | 0 | d | ϕ}) :: Γ) τ2 ->
    (forall (c_x: constant), {st}⟦ {v: B | 0 | d | ϕ} ⟧ c_x -> ctxrR2 bst (<[ x := c_x ]> st) Γ τ1 τ2) ->
    ctxrR2 bst st ((x, {v: B | 0 | d | ϕ}) :: Γ) τ1 τ2
| ctxrR2_cons_under: forall bst st (x: atom) τ_x τ1 τ2 Γ,
    ctxrR_wf (dom _ st) ((x, τ_x) :: Γ) τ1 ->
    ctxrR_wf (dom _ st) ((x, τ_x) :: Γ) τ2 ->
    not_overbasety τ_x ->
    (exists e_x_hat, {st}⟦ τ_x ⟧ e_x_hat /\
                  (forall e_x, {st}⟦ τ_x ⟧ e_x ->
                          (∀ (v_x: value), e_x_hat ↪* v_x -> ctxrR2 bst ({ x ↦ v_x } st) Γ τ1 τ2))) ->
    ctxrR2 bst st ((x, τ_x) :: Γ) τ1 τ2.

Notation " '{' st '}⟦' τ1 '⟧⊆⟦' τ2 '⟧{' Γ '}' " := (forall bst, ctxrR2 bst st Γ τ1 τ2) (at level 20, format "{ st }⟦ τ1 ⟧⊆⟦ τ2 ⟧{ Γ }", st constr, τ1 constr, τ2 constr, Γ constr).
Notation " '⟦' τ1 '⟧⊆⟦' τ2 '⟧{' Γ '}' " := (forall bst, ctxrR2 bst ∅ Γ τ1 τ2) (at level 20, format "⟦ τ1 ⟧⊆⟦ τ2 ⟧{ Γ }", τ1 constr, τ2 constr, Γ constr).
