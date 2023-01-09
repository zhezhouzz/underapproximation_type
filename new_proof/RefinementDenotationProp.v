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
From CT Require Import RefinementDenotationTac.
From CT Require Import TermOrdering.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Classical.

Import Atom.
Import CoreLang.
Import CoreLangProp.
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
Import RefinementDenotationTac.
Import TermOrdering.

Lemma over_inhabitant_only_constant: forall (u: value) st b n d ϕ,
    ({0;b∅;st}⟦{v:b|n|d|ϕ}⟧) u -> (exists (c: constant), u = c).
Proof.
  intros. invclear H; mydestr. exists x. invclear H3. auto.
Qed.

Ltac RD_simp :=
  repeat match goal with
  | [H: ({_}⟦ _ ⟧{ [] }) _ |- _ ] => invclear H
  | [|- ({_}⟦ _ ⟧{ [] }) _ ] => constructor
  | [H: ({_}⟦_⟧{ (_, {v:_|_|_|_}) :: _ }) _ |- _ ] => invclear H; try auto_ty_exfalso
  | [H: ({0;b∅;_}⟦{v:_|_|_|_}⟧) (tvalue ?u) |- _ ] =>
      match u with
      | vconst _ => idtac
      | _ => destruct (over_inhabitant_only_constant _ _ _ _ _ _ H); subst
      end
  end.

Lemma wf_r_implies_state_dummy_insert: forall n2 d2 ϕ z,
    wf_r n2 d2 ϕ -> z ∉ d2 -> (forall st bst c v, (ϕ bst (<[z:=c]> st) v <-> ϕ bst st v)).
Proof.
  intros. invclear H. apply H1. intros.
  my_simplify_map_eq3.
Qed.

Lemma closed_rty_over_implies_state_dummy_insert: forall n1 d1 n2 d2 B ϕ z,
    closed_rty n1 d1 {v:B|n2|d2|ϕ} -> z ∉ d2 -> (forall st bst c v, (ϕ bst (<[z:=c]> st) v <-> ϕ bst st v)).
Proof.
  intros. eapply wf_r_implies_state_dummy_insert; eauto. refinement_solver3.
Qed.

Lemma closed_rty_under_implies_state_dummy_insert: forall n1 d1 n2 d2 B ϕ z,
    closed_rty n1 d1 [v:B|n2|d2|ϕ] -> z ∉ d2 -> (forall st bst c v, (ϕ bst (<[z:=c]> st) v <-> ϕ bst st v)).
Proof.
  intros. eapply wf_r_implies_state_dummy_insert; eauto. refinement_solver3.
Qed.

Lemma denotation_st_update_iff_subst:
  forall τ (z: atom) (c: constant) n bst (st: state) e,
    z ∉ (dom aset st) ->
    ({n;bst;<[z:=c]> st}⟦τ⟧ e) <-> (closed_rty n ({[z]} ∪ dom aset st) τ /\ {n;bst;st}⟦({z:=c}r) τ⟧ e).
Proof.
  induction τ; split; simpl; intros; mydestr; subst.
  - do 2 (split; denotation_simp; auto). split; auto.
    + eapply closed_rty_subst_excluded_forward in H1; eauto.
    + unfold refinement_subst. amap_dec_solver.
    repeat eexists; eauto. rewrite closed_rty_over_implies_state_dummy_insert in H3; eauto.
  - split; auto. split; auto.
    + denotation_simp.
    + repeat eexists; eauto.
      unfold refinement_subst in H4. unfold refinement_subst in H2. unfold refinement_set_subst in H2.
      amap_dec_solver.
      rewrite closed_rty_over_implies_state_dummy_insert; eauto.
  - split; auto; denotation_simp. split; auto. split; auto.
    + eapply closed_rty_subst_excluded_forward in H1; eauto.
    + unfold refinement_subst. amap_dec_solver.
      intros. apply H2; auto. rewrite closed_rty_under_implies_state_dummy_insert; eauto.
  - split; auto. split; auto.
    + denotation_simp.
    + repeat eexists; eauto. intros. apply H3; auto.
      unfold refinement_subst. unfold refinement_subst in H2. unfold refinement_set_subst in H2.
      amap_dec_solver.
      rewrite closed_rty_under_implies_state_dummy_insert in H5; eauto.
  - split; auto; denotation_simp. split; auto; ctx_erase_simp4. split; auto.
    + eapply closed_rty_subst_excluded_forward in H1; eauto.
    + intros. unfold refinement_subst in H4.
      assert (ϕ bst (<[z:=c]> st) c_x).
      { amap_dec_solver; auto. rewrite wf_r_implies_state_dummy_insert; eauto. refinement_solver3. }
      apply H2 in H5; auto. rewrite IHτ in H5; mydestr; auto.
  - split; auto; ctx_erase_simp4. split; auto.
    + denotation_simp.
    + intros. rewrite IHτ; auto. split; refinement_solver. apply H3; auto.
      unfold refinement_subst. amap_dec_solver.
      rewrite wf_r_implies_state_dummy_insert in H5; eauto. refinement_solver3.
  - split; auto; denotation_simp. split; auto; ctx_erase_simp4. split; auto.
    + eapply closed_rty_subst_excluded_forward in H1; eauto.
    + intros.
      assert (({n;bst;<[z:=c]> st}⟦τ1⟧) v_x) as HH. rewrite IHτ1; auto. split; refinement_solver.
      apply H2 in HH. rewrite IHτ2 in HH; auto. mydestr; auto.
  - split; auto; ctx_erase_simp4. split; auto; denotation_simp.
    + intros. rewrite IHτ2; auto. split; refinement_solver. apply H3. rewrite IHτ1 in H4; auto. mydestr; auto.
Qed.

Fixpoint random_inhabitant (τ: rty) :=
  match τ with
  | {v: _ | _ | _ | _ } => terr
  | [v: TNat | _ | _ | _ ] => nat-gen
  | [v: TBool | _ | _ | _ ] => bool-gen
  | -:{v: T1 | _ | _ | _ } ⤑ τ => vlam T1 (random_inhabitant τ)
  | τ1 ⤑ τ2 => vlam (rty_erase τ1) (random_inhabitant τ2)
  end.

Lemma random_inhabitant_tyable: ∀ (τ: rty) Γ,
    ok Γ -> Γ ⊢t (random_inhabitant τ) ⋮t ⌊τ⌋.
Proof.
  induction τ; simpl; intros; auto.
  - destruct B; auto.
  - basic_typing_solver6.
  - basic_typing_solver6.
Qed.

Global Hint Resolve random_inhabitant_tyable: core.

Lemma random_inhabitant_lc: forall τ, lc (random_inhabitant τ).
Proof.
  intros.
  assert ([] ⊢t (random_inhabitant τ) ⋮t ⌊τ⌋);
  basic_typing_solver6.
Qed.

Global Hint Resolve random_inhabitant_lc: core.

(* Ltac lc_solver3 := *)
(*   lc_solver || *)
(*   match goal with *)
(*   | [|- lc (tvalue (value_msubst _ _))] => *)
(*       eapply instantiation_implies_value_msubst_lc; eauto; *)
(*       basic_typing_solver *)
(*   end. *)

(* Ltac reduction_simpl1 := *)
(*   repeat (simpl; msubst_simpl || *)
(*             match goal with *)
(*             | [H: (tvalue _) ↪* (tvalue _) |- _ ] => *)
(*                 rewrite value_reduce_to_value_implies_same in H; mydestr; subst *)
(*             | [|- (tvalue _) ↪* (tvalue _)] => *)
(*                 rewrite value_reduce_to_value_implies_same; split; eauto *)
(*             | [H: context [?e ^v^ _] |- _ ] => *)
(*                assert (lc e) as Htmp by (auto; lc_solver3); *)
(*                rewrite (open_rec_lc_value _ e) in H; auto; *)
(*                try clear Htmp *)
(*            | [|- context [?e ^v^ _] ] => *)
(*                assert (lc e) as Htmp by (auto; lc_solver3); *)
(*                rewrite (open_rec_lc_value _ e); auto; *)
(*                try clear Htmp *)
(*            | [H: context [?e ^t^ _] |- _ ] => *)
(*                assert (lc e) as Htmp by (auto; lc_solver3); *)
(*                rewrite (open_rec_lc_tm _ e) in H; auto; *)
(*                try clear Htmp *)
(*            | [|- context [?e ^t^ _] ] => *)
(*                assert (lc e) as Htmp by (auto; lc_solver3); *)
(*                rewrite (open_rec_lc_tm _ e); auto; *)
(*                try clear Htmp *)
(*             end || auto_reduction_exfalso). *)

Ltac reduction_solver2 :=
  RD_simp; repeat (reduction_solver1 || refinement_solver4).
  (* reduction_simpl1; eauto; *)
  (* repeat ( *)
  (*     (match goal with *)
  (*      | [|- lc (tvalue (value_msubst _ _))] => *)
  (*          eapply instantiation_implies_value_msubst_lc; eauto; *)
  (*          basic_typing_solver6 *)
  (*      | [|- lc (tm_msubst _ _)] => *)
  (*          eapply instantiation_implies_msubst_lc; eauto; *)
  (*          basic_typing_solver6 *)
  (*      end) || op_solver1 || refinement_solver4 || basic_typing_solver6). *)

Lemma rR_inhabitant_err_or_halt: forall τ,
    valid_rty τ ->
    (forall n bst st, (∀ e, ({n;bst;st}⟦τ⟧) e -> (exists v, e ↪* v)) \/ (({n;bst;st}⟦τ⟧) terr)).
Proof.
  intros τ Hv. induction Hv; intros.
  - left. intros. invclear H0; mydestr; subst. eexists; eauto.
  - destruct (classic (({n0;bst;st}⟦[v:B|n|d|ϕ]⟧) terr)); auto. left; intros.
    neg_apply H0. invclear H1; mydestr. do 2 (split; auto). intros.
    apply H3 in H5; auto. exfalso_apply H2.
  - destruct (classic (({n0;bst;st}⟦-:{v: B | n | d | ϕ}⤑ τ⟧) terr)); auto. left; intros.
    neg_apply H1. invclear H2; mydestr. do 2 (split; auto). intros.
    apply H4 in H6; auto. eapply termR_perserve_rR; eauto.
    eapply mk_app_perserve_termR; eauto.
    apply stuck_tm_termR_terr; auto.
  - destruct (classic ( ({n0;bst;st}⟦(-:{v: B | n | d | ϕ}⤑ τ1) ⤑ τ2⟧) terr)); auto. left; intros.
    neg_apply H0. invclear H1; mydestr. do 2 (split; auto). intros.
    assert ([] ⊢t v_x ⋮v B ⤍ ⌊τ1⌋) by denotation_simp.
    apply H3 in H4; auto.
    eapply termR_perserve_rR; eauto.
    eapply mk_app_perserve_termR; eauto.
    apply stuck_tm_termR_terr; auto.
  - destruct (classic ( {n;bst;st}⟦(τ11 ⤑ τ12) ⤑ τ2⟧ terr )); auto. left; intros.
    neg_apply H0. invclear H1; mydestr. do 2 (split; auto). intros.
    assert ([] ⊢t v_x ⋮v ⌊τ11 ⤑ τ12⌋) by denotation_simp.
    apply H3 in H4; auto.
    eapply termR_perserve_rR; eauto.
    eapply mk_app_perserve_termR; eauto.
    apply stuck_tm_termR_terr; auto.
Qed.

Lemma not_rR_inhabitant_err_implies_halt: forall τ n bst st,
    valid_rty τ -> ~ ({n;bst;st}⟦τ⟧) terr -> ∀ e, ({n;bst;st}⟦τ⟧) e -> (exists v, e ↪* v).
Proof.
  intros.
  eapply rR_inhabitant_err_or_halt in H. destruct H; eauto.
  exfalso_apply H.
Qed.

Lemma not_empty_ctxrR_inhabitant_err_implies_halt: forall τ st,
    valid_rty τ -> ~ ({st}⟦τ⟧{ [] }) terr -> ∀ e, ({st}⟦τ⟧) e -> (exists v, e ↪* v).
Proof.
  intros.
  eapply rR_inhabitant_err_or_halt in H. destruct H; eauto.
  exfalso_apply H0. RD_simp; auto.
Qed.

Lemma random_inhabitant_in_any_under: ∀ (τ: rty) (bst : bstate) n st,
    not_overbasety τ ->
    closed_rty n (dom _ st) τ ->
    ({n;bst;st}⟦τ⟧) (random_inhabitant τ).
Proof.
  induction τ; intros; invclear H; mydestr.
  - destruct B.
    + do 2 (split; simpl; auto).
      intros. destruct c; invclear H.
      apply mk_nat_gen_reduce_to_all_nat.
    + do 2 (split; simpl; auto).
      intros. destruct c; invclear H.
      apply mk_bool_gen_reduce_to_all_bool.
  - denotation_simp. repeat split; refinement_solver4.
    + basic_typing_solver6.
    + intros.
      apply termR_perserve_rR with (e:= (random_inhabitant τ)); refinement_solver4.
      assert
        ((random_inhabitant τ ^t^ c_x) <-<{ []; ⌊τ⌋} (mk_app (vlam B (random_inhabitant τ)) c_x)). apply mk_app_reduce_to_open'; auto; basic_typing_solver6.
      reduction_simpl1.
      apply IHτ; refinement_solver4.
  - denotation_simp. repeat split; refinement_solver4.
    + basic_typing_solver6.
    + intros.
      (* assert (closed_rty n (dom aset st) τ1) by refinement_solver4. *)
      assert (closed_rty n (dom aset st) τ2) by refinement_solver4.
      (* assert (not_overbasety τ1) by (invclear H0; invclear H3; simpl; auto). *)
      assert (not_overbasety τ2) by (invclear H0; invclear H2; simpl; auto).
      (* apply IHτ1 with (bst:=bst) in H1; auto. *)
      apply IHτ2 with (bst:=bst) in H1; auto.
      apply termR_perserve_rR with (e:= tlete v_x (random_inhabitant τ2)); refinement_solver4.
      apply mk_app_reduce_to_let'; auto; refinement_solver4; basic_typing_solver6.
      apply termR_perserve_rR with (e:= random_inhabitant τ2); refinement_solver4.
      eapply let_value_in_lc_termR_drop; eauto; reduction_solver2.
Qed.

Lemma rRctx_last_underbase_not_err_implies_drop: forall Γ st τ x b n d ϕ e,
    not_overbasety τ ->
    ¬ ({st}⟦[v:b|n|d|ϕ]⟧{Γ} terr) ->
    ok_dctx (dom _ st) (Γ ++ [(x, [v:b|n|d|ϕ])]) ->
    (* closed_rty 0 (ctxdom Γ ∪ (dom _ st)) τ -> *)
    {st}⟦τ⟧{Γ} e -> {st}⟦τ⟧{Γ ++ [(x, [v:b|n|d|ϕ])] } e.
Proof.
  induction Γ; simpl; intros; auto.
  - constructor.
    + refinement_solver.
    + denotation_simp; refinement_solver.
    + RD_simp; denotation_simp; refinement_solver.
    + RD_simp; denotation_simp; refinement_solver.
    + exists (random_inhabitant [v:b|n|d|ϕ]). split.
      apply random_inhabitant_in_any_under; refinement_solver4.
      intros. RD_simp.
      eapply not_empty_ctxrR_inhabitant_err_implies_halt in H0; eauto.
      assert (({0;b∅;st}⟦τ⟧) (tlete e_x (x \t\ e))).
      admit.
      admit.
      denotation_simp; refinement_solver4.
  - invclear H2.
    + constructor; auto; try (denotation_simp; refinement_solver4).
      { rewrite app_comm_cons.
        apply basic_typing_weaken_tm_pre; auto. constructor; basic_typing_solver6. denotation_simp.
        admit. }
      intros. apply H11 in H2.
      apply IHΓ; auto. admit. invclear H1; refinement_solver.
    + mydestr. constructor; auto; refinement_solver.
      admit.
      exists x1. split; auto. intros.
      eapply H3 in H4; eauto.
      apply IHΓ; auto. admit. invclear H1; refinement_solver.
      eapply (ok_dctx_trans _ (dom aset st)); simpl; eauto. admit.
      refinement_solver4.
Admitted.

Lemma rRctx_last_not_underbase_implies_drop: forall Γ st τ x r e,
    not_overbasety τ ->
    not_underbasety r ->
    ok_dctx (dom _ st) (Γ ++ [(x, r)]) ->
    {st}⟦τ⟧{Γ} e -> {st}⟦τ⟧{Γ ++ [(x, r)] } e.
Proof.
  induction Γ; simpl; intros; auto.
  - destruct (classic (not_overbasety r)).
    + constructor; auto.
      { denotation_simp; refinement_solver. }
      { RD_simp; denotation_simp; refinement_solver. }
      exists (random_inhabitant r). split.
      apply random_inhabitant_in_any_under; refinement_solver4.
      intros. RD_simp. admit.
    + refinement_simp1. RD_simp. constructor; auto.
      { denotation_simp; refinement_solver. }
      { RD_simp; denotation_simp; refinement_solver. }
      intros. constructor. admit.
  - invclear H2.
    + constructor; auto; refinement_solver. admit. admit.
      intros. apply H11 in H2.
      apply IHΓ; auto. admit.
    + mydestr. constructor; auto; refinement_solver. admit. admit.
      exists x1. split; auto. intros.
      eapply H3 in H4; eauto.
      apply IHΓ; auto. admit.
Admitted.

Lemma rRctx_pre_weakening: forall Γ2 Γ1 τ,
    not_overbasety τ ->
    (Γ1 ++ Γ2) ⊢WF τ -> (forall e, ⟦τ⟧{Γ1} e -> ⟦τ⟧{Γ1 ++ Γ2} e).
Proof.
  apply (rev_ind (fun Γ2 => forall Γ1 τ,
                      not_overbasety τ ->
                      (Γ1 ++ Γ2) ⊢WF τ -> (forall e, ⟦τ⟧{Γ1} e -> ⟦τ⟧{Γ1 ++ Γ2} e)));
    intros; denotation_simp; auto.
  - invclear H1. invclear H3; listctx_set_simpl. admit. admit.
Admitted.

Lemma rRctx_weakening_empty: forall Γ τ, Γ ⊢WF τ -> (forall e, ⟦τ⟧ e -> ⟦τ⟧{Γ} e).
Admitted.

(* Lemma rRctx_backward_over: forall Γ z b n d ϕ (e: tm) τ, *)
(*     not_overbasety τ -> *)
(*     (forall st, {st}⟦ τ ⟧{ Γ ++ [(z, {v: b | n | d | ϕ})] } e) <-> *)
(*       (forall st, *)
(*           (closed_rty 0 (ctxdom (Γ ++ [(z, {v: b | n | d | ϕ})]) ∪ dom aset st) τ /\ *)
(*              ok_dctx (dom aset st) (Γ ++ [(z, {v: b | n | d | ϕ})]) /\ *)
(*              (⌊Γ ++ [(z, {v: b | n | d | ϕ})]⌋ ⊢t e ⋮t ⌊τ⌋ /\ *)
(*                 (forall (u: constant), {st}⟦ {v: b | n | d | ϕ} ⟧{ Γ } u -> {st}⟦ { z := u }r τ ⟧{ Γ } ({ z := u }t e)))). *)
(* Proof. *)
(*   induction Γ; split; simpl; intros; denotation_simp. *)
(*   - specialize (H0 st). RD_simp. split; denotation_simp. split; auto. split; auto. *)
(*     intros. RD_simp. apply H13 in H1. RD_simp. *)
(*     rewrite denotation_st_update_iff_subst in H0; mydestr; auto; refinement_solver3. *)
(*   - specialize (H0 st); mydestr. constructor; try refinement_solver. intros. *)
(*     assert (({st}⟦{v:b|n|d|ϕ}⟧{ [] }) c_x) as HH by (constructor; auto). *)
(*     apply H3 in HH. RD_simp. *)
(*     rewrite denotation_st_update_iff_subst; refinement_solver3. *)
(*     split; refinement_solver3. *)
(*   - specialize (H0 st). *)
(*     assert (ok_dctx (dom aset st) ((a, r) :: Γ ++ [(z, {v:b|n|d|ϕ})])) as Hok by *)
(*     (apply ctxrR_regular1 in H0; mydestr; auto). *)
(*     invclear H0; denotation_simp. *)
(*     + do 3 (split; auto). intros. invclear H0. *)
(*       { apply ctxrR_cons_over; denotation_simp; auto. admit. admit. *)
(*         intros. *)
(*         assert (({<[a:=c_x]> st}⟦{v:b|0|d|ϕ}⟧{Γ}) ({a := c_x}v u)) by auto. *)
(*         assert ({<[a:=c_x]> st}⟦τ⟧{ Γ ++ [(z, {v:b|0|d|ϕ})] } ({a := c_x}t e)) by auto. *)
(*         rewrite IHΓ in H3; mydestr; auto. *)
(*         apply H8 in H1. lc_simpl. rewrite subst_commute_tm; simpl; auto; refinement_solver. *)
(*         admit. fast_set_solver. *)
(*       } *)
(*       { mydestr. constructor; denotation_simp; auto. } *)
(*     + do 3 (split; auto). intros. *)
(*       invclear H2. *)
(*       { apply ctxrR_cons_over; denotation_simp; auto. } *)
(*       { mydestr. constructor; denotation_simp; auto. admit. admit. *)
(*         exists x. *)
(*         (* assert (({0;b∅;st}⟦r⟧) (random_inhabitant r)) as Hzz. admit. *) *)
(*         split; auto. intros. eapply H1 in H6; eauto. *)
(*         rewrite IHΓ in H6; mydestr; auto. *)
(*         assert (({({a↦v_x}) st}⟦{v:b|0|d|ϕ}⟧{Γ}) u) as Hu. admit. *)
(*         apply H16 in Hu. simpl in Hu. admit. *)
(*       } *)
(*   - destruct (classic (not_overbasety r)). *)
(*     + constructor; try refinement_solver3. admit. *)
(*       (* exists (random_inhabitant r). split. admit. intros. *) *)
(*       (* rewrite IHΓ; auto. split. admit. split. admit. split. admit. *) *)
(*       (* intros. *) *)
(*       (* assert (({st}⟦{v:b|n|d|ϕ}⟧{(a, r) :: Γ}) u) as Hu. *) *)
(*       (* { constructor; auto. admit. admit. admit. admit. } *) *)
(*       (* apply H3 in Hu. invclear Hu. denotation_simp. mydestr. *) *)
(*     + refinement_simp1. apply ctxrR_cons_over; denotation_simp; auto. *)
(*       intros. *)
(*       rewrite IHΓ; auto. split. admit. split. admit. split. admit. *)
(*       intros. *)
(*       assert (({st}⟦{v:b|n|d|ϕ}⟧{(a, {v:x|x0|x1|x2}) :: Γ}) u) as Hu. *)
(*       { constructor. admit. admit. admit. admit. } *)

(*       auto_ty_exfalso. apply ctxrR_cons_over; try refinement_solver3. *)
(*       intros. rewrite IHΓ. split. admit. split. admit. split. admit. *)
(*       intros. admit. *)
(* Admitted. *)

Lemma rRctx_backward_over_forward: forall Γ st z b n d ϕ (e: tm) τ,
    not_overbasety τ ->
    {st}⟦ τ ⟧{ Γ ++ [(z, {v: b | n | d | ϕ})] } e ->
      (closed_rty 0 (ctxdom (Γ ++ [(z, {v: b | n | d | ϕ})]) ∪ dom aset st) τ /\
         ok_dctx (dom aset st) (Γ ++ [(z, {v: b | n | d | ϕ})]) /\
         (⌊Γ ++ [(z, {v: b | n | d | ϕ})]⌋*) ⊢t e ⋮t ⌊τ⌋ /\
            (forall (u: constant), {st}⟦ {v: b | n | d | ϕ} ⟧{ Γ } u -> {st}⟦ { z := u }r τ ⟧{ Γ } ({ z := u }t e))).
Proof.
  induction Γ; simpl; intros; denotation_simp.
  - RD_simp. split; denotation_simp. split; auto. split; auto.
    intros. RD_simp. apply H13 in H1. RD_simp.
    rewrite denotation_st_update_iff_subst in H0; mydestr; auto; refinement_solver3.
  - assert (ok_dctx (dom aset st) ((a, r) :: Γ ++ [(z, {v:b|n|d|ϕ})])) as Hok by
    (apply ctxrR_regular1 in H0; mydestr; auto).
    invclear H0; denotation_simp.
    + do 3 (split; auto). intros. invclear H0.
      { apply ctxrR_cons_over; denotation_simp; auto. admit. admit.
        intros.
        assert (({<[a:=c_x]> st}⟦{v:b|0|d|ϕ}⟧{Γ}) ({a := c_x}v u)) by auto.
        assert ({<[a:=c_x]> st}⟦τ⟧{ Γ ++ [(z, {v:b|0|d|ϕ})] } ({a := c_x}t e)) by auto.
        apply IHΓ in H3; mydestr; auto.
        apply H8 in H1. lc_simpl. rewrite subst_commute_tm; simpl; auto; refinement_solver.
        admit. fast_set_solver.
      }
      { mydestr. constructor; denotation_simp; auto. }
    + do 3 (split; auto). intros.
      invclear H2.
      { apply ctxrR_cons_over; denotation_simp; auto. }
      { mydestr. constructor; denotation_simp; auto. admit. admit.
        exists x.
        (* assert (({0;b∅;st}⟦r⟧) (random_inhabitant r)) as Hzz. admit. *)
        split; auto. intros. eapply H1 in H6; eauto.
        apply IHΓ in H6; mydestr; auto.
        assert (({({a↦v_x}) st}⟦{v:b|0|d|ϕ}⟧{Γ}) u) as Hu. admit.
        apply H16 in Hu. simpl in Hu. admit.
      }
Admitted.

(* Lemma rRctx_backward_over: forall Γ st z b n d ϕ (e: tm) τ, *)
(*     not_overbasety τ -> *)
(*     {st}⟦ τ ⟧{ Γ ++ [(z, {v: b | n | d | ϕ})] } e <-> *)
(*       (closed_rty 0 (ctxdom (Γ ++ [(z, {v: b | n | d | ϕ})]) ∪ dom aset st) τ /\ *)
(*          ok_dctx (dom aset st) (Γ ++ [(z, {v: b | n | d | ϕ})]) /\ *)
(*          (⌊Γ ++ [(z, {v: b | n | d | ϕ})]⌋ ⊢t e ⋮t ⌊τ⌋ /\ *)
(*             (forall (u: constant), {st}⟦ {v: b | n | d | ϕ} ⟧{ Γ } u -> {st}⟦ { z := u }r τ ⟧{ Γ } ({ z := u }t e))). *)
(* Proof. *)
(*   induction Γ; split; simpl; intros; denotation_simp. *)
(*   - RD_simp. split; denotation_simp. split; auto. split; auto. *)
(*     intros. RD_simp. apply H13 in H1. RD_simp. *)
(*     rewrite denotation_st_update_iff_subst in H0; mydestr; auto; refinement_solver3. *)
(*   - constructor; try refinement_solver. intros. *)
(*     assert (({st}⟦{v:b|n|d|ϕ}⟧{ [] }) c_x) as HH by (constructor; auto). *)
(*     apply H3 in HH. RD_simp. *)
(*     rewrite denotation_st_update_iff_subst; refinement_solver3. *)
(*     split; refinement_solver3. *)
(*   - assert (ok_dctx (dom aset st) ((a, r) :: Γ ++ [(z, {v:b|n|d|ϕ})])) as Hok by *)
(*     (apply ctxrR_regular1 in H0; mydestr; auto). *)
(*     invclear H0; denotation_simp. *)
(*     + do 3 (split; auto). intros. invclear H0. *)
(*       { apply ctxrR_cons_over; denotation_simp; auto. admit. admit. *)
(*         intros. *)
(*         assert (({<[a:=c_x]> st}⟦{v:b|0|d|ϕ}⟧{Γ}) ({a := c_x}v u)) by auto. *)
(*         assert ({<[a:=c_x]> st}⟦τ⟧{ Γ ++ [(z, {v:b|0|d|ϕ})] } ({a := c_x}t e)) by auto. *)
(*         rewrite IHΓ in H3; mydestr; auto. *)
(*         apply H8 in H1. lc_simpl. rewrite subst_commute_tm; simpl; auto; refinement_solver. *)
(*         admit. fast_set_solver. *)
(*       } *)
(*       { mydestr. constructor; denotation_simp; auto. } *)
(*     + do 3 (split; auto). intros. *)
(*       invclear H2. *)
(*       { apply ctxrR_cons_over; denotation_simp; auto. } *)
(*       { mydestr. constructor; denotation_simp; auto. admit. admit. *)
(*         exists x. *)
(*         (* assert (({0;b∅;st}⟦r⟧) (random_inhabitant r)) as Hzz. admit. *) *)
(*         split; auto. intros. eapply H1 in H6; eauto. *)
(*         rewrite IHΓ in H6; mydestr; auto. *)
(*         assert (({({a↦v_x}) st}⟦{v:b|0|d|ϕ}⟧{Γ}) u) as Hu. admit. *)
(*         apply H16 in Hu. simpl in Hu. admit. *)
(*       } *)
(*   - destruct (classic (not_overbasety r)). *)
(*     + constructor; try refinement_solver3. admit. *)
(*       (* exists (random_inhabitant r). split. admit. intros. *) *)
(*       (* rewrite IHΓ; auto. split. admit. split. admit. split. admit. *) *)
(*       (* intros. *) *)
(*       (* assert (({st}⟦{v:b|n|d|ϕ}⟧{(a, r) :: Γ}) u) as Hu. *) *)
(*       (* { constructor; auto. admit. admit. admit. admit. } *) *)
(*       (* apply H3 in Hu. invclear Hu. denotation_simp. mydestr. *) *)
(*     + refinement_simp1. apply ctxrR_cons_over; denotation_simp; auto. *)
(*       intros. *)
(*       rewrite IHΓ; auto. split. admit. split. admit. split. admit. *)
(*       intros. *)
(*       assert (({st}⟦{v:b|n|d|ϕ}⟧{(a, {v:x|x0|x1|x2}) :: Γ}) u) as Hu. *)
(*       { constructor. admit. admit. admit. admit. } *)
(*       (* auto_ty_exfalso. apply ctxrR_cons_over; try refinement_solver3. *) *)
(*       (* intros. rewrite IHΓ. split. admit. split. admit. split. admit. *) *)
(*       (* intros. admit. *) *)
(* Admitted. *)


(* Lemma rRctx_backward_over: forall Γ st z b n d ϕ (e: tm) τ, *)
(*     not_overbasety τ -> *)
(*     {st}⟦ τ ⟧{ Γ ++ [(z, {v: b | n | d | ϕ})] } e <-> *)
(*       (closed_rty 0 (ctxdom (Γ ++ [(z, {v: b | n | d | ϕ})]) ∪ dom aset st) τ /\ *)
(*          ok_dctx (dom aset st) (Γ ++ [(z, {v: b | n | d | ϕ})]) /\ *)
(*          (⌊Γ ++ [(z, {v: b | n | d | ϕ})]⌋ ⊢t e ⋮t ⌊τ⌋ /\ *)
(*             (forall (u: value), {st}⟦ {v: b | n | d | ϕ} ⟧{ Γ } u -> {st}⟦ { z := u }r τ ⟧{ Γ } ({ z := u }t e))). *)
(* Proof. *)
(*   induction Γ; split; simpl; intros; denotation_simp. *)
(*   - RD_simp. split; denotation_simp. split; auto. split; auto. *)
(*     intros. RD_simp. apply H13 in H1. RD_simp. *)
(*     rewrite denotation_st_update_iff_subst in H0; mydestr; auto; refinement_solver3. *)
(*     (* eapply termR_perserve_rR; eauto; refinement_solver3. ctx_erase_simp4. *) *)
(*     (* apply termR_let_one_step_from_basic_type; auto. *) *)
(*     (* basic_typing_solver. refinement_solver3. refinement_solver. denotation_simp. *) *)
(*   - constructor; try refinement_solver. intros. *)
(*     assert (({st}⟦{v:b|n|d|ϕ}⟧{ [] }) c_x) as HH by (constructor; auto). *)
(*     apply H3 in HH. RD_simp. *)
(*     rewrite denotation_st_update_iff_subst; refinement_solver3. *)
(*     split; refinement_solver3. *)
(*     (* denotation_simp. *) *)
(*     (* eapply termR_perserve_rR; eauto; denotation_simp; refinement_solver3. *) *)
(*     (* apply termR_let_one_step_from_basic_type'; basic_typing_solver5. *) *)
(*     (* eapply let_store_tyable; eauto. *) *)
(*   - invclear H0; denotation_simp. *)
(*     + do 3 (split; auto). intros. invclear H0. *)
(*       { apply ctxrR_cons_over; denotation_simp; auto. admit. admit. *)
(*         intros. *)
(*         assert (({<[a:=c_x]> st}⟦{v:b|0|d|ϕ}⟧{Γ}) ({a := c_x}v u)) by auto. *)
(*         assert ({<[a:=c_x]> st}⟦τ⟧{ Γ ++ [(z, {v:b|0|d|ϕ})] } ({a := c_x}t e)) by auto. *)
(*         rewrite IHΓ in H3; mydestr; auto. *)
(*         apply H8 in H1. admit. *)
(*       } *)
(*       { admit. } *)
(*     + admit. *)
(*   - destruct (classic (not_overbasety r)). *)
(*     + constructor; try refinement_solver3. admit. *)
(*     + auto_ty_exfalso. apply ctxrR_cons_over; try refinement_solver3. *)
(*       intros. rewrite IHΓ. split. admit. split. admit. split. admit. *)
(*       intros. admit. *)
(* Admitted. *)

Lemma rRctx_backward_under_forward: forall Γ st z τ_z (e: tm) τ,
    not_overbasety τ ->
    ~ not_overbasety τ_z ->
    {st}⟦ τ ⟧{ Γ ++ [(z, τ_z)] } e ->
      (∃ (u_hat: tm), {st}⟦ τ_z ⟧{ Γ } u_hat /\
                        (forall (u: tm), {st}⟦ τ_z ⟧{ Γ } u ->
                                    (∀ (v_u: value), u_hat ↪* v_u -> {st}⟦ { z:= v_u }r τ ⟧{ Γ } (tlete u (e ^t^ z)))
                        )).
Admitted.

(* Lemma random_inhabitant_in_any_under: ∀ (τ: rty) (bst : bstate) n st, *)
(*     not_overbasety τ -> *)
(*     closed_rty n (dom _ st) τ -> *)
(*     ({n;bst;st}⟦τ⟧) (random_inhabitant τ). *)
(* Proof. *)
(*   induction τ; intros; invclear H; mydestr. *)
(*   - destruct B. *)
(*     + split; simpl; auto. admit. split; auto. intros. admit. *)
(*     + split; simpl; auto. apply mk_bool_gen_typable; auto. split; auto. intros. destruct c; invclear H. *)
(*       apply mk_bool_gen_reduce_to_all_bool. *)
(*   - denotation_simp. constructor; auto. admit. constructor; auto. *)
(*     intros. *)
(*     apply (termR_perserve_rR _ (random_inhabitant τ)); auto; try refinement_solver1. admit. *)
(*     apply IHτ; refinement_solver1. *)
(*   - denotation_simp. constructor; auto. admit. constructor; auto. *)
(*     intros. *)
(*     assert (exists (v_x: value), e_x ↪* v_x). admit. destruct H1. *)
(*     apply (termR_perserve_rR _ (random_inhabitant τ2)); auto; try refinement_solver1. admit. *)
(*     apply IHτ2; refinement_solver1. *)
(* Admitted. *)

(* Lemma denotation_weaken_empty: forall Γ (e: tm) τ (st: state), *)
(*     not_overbasety τ -> *)
(*     valid_rty τ -> *)
(*     ok_dctx (dom _ st) Γ -> { st }⟦ τ ⟧ e -> {st}⟦ τ ⟧{ Γ } e. *)
(* Proof. *)
(*   induction Γ; intros; denotation_simp; mydestr. *)
(*   constructor; auto. *)
(*   destruct (classic (not_overbasety r)). *)
(*   - constructor; auto; try refinement_solver1. *)
(*     exists (random_inhabitant r); invclear H1; *)
(*     assert (({0;b∅;st}⟦r⟧) (random_inhabitant r)) by *)
(*       (intros; apply random_inhabitant_in_any_under; auto); *)
(*     split; try (apply random_inhabitant_in_any_under; auto); intros. *)
(*     + assert ([] ⊢t v_x ⋮v ⌊r⌋) as Hv_x by refinement_solver1. *)
(*       destruct v_x; mydestr; simpl; try auto_ty_exfalso. *)
(*       apply (termR_perserve_ctxrR _ _ e); auto. admit. *)
(*       apply IHΓ; auto. denotation_simp. intros. rewrite rR_shadow_update_st; auto; refinement_solver1. *)
(*       apply rR_regular1 in H2; mydestr. refinement_solver1. *)
(*     + apply (termR_perserve_ctxrR _ _ e); auto. admit. *)
(*       apply IHΓ; auto. denotation_simp. admit. admit. *)
(*   - refinement_solver1. constructor; auto; try refinement_solver1. *)
(*     intros. apply (termR_perserve_ctxrR _ _ e); auto. admit. *)
(*     invclear H1; apply IHΓ; auto; try auto_ty_exfalso; try ok_dctx_solver_slow. *)
(*     intros. rewrite rR_shadow_update_st; auto; refinement_solver1. *)
(*     apply rR_regular1 in H2; mydestr. refinement_solver1. *)
(* Admitted. *)

(* Lemma denotation_weaken: forall Γ1 Γ3 (e: tm) τ (st: state) Γ2, *)
(*     ctxrR_wf (dom _ st) (Γ1 ++ Γ2 ++ Γ3) τ -> {st}⟦ τ ⟧{ Γ1 ++ Γ3 } e -> {st}⟦ τ ⟧{ Γ1 ++ Γ2 ++ Γ3 } e. *)
(* Proof. *)
(*   intros. generalize dependent Γ2. *)
(*   induction H0; intros; listctx_set_simpl. *)
(*   - specialize (H b∅). invclear H; mydestr. denotation_simp. *)
(*   induction Γ1; intros; listctx_set_simpl. *)
(*   - admit. *)
(*   -  *)

(* Lemma err_exists_denotation_excluded: forall Γ b n (d: aset) (ϕ: refinement), *)
(*     closed_rty 0 (dom aset (∅: state)) [v:b|n|d|ϕ] -> *)
(*     ~ (⟦ [v: b | n | d | ϕ ] ⟧{ Γ } terr) -> (∃ (v: value), ⟦ {v: b | n | d | ϕ } ⟧{ Γ } v). *)
(* Proof. *)
(*   induction Γ; intros. *)
(*   - destruct (classic (∃ v : value, (⟦{v:b|n|d|ϕ}⟧{ [] }) v)); auto; exfalso; apply H0; clear H0. *)
(*     rewrite forall_iff_not_exists1 in H1. *)
(*     constructor. intros. *)
(*     do 2 constructor; auto. rewrite terr_reduction_iff_false_phi. *)
(*     intros. intros Hf. *)
(*     specialize (H1 c). apply H1. *)
(*     constructor; auto. intros. constructor; auto. constructor; auto. *)
(*     rewrite closed_rty_under_iff_over; auto. *)
(*     exists c. constructor; auto. constructor; auto. *)
(*     rewrite bound_in_refinement_0. apply Hf. refinement_solver1. *)
(*   - mydestr. refinement_simp1. *)
(*     destruct (classic (∃ v : value, (⟦{v:b|n|d|ϕ}⟧{ a :: Γ }) v)); auto; exfalso; apply H0; clear H0. *)
(*     rewrite forall_iff_not_exists1 in H1. *)
(*     mydestr. *)
(*     constructor. *)


(*     repeat match goal with *)
(*            | [H: closed_rty _ _ _ |- bound_in_refinement _ _ ] => destruct H; mydestr *)
(*            | [H: valid_rty _ |- bound_in_refinement _ _ ] => invclear H *)
(*            | [H: wf_r _ _ ?ϕ |- bound_in_refinement _ ?ϕ ] =>  destruct H *)
(*            end. *)
(*     refinement_simp1; auto. *)
(*     repeat match goal with *)
(*     | [H: lc_rty_idx 0 [v:_|?n|_|_] |- _ ] => *)
(*         match n with *)
(*         | 0 => fail 1 *)
(*         | _ =>  assert (n = 0) by (apply lc_rty_idx_under_0_is_0 in H; auto); subst *)
(*         end *)
(*     end. *)
(*     repeat match goal with *)
(*            | [H: closed_rty _ _ _ |- bound_in_refinement _ _ ] => destruct H; mydestr *)
(*            | [H: valid_rty _ |- bound_in_refinement _ _ ] => invclear H *)
(*            | [H: wf_r _ _ ?ϕ |- bound_in_refinement _ ?ϕ ] =>  destruct H *)
(*            end. *)
(*     assert (n = 0) by (apply lc_rty_idx_under_0_is_0 in H2; auto); subst. *)
(*     invclear H2. *)
(*     destruct H5.  *)
(*     invclear H. *)
(*  mydestr. invclear H. invclear H6. unfold bound_in_refinement in H5. *)
(*     assert (closed_rty 0 ∅ [v:b|n|d|ϕ]). setoid_rewrite <- H2; auto. *)

(*     setoid_rewrite H2 in H. *)

(*     admit. auto. *)
(*     intros. apply multistep_R. *)

(*     constructor. *)
(*     exfalso. apply H. constructor; intros. rewrite terr_inhabitant_iff_false_phi. *)


(*   intros. destruct (classic (∃ v : value, (⟦[v:b|n|d|ϕ]⟧{Γ}) v)); auto. *)
(*   exfalso. apply H. rewrite terr_inhabitant_iff_false_phi. *)

(*   intro Hf. mydestr. invclear Hf. *)
(*     + invclear H. denotation_simp. *)
(*       match goal with *)
(*       | [H: ∀ c, [] ⊢t (vconst c) ⋮v _ → _ _ ∅ c → terr ↪* (tvalue (vconst c)) |- _ ] => idtac H *)
(*       end. *)
(*       rewrite terr_inhabitant_implies_false_phi in H4. *)
(*       apply H4 in H7. *)
(*       repeat match goal with *)
(*       | [H: _ ⊢t ?v ⋮v (TBase _) |- _ ] => *)
(*           match v with *)
(*           | vconst _ => fail 1 *)
(*           | _ => set H7 as Htmp; apply empty_basic_typing_base_const_exists in Htmp; mydestr; subst *)
(*           end *)
(*       end. *)
(*       set H7 as Htmp; apply empty_basic_typing_base_const_exists in Htmp; mydestr; subst. *)
(*       match goal with *)
(*       | [H: _ ⊢t (tvalue ?v) ⋮t _ |- _ ] => invclear H *)
(*       end. *)

(*       apply empty_basic_typing_base_const_exists in H. *)

(*        specialize (H1 b∅). *)
(*       invclear H0. invclear H1. mydestr. *)
(* Admitted. *)
