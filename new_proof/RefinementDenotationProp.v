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

Inductive wf_ctxrR: state -> listctx rty -> Prop :=
| wf_ctxrR_nil: forall st τ, wf_ctxrR st τ
| ctxrR_cons_over: forall st (x: atom) B n d ϕ Γ,
    ok_dctx (dom _ st) ((x, {v: B | n | d | ϕ}) :: Γ) ->
    (exists (c_wf: constant), {st}⟦ {v: B | n | d | ϕ} ⟧ c_wf) ->
    (forall (c_x: constant), {st}⟦ {v: B | n | d | ϕ} ⟧ c_x ->
                        wf_ctxrR (<[ x := c_x ]> st) Γ) ->
    wf_ctxrR st ((x, {v: B | n | d | ϕ}) :: Γ)
| ctxrR_cons_under: forall st (x: atom) τ_x Γ,
    not_overbasety τ_x ->
    ok_dctx (dom _ st) ((x, τ_x) :: Γ) ->
    (forall e_wf, {st}⟦ τ_x ⟧ e_wf -> (exists (v_wf: value), e_wf ↪* v_wf)) ->
    (exists e_x_hat, {st}⟦ τ_x ⟧ e_x_hat /\
                  (forall e_x, {st}⟦ τ_x ⟧ e_x ->
                          (∀ (v_x: value), e_x_hat ↪* v_x ->
                                           wf_ctxrR ({ x ↦ v_x } st) Γ ))) ->
     wf_ctxrR st ((x, τ_x) :: Γ).

(* Global Hint Resolve terr_is_not_inhabitant_of_overbase: core. *)

(* Lemma ctx_trans_terr_inhabitant: forall Γ st τ x τ_x, *)
(*     ok_dctx (dom aset st) ((x, τ_x) :: Γ) -> *)
(*     ¬ ({st}⟦ τ ⟧{(x, τ_x) :: Γ}) terr -> ¬ ({st}⟦ τ ⟧{ Γ }) terr. *)
(* Proof. *)
(*   induction Γ; intros. *)
(*   - neg_apply H0. RD_simp. *)
(*     destruct (classic (not_overbasety τ_x)). *)
(*     + constructor; refinement_solver5. *)
(*       exists (random_inhabitant τ_x). split. *)
(*       apply random_inhabitant_in_any_under; refinement_solver4. *)
(*       intros. constructor. *)
(*       assert (({0;b∅;st}⟦τ⟧) (tlete e_x (x \t\ terr))). admit. *)
(*       assert ([] ⊢t (random_inhabitant τ_x) ⋮t ⌊ τ_x ⌋); auto. *)
(*       assert ([] ⊢t v_x ⋮v ⌊ τ_x ⌋); reduction_solver2. *)
(*       destruct τ_x; auto_ty_exfalso2; try (simpl in H3; reduction_simpl1; auto). *)
(*       invclear H6; refinement_solver5. *)
(*       setoid_rewrite rR_shadow_update_st; auto; refinement_solver5. *)
(*       refinement_solver5. *)
(*     + RD_simp. denotation_simp1. auto_ty_exfalso2. *)
(*       constructor; refinement_solver5. *)
(*       intros. constructor. invclear H1; mydestr; subst. reduction_solver2. *)
(*       setoid_rewrite rR_shadow_update_st_c; auto; refinement_solver5. *)
(*       refinement_solver5. *)
(*   - mydestr. neg_apply H0. RD_simp. destruct (classic (not_overbasety τ_x)). *)
(*     + constructor; refinement_solver5. *)
(*       exists (random_inhabitant τ_x). *)
(*       assert (({0;b∅;st}⟦τ_x⟧) (random_inhabitant τ_x)) by *)
(*         (apply random_inhabitant_in_any_under; refinement_solver5). *)
(*       split; auto. intros. *)
(*       match goal with *)
(*       | [H: ({_}⟦?τ⟧{ _ }) _ |- closed_rty _ _ ?τ] => *)
(*           apply ctxrR_regular in H; mydestr; refinement_solver4 *)
(*       end. *)
(*       apply ctxrR_regular in H1. mydestr. refinement_solver5. *)
(*       constructor. admit. constructor. admit. *)
(*       { constructor; refinement_solver5. *)
(*       } *)
(*       constructor; refinement_solver5. *)

Lemma not_terr_inhabitant_implies_halt: forall Γ st τ,
    not_overbasety τ ->
    ¬ ({st}⟦ τ ⟧{Γ} terr) -> (forall e, {st}⟦ τ ⟧{Γ} e -> ∃ (v: value), e ↪* v).
Proof.
  intros. neg_apply H0.
  eapply termR_perserve_ctxrR; eauto; refinement_solver5. admit.
  eapply stuck_tm_termR_terr.


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
      eapply not_empty_ctxrR_inhabitant_err_implies_halt in H0; eauto; refinement_solver5.
      assert (({0;b∅;st}⟦τ⟧) (tlete e_x (x \t\ e))).
      eapply termR_perserve_rR; eauto; refinement_solver5.
      eapply termR_tlete_drop_halt_lhs'; eauto; refinement_solver5.
      setoid_rewrite rR_shadow_update_st; auto; refinement_solver5.
  - invclear H2.
    + constructor; auto; refinement_solver.
      { rewrite app_comm_cons.
        apply basic_typing_weaken_tm_pre; auto. constructor; basic_typing_solver6. denotation_simp.
        apply ok_dctx_regular2 in H1; mydestr. refinement_solver5. }
      intros. apply H11 in H2.
      (* assert (({<[x0:=c_x]> st}⟦τ⟧{Γ}) ({x0 := c_x }t e)); auto. *)
      apply IHΓ; auto.
      neg_apply H0. constructor; refinement_solver6. admit.
      intros. reduction_solver2.
      setoid_rewrite ctxrR_shadow_update_st_c; auto; refinement_solver.
      apply ctx_trans_terr_inhabitant in H0; refinement_solver6.
      setoid_rewrite ctxrR_shadow_update_st_c; auto; refinement_solver. admit.
      invclear H1; refinement_solver.
      invclear H1; refinement_solver.
      setoid_rewrite ctxrR_shadow_update_st_c; auto; refinement_solver.
      admit. invclear H1; refinement_solver.
    + mydestr. constructor; auto; refinement_solver. denotation_simp.
      { apply ok_dctx_regular2 in H1; mydestr.
        rewrite app_comm_cons. eapply basic_typing_weaken_tm_pre; auto.
        rewrite ctx_erase_perserve_ok in H8. simpl in H8.
        ctx_erase_simp.
      }
      exists x1. split; auto. intros.
      eapply H3 in H4; eauto.
      apply IHΓ; auto. admit. invclear H1; refinement_solver.
      eapply (ok_dctx_trans _ (dom aset st)); simpl; eauto.
      apply ok_dctx_regular2 in H18; mydestr.
      assert (dom aset (({x0↦v_x}) st) ⊆ {[x0]} ∪ dom aset st); auto.
      listctx_set_simpl. set_solver.
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
