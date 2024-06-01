From stdpp Require Import mapset.
From CT Require Import CoreLangClass.
From CT Require Import OperationalSemantics.
From CT Require Import BasicTypingClass.
From CT Require Import QualifierClass.
From CT Require Import RefinementTypeClass.
From CT Require Import Denotation.
From CT Require Import Instantiation.
From CT Require Import DenotationCtx.

Import Atom.
Import CoreLang.
Import Tactics.
Import NamelessTactics.
Import OperationalSemantics.
Import BasicTyping.
Import ListCtx.
Import Qualifier.
Import RefinementType.

(** This file proves auxiliary lemmas about multi-substitution and provides
  handy tactics. *)

Lemma closed_env_insert env x v :
  env !! x = None ->
  closed_env (<[x:=v]>env) ->
  closed v /\ closed_env env.
Proof.
  intro.
  unfold closed_env.
  rewrite map_Forall_insert; eauto.
Qed.

Ltac apply_msubst_ind :=
  unfold msubst;
  match goal with
  | |- ?T =>
      match T with
      | context [map_fold ?a ?b ?m] =>
          match eval pattern (map_fold a b m) in T with
          | ?P _ =>
              match eval pattern m in P with
              | ?P _ =>
                  let P := eval simpl in (fun r m => P m r) in
                    apply (map_fold_ind P)
              end
          end
      end
  end.

Ltac gen_closed_env :=
  repeat
    match goal with
    | H : closed_env (<[?i:=_]> ?m), H' : ?m !! ?i = None |- _ =>
        let H1 := fresh "H" in
        let H2 := fresh "H" in
        destruct (closed_env_insert _ _ _ H' H) as [H1 H2];
        uniq_hyp H1; uniq_hyp H2
    | H : closed_env ?m, H' : ?m !! _ = Some ?v |- _ =>
        let T := fresh "H" in
        assert (closed v) as T by eauto;
        uniq_hyp T
    (* | H : ctxRst _ ?env |- _ => *)
    (*     let T := fresh "H" in *)
    (*     assert (closed_env env) as T by eauto using ctxRst_closed_env; *)
    (*     uniq_hyp T *)
    end.

Lemma msubst_insert {T: Type}
  (f_subst: atom -> value -> T -> T)
  (subst_commute: forall x u_x y u_y e,
      x <> y -> x ∉ fv_value u_y -> y ∉ fv_value u_x ->
      f_subst x u_x (f_subst y u_y e) =
        f_subst y u_y (f_subst x u_x e))
  :
  forall (Γv: env) (x: atom) (v_x: value) (e: T),
    closed_env Γv ->
    closed v_x ->
    Γv !! x = None ->
    msubst f_subst (<[x:=v_x]> Γv) e = f_subst x v_x (msubst f_subst Γv e).
Proof.
  intros.
  apply map_fold_insert_L; eauto.
  intros.
  assert (closed_env (<[x:=v_x]>Γv)). {
    apply map_Forall_insert; eauto.
  }
  gen_closed_env.
  apply subst_commute; eauto; my_set_solver.
Qed.

Ltac msubst_tac :=
  intros *; apply_msubst_ind; intros; subst; simpl; eauto;
  gen_closed_env; simp_hyps; subst.

Ltac fold_msubst := change (map_fold ?s ?e ?m) with (msubst s m e) in *.

Ltac rewrite_msubst_insert :=
  cbn; fold_msubst; rewrite !msubst_insert;
  intros; eauto; try (fold_nameless; rewrite subst_commute; eauto).

Lemma rty_erase_msubst_eq ρ Γv :
  rty_erase ρ = rty_erase (m{Γv} ρ).
Proof.
  msubst_tac.
  qauto using rty_erase_subst_eq.
Qed.

Lemma msubst_qualifier: forall Γv ϕ,
    closed_env Γv ->
    (m{Γv}) ϕ =
      match ϕ with
      | qual vals prop =>
          qual (vmap (m{Γv}) vals) prop
      end.
Proof.
  msubst_tac.
  - destruct ϕ.
    f_equal.
    erewrite Vector.map_ext.
    by rewrite Vector.map_id.
    intros. simpl.
    by rewrite map_fold_empty.
  - destruct ϕ. unfold substitute. simpl. f_equal.
    rewrite Vector.map_map.
    apply Vector.map_ext.
    intros; rewrite_msubst_insert.
Qed.

Lemma msubst_qualifier_and Γv q1 q2 :
  closed_env Γv ->
  m{Γv} (q1 & q2) = (m{Γv} q1) & (m{Γv} q2).
Proof.
  msubst_tac.
  rewrite_msubst_insert.
  apply qualifier_and_subst.
Qed.

Lemma msubst_constant: forall Γv (c: constant), m{Γv} (vconst c) = c.
Proof.
  msubst_tac.
Qed.

Lemma msubst_bvar: forall Γv n, (m{Γv}) (vbvar n) = vbvar n.
Proof.
  msubst_tac.
Qed.

Lemma msubst_fvar: forall Γv (x : atom),
    closed_env Γv ->
    (m{Γv}) (vfvar x) = match Γv !! x with
                 | Some v => v
                 | None => x
                 end.
Proof.
  msubst_tac. destruct (decide (x = i)); subst; simplify_map_eq. nameless_eval. simplify_map_eq; auto.
  case_match.
  apply subst_fresh.
  gen_closed_env. my_set_solver.
  nameless_eval. rewrite decide_False; eauto.
Qed.

Lemma msubst_lam: forall Γv T e,
    closed_env Γv ->
    ((m{Γv}) (vlam T e)) = (vlam T ((m{Γv}) e)).
Proof.
  msubst_tac. rewrite_msubst_insert.
Qed.

Lemma msubst_fix: forall Γv T e,
    closed_env Γv ->
    ((m{Γv}) (vfix T e)) = (vfix T ((m{Γv}) e)).
Proof.
  msubst_tac. rewrite_msubst_insert.
Qed.

Lemma msubst_value: forall Γv (v:value),
    closed_env Γv ->
    (m{Γv}) (treturn v) = (m{Γv}) v.
Proof.
  msubst_tac. rewrite_msubst_insert.
Qed.

Lemma msubst_match: forall Γv (v: value) e1 e2,
    closed_env Γv ->
    ((m{Γv}) (tmatchb v e1 e2)) = tmatchb (m{Γv} v) (m{Γv} e1) (m{Γv} e2).
Proof.
  msubst_tac. rewrite_msubst_insert.
Qed.

Lemma msubst_lete: forall (Γv: env) e_x e,
    closed_env Γv ->
    (m{Γv} (tlete e_x e) = tlete ((m{Γv}) e_x) ((m{Γv}) e)).
Proof.
  msubst_tac. rewrite_msubst_insert.
Qed.

Lemma msubst_tletop: forall Γv op (v2: value) e,
    closed_env Γv ->
    (m{Γv}) (tletop op v2 e) = (tletop op (m{Γv} v2) (m{Γv} e)).
Proof.
  msubst_tac. rewrite_msubst_insert.
Qed.

Lemma msubst_tletapp: forall Γv (v1 v2: value) e,
    closed_env Γv ->
    (m{Γv}) (tletapp v1 v2 e) = (tletapp (m{Γv} v1) (m{Γv} v2) (m{Γv} e)).
Proof.
  msubst_tac. rewrite_msubst_insert.
Qed.

Lemma msubst_baserty_over: forall Γv b ϕ,
    closed_env Γv ->
    (m{Γv}) {:b|ϕ} = {:b| (m{Γv}) ϕ}.
Proof.
  msubst_tac. rewrite_msubst_insert.
Qed.

Lemma msubst_baserty: forall Γv b ϕ,
    closed_env Γv ->
    (m{Γv}) [:b|ϕ] = [:b| (m{Γv}) ϕ].
Proof.
  msubst_tac. rewrite_msubst_insert.
Qed.

Definition msubst_baserty_under :=  msubst_baserty.

Lemma msubst_arrrty: forall Γv ρ τ,
    closed_env Γv ->
    ((m{Γv}) (ρ⇨τ)) = ((m{Γv} ρ)⇨(m{Γv} τ)).
Proof.
  msubst_tac. rewrite_msubst_insert.
Qed.

Tactic Notation "rewrite_msubst" constr(lem) "in" hyp(H) :=
  rewrite lem in H; eauto.

Tactic Notation "rewrite_msubst" constr(lem) :=
  rewrite lem in *; eauto.

Lemma msubst_mk_top: forall (Γv: env) b,
    closed_env Γv ->
    m{Γv} (mk_top b) = mk_top b.
Proof.
  intros.
  unfold mk_top, mk_q_under_top.
  rewrite_msubst msubst_baserty.
  rewrite_msubst msubst_qualifier.
Qed.

Lemma msubst_mk_eq_constant: forall (Γv: env) c,
    closed_env Γv ->
    (m{Γv}) (mk_eq_constant c) = (mk_eq_constant c).
Proof.
  intros.
  unfold mk_eq_constant, mk_q_bvar_eq_val.
  repeat rewrite_msubst msubst_baserty.
  repeat rewrite_msubst msubst_qualifier.
  simpl.
  repeat rewrite_msubst msubst_bvar.
  repeat rewrite_msubst msubst_constant.
Qed.

Lemma msubst_mk_eq_var: forall (Γv: env) b x v,
    closed_env Γv ->
    Γv !! x = Some v ->
    (m{Γv}) (mk_eq_var b x) = [:b | b0:v= v].
Proof.
  intros.
  unfold mk_eq_var.
  repeat rewrite_msubst msubst_baserty.
  repeat rewrite_msubst msubst_qualifier.
  simpl.
  repeat rewrite_msubst msubst_bvar.
  repeat rewrite_msubst msubst_fvar.
  rewrite H0.
  eauto.
Qed.

Ltac msubst_simp :=
  match goal with
  | H: context [ m{ _ } (tlete _ _) ] |- _ => rewrite msubst_lete in H
  | |- context [ m{ _ } (tlete _ _) ] => rewrite msubst_lete
  | H: context [ m{ _ } (tletop _ _ _) ] |- _ => rewrite msubst_tletop in H
  | |- context [ m{ _ } (tletop _ _ _) ] => rewrite msubst_tletop
  | H: context [ m{ _ } (tletapp _ _ _) ] |- _ => rewrite msubst_tletapp in H
  | |- context [ m{ _ } (tletapp _ _ _) ] => rewrite msubst_tletapp
  | H: context [ m{ _ } (vfix _ _) ] |- _ => rewrite msubst_fix in H
  | |- context [ m{ _ } (vfix _ _) ] => rewrite msubst_fix
  | H: context [ m{ _ } (treturn _) ] |- _ => rewrite msubst_value in H
  | |- context [ m{ _ } (treturn _) ] => rewrite msubst_value
  | H: context [ m{ _ } (vlam _ _) ] |- _ => rewrite msubst_lam in H
  | |- context [ m{ _ } (vlam _ _) ] => rewrite msubst_lam
  | H: context [ m{ _ } (tmatchb _ _ _) ] |- _ => rewrite msubst_match in H
  | |- context [ m{ _ } (tmatchb _ _ _) ] => rewrite msubst_match
  | H: context [ m{ _ } (vbvar _) ] |- _ => rewrite msubst_bvar in H
  | |- context [ m{ _ } (vbvar _) ] => rewrite msubst_bvar
  | H: context [ m{ _ } (vfvar _) ] |- _ => rewrite msubst_fvar in H
  | |- context [ m{ _ } (vfvar _) ] => rewrite msubst_fvar
  | H: context [ m{ _ } (vconst _) ] |- _ => rewrite msubst_constant in H
  | |- context [ m{ _ } (vconst _) ] => rewrite msubst_constant
  (* | H: context [ m{ _ } _ ] |- _ => rewrite msubst_qualifier in H *)
  (* | |- context [ m{ _ } _ ] => rewrite msubst_qualifier *)
  | H: context [ m{ _ } (_ & _) ] |- _ => rewrite msubst_qualifier_and in H
  | |- context [ m{ _ } (_ & _) ] => rewrite msubst_qualifier_and
  | H: context [ m{ _ } {: _ | _ } ] |- _ => rewrite msubst_baserty in H
  | |- context [ m{ _ } {: _ | _ } ] => rewrite msubst_baserty
  | H: context [ m{ _ } (_ ⇨ _) ] |- _ => rewrite msubst_arrrty in H
  | |- context [ m{ _ } (_ ⇨ _) ] => rewrite msubst_arrrty
  | H: context [ m{ _ } (mk_top _) ] |- _ => rewrite msubst_mk_top in H
  | |- context [ m{ _ } (mk_top _) ] => rewrite msubst_mk_top
  | H: context [ m{ _ } (mk_eq_constant _) ] |- _ => rewrite msubst_mk_eq_constant in H
  | |- context [ m{ _ } (mk_eq_constant _) ] => rewrite msubst_mk_eq_constant
  | H: context [ m{ _ } (mk_eq_var _ ?x) ], H': _ !! ?x = Some ?v |- _ => rewrite msubst_mk_eq_var with (v:=v) in H
  | H': _ !! ?x = Some ?v |- context [ m{ _ } (mk_eq_var _ ?x) ] => rewrite msubst_mk_eq_var with (v:=v)
  end; eauto.

(* Most lemmas here are generalization of the corresponding lemmas about
single-substitution. *)

Lemma msubst_open_var_tm: forall (Γv: env) e k (x: atom),
    closed_env Γv ->
    map_Forall (fun _ v => lc (treturn v)) Γv ->
    x ∉ dom Γv ->
    (m{Γv}) ({k ~> x} e) = {k ~> x} ((m{Γv}) e).
Proof.
  msubst_tac.
  rewrite_msubst_insert.
  apply map_Forall_insert in H2; eauto.
  rewrite H6; eauto.
  apply subst_open_var. my_set_solver.
  qauto. qauto.
  my_set_solver.
Qed.

Lemma msubst_open_rty: forall (Γv: env) (ρ: rty) k (v_x: value),
    closed_env Γv ->
    map_Forall (fun _ v => lc (treturn v)) Γv ->
    (m{Γv}) ({k ~> v_x} ρ) = {k ~> (m{Γv} v_x)} (m{Γv} ρ).
Proof.
  msubst_tac.
  rewrite_msubst_insert.
  apply map_Forall_insert in H2; eauto. simp_hyps.
  subst.
  by apply subst_open_rty.
Qed.

Lemma msubst_fresh_qualifier Γv ϕ :
  dom Γv ## qualifier_fv ϕ ->
  (m{Γv}) ϕ = ϕ.
Proof.
  msubst_tac.
  rewrite H0.
  apply subst_fresh_qualifier.
  my_set_solver.
  my_set_solver.
Qed.

Lemma msubst_fresh_rty Γv ρ :
  dom Γv ## rty_fv ρ ->
  (m{Γv}) ρ = ρ.
Proof.
  msubst_tac.
  rewrite H0.
  apply subst_fresh_rty.
  my_set_solver.
  my_set_solver.
Qed.

Lemma msubst_fresh_value Γv v :
  dom Γv ## fv_value v ->
  (m{Γv}) v = v.
Proof.
  msubst_tac.
  rewrite H0.
  apply subst_fresh.
  my_set_solver.
  my_set_solver.
Qed.

(* The proof can be reduced to [msubst_open_var_tm] and [msubst_fresh_tm]
(haven't defined yet; see [msubst_fresh_rty] for example). It's a pain to define
those for every [msubst_intro_*]. Don't bother yet. *)
Lemma msubst_intro_tm: forall (Γv: env) e k (v_x: value) (x: atom),
    closed_env Γv ->
    closed v_x ->
    map_Forall (fun _ v => lc (treturn v)) Γv ->
    x ∉ dom Γv ∪ stale e ->
    {k ~> v_x} ((m{Γv}) e) = (m{<[x:=v_x]> Γv}) ({k ~> x} e).
Proof.
  intros.
  rewrite_msubst_insert.
  2 : apply not_elem_of_dom; my_set_solver.
  revert_all.
  intros *.
  msubst_tac.
  rewrite map_fold_empty. rewrite open_subst_same. auto. my_set_solver.
  rewrite_msubst_insert.
  apply map_Forall_insert in H3; eauto.
  rewrite <- H0 by my_set_solver.
  by rewrite subst_open_closed by qauto.
  all: set_solver.
Qed.

Lemma msubst_intro_value: forall (Γv: env) (v: value) k (v_x: value) (x: atom),
    closed_env Γv ->
    closed v_x ->
    map_Forall (fun _ v => lc (treturn v)) Γv ->
    x ∉ dom Γv ∪ stale v ->
    {k ~> v_x} ((m{Γv}) v) = (m{<[x:=v_x]> Γv}) ({k ~> x} v).
Proof.
  intros.
  rewrite_msubst_insert.
  2 : apply not_elem_of_dom; my_set_solver.
  revert_all.
  intros *.
  msubst_tac.
  rewrite map_fold_empty. rewrite open_subst_same. auto. my_set_solver.
  rewrite_msubst_insert.
  apply map_Forall_insert in H3; eauto.
  rewrite <- H0 by my_set_solver.
  by rewrite subst_open_closed by qauto.
  all: set_solver.
Qed.

Lemma msubst_intro_rty: forall (Γv: env) e k (v_x: value) (x: atom),
    closed_env Γv ->
    closed v_x ->
    map_Forall (fun _ v => lc (treturn v)) Γv ->
    x ∉ dom Γv ∪ stale e ->
    {k ~> v_x} ((m{Γv}) e) = (m{<[x:=v_x]> Γv}) ({k ~> x} e).
Proof.
  intros.
  rewrite_msubst_insert.
  2 : apply not_elem_of_dom; my_set_solver.
  revert_all.
  intros *.
  msubst_tac.
  rewrite map_fold_empty.
  rewrite open_subst_same_rty. auto. my_set_solver.
  rewrite_msubst_insert.
  apply map_Forall_insert in H3; eauto.
  rewrite <- H0 by my_set_solver.
  by rewrite subst_open_rty_closed by qauto.
  all: set_solver.
Qed.

Lemma msubst_intro_qualifier: forall (Γv: env) e k (v_x: value) (x: atom),
    closed_env Γv ->
    closed v_x ->
    map_Forall (fun _ v => lc (treturn v)) Γv ->
    x ∉ dom Γv ∪ stale e ->
    {k ~> v_x} ((m{Γv}) e) = (m{<[x:=v_x]> Γv}) ({k ~> x} e).
Proof.
  intros.
  rewrite_msubst_insert.
  2 : apply not_elem_of_dom; my_set_solver.
  revert_all.
  intros *.
  msubst_tac.
  rewrite map_fold_empty. rewrite open_subst_same. auto. my_set_solver.
  rewrite_msubst_insert.
  apply map_Forall_insert in H3; eauto.
  rewrite <- H0 by my_set_solver.
  by rewrite subst_open_rty_closed by qauto.
  all: set_solver.
Qed.

Lemma msubst_rty_fv_subseteq Γv ρ:
  closed_env Γv ->
  rty_fv (m{Γv} ρ) ⊆ rty_fv ρ.
Proof.
  msubst_tac.
  rewrite fv_of_subst_rty_closed by eauto. my_set_solver.
Qed.

Lemma msubst_qualifier_fv_subseteq Γv ϕ:
  closed_env Γv ->
  qualifier_fv (m{Γv} ϕ) ⊆ qualifier_fv ϕ.
Proof.
  msubst_tac.
  rewrite fv_of_subst_qualifier_closed by eauto. my_set_solver.
Qed.

Lemma msubst_tm_fv_subseteq Γv (t: tm):
  closed_env Γv ->
  fv (m{Γv} t) ⊆ fv t.
Proof.
  msubst_tac.
  rewrite fv_of_subst by eauto. my_set_solver.
Qed.

Lemma msubst_value_fv_subseteq Γv (v : value):
  closed_env Γv ->
  fv (m{Γv} v) ⊆ fv v.
Proof.
  msubst_tac.
  rewrite fv_of_subst by eauto. my_set_solver.
Qed.

Lemma msubst_insert_fresh_rty Γv x v_x (ρ: rty):
  closed_env Γv ->
  closed v_x ->
  x ∉ dom Γv ∪ rty_fv ρ ->
  (m{<[x:=v_x]> Γv}) ρ = (m{Γv}) ρ.
Proof.
  intros.
  rewrite_msubst_insert. 2: apply not_elem_of_dom; my_set_solver.
  apply subst_fresh_rty.
  eapply not_elem_of_weaken; eauto.
  rewrite msubst_rty_fv_subseteq by eauto. my_set_solver.
Qed.

Lemma msubst_insert_fresh_qualifier Γv x v_x ϕ:
  closed_env Γv ->
  closed v_x ->
  x ∉ dom Γv ∪ qualifier_fv ϕ ->
  (m{<[x:=v_x]> Γv}) ϕ = (m{Γv}) ϕ.
Proof.
  intros.
  rewrite_msubst_insert. 2: apply not_elem_of_dom; my_set_solver.
  apply subst_fresh_qualifier.
  eapply not_elem_of_weaken; eauto.
  rewrite msubst_qualifier_fv_subseteq by eauto. my_set_solver.
Qed.

Lemma msubst_insert_fresh_tm Γv x v_x t:
  closed_env Γv ->
  closed v_x ->
  x ∉ dom Γv ∪ fv_tm t ->
  (m{<[x:=v_x]> Γv}) t = (m{Γv}) t.
Proof.
  intros.
  rewrite_msubst_insert. 2: apply not_elem_of_dom; my_set_solver.
  apply subst_fresh.
  eapply not_elem_of_weaken; eauto.
  rewrite msubst_tm_fv_subseteq by eauto. my_set_solver.
Qed.

Lemma msubst_insert_fresh_value Γv x v_x (v : value):
  closed_env Γv ->
  closed v_x ->
  x ∉ dom Γv ∪ fv_tm v ->
  (m{<[x:=v_x]> Γv}) v = (m{Γv}) v.
Proof.
  intros.
  rewrite_msubst_insert. 2: apply not_elem_of_dom; my_set_solver.
  apply subst_fresh.
  eapply not_elem_of_weaken; eauto.
  rewrite msubst_value_fv_subseteq by eauto. my_set_solver.
Qed.

Lemma lc_msubst_rty Γv (ρ: rty):
  lc_rty (m{Γv} ρ) ->
  map_Forall (fun _ v => lc (treturn v)) Γv ->
  lc_rty ρ.
Proof.
  msubst_tac.
  apply map_Forall_insert in H2; eauto. simp_hyps.
  eauto using lc_subst_rty.
Qed.

Lemma msubst_lc_rty Γv (ρ: rty):
  lc_rty ρ ->
  map_Forall (fun _ v => lc (treturn v)) Γv ->
  lc_rty (m{Γv} ρ).
Proof.
  msubst_tac.
  apply map_Forall_insert in H2; eauto. simp_hyps.
  eauto using subst_lc_rty.
Qed.

Lemma rtyR_msubst_insert_eq Γv ρ v x u :
  closed_env Γv ->
  closed u ->
  Γv !! x = None ->
  (⟦(m{ Γv }) ρ⟧) v ->
  (⟦(m{ <[x:=u]> Γv }) ρ⟧) v.
Proof.
  intros. rewrite_msubst_insert.
  rewrite subst_fresh_rty. auto.
  select (⟦_⟧ _) (fun H => apply rtyR_typed_closed in H).
  simp_hyps. select (closed_rty _ _) (fun H => sinvert H). my_set_solver.
Qed.

Lemma fv_of_msubst_rty_closed Γv ρ:
  closed_env Γv ->
  rty_fv (m{Γv} ρ) = rty_fv ρ ∖ dom Γv.
Proof.
  revert Γv.
  msubst_tac. my_set_solver.
  rewrite fv_of_subst_rty_closed by eauto.
  rewrite dom_insert_L. my_set_solver.
Qed.

Lemma msubst_preserves_rty_measure ρ Γv:
  rty_measure ρ = rty_measure (m{Γv} ρ).
Proof.
  msubst_tac. qauto using subst_preserves_rty_measure.
Qed.

Lemma msubst_fvar_inv (Γv : env) v (x : atom) :
  closed_env Γv ->
  m{Γv} (vfvar v) = x ->
  v = x /\ x ∉ dom Γv.
Proof.
  msubst_tac. my_set_solver.
  unfold substitute in H2.
  destruct r; simpl in H2; simplify_eq.
  case_decide; simplify_eq. exfalso.
  apply map_Forall_insert in H1; eauto. simp_hyps.
  unfold closed in *. unfold fv in *.
  cbn in *. qauto using non_empty_singleton.
  simp_hyps. split; eauto. subst.
  rewrite dom_insert. my_set_solver.
Qed.


Lemma msubst_preserves_closed_rty Γ Γ' ρ :
  closed_rty (ctxdom (Γ ++ Γ')) ρ ->
  ctxRst Γ (fun Γv => closed_rty (ctxdom (Γ')) (m{Γv} ρ)).
Proof.
Admitted.
(*   intros HΓv H. *)
(*   sinvert H. *)
(*   induction HΓv. *)
(*   - intuition. econstructor. *)
(*   - intuition. eauto using msubst_lc_rty, ctxRst_lc. *)
(*   econstructor. eauto using msubst_lc_rty, ctxRst_lc. *)
(*   rewrite fv_of_msubst_rty_closed by eauto using ctxRst_closed_env. *)
(*   rewrite ctxdom_app_union in *. *)
(*   rewrite ctxRst_dom in * by eauto. *)
(*   my_set_solver. *)
(* Qed. *)

Lemma msubst_preserves_closed_rty_empty Γ ρ :
  closed_rty (ctxdom Γ) ρ -> ctxRst Γ (fun Γv => closed_rty ∅ (m{Γv} ρ)).
Proof.
  intros. eapply msubst_preserves_closed_rty with (Γ':=[]); eauto.
  by simplify_list_eq.
Qed.

Lemma msubst_preserves_basic_typing_tm Γ:
  forall Γ' (e: tm) T,
    (⌊Γ⌋* ∪ Γ') ⊢t e ⋮t T ->
    ctxRst Γ (fun Γv => Γ' ⊢t m{Γv} e ⋮t T).
Proof.
Admitted.
(*   induction 1; intros; eauto. *)
(*   apply_eq H. cbn. apply map_empty_union. *)
(*   rewrite ctx_erase_app in H2. *)
(*   rewrite <- map_union_assoc in H2. *)
(*   apply IHctxRst in H2. *)
(*   rewrite msubst_insert; *)
(*     eauto using subst_commute_tm, ctxRst_closed_env, *)
(*                 rtyR_closed, ctxRst_ok_insert. *)
(*   eapply basic_typing_subst_tm; cycle 1. *)
(*   eapply_eq H2. *)
(*   cbn. rewrite map_union_empty. rewrite insert_empty. *)
(*   rewrite <- insert_union_singleton_l. reflexivity. *)
(*   eapply basic_typing_weaken_value. apply map_empty_subseteq. *)
(*   apply rtyR_typed_closed in H1. simp_hyps. *)
(*   sinvert H1. apply_eq H6. eauto using rty_erase_msubst_eq. *)
(* Qed. *)

Lemma msubst_preserves_basic_typing_tm_empty Γ :
  forall (e: tm) T,
    ( ⌊Γ⌋* ) ⊢t e ⋮t T ->
    ctxRst Γ (fun Γv => ∅ ⊢t m{Γv} e ⋮t T).
Proof.
  intros. eapply msubst_preserves_basic_typing_tm; eauto.
  rewrite map_union_empty. eauto.
Qed.

Lemma msubst_preserves_basic_typing_value Γ:
  forall Γ' (e: value) T,
    (⌊Γ⌋* ∪ Γ') ⊢t e ⋮t T ->
    ctxRst Γ (fun Γv => Γ' ⊢t m{Γv} e ⋮t T).
Proof.
Admitted.
(*   induction 1; intros; eauto. *)
(*   apply_eq H. cbn. apply map_empty_union. *)
(*   rewrite ctx_erase_app in H2. *)
(*   rewrite <- map_union_assoc in H2. *)
(*   apply IHctxRst in H2. *)
(*   rewrite msubst_insert; *)
(*     eauto using subst_commute_tm, ctxRst_closed_env, *)
(*                 rtyR_closed, ctxRst_ok_insert. *)
(*   eapply basic_typing_subst_tm; cycle 1. *)
(*   eapply_eq H2. *)
(*   cbn. rewrite map_union_empty. rewrite insert_empty. *)
(*   rewrite <- insert_union_singleton_l. reflexivity. *)
(*   eapply basic_typing_weaken_value. apply map_empty_subseteq. *)
(*   apply rtyR_typed_closed in H1. simp_hyps. *)
(*   sinvert H1. apply_eq H6. eauto using rty_erase_msubst_eq. *)
(* Qed. *)

Lemma msubst_preserves_basic_typing_value_empty Γ :
  forall (e: value) T,
    ( ⌊Γ⌋* ) ⊢t e ⋮t T ->
    ctxRst Γ (fun Γv => ∅ ⊢t m{Γv} e ⋮t T).
Proof.
  intros. eapply msubst_preserves_basic_typing_value; eauto.
  rewrite map_union_empty. eauto.
Qed.
