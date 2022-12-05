From stdpp Require Import mapset.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Classical.
From CT Require Import CoreLang.
From CT Require Import CoreLangProp.
From CT Require Import OperationalSemantics.

Import Atom.
Import CoreLang.
Import Tactics.
Import NamelessTactics.
Import OperationalSemantics.

Definition termR: relation tm := fun (e e': tm) => forall (v: value), e ↪* v -> e' ↪* v.
Notation " e1 '<-<' e2 " := (termR e1 e2) (at level 10).
Notation " e1 '>=<' e2 " := (termR e1 e2 /\ termR e2 e1) (at level 10).

#[global]
Instance termR_refl: Reflexive termR.
Proof. intros e v. auto. Qed.
Arguments termR_refl /.

#[global]
Instance termR_trans: Transitive termR.
Proof. intros e1 e2 e3. unfold termR. intros. auto. Qed.
Arguments termR_trans /.


Lemma let_reduction_spec_forward_one_step: forall (e_x: tm) e (e': tm),
    tlete e_x e ↪ e' -> (exists (v_x: value), e_x = v_x /\ e' = e ^t^ v_x) \/ (exists (e_x': tm), e_x ↪ e_x' /\ e' = tlete e_x' e).
Proof.
  intros.
  inversion H; subst; clear H.
  - right. exists e1'. split; auto.
  - left. exists v1. split; auto.
Qed.

(* Need logical relation *)

Lemma let_reduction_spec_forward: forall (e_x: tm),
    lc e_x ->
    (forall e (v: value),
        (tlete e_x e ↪* v -> exists (v_x: value), e_x ↪* v_x /\ e ^t^ v_x ↪* v)).
Proof.
  intros e_x H.
  induction H; intros.
  - admit.
  - admit.
  - admit.
  - admit.
  - admit.
  - admit.
  - 

  induction e_x; intros.
  - op_simpl.
  - op_simpl. eexists. split; eauto.
  - 




Lemma let_reduction_spec_forward: forall (e_x: tm) e (v: value),
    (tlete e_x e ↪* v -> exists (v_x: value), e_x ↪* v_x /\ e ^t^ v_x ↪* v).
Proof.
  induction e_x; induction e; intros; try op_simpl; try (op_simpl; eexists; split; eauto).
  - admit.
  - op_simpl.
    
    end.
      exists v. split; auto.
  - op_simpl.
  - op_simpl. eexists. split; eauto.
  - specialize (IHe_x1 e_x2)

Lemma let_reduction_spec_forward: forall (e_x: tm) e (v: value),
    (tlete e_x e ↪* v -> exists (v_x: value), e_x ↪* v_x /\ e ^t^ v_x ↪* v).
Proof.
  induction e_x; intros.
  - op_simpl.
  - op_simpl. eexists. split; eauto.
  - 

    

    admit.
  - 
  intros.
  induction H0. admit.

Lemma let_reduction_spec_tm: forall (e_x: tm) e (v: value),
    lc e_x ->
    (tlete e_x e ↪* v <-> exists (v_x: value), e_x ↪* v_x /\ e ^t^ v_x ↪* v).
Proof.


(* Lemma termR_fst: forall (e e': tm), termR e e' -> (forall (v: value), e -->* v -> e' -->* v). *)
(* Proof. intros. destruct H. auto. Qed. *)

(* Global Hint Resolve termR_fst: core. *)

(* Lemma termR_snd: forall (e e': tm), termR e e' -> (forall Gamma T, Gamma \N- e \Tin T -> Gamma \N- e' \Tin T). *)
(* Proof. intros. destruct H. eapply H1. auto. Qed. *)

(* Global Hint Resolve termR_snd: core. *)
