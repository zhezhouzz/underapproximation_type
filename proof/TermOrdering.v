(** * MoreStlc: More on the Simply Typed Lambda-Calculus *)

Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From PLF Require Import CoreLangSimp.
(* From PLF Require Import NormalTypeSystemSimp. *)
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.
From Coq Require Import Lists.List.
From PLF Require Import Smallstep (multi_step).

Set Warnings "-notation-overridden,-parsing".


(* Import CoreLangSimp. *)
(* Import NormalTypeSystemSimp. *)
Import ListNotations.

Definition term_order (e e': tm) :=
  (forall v, e -->* v -> e' -->* v) /\ (forall Gamma T, Gamma \N- e \Tin T -> forall Gamma T, Gamma \N- e' \Tin T).

Notation " e1 '<-<' e2 " := (term_order e1 e2) (at level 90).

Notation " e1 '<=<' e2 " := (term_order e1 e2 /\ term_order e2 e1) (at level 90).

Lemma term_order_fst: forall (e e': tm), term_order e e' -> (forall v, e -->* v -> e' -->* v).
Proof. intros. destruct H. auto. Qed.

Global Hint Resolve term_order_fst: core.

Lemma term_order_snd: forall (e e': tm), term_order e e' -> (forall Gamma T, Gamma \N- e \Tin T -> forall Gamma T, Gamma \N- e' \Tin T).
Proof. intros. destruct H. eapply H1. eauto. Qed.

Global Hint Resolve term_order_snd: core.

Lemma term_order_eq_trans (e1 e2 e3: tm): e1 <=< e2 -> e2 <=< e3 -> e1 <=< e3.
Admitted.

Lemma term_order_const_bound (e1: tm) (c2: constant): e1 <-< c2 -> (forall v, e1 -->* v -> v = c2).
Proof with eauto.
  intros. destruct H... apply H in H0. inversion H0; subst... inversion H2.
Qed.

Global Hint Resolve term_order_const_bound: core.

Lemma term_order_trans (e1 e2 e3: tm): e1 <-< e2 -> e2 <-< e3 -> e1 <-< e3.
Proof.
    intros.
    simpl.
    unfold term_order in H.
    unfold term_order in H0.
    intro.
    intro.
    apply H in H1.
    apply H0 in H1.
    trivial.
Qed.    

(* Global Hint Resolve term_order_trans: core. *)

Lemma eta_app_value_value: forall x (v1 v2: value) x1 x2,
    x1 <> x2 -> ~ x1 \FVtm v2 ->
    tletapp x v1 v2 x <=< tlete x1 v1 (tlete x2 v2 (tletapp x x1 x2 x)).
Admitted.

Lemma eta_reduction: forall x1 (f: value) x2 (v: value),
    ~ x1 \FVvalue v ->
    (tletapp x2 f v x2) <=< (tlete x1 f (tletapp x2 x1 v x2)).
Proof.
    intros.
    unfold term_order.
    split.
    intros.
    simpl.
    eapply multi_step.
    eapply ST_Lete2.
    simpl.

    rewrite eqb_refl.
    destruct (eqb_spec x1 x2); subst.
    induction v.
    simpl.
    auto.
    simpl.
    destruct (eqb_spec x2 y); subst.
    
Admitted.
    (* Pen and Paper Proof
    proving tletapp x2 f v x2 <--< (tlete x1 f (tletapp x2 x1 v x2))
    begin: 
        unfolding the definintion of <--< 
        \forall val. tletapp x2 f v x2 -->* val => (tlete x1 f (tletapp x2 x1 v x2)) --> * val
        assume : \forall val. tletapp x2 f v x2 -->* val -- H0
        to prove : (tlete x1 f (tletapp x2 x1 v x2)) --> * val
            apply multi_step.
            (tlete x1 f (tletapp x2 x1 v x2)) --> ?y /\ ? -->* val 
            apply ST_Lete2 in goal 
            (subst x1 f (tletapp x2 x1 v x2))) -->* val
            apply definition subst 
            tletapp x2 f v x2 -->* val
            apply H0.
    end
    proving the other side 
    (tlete x1 f (tletapp x2 x1 v x2)) <--< tletapp x2 f v x2 
    begin:
        unfolding the definintion of <--< 
        \forall val. (tlete x1 f (tletapp x2 x1 v x2)) -->* val => tletapp x2 f v x2 -->* val
        assume : (tlete x1 f (tletapp x2 x1 v x2)) --> * val -- H0 
        to prove : tletapp x2 f v x2 -->* val
            apply multi_step
            tletapp x2 f v x2 --> ?y /\ ?y -->* val
            apply ST_LetAppLam in goal


    
    *)
    
        
    
Lemma eta_application_const_to_lete_const: forall x2 x T e (c_x: constant),
    (tlete x c_x e) <=< (tletapp x2 (vlam x T e) c_x x2).


    Admitted.

Lemma eta_lete_const_to_subst: forall x e (c_x: constant),
    (subst x c_x e) <=< (tlete x c_x e).
Admitted.

Lemma eta_lete_const_to_subst_in_lam: forall a T x e (c_x: constant),
    (vlam a T (subst x c_x e)) <=< (vlam a T (tlete x c_x e)).
Admitted.

Lemma eta_closed_term_can_captured_by_lam: forall a e_a Ta x T e,
    empty \N- e_a \Tin Ta -> a <> x ->
    (vlam x T (tlete a e_a e)) <=< (tlete a e_a (vlam x T e)).
Admitted.

Lemma eta_matchb_true: forall e1 e2, e1 <=< (tmatchb true e1 e2).
Admitted.

Lemma eta_matchb_false: forall e1 e2, e2 <=< (tmatchb false e1 e2).
Admitted.

Lemma eta1: forall x1 x T e x2 (c_x:constant) x0,
    x1<> x2 -> ~ x1 \FVtm c_x ->
    (tlete x1 (vlam x T e) (tlete x2 c_x (tletapp x0 x1 x2 x0))) <=< (tlete x c_x e).
Admitted.

Lemma eta11: forall x e_x e x1 x2 x0 T1 T2,
    ~ x1 \FVtm e_x ->
    (tlete x e_x e) <-< tlete x1 (vlam x (T1 t--> T2) e) (tlete x2 e_x (tletapp x0 x1 x2 x0)).
Admitted.

Lemma eta2: forall e2 (c2: constant) x1 e1 x2 x,
    e2 -->* c2 ->
    (tlete x1 e1 (tlete x2 c2 (tletapp x x1 x2 x))) <-< (tlete x1 e1 (tlete x2 e2 (tletapp x x1 x2 x))).
Admitted.

Lemma eta3: forall x1 a (e_a: tm) Ta e1 x2 e2 x,
    empty \N- e_a \Tin Ta ->
    (tlete a e_a (tlete x1 e1 (tlete x2 e2 (tletapp x x1 x2 x))))
      <=< (tlete x1 (tlete a e_a e1) (tlete x2 (tlete a e_a e2) (tletapp x x1 x2 x))).
Admitted.

Lemma eta_subst_in_const: forall a e_x (c: constant), (tlete a e_x c) <=< c.
Admitted.

Lemma eta_snd_let_reduce: forall x1 e1 x2 e2 x (c2: constant),
    e2 -->* c2 ->
    (tlete x1 e1 (tlete x2 c2 (tletapp x x1 x2 x))) <-< (tlete x1 e1 (tlete x2 e2 (tletapp x x1 x2 x))).
Admitted.

Lemma eta4: forall x1 (v1: value) x2 (c2: constant) x,
    x1 <> x2 ->
    tlete x1 v1 (tlete x2 c2 (tletapp x x1 x2 x)) <-< tletapp x v1 c2 x.
Admitted.

Lemma eta5: forall x1 (v1: value) x2 (id2: string) x,
    x1 <> x2 -> ~ x1 \FVvalue id2 ->
    (tlete x1 v1 (tlete x2 id2 (tletapp x x1 x2 x))) <-< tletapp x v1 id2 x.
Admitted.

Lemma eta6: forall x1 (v1 v2: value) x2 x,
    ~ x1 \FVvalue v2 ->
    tlete x1 v1 (tlete x2 v2 (tletapp x x1 x2 x)) <-< tletapp x v1 v2 x.
Admitted.

Lemma eta7: forall x v1 v2 e, (tlete x (tletapp x v1 v2 x) e) <=< (tletapp x v1 v2 e).
Admitted.

Lemma eta8: forall x op v1 v2 e, (tlete x (tletbiop x op v1 v2 x) e) <=< (tletbiop x op v1 v2 e).
Admitted.

Lemma eta_op: forall x1 e1 x2 e2 x op (c1 c2: constant),
    e1 -->* c1 -> e2 -->* c2 ->
    (tletbiop x op c1 c2 x) <-< tlete x1 e1 (tlete x2 e2 (tletbiop x op x1 x2 x)).
Admitted.

Lemma eta9: forall x1 a (c_a: tm) e1 x2 e2 x op,
    (tlete x1 (tlete a c_a e1) (tlete x2 (tlete a c_a e2) (tletbiop x op x1 x2 x)))
      <-< (tlete a c_a (tlete x1 e1 (tlete x2 e2 (tletbiop x op x1 x2 x)))).
Admitted.

Lemma eta10: forall op x1 (id1 id2: cid) x2 x,
    x1 <> x2 -> ~ x1 \FVvalue id2 ->
    (tlete x1 id1 (tlete x2 id2 (tletbiop x op x1 x2 x))) <-< (tletbiop x op id1 id2 x).
Admitted.

Lemma eta_drop_lete_not_bot: forall x e_x0 e,
    (exists c0 : constant, e_x0 -->* c0) ->
    ~ x \FVtm e ->
    (tlete x e_x0 e) <=< e.
Admitted.

Lemma eta_lete_neq: forall x a c_x e_x e,
    a <> x ->
    tlete x (tlete a c_x e_x) (tlete a c_x e) <-< tlete a c_x (tlete x e_x e).
Admitted.

Lemma eta_fix1: forall x1 f (T: base_ty) tau x e x2 (c_x': constant) x0,
    (tletapp x (vfix f (T t--> tau) x T e) c_x' x) <-< (tlete x1 (vfix f (T t--> tau) x T e) (tlete x2 c_x' (tletapp x0 x1 x2 x0))).
Admitted.

Lemma eta_fix2: forall x1 f (T: base_ty) tau x e x2 (c_x: constant),
    tlete x1 (vlam f (T t--> tau) (tlete x c_x e))
          (tlete x2 (vfix f (T t--> tau) x T e) (tletapp x x1 x2 x)) <-<
          tletapp x (vfix f (T t--> tau) x T e) c_x x.
Admitted.

Lemma eta_fix3: forall x1 f (T: base_ty) tau x e x2 (c_x': constant),
    tlete x1 (vlam x T (vlam f (T t--> tau) e)) (tlete x2 c_x' (tletapp x x1 x2 x)) <-<
          vlam f (T t--> tau) (tlete x c_x' e).
Admitted.

Lemma eta_fix4: forall a f (Tx: base_ty) tau x e (c_x: constant),
    tlete a c_x (vlam x Tx (vlam f (Tx t--> tau) e)) <-< vlam x Tx (vlam f (Tx t--> tau) (tlete a c_x e)).
Admitted.

Lemma eta_fix5: forall a f (Tx: base_ty) tau x e (c_x: constant),
    vfix f (Tx t--> tau) x Tx (tlete a c_x e) <-< tlete a c_x (vfix f (Tx t--> tau) x Tx e).
Admitted.

Lemma eta_fix6: forall a f (Tx: base_ty) tau x e e_x,
    tlete a e_x (vlam x Tx (vlam f (Tx t--> tau) e)) <-< vlam x Tx (vlam f (Tx t--> tau) (tlete a e_x e)).
Admitted.

Lemma eta_fix7: forall a f (Tx: base_ty) tau x e (e_x: tm),
    vfix f (Tx t--> tau) x Tx (tlete a e_x e) <-< tlete a e_x (vfix f (Tx t--> tau) x Tx e).
Admitted.

Lemma eta_self1: forall x1 e e' x2 (c_x: tm) x,
    e <-< e' ->
    tlete x1 e (tlete x2 c_x (tletapp x x1 x2 x)) <-< tlete x1 e' (tlete x2 c_x (tletapp x x1 x2 x)).
Admitted.
