Set Warnings "-notation-overridden,-parsing".
From CT Require Import Maps.
From CT Require Import CoreLang.
From CT Require Import NormalTypeSystem.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.

From Coq Require Import Lists.List.
Import ListNotations.
Import NormalTypeSystem.

(* Renaming assumptions related to binding and duplication. *)

Definition state_permute {A:Type}: forall (st: string -> option A) x y (a a': A),
    (x |-> a; y |-> a'; st) = (y |-> a'; x |-> a; st).
Admitted.

Definition exists_not_free_var_in_tm: forall e, exists x, ~ x \FVtm e.
Admitted.

Lemma term_has_no_free_var: forall e x, ~ x \FVtm e.
Admitted.

(* Programs always halt. *)

Definition const_order (c1 c2: constant) : Prop :=
  match c1 with
  | cbool c1 =>
      match c2 with
      | cbool c2 =>
          (match c1 with
           | true => False
           | false => c2 = true
           end)
      | cnat _ => True
      end
  | cnat c1 =>
      match c2 with
      | cbool _ => False
      | cnat c2 => c1 < c2
      end
  end.

Lemma const_order_is_well_founded: well_founded const_order.
Admitted.

Theorem multi_preservation : forall t t' T,
    empty \N- t \Tin T  -> t -->* t'  ->
                  empty \N- t' \Tin T.
Proof with eauto.
Admitted.

Lemma ty_unique: forall e Gamma T1 T2,
    Gamma \N- e \Tin T1  -> Gamma \N- e \Tin T2 -> T1 = T2.
Admitted.

(* Some syntactic equivalence facts *)

Lemma let_reduction_spec: forall x e_x e (v: value),
    tlete x e_x e -->* v <-> (exists (v_x: value), e_x -->* v_x /\ (subst x v_x e) -->* v).
Admitted.

Lemma letapp_reduction_spec: forall x (f: value) (v1 : value) (v : value) e,
    tletapp x f v1 e -->* v <->
    (exists x_m T_m e_m,
        f = vlam x_m T_m e_m /\ (tlete x (subst x_m v1 e_m) e) -->* v).
Admitted.

Lemma eta_in_lam: forall a T e1 e2,
    (forall (v: value), e1 -->* v -> e2 -->* v) <-> (forall (v: value), vlam a T e1 -->* v -> vlam a T e2 -->* v).
Admitted.

Lemma eta_in_fix: forall a T f T' e1 e2,
    (forall (v: value), e1 -->* v -> e2 -->* v) <-> (forall (v: value), vfix a T f T' e1 -->* v -> vfix a T f T' e2 -->* v).
Admitted.

Lemma tmatch_reduction_spec: forall e e1 e2 e1' e2',
    (forall (v: value), e1 -->* v -> e1' -->* v) /\ (forall (v: value), e2 -->* v -> e2' -->* v) <->
      (forall (v: value), tmatchb e e1 e2 -->* v -> tmatchb e e1' e2' -->* v).
Admitted.

Lemma reduction_eq_implies_ty_eq: forall e1 e2,
    (forall (v: value), e1 -->* v -> e2 -->* v) ->
    (forall Gamma T, Gamma \N- e1 \Tin T -> Gamma \N- e2 \Tin T).
Admitted.

Lemma eta_match2: forall (a id: string) (c_x: tm) e1 e2,
    id <> a ->
    tmatchb id (tlete a c_x e1) (tlete a c_x e2) <-< tlete a c_x (tmatchb id e1 e2).
Admitted.

Lemma eta_match3: forall (id: string) (c: constant) (e_x: tm) e1 e2,
    tlete id e_x (tmatchb c e1 e2) <-< tlete id e_x (tmatchb id e1 e2).
Admitted.

Lemma meet_of_two_terms_exists: forall e1 e2 T,
    empty \N- e1 \Tin T -> empty \N- e2 \Tin T ->
    (exists e3, (empty \N- e3 \Tin T) /\ (forall c, e3 -->* c <-> e1 -->* c /\ e2 -->* c)).
Admitted.

Lemma eta_closed_term_can_captured_by_lam: forall a e_a Ta x T e,
    empty \N- e_a \Tin Ta -> a <> x ->
    (vlam x T (tlete a e_a e)) <=< (tlete a e_a (vlam x T e)).
Admitted.

Lemma eta3: forall x1 a (e_a: tm) Ta e1 x2 e2 x,
    empty \N- e_a \Tin Ta -> x1 <> x2 ->
    (tlete a e_a (tlete x1 e1 (tlete x2 e2 (tletapp x x1 x2 x))))
      <=< (tlete x1 (tlete a e_a e1) (tlete x2 (tlete a e_a e2) (tletapp x x1 x2 x))).
Admitted.

Lemma eta_subst_in_const: forall a (e_x: tm) (v_x: constant) (c: constant),
    c <-< (tlete a e_x c).
Proof with eauto.
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

Lemma eta_fix2: forall x1 f (T: base_ty) tau x e x2 (c_x: constant),
    tlete x1 (vlam f (T t--> tau) (tlete x c_x e))
          (tlete x2 (vfix f (T t--> tau) x T e) (tletapp x x1 x2 x)) <-<
          tletapp x (vfix f (T t--> tau) x T e) c_x x.
Admitted.

Lemma eta_fix3: forall x1 f (T: base_ty) tau x e x2 (c_x': constant),
    x1 <> x2 ->
    tlete x1 (vlam x T (vlam f (T t--> tau) e)) (tlete x2 c_x' (tletapp x x1 x2 x)) <-<
          vlam f (T t--> tau) (tlete x c_x' e).
Admitted.

Lemma eta_fix7: forall a f (Tx: base_ty) tau x e (e_x: tm),
    vfix f (Tx t--> tau) x Tx (tlete a e_x e) <-< tlete a e_x (vfix f (Tx t--> tau) x Tx e).
Proof with eauto.
Admitted.

Lemma eta_op_reducetion: forall op c_a c_b c_res x x0 x1 x2 x3 x4,
    eval_op op c_a c_b c_res ->
    tlete x0 (tlete x1 (vbiop op) (tlete x2 c_a (tletapp x x1 x2 x))) (tlete x3 c_b (tletapp x4 x0 x3 x4)) -->* c_res.
Admitted.
