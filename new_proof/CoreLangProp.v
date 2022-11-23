From stdpp Require Import mapset.
From CT Require Import Atom.
From CT Require Import Tactics.
From CT Require Import CoreLang.
From CT Require Import NamelessTactics.

Import CoreLang.
Import NamelessTactics.

Lemma constant_eqb_spec: forall (c c': constant), c = c' \/ c <> c'.
Proof with eauto.
  destruct c, c'...
  - destruct b, b0; firstorder.
  - destruct (Nat.eq_dec n n0); firstorder.
    right. intro HH. inversion HH...
Qed.

(* properties *)

Ltac specialize_L :=
  match goal with
  | [ H : forall (x: atom), x ∉ ?L -> _ |- _] => specialize (H (fv_of_set L) (fv_of_set_fresh L))
  end.

Lemma lc_exfalso: forall bn, ~ lc (vbvar bn).
Proof.
  intros bn HF. inversion HF.
Qed.

Lemma body_exfalso: forall bn, ~ body (vbvar (S bn)).
Proof.
  intros bn HF. inversion HF. specialize_L. inversion H.
Qed.

Ltac solve_lc_exfalso :=
  match goal with
    | [ H: lc (tvalue (vbvar ?n)) |- _ ] => exfalso; apply (lc_exfalso n); auto
    | [ H: body (tvalue (vbvar (S ?n))) |- _ ] => exfalso; apply (body_exfalso n); auto
  end.

Lemma lc_abs_iff_body: forall T e, lc (vlam T e) <-> body e.
Proof.
  split; unfold body; intros.
  - inversion H; subst. exists L. auto.
  - destruct H as (L & HL). eapply lc_vlam. apply HL.
Qed.

Ltac dec_tf := try (subst; rewrite decide_True; auto; fast_set_solver!!); try (rewrite decide_False; auto; fast_set_solver!!).

Lemma close_open_var_tm: forall (e: tm) (x: atom) (k: nat), x ∉ (fv_tm e) -> {k <t~ x} ({k ~t> x} e) = e.
Proof with eauto.
  apply (tm_mutual_rec
           (fun (v: value) => forall (x: atom) (k: nat), x ∉ (fv_value v) -> {k <v~ x} ({k ~v> x} v) = v)
           (fun (e: tm) => forall (x: atom) (k: nat), x ∉ (fv_tm e) -> {k <t~ x} ({k ~t> x} e) = e)
        ); simpl; try (intros; repeat rewrite_by_set_solver; auto).
  - assert (x <> atom) by fast_set_solver!!. apply decide_False...
  - destruct (Nat.eq_dec k bn).
    + rewrite decide_True... simpl... rewrite decide_True...
    + rewrite decide_False...
Qed.

Lemma close_open_var_value: forall (e: value) (x: atom) (k: nat), x ∉ (fv_value e) -> {k <v~ x} ({k ~v> x} e) = e.
Proof with eauto.
  apply (value_mutual_rec
           (fun (v: value) => forall (x: atom) (k: nat), x ∉ (fv_value v) -> {k <v~ x} ({k ~v> x} v) = v)
           (fun (e: tm) => forall (x: atom) (k: nat), x ∉ (fv_tm e) -> {k <t~ x} ({k ~t> x} e) = e)
        ); simpl; try (intros; repeat rewrite_by_set_solver; auto).
  - assert (x <> atom) by fast_set_solver!!. apply decide_False...
  - destruct (Nat.eq_dec k bn).
    + rewrite decide_True... simpl... rewrite decide_True...
    + rewrite decide_False...
Qed.

Lemma open_var_fv_value: forall (v: value) (x: atom) (k: nat), fv_value ({k ~v> x} v) ⊆ {[ x ]} ∪ fv_value v.
Proof with eauto.
  apply (value_mutual_rec
           (fun (v: value) => forall (x: atom) (k: nat), fv_value ({k ~v> x} v) ⊆ {[ x ]} ∪ fv_value v)
           (fun (e: tm) => forall (x: atom) (k: nat), fv_tm ({k ~t> x} e) ⊆ {[ x ]} ∪ fv_tm e)
        ); simpl;
    try (intros; repeat rewrite_by_set_solver; auto);
    try var_dec_solver;
    try my_set_solver.
Qed.

Lemma open_var_fv_tm: forall (e: tm) (x: atom) (k: nat), fv_tm ({k ~t> x} e) ⊆ {[ x ]} ∪ fv_tm e.
Proof with eauto.
  apply (tm_mutual_rec
           (fun (v: value) => forall (x: atom) (k: nat), fv_value ({k ~v> x} v) ⊆ {[ x ]} ∪ fv_value v)
           (fun (e: tm) => forall (x: atom) (k: nat), fv_tm ({k ~t> x} e) ⊆ {[ x ]} ∪ fv_tm e)
        ); simpl; intros; auto;
    try var_dec_solver;
    my_set_solver.
Qed.

Lemma close_var_fv_value:
  forall (v: value) (x: atom) (k: nat), fv_value ({k <v~ x} v) = (fv_value v) ∖ {[x]}.
Proof.
  apply (value_mutual_rec
           (fun (v: value) => forall (x: atom) (k: nat), fv_value ({k <v~ x} v) = (fv_value v) ∖ {[x]})
           (fun (e: tm) => forall (x: atom) (k: nat), fv_tm ({k <t~ x} e) = (fv_tm e) ∖ {[x]})
        ); simpl; intros; auto;
    try var_dec_solver;
    my_set_solver.
Qed.

Lemma subst_fresh_value: forall (v: value) (x:atom) (u: value),
    x ∉ (fv_value v) -> {x := u}v v = v.
Proof with eauto.
  apply (value_mutual_rec
           (fun (v: value) => forall (x:atom) (u: value), x ∉ (fv_value v) -> {x := u}v v = v)
           (fun (e: tm) => forall (x:atom) (u: value), x ∉ (fv_tm e) -> {x := u}t e = e)
        ); simpl; intros; eauto; try (repeat rewrite_by_set_solver; auto).
  - assert (x <> atom) by my_set_solver. rewrite decide_False...
Qed.

Lemma subst_fresh_tm: forall (e: tm) (x:atom) (u: value),
    x ∉ (fv_tm e) -> {x := u}t e = e.
Proof with eauto.
  apply (tm_mutual_rec
           (fun (v: value) => forall (x:atom) (u: value), x ∉ (fv_value v) -> {x := u}v v = v)
           (fun (e: tm) => forall (x:atom) (u: value), x ∉ (fv_tm e) -> {x := u}t e = e)
        ); simpl; intros; eauto; try (repeat rewrite_by_set_solver; auto).
  - assert (x <> atom) by my_set_solver. rewrite decide_False...
Qed.

(* Fixpoint lc_at_value (k : nat) (v : value): Prop := *)
(*   match v with *)
(*   | vbiop _ => True *)
(*   | vconst _ => True *)
(*   | vfvar _ => True *)
(*   | vbvar n => n < k *)
(*   | vlam T e => lc_at_tm (S k) e *)
(*   | vfix Tf e => lc_at_tm (S k) e *)
(*   end *)
(* with lc_at_tm (k : nat) (e : tm): Prop := *)
(*        match e with *)
(*        | terr => True *)
(*        | tvalue v => lc_at_value k v *)
(*        | tlete e1 e2 => lc_at_tm k e1 /\ lc_at_tm (S k) e2 *)
(*        | tletapp v1 v2 e => lc_at_value k v1 /\ lc_at_value k v2 /\ lc_at_tm (S k) e *)
(*        | tletbiop op v1 v2 e => lc_at_value k v1 /\ lc_at_value k v2 /\ lc_at_tm (S k) e *)
(*        | tmatchb v e1 e2 => lc_at_value k v /\ lc_at_tm k e1 /\ lc_at_tm k e2 *)
(*        end. *)

Ltac ex_specialize_L :=
  match goal with
  | [ H : ex (fun (L: aset) => _) |- _] => destruct H; specialize_L
  end.

(* Definition lc_implies_lc_at_tm: forall e, lc e -> lc_at_tm 0 e. *)
(* Proof with auto. *)
(*   intros. induction H; simpl; repeat split; auto. *)
(*   - rewrite lc_at_eq_cofinite_tm. auto_exists_L_and_solve. *)
(*   - rewrite lc_at_eq_cofinite_tm. auto_exists_L_and_solve. *)
(*   - split. *)
(* Qed. *)

(* Lemma lc_at_eq_cofinite_tm: forall e k, lc_at_tm (S k) e <-> (exists (L:aset), forall (x: atom), x ∉ L -> lc_at_tm k ({k ~t> x} e)). *)
(* Proof with auto. *)
(*   apply (tm_mutual_rec *)
(*            (fun (e: value) => forall k, lc_at_value (S k) e <-> (exists (L:aset), forall (x: atom), x ∉ L -> lc_at_value k ({k ~v> x} e))) *)
(*            (fun (e: tm) => forall k, lc_at_tm (S k) e <-> (exists (L:aset), forall (x: atom), x ∉ L -> lc_at_tm k ({k ~t> x} e)))); *)
(*     simpl; split; intros; auto; *)
(*     try eempty_aset; *)
(*     try (repeat lc_at_tm_rewrite; auto_exists_L_and_solve). *)
(*   - destruct (Nat.eq_dec k bn). *)
(*     + subst. exists ∅... intros. rewrite decide_True... simpl... *)
(*     + exists ∅... intros. rewrite decide_False... simpl. lia. *)
(*   - ex_specialize_L. *)
(*     destruct (Nat.eq_dec k bn). *)
(*     + subst. rewrite decide_True in H... *)
(*     + rewrite decide_False in H... *)
(* Qed. *)

(* Lemma lc_at_eq_cofinite_value: forall e k, lc_at_value (S k) e <-> (exists (L:aset), forall (x: atom), x ∉ L -> lc_at_value k ({k ~v> x} e)). *)
(* Proof with auto. *)
(*   apply (value_mutual_rec *)
(*            (fun (e: value) => forall k, lc_at_value (S k) e <-> (exists (L:aset), forall (x: atom), x ∉ L -> lc_at_value k ({k ~v> x} e))) *)
(*            (fun (e: tm) => forall k, lc_at_tm (S k) e <-> (exists (L:aset), forall (x: atom), x ∉ L -> lc_at_tm k ({k ~t> x} e)))); *)
(*     simpl; split; intros; auto; *)
(*     try eempty_aset; *)
(*     try (repeat lc_at_tm_rewrite; auto_exists_L_and_solve). *)
(*   - destruct (Nat.eq_dec k bn). *)
(*     + subst. exists ∅... intros. rewrite decide_True... simpl... *)
(*     + exists ∅... intros. rewrite decide_False... simpl. lia. *)
(*   - ex_specialize_L. *)
(*     destruct (Nat.eq_dec k bn). *)
(*     + subst. rewrite decide_True in H... *)
(*     + rewrite decide_False in H... *)
(* Qed. *)

(* Lemma open_rec_lc_tm_aux: forall (v: value) (u: tm) (k: nat), lc_at_tm k u -> {k ~t> v} u = u. *)
(* Proof with eauto. *)
(*   intro v. *)
(*   apply (tm_mutual_rec *)
(*            (fun (u: value) => forall (k: nat), lc_at_value k u -> {k ~v> v} u = u) *)
(*            (fun (u: tm) => forall (k: nat), lc_at_tm k u -> {k ~t> v} u = u)); simpl; auto; intros; *)
(*     try repeat rewrite_by_fol; auto. *)
(*   - rewrite decide_False... lia. *)
(* Qed. *)

(* Lemma open_rec_lc_value_aux: forall (v: value) (u: value) (k: nat), lc_at_value k u -> {k ~v> v} u = u. *)
(* Proof with eauto. *)
(*   intro v. *)
(*   apply (value_mutual_rec *)
(*            (fun (u: value) => forall (k: nat), lc_at_value k u -> {k ~v> v} u = u) *)
(*            (fun (u: tm) => forall (k: nat), lc_at_tm k u -> {k ~t> v} u = u)); simpl; auto; intros; *)
(*     try repeat rewrite_by_fol; auto. *)
(*   - rewrite decide_False... lia. *)
(* Qed. *)

(* Ltac ltac_lc_at_tm_order := *)
(*   match goal with *)
(*   | [H : forall k k', _ -> _ -> _, H': ?k <= ?k' |- _ (S ?k') _] => *)
(*       specialize (H (S k) (S k')); apply H; auto; lia *)
(*   | [H : forall k k', _ -> _ -> _, H': ?k <= ?k' |- _ ?k' _] => *)
(*       specialize (H k k'); apply H; auto; lia *)
(*   | _ => auto; lia *)
(*   end. *)

(* Lemma lc_at_tm_order: forall u k k', k <= k' -> lc_at_tm k u -> lc_at_tm k' u. *)
(* Proof with auto. *)
(*   apply (tm_mutual_rec *)
(*            (fun (u: value) => forall k k', k <= k' -> lc_at_value k u -> lc_at_value k' u) *)
(*            (fun (u: tm) => forall k k', k <= k' -> lc_at_tm k u -> lc_at_tm k' u)); *)
(*     simpl; intros; auto; *)
(*     try repeat destruct_hyp_conj; repeat split; ltac_lc_at_tm_order. *)
(* Qed. *)

(* Lemma lc_at_value_order: forall u k k', k <= k' -> lc_at_value k u -> lc_at_value k' u. *)
(* Proof with auto. *)
(*   apply (value_mutual_rec *)
(*            (fun (u: value) => forall k k', k <= k' -> lc_at_value k u -> lc_at_value k' u) *)
(*            (fun (u: tm) => forall k k', k <= k' -> lc_at_tm k u -> lc_at_tm k' u)); *)
(*     simpl; intros; auto; *)
(*     try repeat destruct_hyp_conj; repeat split; ltac_lc_at_tm_order. *)
(* Qed. *)

(* There is a typo in the paper *)
Lemma fact1_tm: forall u v (e: tm) i j,
    i <> j -> {i ~t> u} ({j ~t> v} e) = {j ~t> v} e -> {i ~t> u} e = e.
Proof with auto.
  intros u v.
  apply (tm_mutual_rec
           (fun (e: value) => forall i j, i <> j -> {i ~v> u} ({j ~v> v} e) = {j ~v> v} e -> {i ~v> u} e = e)
           (fun (e: tm) => forall i j, i <> j -> {i ~t> u} ({j ~t> v} e) = {j ~t> v} e -> {i ~t> u} e = e)
        ); simpl; intros; auto;
    try repeat match goal with
               | [H: ?i <> ?j, H': context [_ -> _ = _], H'': ?a _ = ?a _ |- _ = _ ] =>
                   inversion H'';
                   (specialize (H' (S i) (S j)); rewrite H'; auto) || (specialize (H' i j); rewrite H'; auto)
               | [H: ?i <> ?j, H': context [_ -> _ = _], H'': ?a _ _ = ?a _ _ |- _ = _ ] =>
                   inversion H'';
                   (specialize (H' (S i) (S j)); rewrite H'; auto) || (specialize (H' i j); rewrite H'; auto)
               | [H: ?i <> ?j, H': context [_ -> _ = _], H'': ?a _ _ _ = ?a _ _ _ |- _ = _ ] =>
                   inversion H'';
                   (specialize (H' (S i) (S j)); rewrite H'; auto) || (specialize (H' i j); rewrite H'; auto)
               end.
  - repeat var_dec_solver.
Qed.

Lemma fact1_value: forall u v (e: value) i j,
    i <> j -> {i ~v> u} ({j ~v> v} e) = {j ~v> v} e -> {i ~v> u} e = e.
Proof with auto.
  intros u v.
  apply (value_mutual_rec
           (fun (e: value) => forall i j, i <> j -> {i ~v> u} ({j ~v> v} e) = {j ~v> v} e -> {i ~v> u} e = e)
           (fun (e: tm) => forall i j, i <> j -> {i ~t> u} ({j ~t> v} e) = {j ~t> v} e -> {i ~t> u} e = e)
        ); simpl; intros; auto;
    try repeat match goal with
               | [H: ?i <> ?j, H': context [_ -> _ = _], H'': ?a _ = ?a _ |- _ = _ ] =>
                   inversion H'';
                   (specialize (H' (S i) (S j)); rewrite H'; auto) || (specialize (H' i j); rewrite H'; auto)
               | [H: ?i <> ?j, H': context [_ -> _ = _], H'': ?a _ _ = ?a _ _ |- _ = _ ] =>
                   inversion H'';
                   (specialize (H' (S i) (S j)); rewrite H'; auto) || (specialize (H' i j); rewrite H'; auto)
               | [H: ?i <> ?j, H': context [_ -> _ = _], H'': ?a _ _ _ = ?a _ _ _ |- _ = _ ] =>
                   inversion H'';
                   (specialize (H' (S i) (S j)); rewrite H'; auto) || (specialize (H' i j); rewrite H'; auto)
               end.
  - repeat var_dec_solver.
Qed.

Lemma tvalue_eq: forall (v1 v2: value), tvalue v1 = tvalue v2 -> v1 = v2.
Proof.
  intros. inversion H. auto.
Qed.

Lemma open_rec_lc_tm: forall (v: value) (u: tm) (k: nat), lc u -> {k ~t> v} u = u.
Proof with eauto.
  intros. generalize dependent k.
  induction H; simpl; intros; auto;
    try (repeat match goal with
                | [H: forall (k: nat), _ = _ |- (_: tm) ] => rewrite H; auto
                | [H: forall (k: nat), _ = _ |- (_: value) ] =>
                    specialize (H k); simpl in H; apply tvalue_eq in H; rewrite H
                end; auto;
         auto_eq_post;
         let accL := collect_stales tt in
         pose (Atom.fv_of_set accL) as y;
         pose (Atom.fv_of_set_fresh accL);
         repeat match goal with
                | [H: forall (x: atom), _ |- _ ] =>
                    specialize (H y)
                end;
         apply fact1_tm with (j := 0) (v:= vfvar y); auto; auto_apply; my_set_solver).
Qed.

Lemma open_rec_lc_value: forall (v: value) (u: value) (k: nat), lc u -> {k ~v> v} u = u.
Proof with eauto.
  intros.
  apply open_rec_lc_tm with (v:=v) (k:=k) in H. simpl in H. apply tvalue_eq in H. auto.
Qed.

Lemma subst_open_value: forall (v: value) (x:atom) (u: value) (w: value) (k: nat),
    lc w -> {x := w}v ({k ~v> u} v) = ({k ~v> {x := w}v u} ({x := w}v v)).
Proof with eauto.
  apply (value_mutual_rec
           (fun (v: value) => forall (x:atom) (u: value) (w: value) (k: nat),
                lc w -> {x := w}v ({k ~v> u} v) = ({k ~v> {x := w}v u} ({x := w}v v)))
           (fun (e: tm) => forall (x:atom) (u: value) (w: value) (k: nat),
                lc w -> {x := w}t ({k ~t> u} e) = ({k ~t> {x := w}v u} ({x := w}t e)))
        ); simpl; intros; eauto; try (repeat rewrite_by_set_solver; auto);
    try repeat match goal with
               | [H: context [ tm_subst _ _ _ = _ ] |- _ ] => rewrite H; auto
               | [H: context [ value_subst _ _ _ = _ ] |- _ ] => rewrite H; auto
               end.
  - destruct (atom_dec x atom).
    + subst. rewrite decide_True... rewrite open_rec_lc_value; auto.
    + rewrite decide_False...
  - destruct (Nat.eq_dec k bn); simpl.
    + subst. rewrite decide_True... rewrite decide_True...
    + rewrite decide_False... rewrite decide_False...
Qed.

Lemma subst_open_tm: forall (v: tm) (x:atom) (u: value) (w: value) (k: nat),
    lc w -> {x := w}t ({k ~t> u} v) = ({k ~t> {x := w}v u} ({x := w}t v)).
Proof with eauto.
  apply (tm_mutual_rec
           (fun (v: value) => forall (x:atom) (u: value) (w: value) (k: nat),
                lc w -> {x := w}v ({k ~v> u} v) = ({k ~v> {x := w}v u} ({x := w}v v)))
           (fun (e: tm) => forall (x:atom) (u: value) (w: value) (k: nat),
                lc w -> {x := w}t ({k ~t> u} e) = ({k ~t> {x := w}v u} ({x := w}t e)))
        ); simpl; intros; eauto; try (repeat rewrite_by_set_solver; auto);
    try repeat match goal with
               | [H: context [ tm_subst _ _ _ = _ ] |- _ ] => rewrite H; auto
               | [H: context [ value_subst _ _ _ = _ ] |- _ ] => rewrite H; auto
               end.
  - destruct (atom_dec x atom).
    + subst. rewrite decide_True... rewrite open_rec_lc_value; auto.
    + rewrite decide_False...
  - destruct (Nat.eq_dec k bn); simpl.
    + subst. rewrite decide_True... rewrite decide_True...
    + rewrite decide_False... rewrite decide_False...
Qed.

Lemma close_var_rename_tm: forall x y (e: tm) k, y ∉ (fv_tm e) -> {k <t~ x} e = {k <t~ y} ({x := y}t e).
Proof with auto.
  intros x y.
  apply (tm_mutual_rec
           (fun (v: value) => forall k, y ∉ (fv_value v) -> {k <v~ x} v = {k <v~ y} ({x := y}v v))
           (fun (e: tm) => forall k, y ∉ (fv_tm e) -> {k <t~ x} e = {k <t~ y} ({x := y}t e))
        ); simpl; intros; auto; try (repeat rewrite_by_set_solver; auto).
  - destruct (atom_dec x atom).
     + subst. rewrite decide_True... rewrite decide_True... simpl. rewrite decide_True...
     + rewrite decide_False... rewrite decide_False... simpl.
       assert (y <> atom) by my_set_solver. rewrite decide_False...
Qed.

Lemma close_var_rename_value: forall x y (e: value) k, y ∉ (fv_value e) -> {k <v~ x} e = {k <v~ y} ({x := y}v e).
Proof with auto.
  intros x y.
  apply (value_mutual_rec
           (fun (v: value) => forall k, y ∉ (fv_value v) -> {k <v~ x} v = {k <v~ y} ({x := y}v v))
           (fun (e: tm) => forall k, y ∉ (fv_tm e) -> {k <t~ x} e = {k <t~ y} ({x := y}t e))
        ); simpl; intros; auto; try (repeat rewrite_by_set_solver; auto).
  - destruct (atom_dec x atom).
     + subst. rewrite decide_True... rewrite decide_True... simpl. rewrite decide_True...
     + rewrite decide_False... rewrite decide_False... simpl.
       assert (y <> atom) by my_set_solver. rewrite decide_False...
Qed.

(* The second class of lemmas *)

Lemma fact2_tm: forall (x y z: atom) (e: tm) i j,
    x <> y -> i <> j -> y ∉ fv_tm e ->
    {i ~t> y} ({j ~t> z} ({j <t~ x} e)) = {j ~t> z} ({j <t~ x} ({i ~t> y} e)).
Proof with auto.
  intros x y z.
  apply (tm_mutual_rec
           (fun (e: value) => forall i j,
                x <> y -> i <> j -> y ∉ fv_value e ->
                {i ~v> y} ({j ~v> z} ({j <v~ x} e)) = {j ~v> z} ({j <v~ x} ({i ~v> y} e)))
           (fun (e: tm) => forall i j,
                x <> y -> i <> j -> y ∉ fv_tm e ->
                {i ~t> y} ({j ~t> z} ({j <t~ x} e)) = {j ~t> z} ({j <t~ x} ({i ~t> y} e)))
        ); simpl; intros; auto; try (repeat rewrite_by_set_solver; auto); try (rewrite H; auto).
  - repeat var_dec_solver.
  - repeat var_dec_solver.
  - auto_eq_post; rewrite H0; auto; set_solver.
  - auto_eq_post; rewrite H1; auto; set_solver.
  - auto_eq_post; rewrite H1; auto; set_solver.
Qed.

Lemma fact2_value: forall (x y z: atom) (e: value) i j,
    x <> y -> i <> j -> y ∉ fv_tm e ->
    {i ~v> y} ({j ~v> z} ({j <v~ x} e)) = {j ~v> z} ({j <v~ x} ({i ~v> y} e)).
Proof with auto.
  intros x y z.
  apply (value_mutual_rec
           (fun (e: value) => forall i j,
                x <> y -> i <> j -> y ∉ fv_value e ->
                {i ~v> y} ({j ~v> z} ({j <v~ x} e)) = {j ~v> z} ({j <v~ x} ({i ~v> y} e)))
           (fun (e: tm) => forall i j,
                x <> y -> i <> j -> y ∉ fv_tm e ->
                {i ~t> y} ({j ~t> z} ({j <t~ x} e)) = {j ~t> z} ({j <t~ x} ({i ~t> y} e)))
        ); simpl; intros; auto; try (repeat rewrite_by_set_solver; auto); try (rewrite H; auto).
  - repeat var_dec_solver.
  - repeat var_dec_solver.
  - auto_eq_post; rewrite H0; auto; set_solver.
  - auto_eq_post; rewrite H1; auto; set_solver.
  - auto_eq_post; rewrite H1; auto; set_solver.
Qed.

Lemma subst_lc_tm: forall x (u: value) (t: tm), lc t -> lc u -> lc ({x := u}t t).
Proof with auto.
  intros x u t Hlct.
  induction Hlct; simpl; intros; auto; try (constructor; auto);
    try (auto_exists_L; intros; repeat split;
         repeat match goal with
                | [ H : context [lc _] |- lc (_ _ (vfvar ?x) _)] =>
                    specialize (H x);
                    rewrite subst_open_tm in H; auto; simpl in H;
                    rewrite decide_False in H; my_set_solver
                end).
  - repeat var_dec_solver.
Qed.

Lemma subst_lc_value: forall x (u: value) (t: value), lc t -> lc u -> lc ({x := u}t t).
Proof with auto.
  intros x u t Hlct.
  induction Hlct; simpl; intros; auto; try (constructor; auto);
    try (auto_exists_L; intros; repeat split;
         repeat match goal with
                | [ H : context [lc _] |- lc (_ _ (vfvar ?x) _)] =>
                    specialize (H x);
                    rewrite subst_open_tm in H; auto; simpl in H;
                    rewrite decide_False in H; my_set_solver
                end).
  - repeat var_dec_solver.
Qed.

Lemma open_close_var_tm_aux: forall (x: atom) (t: tm) (k: nat),
    {k ~t> x} t = t -> {k ~t> x} ({k <t~ x} t) = t.
Proof.
  intros x.
  apply (tm_mutual_rec
           (fun (t: value) => forall k, {k ~v> x} t = t -> {k ~v> x} ({k <v~ x} t) = t)
           (fun (t: tm) => forall k, {k ~t> x} t = t -> {k ~t> x} ({k <t~ x} t) = t));
    simpl; intros; auto;
    repeat (match goal with
         | [H: forall k, _ -> _ = _ |- _ ] => rewrite H; auto; clear H
         end); auto_eq_post; auto.
  - repeat var_dec_solver.
Qed.

Lemma open_close_var_tm: forall (x: atom) (t: tm), lc t -> {0 ~t> x} ({0 <t~ x} t) = t.
Proof.
  intros. apply open_close_var_tm_aux. apply open_rec_lc_tm; auto.
Qed.

Lemma open_close_var_value_aux: forall (x: atom) (t: value) (k: nat),
    {k ~v> x} t = t -> {k ~v> x} ({k <v~ x} t) = t.
Proof.
  intros x.
  apply (value_mutual_rec
           (fun (t: value) => forall k, {k ~v> x} t = t -> {k ~v> x} ({k <v~ x} t) = t)
           (fun (t: tm) => forall k, {k ~t> x} t = t -> {k ~t> x} ({k <t~ x} t) = t));
    simpl; intros; auto;
    repeat (match goal with
         | [H: forall k, _ -> _ = _ |- _ ] => rewrite H; auto; clear H
         end); auto_eq_post; auto.
  - repeat var_dec_solver.
Qed.

Lemma open_close_var_value: forall (x: atom) (t: value), lc t -> {0 ~v> x} ({0 <v~ x} t) = t.
Proof.
  intros. apply open_close_var_value_aux. apply open_rec_lc_value; auto.
Qed.

Lemma close_var_lc_tm': forall (x: atom) (t: tm),
    lc t -> forall k, body ({k ~t> x} t).
Proof.
  intros. generalize dependent k.
  induction H; simpl; intros; auto.
    (* try (auto_exists_L; intros; repeat split; auto). *)
  - auto_exists_L. intros; repeat split; auto. auto_exists_L.
  - auto_exists_L. intros; repeat split; auto. auto_exists_L.
  - auto_exists_L. intros; repeat split; auto. auto_exists_L.
  - let acc := collect_stales tt in pose acc.
    pose (fv_of_set a).
    pose (fv_of_set_fresh a).
    assert (s ∉ L) by my_set_solver.
    specialize (H0 s H1 (S k)). destruct H0.
    auto_exists_L. intros; repeat split; auto.
    auto_exists_L. intros; repeat split; auto. fold _open_tm.
Admitted.

Lemma close_var_lc_tm: forall (x: atom) (t: tm) (k: nat),
    lc t ->
    (exists (L: aset), forall (x': atom), x' ∉ L -> lc ({k ~t> x'} ({k <t~ x} t))).
Proof.
  intros. generalize dependent k.
  induction H; simpl; intros; auto.
    (* try (auto_exists_L; intros; repeat split; auto). *)
  - auto_exists_L; intros; repeat split; auto.
  - auto_exists_L; intros; repeat split; auto.
  - auto_exists_L; intros; repeat split; auto. repeat var_dec_solver. constructor.
  - 
    auto_exists_L; intros; repeat split; auto.
    auto_exists_L; intros; repeat split; auto.
    rewrite fact2_tm; auto.
    assert (x0 ∉ L) by my_set_solver.
    specialize (H0 x0 H3 (S k)).
    destruct H0. apply H0.
Admitted.

(* The third class of lemmas *)

Lemma subst_intro_tm: forall (v: tm) (x:atom) (w: value) (k: nat),
    x # v ->
    lc w -> {x := w}t ({k ~t> x} v) = ({k ~t> w} v).
Proof.
  intros.
  specialize (subst_open_tm v x x w k) as J.
  simpl in J. rewrite decide_True in J; auto.
  rewrite J; auto. rewrite subst_fresh_tm; auto.
Qed.

Lemma subst_intro_value: forall (v: value) (x:atom) (w: value) (k: nat),
    x # v ->
    lc w -> {x := w}v ({k ~v> x} v) = ({k ~v> w} v).
Proof.
  intros.
  specialize (subst_open_value v x x w k) as J.
  simpl in J. rewrite decide_True in J; auto.
  rewrite J; auto. rewrite subst_fresh_value; auto.
Qed.

Lemma subst_open_var_tm: forall x y (u: value) (t: tm) (k: nat),
    x <> y -> lc u -> {x := u}t ({k ~t> y} t) = ({k ~t> y} ({x := u}t t)).
Proof.
  intros.
  rewrite subst_open_tm; auto. simpl. rewrite decide_False; auto.
Qed.

Lemma subst_open_var_value: forall x y (u: value) (t: value) (k: nat),
    x <> y -> lc u -> {x := u}v ({k ~v> y} t) = ({k ~v> y} ({x := u}v t)).
Proof.
  intros.
  rewrite subst_open_value; auto. simpl. rewrite decide_False; auto.
Qed.

Lemma subst_body_tm: forall x (u: value) (t: tm), body t -> lc u -> body ({x := u}t t).
Proof with auto.
  intros.
  destruct H. auto_exists_L; intros; repeat split; auto.
  rewrite <- subst_open_var_tm; auto.
  apply subst_lc_tm; auto. apply H. my_set_solver. my_set_solver.
Qed.

Lemma subst_body_value: forall x (u: value) (t: value), body t -> lc u -> body ({x := u}v t).
Proof with auto.
  intros.
  destruct H. auto_exists_L; intros; repeat split; auto. simpl.
  rewrite <- subst_open_var_value; auto.
  apply subst_lc_value; auto. apply H. my_set_solver. my_set_solver.
Qed.

Lemma open_lc_tm: forall (u: value) (t: tm), body t -> lc u -> lc ({0 ~t> u} t).
Proof.
  intros. destruct H.
  let acc := collect_stales tt in pose acc.
  pose (Atom.fv_of_set a).
  assert (s ∉ a). apply Atom.fv_of_set_fresh.
  erewrite <- subst_intro_tm; auto. instantiate (1:= s).
  apply subst_lc_tm; auto. apply H.
  my_set_solver. my_set_solver.
Qed.

Lemma open_lc_value: forall (u: value) (t: value), body t -> lc u -> lc ({0 ~v> u} t).
Proof.
  intros. destruct H.
  let acc := collect_stales tt in pose acc.
  pose (Atom.fv_of_set a).
  assert (s ∉ a). apply Atom.fv_of_set_fresh.
  erewrite <- subst_intro_value; auto. instantiate (1:= s).
  apply subst_lc_value; auto. apply H.
  my_set_solver. my_set_solver.
Qed.

(* Lemma for MNF *)

Ltac solve_let_lc_body H :=
  split; intros; try repeat split; auto;
    inversion H; subst; auto;
    try destruct_hyp_conj; try match goal with
    | [ H: body _ |- _ ] => inversion H; subst; clear H
    (* | [ H: lc _ |- _ ] => inversion H; subst; clear H *)
    end;
    auto_exists_L; intros; repeat split; auto;
    auto_apply; my_set_solver.

Lemma lete_lc_body: forall e1 e, lc (tlete e1 e) <-> lc e1 /\ body e.
Proof.
  solve_let_lc_body H.
Qed.

Lemma letapp_lc_body: forall (v1 v2: value) e, lc (tletapp v1 v2 e) <-> lc v1 /\ lc v2 /\ body e.
Proof.
  solve_let_lc_body H.
Qed.

Lemma letbiop_lc_body: forall op (v1 v2: value) e, lc (tletbiop op v1 v2 e) <-> lc v1 /\ lc v2 /\ body e.
Proof.
  solve_let_lc_body H.
Qed.

Lemma lc_fix_iff_body: forall T e, lc (vfix T e) <-> body e.
Proof.
  split; unfold body; intros.
  - inversion H; subst. exists L. auto.
  - destruct H as (L & HL). econstructor. apply HL.
Qed.
