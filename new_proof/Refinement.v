From stdpp Require Import mapset.
From stdpp Require Import natmap.
From CT Require Import CoreLangProp.
From CT Require Import OperationalSemanticsProp.
From CT Require Import BasicTypingProp.
From CT Require Import SyntaxSugar.

Import Atom.
Import CoreLang.
Import Tactics.
Import NamelessTactics.
Import ListCtx.
Import OperationalSemantics.
Import OperationalSemanticsProp.
Import BasicTyping.
Import SyntaxSugar.

(* state works like vfvar *)
Definition state := amap constant.

Lemma empty_map_dom_is_empty: dom aset (∅ : state) = ∅.
Proof. fast_set_solver. Qed.

#[global]
Instance state_stale : @Stale aset state := dom aset.
Arguments state_stale /.

(* bstate works like vfbar, which is a finite map with a bound. *)
Definition bstate := nat -> constant.

Definition bstate_insert (k: nat) (c: constant) (bst: bstate) := fun i => if decide (i = k) then c else bst i.

Definition bstate_push (c: constant) (bst: bstate) : bstate := fun i => match i with 0 => c | S i => bst i end.

(* Definition bstate_find (bst: bstate) (i: nat) : option constant := *)
(*   match bst with *)
(*   | b{ n }[ m ] => if decide (i < n) then Some (m i) else None *)
(*   end. *)

Definition bstate_emp: bstate := fun _ => 0.

Notation "b∅" := bstate_emp (format "b∅").
Notation " '<b[' k ':=' c ']>' " := (bstate_insert k c) (at level 5, right associativity, format "<b[ k := c ]>", c constr).
Notation " '<b[↦' c ']>' " := (bstate_push c) (at level 5, right associativity, format "<b[↦ c ]>", c constr).
(* Notation " m 'b!!' i " := (m i) (at level 80, m constr, i constr). *)

Definition ty_of_state (st: state) : amap base_ty := ty_of_const <$> st.

(* In order to defined the type denotation (logical relation) which is a recursive function, we should make sure the refinement type is structurally decreasing, which means we cannot do substution (or open, close) over the refinement types. Thus we lift all substition (open, close) into states: bstate is for bound variables, state is for free variables. *)
Definition refinement : Type := bstate -> state -> constant -> Prop.

(* n is the upper bound of indices that are allowed to be accessed; doesn't mean all idices appear in the refinement. *)
(* d is a finite set of free variables that are allowed to be accessed; doesn't mean all free variables appear in the refinement. *)
Inductive rty : Type :=
| BaseOver (B: base_ty) (n: nat) (d: aset) (ϕ: refinement)
| BaseUnder (B: base_ty) (n: nat) (d: aset) (ϕ: refinement)
| DependArrow (B: base_ty) (n: nat) (d: aset) (ϕ: refinement) (τ: rty)
| IndependArrow (τ1: rty) (τ2: rty).

Global Hint Constructors rty: core.

Notation "'[v:' B '|' n '|' d '|' ϕ ']'" :=
  (BaseUnder B n d ϕ) (at level 5, format "[v: B | n | d | ϕ ]", B constr, ϕ constr).
Notation "'{v:' B '|' n '|' d '|' ϕ '}'" :=
  (BaseOver B n d ϕ) (at level 5, format "{v: B | n | d | ϕ }", B constr, ϕ constr).
Notation "'-:{v:' B '|' n '|' d '|' ϕ '}' '⤑' τ" := (DependArrow B n d ϕ τ) (at level 80, right associativity, B constr, ϕ constr, τ constr).
Notation " τ1 '⤑' τ2 " := (IndependArrow τ1 τ2) (at level 80, right associativity, τ1 constr, τ2 constr).

(* free variables *)

Fixpoint rty_fv τ : aset :=
  match τ with
  | {v: _ | _ | d | _ } => d
  | [v: _ | _ | d | _ ] => d
  | -:{v: _ | _ | d | _ } ⤑ τ => d ∪ (rty_fv τ)
  | τ1 ⤑ τ2 => (rty_fv τ1) ∪ (rty_fv τ2)
  end.

#[global]
Instance rty_stale : @Stale aset rty := rty_fv.
Arguments rty_stale /.

(* open; here we still open to a value *)

Definition refinement_open (k: nat) (s: value) (ϕ: refinement) : refinement :=
  fun bst st v => ϕ (match s with
                  | vfvar x =>
                      match st !! x with
                      | None => bst
                      | Some c => <b[ k := c ]> bst
                      end
                  | vconst c => <b[ k := c ]> bst
                  | vbvar _ => bst
                  | vlam _ _ => bst
                  | vfix _ _ => bst
                  end) st v.

Definition refinement_set_open (k: nat) (s: value) (d: aset) : aset :=
  match s with
  | vfvar x => {[x]} ∪ d
  | vconst c => d
  | vbvar _ => d
  | vlam _ _ => d
  | vfix _ _ => d
  end.

Fixpoint rty_open (k: nat) (s: value) (τ: rty) : rty :=
  match τ with
  | {v: B | n | d | ϕ } => {v: B | n `min` k | refinement_set_open k s d | refinement_open k s ϕ }
  | [v: B | n | d | ϕ ] => [v: B |  n `min` k | refinement_set_open k s d | refinement_open k s ϕ ]
  | -:{v: B | n | d | ϕ } ⤑ τ =>
      -:{v: B |  n `min` k | refinement_set_open k s d | refinement_open k s ϕ } ⤑ (rty_open (S k) s τ)
  | τ1 ⤑ τ2 => (rty_open k s τ1) ⤑ (rty_open k s τ2)
  end.

Notation "'{' k '~r>' s '}' e" := (rty_open k s e) (at level 20, k constr).

Notation "e '^r^' s" := (rty_open 0 s e) (at level 20).

(* subst *)

(* The refinement type only works for the value with the base type (constants), thus the subst over lambda terms doesn't (shouldn't) works; and we should never update a non-lc term (e.g., vbvar _). *)
(* The subst should guarantee: *)
(* 1. the variable x1 is not in in the current state. *)
(* 2. if the value v2 is a free variable x2, it should be in the current state, but not used in the further refinements. *)

Definition state_insert_value: atom -> value -> state -> state :=
  fun x1 v2 st => match v2 with
               | vfvar x2 =>
                   match st !! x2 with
                   | None => st
                   | Some c2 => <[x1 := c2]> st
                   end
               | vconst c2 => (<[x1 := c2]> st)
               | vbvar _ => st
               | vlam _ _ => st
               | vfix _ _ => st
               end.

Notation " '{' x '↦' v '}' " := (state_insert_value x v) (at level 20, format "{ x ↦ v }", x constr, v constr).

Definition state_subst_var (a b: atom) (st: state) :=
  match st !! b with
  | None => delete a st
  | Some c => <[a := c]> st
  end.

Definition state_subst: aset -> atom -> value -> state -> state :=
  fun d x1 v2 st =>
    if decide (x1 ∈ d) then
      match v2 with
      | vfvar x2 =>
          if decide (x2 ∈ d) then
            state_subst_var x1 x2 st
          else st
      | vconst c2 => <[x1 := c2]> st
      | vbvar _ => st
      | vlam _ _ => st
      | vfix _ _ => st
      end
    else st.

Notation " '{' x ':={' d '}' v '}' " := (state_subst d x v) (at level 20, format "{ x :={ d } v }", x constr, v constr).

Definition refinement_subst (x1: atom) (v2: value) (d: aset) (ϕ: refinement) : refinement :=
  (fun bst st v => ϕ bst (state_subst d x1 v2 st) v).
  (* if decide (x1 ∈ d) then *)
  (*   (fun bst st v => ϕ bst (state_insert_value x1 v2 st) v) *)
  (* else *)
  (*   ϕ. *)
    (* (fun bst st v => ϕ bst (delete x1 st) v). *)
  (* fun bst st v => ϕ bst (state_insert_value x1 v2 st) v. *)
  (* fun bst st v => ϕ bst (state_subst x1 v2 st) v. *)

Definition refinement_set_subst (x1: atom) (s: value) (d: aset) : aset :=
  if decide (x1 ∈ d) then
    match s with
    | vfvar x =>
        if decide (x ∈ d) then
          if decide (x1 = x) then d else (d ∖ {[x1]})
        else d
    | vconst c => d ∖ {[x1]}
    | vbvar _ => d
    | vlam _ _ => d
    | vfix _ _ => d
    end
  else
    d.

Fixpoint rty_subst (x: atom) (s: value) (τ: rty) : rty :=
  match τ with
  | {v: B | n | d | ϕ } =>
      {v: B | n | refinement_set_subst x s d | refinement_subst x s d ϕ }
      (* if decide (x ∈ d) *)
      (* then {v: B | n | d | refinement_subst x s ϕ } *)
      (* else {v: B | n | d | ϕ } *)
  | [v: B | n | d | ϕ ] => [v: B | n | refinement_set_subst x s d | refinement_subst x s d ϕ ]
      (* if decide (x ∈ d) *)
      (* then [v: B | n | refinement_set_subst x s d | refinement_subst x s ϕ ] *)
      (* else [v: B | n | d | ϕ ] *)
  | -:{v: B | n | d | ϕ } ⤑ τ =>
      -:{v: B | n | refinement_set_subst x s d | refinement_subst x s d ϕ } ⤑ (rty_subst x s τ)
      (* if decide (x ∈ d) *)
      (* then -:{v: B | n | refinement_set_subst x s d | refinement_subst x s ϕ } ⤑ (rty_subst x s τ) *)
      (* else -:{v: B | n | d | ϕ } ⤑ (rty_subst x s τ) *)
  | τ1 ⤑ τ2 => (rty_subst x s τ1) ⤑ (rty_subst x s τ2)
  end.

Notation "'{' x ':=' s '}r'" := (rty_subst x s) (at level 20, format "{ x := s }r", x constr).

(* well formed, locally closed, closed with state *)

Definition not_fv_in_refinement (d: aset) (ϕ: refinement) :=
  forall (m m': state),
    (forall (x: atom), x ∈ d -> m !! x = m' !! x) ->
    forall bst (v: constant), ϕ bst m v <-> ϕ bst m' v.

Definition bst_eq (n: nat) (bst1 bst2: bstate) := forall i, i < n -> bst1 i = bst2 i.

Definition bound_in_refinement (bound: nat) (ϕ: refinement) :=
  forall (bst bst': bstate) (st: state) v, bst_eq bound bst bst' -> ϕ bst st v <-> ϕ bst' st v.

Lemma bound_in_refinement_0: forall ϕ,
    bound_in_refinement 0 ϕ -> (forall bst bst' st v, ϕ bst st v <-> ϕ bst' st v).
Proof.
  intros. apply H. intros. intro n. intros. exfalso. lia.
Qed.

Ltac var_dec_solver2 :=
  repeat (match goal with
          | [|- context [decide ?H]] => destruct (decide H); simpl; auto
          end; try var_dec_solver).

Lemma not_in_aset_implies_same_in_refinement: forall (d: aset) (ϕ: refinement),
    not_fv_in_refinement d ϕ ->
    forall (x: atom), x ∉ d -> forall (m: state), forall bst v_x (v: constant), ϕ bst ({x :={d} v_x} m) v <-> ϕ bst m v.
Proof.
  intros. apply H. intros.
  destruct v_x; unfold state_subst; simpl; auto; var_dec_solver2.
Qed.

Definition wf_r (n: nat) (d: aset) (ϕ: refinement) :=
  not_fv_in_refinement d ϕ /\ bound_in_refinement n ϕ.

Definition not_underbasety rty: Prop :=
  match rty with
  | BaseUnder _ _ _ _ => False
  | _ => True
  end.

Lemma not_not_underbasety: forall τ, ~ not_underbasety τ -> (exists b n d ϕ, τ = [v: b | n | d | ϕ ]).
Proof.
  intros.
  destruct τ; simpl in H.
  - exfalso; auto.
  - repeat eexists; auto.
  - exfalso; auto.
  - exfalso; auto.
Qed.

Definition not_overbasety rty: Prop :=
  match rty with
  | BaseOver _ _ _ _ => False
  | _ => True
  end.

Definition is_arr rty: Prop :=
  match rty with
  | BaseOver _ _ _ _ => False
  | BaseUnder _ _ _ _ => False
  | _ => True
  end.

Lemma not_is_arr: forall τ, ~ is_arr τ -> (exists b n d ϕ, τ = [v: b | n | d | ϕ ]) \/ (exists b n d ϕ, τ = {v: b | n | d | ϕ }).
Proof.
  intros.
  destruct τ; simpl in H.
  - right. repeat eexists; auto.
  - left. repeat eexists; auto.
  - exfalso; auto.
  - exfalso; auto.
Qed.

Inductive valid_rty: rty -> Prop :=
| valid_rty_over_base: forall B n d ϕ, wf_r n d ϕ -> valid_rty {v: B | n | d | ϕ }
| valid_rty_under_base: forall B n d ϕ, wf_r n d ϕ -> valid_rty [v: B | n | d | ϕ ]
| valid_rty_oarr: forall B n d ϕ (τ: rty),
    wf_r n d ϕ ->
    not_overbasety τ ->
    valid_rty τ -> valid_rty ( -:{v: B | n | d | ϕ } ⤑ τ)
| valid_rty_arrarr1: forall B n d ϕ (τ1 τ2: rty),
    valid_rty ( -:{v: B | n | d | ϕ } ⤑ τ1) ->
    not_overbasety τ2 ->
    valid_rty τ2 ->
    valid_rty (( -:{v: B | n | d | ϕ } ⤑ τ1) ⤑ τ2)
| valid_rty_arrarr2: forall (τ11 τ12 τ2: rty),
    valid_rty (τ11 ⤑ τ12) ->
    valid_rty τ2 ->
    not_overbasety τ2 ->
    valid_rty ((τ11 ⤑ τ12) ⤑ τ2).

Inductive lc_rty_idx: nat -> rty -> Prop :=
| lc_rty_idx_baseover: forall B n m d ϕ, m <= n -> lc_rty_idx n {v: B | m | d | ϕ }
| lc_rty_idx_underover: forall B n m d ϕ, m <= n -> lc_rty_idx n [v: B | m | d | ϕ ]
| lc_rty_idx_oarr: forall B n m d ϕ τ,
    lc_rty_idx (S n) τ ->
    m <= n ->
    lc_rty_idx n (-:{v: B | m | d | ϕ } ⤑ τ)
| lc_rty_idx_arrarr: forall n τ1 τ2, lc_rty_idx n τ1 -> lc_rty_idx n τ2 -> lc_rty_idx n (τ1 ⤑ τ2).

Definition lc_rty τ := lc_rty_idx 0 τ.

Definition rty_body τ := lc_rty_idx 1 τ.

(* Definition bstdom (bst: bstate) := *)
(*   match bst with *)
(*   | bstate_constr b _ => b *)
(*   end. *)

Definition closed_rty (b: nat) (d: aset) (τ: rty) :=
  valid_rty τ /\ lc_rty_idx b τ /\ (rty_fv τ) ⊆ d.

Inductive ok_dctx: aset -> listctx rty -> Prop :=
| ok_dctx_nil: forall (d: aset), ok_dctx d []
| ok_dctx_cons_base: forall (d: aset) (x: atom) (τ: rty) (Γ: listctx rty),
    ~ is_arr τ ->
    closed_rty 0 d τ -> x ∉ d -> x ∉ ctxdom Γ -> ok_dctx ({[x]} ∪ d) Γ ->
    ok_dctx d ((x, τ) :: Γ)
| ok_dctx_cons_arr: forall (d: aset) (x: atom) (τ: rty) (Γ: listctx rty),
    is_arr τ ->
    closed_rty 0 d τ -> x ∉ d -> x ∉ ctxdom Γ -> ok_dctx  d Γ ->
    ok_dctx d ((x, τ) :: Γ).

Lemma ok_dctx_regular1: forall d Γ, ok_dctx d Γ -> ok Γ /\ (ctxdom Γ ∩ d ≡ ∅).
Proof.
  intros. induction H; mydestr.
  - split; auto. fast_set_solver.
  - split. rewrite ok_pre_destruct. split; auto. simpl. set_solver.
  - split. rewrite ok_pre_destruct. split; auto. simpl. set_solver.
Qed.

(* Inductive cl_dctx: aset -> listctx rty -> Prop := *)
(* | cl_dctx_nil: forall d, cl_dctx d [] *)
(* | cl_dctx_cons: forall d (x: atom) τ Γ, *)
(*     ok_dctx d ((x, τ) :: Γ) -> *)
(*     cl_dctx ({[x]} ∪ d) Γ -> closed_rty 0 d τ -> cl_dctx d ((x, τ) :: Γ). *)

Definition ctx_closed_rty (d: aset) (Γ: listctx rty) :=
  forall Γ1 (x: atom) (τ: rty) Γ2, Γ = Γ1 ++ [(x, τ)] ++ Γ2 -> closed_rty 0 (ctxdom Γ1 ∪ d) τ.

(* Erase *)

Fixpoint rty_erase ut : ty :=
  match ut with
  | {v: T | _ | _ | _ } => T
  | [v: T | _ | _ | _ ] => T
  | -:{v: T1 | _ | _ | _ } ⤑ τ => T1 ⤍ (rty_erase τ)
  | t1 ⤑ t2 => (rty_erase t1) ⤍ (rty_erase t2)
  end.

Notation " '⌊' ty '⌋' " := (rty_erase ty) (at level 5, format "⌊ ty ⌋", ty constr).

Definition listctx_fmap {A: Type} {B: Type} (f: A -> B) (l: listctx A) :=
  List.map (fun e => (e.1, f e.2)) l.

Definition ctx_erase := listctx_fmap rty_erase.

Notation " '⌊' Γ '⌋*' " := (ctx_erase Γ) (at level 5, format "⌊ Γ ⌋*", Γ constr).

(* Ty Function *)

Definition mk_eq_constant c := [v: ty_of_const c | 0 | ∅ | fun _ _ v => v = c ].
Definition mk_under_bot ty := [v: ty | 0 | ∅ | fun _ _ _ => False ].
Definition mk_under_top ty := [v: ty | 0 | ∅ | fun _ _ _ => True ].
Definition mk_over_top ty := {v: ty | 0 | ∅ | fun _ _ _ => True }.
Definition mk_eq_var ty (x: atom) := [v: ty | 0 | {[x]} | fun _ st v => Some v = st !! x ].
Definition mk_op_ret op :=
  [v: ret_ty_of_op op | 2 | ∅ | fun bst _ v => eval_op op (bst 0) (bst 1) v ].
Definition mk_op op :=
  -:{v: fst_ty_of_op op | 0 | ∅ | fun _ _ _ => True } ⤑
      (-:{v: snd_ty_of_op op | 1 | ∅ | fun _ _ _ => True } ⤑
          mk_op_ret op).

Definition well_founded_R_constant (c1 c2: constant) :=
  match c1 with
  | cnat n1 =>
      match c2 with
      | cnat n2 => n1 < n2
      | cbool b2 => False
      end
  | cbool b1 =>
      match c2 with
      | cnat n2 => True
      | cbool b2 => b1 = false /\ b2 = true
      end
  end.

Notation " b '≻' a " := (well_founded_R_constant a b) (at level 20, a constr, b constr).

Definition bool_measurement (b: bool) :=
  match b with
  | false => 1
  | true => 2
  end.

Lemma bool_wf': forall (m: nat) (b2: bool), bool_measurement b2 <= m -> Acc well_founded_R_constant b2.
Proof.
  induction m; simpl; intros; simpl in H; intros.
  - destruct b2; inversion H.
  - destruct b2; invclear H; constructor; intros; mydestr; subst; destruct y; try invclear H.
    + apply IHm; simpl; auto.
    + apply IHm; simpl; auto. simpl in H1; lia.
    + inversion H1.
    + inversion H2.
Qed.

Lemma nat_wf': forall (m: nat) (n2: nat), S n2 <= m -> Acc well_founded_R_constant n2.
Proof.
  induction m; simpl; intros; simpl in H; intros. inversion H.
  assert (n2 = m \/ S n2 ≤ m) by lia. destruct H0; auto; subst. clear H.
  constructor; intros; mydestr; subst; destruct y. eapply bool_wf'; eauto.
  apply IHm; simpl in H; lia.
Qed.

Lemma well_founded_R_constant_is_well_founded: well_founded well_founded_R_constant.
Proof.
  red; intros. destruct a.
  - eapply bool_wf'; eauto.
  - eapply nat_wf'; eauto.
Qed.

Definition well_founded_constraint (ϕ: refinement) := fun bst st c => (bst 0) ≻ c /\ ϕ bst st c.

Notation " '≻≻' ϕ " := (well_founded_constraint ϕ) (at level 20, ϕ constr).

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
