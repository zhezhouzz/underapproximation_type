Set Warnings "-notation-overridden,-parsing".
From PLF Require Import Maps.
From PLF Require Import CoreLangSimp.
From Coq Require Import Lists.List.
From Coq Require Import Strings.String.
From Coq Require Import Logic.FunctionalExtensionality.
From Coq Require Import Logic.ClassicalFacts.
From PLF Require Import NormalTypeSystemSimp.
From PLF Require Import RfTypeDef.
From PLF Require Import LinearContext.
Import ListNotations.

Import NormalTypeSystemSimp.

(* First we define the ty state *)

Definition tystate := string -> option base_ty.

Definition state_in_tystate (st: state) (tyst: tystate): Prop :=
  forall x Tx, tyst x = Some Tx -> (exists c, st x = Some c).

Notation " st '\TYSTin' nst " := (state_in_tystate st nst) (at level 40).

(* First we define the non-deter state *)

(* the dependent pair definition is too strict *)
(* Definition typed (x: prod tm base_ty) : Prop := *)
(*   let (e, T) := x in (empty \N- e \Tin T). *)

(* Definition nstate := string -> option (sig typed). *)

(* Definition get_tm (pair: sig typed) : tm := fst (proj1_sig pair). *)
(* Definition get_base_ty (pair: sig typed) : base_ty := snd (proj1_sig pair). *)
(* Definition get_prop (pair: sig typed):= (proj2_sig pair). *)

Definition nstate := string -> option (prod tm base_ty).

(* If the nstate is not well founded the no state is its sub state. *)
Definition state_in_non_deter_state (st: state) (nst: nstate): Prop :=
  forall x, (match nst x with
        | None => True
        | Some (e, T) =>
            empty \N- e \Tin T /\
              match st x with
              | None => False
              | Some c => e -->* c
              end
        end).

Notation " st '\NSTin' nst " := (state_in_non_deter_state st nst) (at level 40).

Definition nstate_to_tystate (nst: nstate) :=
  fun x => (match nst x with
         | None => None
         | Some (_, T) => Some T
         end).

Notation " 'st\_' st '_/' " := (nstate_to_tystate st) (at level 40).

Definition nstate_to_tystate_empty: (st\_ empty _/) = empty.
Proof with eauto.
  apply functional_extensionality...
Qed.

Global Hint Rewrite nstate_to_tystate_empty: core.

Definition nstate_to_tystate_hd: forall nst x e_x T,
    (st\_ x |-> (e_x, T); nst _/) = (x |-> T; (st\_ nst _/)).
Proof with eauto.
  intros. apply functional_extensionality. intros x'. unfold nstate_to_tystate.
  destruct (eqb_spec x x'); subst...
  - rewrite update_eq. rewrite update_eq. reflexivity.
  - rewrite update_neq... rewrite update_neq...
Qed.

Global Hint Rewrite nstate_to_tystate_hd: core.

Definition nstate_tystate_same_none: forall nst x, nst x = None <-> (st\_ nst _/) x = None.
Admitted.

Global Hint Rewrite nstate_tystate_same_none: core.

Definition st_in_nst_implies_in_tyst: forall st (nst: nstate),
    st \NSTin nst -> st \TYSTin (nstate_to_tystate nst).
Proof with eauto.
  intros.
  intros x Tx Herase.
  assert (match nst x with
          | Some (e, T) =>
              empty \N- e \Tin T /\ match st x with
                                   | Some c => e -->* c
                                   | None => False
                                   end
          | None => True
          end)... apply H.
  unfold nstate_to_tystate in Herase.
  destruct (nst x); subst.
  - destruct p; subst. inversion Herase; subst. destruct H0.
    destruct (st x); subst. exists c. auto. inversion H1.
  - inversion Herase.
Qed.

Global Hint Resolve st_in_nst_implies_in_tyst: core.

Definition nstate_not_bot_implies_tystate_update: forall x e_x (T: base_ty) nst st,
    st \NSTin (x |-> (e_x, T); nst) ->
    st \TYSTin (x |-> T; st\_ nst _/).
Proof with eauto.
  intros.
  apply st_in_nst_implies_in_tyst in H.
  intros x' Tx HH. eapply H with (Tx := Tx). unfold nstate_to_tystate.
  destruct (eqb_spec x x'); subst...
  - rewrite update_eq in HH. rewrite update_eq...
  - rewrite update_neq in HH... rewrite update_neq...
Qed.

Global Hint Resolve nstate_not_bot_implies_tystate_update: core.

(* Definition reduction_implies_trans: forall st nst x e_x (c_x: constant), *)
(*     e_x -->* c_x -> *)
(*     st \NSTin (x |-> tvalue c_x; nst) -> *)
(*     st \NSTin (x |-> e_x; nst). *)
(* Proof with eauto. *)
(*   unfold state_in_non_deter_state. intros. *)
(*   assert (match (x |-> c_x; nst) x0 with *)
(*           | Some e => match st x0 with *)
(*                      | Some c => e -->* c *)
(*                      | None => False *)
(*                      end *)
(*           | None => True *)
(*           end *)
(*          ) as HH... apply H0. *)
(*   destruct (eqb_spec x x0); subst... *)
(*   + rewrite update_eq. rewrite update_eq in HH... *)
(*     destruct (st x0)... inversion HH; subst... inversion H1... *)
(*   + rewrite update_neq... rewrite update_neq in HH... *)
(* Qed. *)

(* Global Hint Resolve reduction_implies_trans: core. *)

Definition state_order (st st': state) := (forall x c_x, st x = Some c_x -> st' x = Some c_x).
Notation " st '<st<' st' " := (state_order st st') (at level 80).

Lemma state_order_trans: forall st1 st2 st3, st1 <st< st2 -> st2 <st< st3 -> st1 <st< st3.
Proof.
  unfold state_order. intros. auto.
Qed.

Global Hint Resolve state_order_trans: core.
