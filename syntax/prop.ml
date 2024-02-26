open Sexplib.Std
open Mtyped
module Nt = Normalty.Ntyped
open Lit

type 't prop =
  | Lit of ('t, 't lit) typed
  | Implies of 't prop * 't prop
  | Ite of 't prop * 't prop * 't prop
  | Not of 't prop
  | And of 't prop list
  | Or of 't prop list
  | Iff of 't prop * 't prop
  | MethodPred of { mpred : string; args : ('t, 't lit) typed list }
  | Forall of { qv : (('t, string) typed[@bound]); body : 't prop }
  | Exists of { qv : (('t, string) typed[@bound]); body : 't prop }
[@@deriving sexp]

let rec fv_prop (prop_e : 't prop) =
  match prop_e with
  | Lit _t__tlittyped0 -> [] @ typed_fv_lit _t__tlittyped0
  | Implies (_tprop0, _tprop1) -> ([] @ fv_prop _tprop1) @ fv_prop _tprop0
  | Ite (_tprop0, _tprop1, _tprop2) ->
      (([] @ fv_prop _tprop2) @ fv_prop _tprop1) @ fv_prop _tprop0
  | Not _tprop0 -> [] @ fv_prop _tprop0
  | And _tproplist0 -> [] @ List.concat (List.map fv_prop _tproplist0)
  | Or _tproplist0 -> [] @ List.concat (List.map fv_prop _tproplist0)
  | Iff (_tprop0, _tprop1) -> ([] @ fv_prop _tprop1) @ fv_prop _tprop0
  | MethodPred { args; _ } -> [] @ List.concat (List.map typed_fv_lit args)
  | Forall { qv; body } ->
      Zzdatatype.Datatype.List.substract (typed_eq String.equal)
        ([] @ fv_prop body)
        [ qv ]
  | Exists { qv; body } ->
      Zzdatatype.Datatype.List.substract (typed_eq String.equal)
        ([] @ fv_prop body)
        [ qv ]

and typed_fv_prop (prop_e : ('t, 't prop) typed) = fv_prop prop_e.x

let rec subst_prop (string_x : string) f (prop_e : 't prop) =
  match prop_e with
  | Lit _t__tlittyped0 -> Lit (typed_subst_lit string_x f _t__tlittyped0)
  | Implies (_tprop0, _tprop1) ->
      Implies (subst_prop string_x f _tprop0, subst_prop string_x f _tprop1)
  | Ite (_tprop0, _tprop1, _tprop2) ->
      Ite
        ( subst_prop string_x f _tprop0,
          subst_prop string_x f _tprop1,
          subst_prop string_x f _tprop2 )
  | Not _tprop0 -> Not (subst_prop string_x f _tprop0)
  | And _tproplist0 -> And (List.map (subst_prop string_x f) _tproplist0)
  | Or _tproplist0 -> Or (List.map (subst_prop string_x f) _tproplist0)
  | Iff (_tprop0, _tprop1) ->
      Iff (subst_prop string_x f _tprop0, subst_prop string_x f _tprop1)
  | MethodPred { mpred; args } ->
      MethodPred { mpred; args = List.map (typed_subst_lit string_x f) args }
  | Forall { qv; body } ->
      if String.equal qv.x string_x then Forall { qv; body }
      else Forall { qv; body = subst_prop string_x f body }
  | Exists { qv; body } ->
      if String.equal qv.x string_x then Exists { qv; body }
      else Exists { qv; body = subst_prop string_x f body }

and typed_subst_prop (string_x : string) f (prop_e : ('t, 't prop) typed) =
  prop_e #-> (subst_prop string_x f)

let rec map_prop (f : 't -> 's) (prop_e : 't prop) =
  match prop_e with
  | Lit _t__tlittyped0 -> Lit (typed_map_lit f _t__tlittyped0)
  | Implies (_tprop0, _tprop1) ->
      Implies (map_prop f _tprop0, map_prop f _tprop1)
  | Ite (_tprop0, _tprop1, _tprop2) ->
      Ite (map_prop f _tprop0, map_prop f _tprop1, map_prop f _tprop2)
  | Not _tprop0 -> Not (map_prop f _tprop0)
  | And _tproplist0 -> And (List.map (map_prop f) _tproplist0)
  | Or _tproplist0 -> Or (List.map (map_prop f) _tproplist0)
  | Iff (_tprop0, _tprop1) -> Iff (map_prop f _tprop0, map_prop f _tprop1)
  | MethodPred { mpred; args } ->
      MethodPred { mpred; args = List.map (typed_map_lit f) args }
  | Forall { qv; body } -> Forall { qv = qv #=> f; body = map_prop f body }
  | Exists { qv; body } -> Exists { qv = qv #=> f; body = map_prop f body }

and typed_map_prop (f : 't -> 's) (prop_e : ('t, 't prop) typed) =
  prop_e #-> (map_prop f)

let fv_prop_id e = fv_typed_id_to_id fv_prop e
let typed_fv_prop_id e = fv_typed_id_to_id typed_fv_prop e

let subst_prop_instance x instance e =
  subst_f_to_instance subst_prop x instance e

let typed_subst_prop_instance x instance e =
  subst_f_to_instance typed_subst_prop x instance e
(* Generated from _prop.ml *)
