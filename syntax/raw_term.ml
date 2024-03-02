open Mtyped
open Sexplib.Std
open Constant
open Op

type 't raw_term =
  | Var of (('t, string) typed[@free])
  | Const of constant
  | Lam of {
      lamarg : (('t, string) typed[@bound]);
      lambody : ('t, 't raw_term) typed;
    }
  | Err
  | Let of {
      if_rec : bool;
      rhs : ('t, 't raw_term) typed;
      lhs : (('t, string) typed list[@bound]);
      letbody : ('t, 't raw_term) typed;
    }
  | App of ('t, 't raw_term) typed * ('t, 't raw_term) typed list
  | AppOp of ('t, op) typed * ('t, 't raw_term) typed list
  | Ite of
      ('t, 't raw_term) typed
      * ('t, 't raw_term) typed
      * ('t, 't raw_term) typed
  | Tu of ('t, 't raw_term) typed list
  | Match of {
      matched : ('t, 't raw_term) typed;
      match_cases : 't raw_match_case list;
    }

and 't raw_match_case =
  | Matchcase of {
      constructor : ('t, string) typed;
      args : (('t, string) typed list[@bound]);
      exp : ('t, 't raw_term) typed;
    }
[@@deriving sexp]

let rec fv_raw_term (raw_term_e : 't raw_term) =
  match raw_term_e with
  | Var _t_stringtyped0 -> [] @ [ _t_stringtyped0 ]
  | Const _ -> []
  | Lam { lamarg; lambody } ->
      Zzdatatype.Datatype.List.substract (typed_eq String.equal)
        ([] @ typed_fv_raw_term lambody)
        [ lamarg ]
  | Err -> []
  | Let { rhs; lhs; letbody; _ } ->
      Zzdatatype.Datatype.List.substract (typed_eq String.equal)
        ([] @ typed_fv_raw_term letbody)
        lhs
      @ typed_fv_raw_term rhs
  | App (_t__traw_termtyped0, _t__traw_termtypedlist1) ->
      ([] @ List.concat (List.map typed_fv_raw_term _t__traw_termtypedlist1))
      @ typed_fv_raw_term _t__traw_termtyped0
  | AppOp (_, _t__traw_termtypedlist1) ->
      [] @ List.concat (List.map typed_fv_raw_term _t__traw_termtypedlist1)
  | Ite (_t__traw_termtyped0, _t__traw_termtyped1, _t__traw_termtyped2) ->
      (([] @ typed_fv_raw_term _t__traw_termtyped2)
      @ typed_fv_raw_term _t__traw_termtyped1)
      @ typed_fv_raw_term _t__traw_termtyped0
  | Tu _t__traw_termtypedlist0 ->
      [] @ List.concat (List.map typed_fv_raw_term _t__traw_termtypedlist0)
  | Match { matched; match_cases } ->
      ([] @ List.concat (List.map fv_raw_match_case match_cases))
      @ typed_fv_raw_term matched

and typed_fv_raw_term (raw_term_e : ('t, 't raw_term) typed) =
  fv_raw_term raw_term_e.x

and fv_raw_match_case (raw_match_case_e : 't raw_match_case) =
  match raw_match_case_e with
  | Matchcase { args; exp; _ } ->
      Zzdatatype.Datatype.List.substract (typed_eq String.equal)
        ([] @ typed_fv_raw_term exp)
        args

and typed_fv_raw_match_case (raw_match_case_e : ('t, 't raw_match_case) typed) =
  fv_raw_match_case raw_match_case_e.x

let rec subst_raw_term (string_x : string) f (raw_term_e : 't raw_term) =
  match raw_term_e with
  | Var _t_stringtyped0 ->
      if String.equal _t_stringtyped0.x string_x then f _t_stringtyped0
      else Var _t_stringtyped0
  | Const constant0 -> Const constant0
  | Lam { lamarg; lambody } ->
      if String.equal lamarg.x string_x then Lam { lamarg; lambody }
      else Lam { lamarg; lambody = typed_subst_raw_term string_x f lambody }
  | Err -> Err
  | Let { if_rec; rhs; lhs; letbody } ->
      if List.exists (fun x -> String.equal string_x x.x) lhs then
        Let { if_rec; rhs = typed_subst_raw_term string_x f rhs; lhs; letbody }
      else
        Let
          {
            if_rec;
            rhs = typed_subst_raw_term string_x f rhs;
            lhs;
            letbody = typed_subst_raw_term string_x f letbody;
          }
  | App (_t__traw_termtyped0, _t__traw_termtypedlist1) ->
      App
        ( typed_subst_raw_term string_x f _t__traw_termtyped0,
          List.map (typed_subst_raw_term string_x f) _t__traw_termtypedlist1 )
  | AppOp (_t_optyped0, _t__traw_termtypedlist1) ->
      AppOp
        ( _t_optyped0,
          List.map (typed_subst_raw_term string_x f) _t__traw_termtypedlist1 )
  | Ite (_t__traw_termtyped0, _t__traw_termtyped1, _t__traw_termtyped2) ->
      Ite
        ( typed_subst_raw_term string_x f _t__traw_termtyped0,
          typed_subst_raw_term string_x f _t__traw_termtyped1,
          typed_subst_raw_term string_x f _t__traw_termtyped2 )
  | Tu _t__traw_termtypedlist0 ->
      Tu (List.map (typed_subst_raw_term string_x f) _t__traw_termtypedlist0)
  | Match { matched; match_cases } ->
      Match
        {
          matched = typed_subst_raw_term string_x f matched;
          match_cases = List.map (subst_raw_match_case string_x f) match_cases;
        }

and typed_subst_raw_term (string_x : string) f
    (raw_term_e : ('t, 't raw_term) typed) =
  raw_term_e #-> (subst_raw_term string_x f)

and subst_raw_match_case (string_x : string) f
    (raw_match_case_e : 't raw_match_case) =
  match raw_match_case_e with
  | Matchcase { constructor; args; exp } ->
      if List.exists (fun x -> String.equal string_x x.x) args then
        Matchcase { constructor; args; exp }
      else
        Matchcase
          { constructor; args; exp = typed_subst_raw_term string_x f exp }

and typed_subst_raw_match_case (string_x : string) f
    (raw_match_case_e : ('t, 't raw_match_case) typed) =
  raw_match_case_e #-> (subst_raw_match_case string_x f)

let rec map_raw_term : 't 's. ('t -> 's) -> 't raw_term -> 's raw_term =
 fun f raw_term_e ->
  match raw_term_e with
  | Var _t_stringtyped0 -> Var _t_stringtyped0 #=> f
  | Const constant0 -> Const constant0
  | Lam { lamarg; lambody } ->
      Lam { lamarg = lamarg #=> f; lambody = typed_map_raw_term f lambody }
  | Err -> Err
  | Let { if_rec; rhs; lhs; letbody } ->
      Let
        {
          if_rec;
          rhs = typed_map_raw_term f rhs;
          lhs = List.map (fun x -> x #=> f) lhs;
          letbody = typed_map_raw_term f letbody;
        }
  | App (_t__traw_termtyped0, _t__traw_termtypedlist1) ->
      App
        ( typed_map_raw_term f _t__traw_termtyped0,
          List.map (typed_map_raw_term f) _t__traw_termtypedlist1 )
  | AppOp (_t_optyped0, _t__traw_termtypedlist1) ->
      AppOp
        ( _t_optyped0 #=> f,
          List.map (typed_map_raw_term f) _t__traw_termtypedlist1 )
  | Ite (_t__traw_termtyped0, _t__traw_termtyped1, _t__traw_termtyped2) ->
      Ite
        ( typed_map_raw_term f _t__traw_termtyped0,
          typed_map_raw_term f _t__traw_termtyped1,
          typed_map_raw_term f _t__traw_termtyped2 )
  | Tu _t__traw_termtypedlist0 ->
      Tu (List.map (typed_map_raw_term f) _t__traw_termtypedlist0)
  | Match { matched; match_cases } ->
      Match
        {
          matched = typed_map_raw_term f matched;
          match_cases = List.map (map_raw_match_case f) match_cases;
        }

and typed_map_raw_term :
      't 's. ('t -> 's) -> ('t, 't raw_term) typed -> ('s, 's raw_term) typed =
 fun (f : 't -> 's) (raw_term_e : ('t, 't raw_term) typed) ->
  raw_term_e #=> f #-> (map_raw_term f)

and map_raw_match_case (f : 't -> 's) (raw_match_case_e : 't raw_match_case) =
  match raw_match_case_e with
  | Matchcase { constructor; args; exp } ->
      Matchcase
        {
          constructor = constructor #=> f;
          args = List.map (fun x -> x #=> f) args;
          exp = typed_map_raw_term f exp;
        }

and typed_map_raw_match_case (f : 't -> 's)
    (raw_match_case_e : ('t, 't raw_match_case) typed) =
  raw_match_case_e #-> (map_raw_match_case f)

let fv_raw_term_id e = fv_typed_id_to_id fv_raw_term e
let typed_fv_raw_term_id e = fv_typed_id_to_id typed_fv_raw_term e
let fv_raw_match_case_id e = fv_typed_id_to_id fv_raw_match_case e
let typed_fv_raw_match_case_id e = fv_typed_id_to_id typed_fv_raw_match_case e

let subst_raw_term_instance x instance e =
  subst_f_to_instance subst_raw_term x instance e

let typed_subst_raw_term_instance x instance e =
  subst_f_to_instance typed_subst_raw_term x instance e

let subst_raw_match_case_instance x instance e =
  subst_f_to_instance subst_raw_match_case x instance e

let typed_subst_raw_match_case_instance x instance e =
  subst_f_to_instance typed_subst_raw_match_case x instance e

(* Generated from _raw_term.ml *)
open Sugar

let rec __get_lam_term_ty file line = function
  | Lam { lamarg; lambody } -> (
      let t1 =
        match lamarg.ty with
        | Some t1 -> t1
        | None -> _failatwith file line "__get_lam_term_ty"
      in
      match lambody.ty with
      | Some t2 -> Nt.mk_arr t1 t2
      | None -> Nt.mk_arr t1 (__get_lam_term_ty file line lambody.x))
  | _ -> _failatwith file line "__get_lam_term_ty"
