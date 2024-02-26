open Mtyped
open Sexplib.Std
open Constant
open Op

type 't value =
  | Const of constant
  | Var of (('t, string) typed[@free])
  | Lam of { lamarg : (('t, string) typed[@bound]); body : ('t, 't term) typed }
  | Fix of {
      fixname : (('t, string) typed[@bound]);
      fixarg : (('t, string) typed[@bound]);
      body : ('t, 't term) typed;
    }
  | VTu of ('t, 't value) typed list

and 't term =
  | Err
  | CVal of ('t, 't value) typed
  | LetE of {
      rhs : ('t, 't term) typed;
      lhs : (('t, string) typed[@bound]);
      body : ('t, 't term) typed;
    }
  | CLetDeTu of {
      turhs : ('t, 't value) typed;
      tulhs : (('t, string) typed list[@bound]);
      body : ('t, 't term) typed;
    }
  | CApp of { appf : ('t, 't value) typed; apparg : ('t, 't value) typed }
  | CAppOp of { op : ('t, op) typed; appopargs : ('t, 't value) typed list }
  | Match of {
      matched : ('t, 't value) typed;
      match_cases : 't match_case list;
    }

and 't match_case =
  | Matchcase of {
      constructor : ('t, string) typed;
      args : (('t, string) typed list[@bound]);
      exp : ('t, 't term) typed;
    }
[@@deriving sexp]

let rec fv_value (value_e : 't value) =
  match value_e with
  | Const _ -> []
  | Var _t_stringtyped0 -> [] @ [ _t_stringtyped0 ]
  | Lam { lamarg; body } ->
      Zzdatatype.Datatype.List.substract (typed_eq String.equal)
        ([] @ typed_fv_term body)
        [ lamarg ]
  | Fix { fixname; fixarg; body } ->
      Zzdatatype.Datatype.List.substract (typed_eq String.equal)
        (Zzdatatype.Datatype.List.substract (typed_eq String.equal)
           ([] @ typed_fv_term body)
           [ fixarg ])
        [ fixname ]
  | VTu _t__tvaluetypedlist0 ->
      [] @ List.concat (List.map typed_fv_value _t__tvaluetypedlist0)

and typed_fv_value (value_e : ('t, 't value) typed) = fv_value value_e.x

and fv_term (term_e : 't term) =
  match term_e with
  | Err -> []
  | CVal _t__tvaluetyped0 -> [] @ typed_fv_value _t__tvaluetyped0
  | LetE { rhs; lhs; body } ->
      Zzdatatype.Datatype.List.substract (typed_eq String.equal)
        ([] @ typed_fv_term body)
        [ lhs ]
      @ typed_fv_term rhs
  | CLetDeTu { turhs; tulhs; body } ->
      Zzdatatype.Datatype.List.substract (typed_eq String.equal)
        ([] @ typed_fv_term body)
        tulhs
      @ typed_fv_value turhs
  | CApp { appf; apparg } -> ([] @ typed_fv_value apparg) @ typed_fv_value appf
  | CAppOp { appopargs; _ } ->
      [] @ List.concat (List.map typed_fv_value appopargs)
  | Match { matched; _ } -> [] @ typed_fv_value matched

and typed_fv_term (term_e : ('t, 't term) typed) = fv_term term_e.x

and fv_match_case (match_case_e : 't match_case) =
  match match_case_e with
  | Matchcase { args; exp; _ } ->
      Zzdatatype.Datatype.List.substract (typed_eq String.equal)
        ([] @ typed_fv_term exp)
        args

and typed_fv_match_case (match_case_e : ('t, 't match_case) typed) =
  fv_match_case match_case_e.x

let rec subst_value (string_x : string) f (value_e : 't value) =
  match value_e with
  | Const constant0 -> Const constant0
  | Var _t_stringtyped0 ->
      if String.equal _t_stringtyped0.x string_x then f _t_stringtyped0
      else Var _t_stringtyped0
  | Lam { lamarg; body } ->
      if String.equal lamarg.x string_x then Lam { lamarg; body }
      else Lam { lamarg; body = typed_subst_term string_x f body }
  | Fix { fixname; fixarg; body } ->
      if String.equal fixname.x string_x then Fix { fixname; fixarg; body }
      else if String.equal fixarg.x string_x then Fix { fixname; fixarg; body }
      else Fix { fixname; fixarg; body = typed_subst_term string_x f body }
  | VTu _t__tvaluetypedlist0 ->
      VTu (List.map (typed_subst_value string_x f) _t__tvaluetypedlist0)

and typed_subst_value (string_x : string) f (value_e : ('t, 't value) typed) =
  value_e #-> (subst_value string_x f)

and subst_term (string_x : string) f (term_e : 't term) =
  match term_e with
  | Err -> Err
  | CVal _t__tvaluetyped0 ->
      CVal (typed_subst_value string_x f _t__tvaluetyped0)
  | LetE { rhs; lhs; body } ->
      if String.equal lhs.x string_x then
        LetE { rhs = typed_subst_term string_x f rhs; lhs; body }
      else
        LetE
          {
            rhs = typed_subst_term string_x f rhs;
            lhs;
            body = typed_subst_term string_x f body;
          }
  | CLetDeTu { turhs; tulhs; body } ->
      if List.exists (fun x -> String.equal string_x x.x) tulhs then
        CLetDeTu { turhs = typed_subst_value string_x f turhs; tulhs; body }
      else
        CLetDeTu
          {
            turhs = typed_subst_value string_x f turhs;
            tulhs;
            body = typed_subst_term string_x f body;
          }
  | CApp { appf; apparg } ->
      CApp
        {
          appf = typed_subst_value string_x f appf;
          apparg = typed_subst_value string_x f apparg;
        }
  | CAppOp { op; appopargs } ->
      CAppOp
        { op; appopargs = List.map (typed_subst_value string_x f) appopargs }
  | Match { matched; match_cases } ->
      Match { matched = typed_subst_value string_x f matched; match_cases }

and typed_subst_term (string_x : string) f (term_e : ('t, 't term) typed) =
  term_e #-> (subst_term string_x f)

and subst_match_case (string_x : string) f (match_case_e : 't match_case) =
  match match_case_e with
  | Matchcase { constructor; args; exp } ->
      if List.exists (fun x -> String.equal string_x x.x) args then
        Matchcase { constructor; args; exp }
      else
        Matchcase { constructor; args; exp = typed_subst_term string_x f exp }

and typed_subst_match_case (string_x : string) f
    (match_case_e : ('t, 't match_case) typed) =
  match_case_e #-> (subst_match_case string_x f)

let rec map_value (f : 't -> 's) (value_e : 't value) =
  match value_e with
  | Const constant0 -> Const constant0
  | Var _t_stringtyped0 -> Var _t_stringtyped0 #=> f
  | Lam { lamarg; body } ->
      Lam { lamarg = lamarg #=> f; body = typed_map_term f body }
  | Fix { fixname; fixarg; body } ->
      Fix
        {
          fixname = fixname #=> f;
          fixarg = fixarg #=> f;
          body = typed_map_term f body;
        }
  | VTu _t__tvaluetypedlist0 ->
      VTu (List.map (typed_map_value f) _t__tvaluetypedlist0)

and typed_map_value (f : 't -> 's) (value_e : ('t, 't value) typed) =
  value_e #-> (map_value f)

and map_term (f : 't -> 's) (term_e : 't term) =
  match term_e with
  | Err -> Err
  | CVal _t__tvaluetyped0 -> CVal (typed_map_value f _t__tvaluetyped0)
  | LetE { rhs; lhs; body } ->
      LetE
        {
          rhs = typed_map_term f rhs;
          lhs = lhs #=> f;
          body = typed_map_term f body;
        }
  | CLetDeTu { turhs; tulhs; body } ->
      CLetDeTu
        {
          turhs = typed_map_value f turhs;
          tulhs = List.map (fun x -> x #=> f) tulhs;
          body = typed_map_term f body;
        }
  | CApp { appf; apparg } ->
      CApp { appf = typed_map_value f appf; apparg = typed_map_value f apparg }
  | CAppOp { op; appopargs } ->
      CAppOp { op; appopargs = List.map (typed_map_value f) appopargs }
  | Match { matched; match_cases } ->
      Match { matched = typed_map_value f matched; match_cases }

and typed_map_term (f : 't -> 's) (term_e : ('t, 't term) typed) =
  term_e #-> (map_term f)

and map_match_case (f : 't -> 's) (match_case_e : 't match_case) =
  match match_case_e with
  | Matchcase { constructor; args; exp } ->
      Matchcase
        {
          constructor = constructor #=> f;
          args = List.map (fun x -> x #=> f) args;
          exp = typed_map_term f exp;
        }

and typed_map_match_case (f : 't -> 's)
    (match_case_e : ('t, 't match_case) typed) =
  match_case_e #-> (map_match_case f)

let fv_value_id e = fv_typed_id_to_id fv_value e
let typed_fv_value_id e = fv_typed_id_to_id typed_fv_value e
let fv_term_id e = fv_typed_id_to_id fv_term e
let typed_fv_term_id e = fv_typed_id_to_id typed_fv_term e
let fv_match_case_id e = fv_typed_id_to_id fv_match_case e
let typed_fv_match_case_id e = fv_typed_id_to_id typed_fv_match_case e

let subst_value_instance x instance e =
  subst_f_to_instance subst_value x instance e

let typed_subst_value_instance x instance e =
  subst_f_to_instance typed_subst_value x instance e

let subst_term_instance x instance e =
  subst_f_to_instance subst_term x instance e

let typed_subst_term_instance x instance e =
  subst_f_to_instance typed_subst_term x instance e

let subst_match_case_instance x instance e =
  subst_f_to_instance subst_match_case x instance e

let typed_subst_match_case_instance x instance e =
  subst_f_to_instance typed_subst_match_case x instance e
(* Generated from _term.ml *)
