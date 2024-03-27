open Mtyped
open Sexplib.Std
open Constant
open Op

type 't value =
  | VConst of constant
  | VVar of (('t, string) typed[@free])
  | VLam of {
      lamarg : (('t, string) typed[@bound]);
      body : ('t, 't term) typed;
    }
  | VFix of {
      fixname : (('t, string) typed[@bound]);
      fixarg : (('t, string) typed[@bound]);
      body : ('t, 't term) typed;
    }
  | VTu of ('t, 't value) typed list

and 't term =
  | CErr
  | CVal of ('t, 't value) typed
  | CLetE of {
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
  | CMatch of {
      matched : ('t, 't value) typed;
      match_cases : 't match_case list;
    }

and 't match_case =
  | CMatchcase of {
      constructor : ('t, string) typed;
      args : (('t, string) typed list[@bound]);
      exp : ('t, 't term) typed;
    }
[@@deriving sexp]

let rec fv_value (value_e : 't value) =
  match value_e with
  | VConst _ -> []
  | VVar _t_stringtyped0 -> [] @ [ _t_stringtyped0 ]
  | VLam { lamarg; body } ->
      Zzdatatype.Datatype.List.substract (typed_eq String.equal)
        ([] @ typed_fv_term body)
        [ lamarg ]
  | VFix { fixname; fixarg; body } ->
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
  | CErr -> []
  | CVal _t__tvaluetyped0 -> [] @ typed_fv_value _t__tvaluetyped0
  | CLetE { rhs; lhs; body } ->
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
  | CMatch { matched; match_cases } ->
      (List.concat @@ List.map fv_match_case match_cases)
      @ typed_fv_value matched

and typed_fv_term (term_e : ('t, 't term) typed) = fv_term term_e.x

and fv_match_case (match_case_e : 't match_case) =
  match match_case_e with
  | CMatchcase { args; exp; _ } ->
      Zzdatatype.Datatype.List.substract (typed_eq String.equal)
        ([] @ typed_fv_term exp)
        args

and typed_fv_match_case (match_case_e : ('t, 't match_case) typed) =
  fv_match_case match_case_e.x

let rec subst_value (string_x : string) f (value_e : 't value) =
  match value_e with
  | VConst constant0 -> VConst constant0
  | VVar _t_stringtyped0 ->
      if String.equal _t_stringtyped0.x string_x then f _t_stringtyped0
      else VVar _t_stringtyped0
  | VLam { lamarg; body } ->
      if String.equal lamarg.x string_x then VLam { lamarg; body }
      else VLam { lamarg; body = typed_subst_term string_x f body }
  | VFix { fixname; fixarg; body } ->
      if String.equal fixname.x string_x then VFix { fixname; fixarg; body }
      else if String.equal fixarg.x string_x then VFix { fixname; fixarg; body }
      else VFix { fixname; fixarg; body = typed_subst_term string_x f body }
  | VTu _t__tvaluetypedlist0 ->
      VTu (List.map (typed_subst_value string_x f) _t__tvaluetypedlist0)

and typed_subst_value (string_x : string) f (value_e : ('t, 't value) typed) =
  value_e #-> (subst_value string_x f)

and subst_term (string_x : string) f (term_e : 't term) =
  match term_e with
  | CErr -> CErr
  | CVal _t__tvaluetyped0 ->
      CVal (typed_subst_value string_x f _t__tvaluetyped0)
  | CLetE { rhs; lhs; body } ->
      if String.equal lhs.x string_x then
        CLetE { rhs = typed_subst_term string_x f rhs; lhs; body }
      else
        CLetE
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
  | CMatch { matched; match_cases } ->
      CMatch
        {
          matched = typed_subst_value string_x f matched;
          match_cases = List.map (subst_match_case string_x f) match_cases;
        }

and typed_subst_term (string_x : string) f (term_e : ('t, 't term) typed) =
  term_e #-> (subst_term string_x f)

and subst_match_case (string_x : string) f (match_case_e : 't match_case) =
  match match_case_e with
  | CMatchcase { constructor; args; exp } ->
      if List.exists (fun x -> String.equal string_x x.x) args then
        CMatchcase { constructor; args; exp }
      else
        CMatchcase { constructor; args; exp = typed_subst_term string_x f exp }

and typed_subst_match_case (string_x : string) f
    (match_case_e : ('t, 't match_case) typed) =
  match_case_e #-> (subst_match_case string_x f)

let rec map_value (f : 't -> 's) (value_e : 't value) =
  match value_e with
  | VConst constant0 -> VConst constant0
  | VVar _t_stringtyped0 -> VVar _t_stringtyped0 #=> f
  | VLam { lamarg; body } ->
      VLam { lamarg = lamarg #=> f; body = typed_map_term f body }
  | VFix { fixname; fixarg; body } ->
      VFix
        {
          fixname = fixname #=> f;
          fixarg = fixarg #=> f;
          body = typed_map_term f body;
        }
  | VTu _t__tvaluetypedlist0 ->
      VTu (List.map (typed_map_value f) _t__tvaluetypedlist0)

and typed_map_value (f : 't -> 's) (value_e : ('t, 't value) typed) =
  value_e #=> f #-> (map_value f)

and map_term : 't 's. ('t -> 's) -> 't term -> 's term =
 fun f term_e ->
  match term_e with
  | CErr -> CErr
  | CVal _t__tvaluetyped0 -> CVal (typed_map_value f _t__tvaluetyped0)
  | CLetE { rhs; lhs; body } ->
      CLetE
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
      CAppOp
        { op = op #=> f; appopargs = List.map (typed_map_value f) appopargs }
  | CMatch { matched; match_cases } ->
      CMatch
        {
          matched = typed_map_value f matched;
          match_cases = List.map (map_match_case f) match_cases;
        }

and typed_map_term :
      't 's. ('t -> 's) -> ('t, 't term) typed -> ('s, 's term) typed =
 fun f term_e -> term_e #=> f #-> (map_term f)

and map_match_case : 't 's. ('t -> 's) -> 't match_case -> 's match_case =
 fun f match_case_e ->
  match match_case_e with
  | CMatchcase { constructor; args; exp } ->
      CMatchcase
        {
          constructor = constructor #=> f;
          args = List.map (fun x -> x #=> f) args;
          exp = typed_map_term f exp;
        }

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
open Sugar

let value_to_term v = (CVal v) #: v.ty

let term_to_value e =
  match e.x with
  | CVal v -> v.x #: e.ty
  | _ ->
      Printf.printf "%s\n"
        (Sexplib.Sexp.to_string @@ sexp_of_term Nt.sexp_of_t e.x);
      _failatwith __FILE__ __LINE__ "die"

let id_to_value v = (VVar v) #: v.ty
let id_to_term v = value_to_term @@ id_to_value v

let mk_lam lamarg body =
  (VLam { lamarg; body }) #: (Nt.mk_arr lamarg.ty body.ty)

let mk_id_function ty =
  let lamarg = "x" #: ty in
  (VLam { lamarg; body = id_to_term lamarg }) #: (Nt.mk_arr ty ty)

let mk_fix fixname fixarg body = (VFix { fixname; fixarg; body }) #: fixname.ty

let lam_to_fix fixname body =
  match body.x with
  | VLam { lamarg; body } -> mk_fix fixname lamarg body
  | _ -> _failatwith __FILE__ __LINE__ ""

let lam_to_fix_comp fixname body =
  value_to_term (lam_to_fix fixname (term_to_value body))

let mk_lete lhs rhs body = (CLetE { lhs; rhs; body }) #: body.ty
let mk_app appf apparg = (CApp { appf; apparg }) #: (Nt.get_retty appf.ty)

let mk_appop op appopargs =
  (CAppOp { op; appopargs }) #: (snd @@ Nt.destruct_arr_tp op.ty)

let get_constant_from_typed_value v =
  match v.x with VConst c -> Some c | _ -> None

let get_constants_from_typed_values v =
  let cs = List.filter_map get_constant_from_typed_value v in
  if List.length cs == List.length v then Some cs else None

let rec term_to_const_opt e =
  match e.x with
  | CVal v -> value_to_const_opt v
  | CAppOp { op; appopargs } -> (
      match op.x with
      | DtConstructor opname -> (
          match get_constants_from_typed_values appopargs with
          | Some cs -> Some (Dt (opname, cs)) #: e.ty
          | None -> None)
      | _ -> None)
  | _ -> None

and value_to_const_opt v =
  match v.x with VConst c -> Some c #: v.ty | _ -> None
