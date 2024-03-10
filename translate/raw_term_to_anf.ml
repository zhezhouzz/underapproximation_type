open Raw_term
open Term
open Item
open Mtyped
open Sugar

type 't cont = ('t, 't term) typed -> ('t, 't term) typed
type 't vcont = ('t, 't value) typed -> ('t, 't term) typed
type 't vconts = ('t, 't value) typed list -> ('t, 't term) typed

let new_x () = Rename.unique "x"
let construct_lete lhs rhs body = (CLetE { lhs; rhs; body }) #: body.ty
let var_to_v x = (VVar x) #: x.ty

(* let vcont_to_cont (k : 't vcont) (rhs : comp typed) : *)
(*     comp typed = *)
(*   let lhs = (new_x ()) #: rhs.ty in *)
(*   construct_lete lhs rhs (k (var_to_v lhs)) *)

let decurry (f, args) =
  let rec aux f = function
    | [] -> f
    | arg :: args -> aux (App (f, [ arg ])) #: (Nt.get_retty f.ty) args
  in
  aux f args

let rec normalize_term (tm : ('t, 't raw_term) typed) : ('t, 't term) typed =
  normalize_get_comp (fun x -> x) tm

and normalize_get_value (k : 't vcont) (expr : ('t, 't raw_term) typed) :
    ('t, 't term) typed =
  normalize_get_comp
    (fun e ->
      match e.x with
      | CVal v -> k v
      | _ ->
          let lhs = (new_x ()) #: e.ty in
          construct_lete lhs e (k @@ var_to_v lhs))
    expr

and normalize_get_values (k : 't vconts) (es : ('t, 't raw_term) typed list) :
    ('t, 't term) typed =
  (List.fold_left
     (fun (k : 't vconts) rhs ids ->
       normalize_get_value (fun id -> k (id :: ids)) rhs)
     k es)
    []

and normalize_get_comp (k : 't cont) (expr : ('t, 't raw_term) typed) :
    ('t, 't term) typed =
  (* let k e = k' e #: expr.ty in *)
  let kv v = k (value_to_term v) in
  match expr.x with
  | Err -> k CErr #: expr.ty
  | Tu es ->
      normalize_get_values
        (fun vs -> kv (VTu vs) #: (Nt.mk_tuple (List.map _get_ty vs)))
        es
  | Var var -> kv (VVar var) #: expr.ty
  | Const c -> kv (VConst c) #: expr.ty
  (* NOTE: do we need a name of a function? *)
  | Lam { lamarg; lambody } ->
      kv (VLam { lamarg; body = normalize_term lambody }) #: expr.ty
  | Let { if_rec; lhs; rhs; letbody } -> (
      match (if_rec, lhs) with
      | true, fixname :: fixarg :: args ->
          normalize_get_comp
            (fun rhs ->
              let fixbody =
                List.fold_left
                  (fun e lamarg -> value_to_term (mk_lam lamarg e))
                  rhs args
              in
              let rhs = value_to_term @@ mk_fix fixname fixarg fixbody in
              construct_lete fixname rhs (normalize_get_comp k letbody))
            rhs
      | true, _ -> _failatwith __FILE__ __LINE__ "bad"
      | false, [] -> _failatwith __FILE__ __LINE__ "bad"
      | false, [ lhs ] ->
          normalize_get_comp
            (fun rhs -> construct_lete lhs rhs (normalize_get_comp k letbody))
            rhs
      | false, tulhs ->
          normalize_get_value
            (fun rhs ->
              let body = normalize_get_comp k letbody in
              (CLetDeTu { tulhs; turhs = rhs; body }) #: body.ty)
            rhs)
  | AppOp (op, es) ->
      normalize_get_values (fun appopargs -> k (mk_appop op appopargs)) es
  | App (func, [ arg ]) ->
      normalize_get_value
        (fun appf -> normalize_get_value (fun arg -> k (mk_app appf arg)) arg)
        func
  | App (func, args) -> normalize_get_comp k (decurry (func, args))
  | Ite (cond, et, ef) ->
      normalize_get_comp k
        (Match
           {
             matched = cond;
             match_cases =
               [
                 Matchcase
                   { constructor = "True" #: Nt.bool_ty; args = []; exp = et };
                 Matchcase
                   { constructor = "False" #: Nt.bool_ty; args = []; exp = ef };
               ];
           })
        #: expr.ty
  | Match { matched; match_cases } ->
      normalize_get_value
        (fun matched ->
          let match_cases =
            List.map
              (function
                | Matchcase { constructor; args; exp } ->
                    CMatchcase
                      { constructor; args; exp = normalize_get_comp k exp })
              match_cases
          in
          (CMatch { matched; match_cases }) #: expr.ty)
        matched

let normalize_item (item : Nt.t item) =
  match item with
  | MFuncImpRaw { name; if_rec; body } ->
      let body = normalize_term body in
      let body =
        if if_rec then lam_to_fix_comp name.x #: body.ty body else body
      in
      MFuncImp { name; if_rec; body }
  | _ -> item

let normalize_structure = List.map normalize_item
