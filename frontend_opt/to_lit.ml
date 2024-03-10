open Ocaml5_parser
open Mtyped
open Zzdatatype.Datatype
module Nt = Normalty.Frontend
open Raw_term
open Lit
open Op
open To_op
open To_raw_term
open Sugar

(* NOTE: drop type notation *)
let rec lit_to_raw_term expr =
  let aux expr =
    match expr with
    | AC c -> Const c
    | AAppOp (op, args) ->
        let op = op #-> (fun x -> PrimOp x) in
        let args = List.map typed_lit_to_typed_raw_term args in
        AppOp (op, args)
    | ATu l -> Tu (List.map typed_lit_to_typed_raw_term l)
    | AProj _ -> _failatwith __FILE__ __LINE__ "unimp"
    | AVar x -> Var x.x #: None
  in
  aux expr

and typed_lit_to_typed_raw_term expr = (lit_to_raw_term expr.x) #: None

let rec layout_lit_to_smtlib2 expr =
  let aux expr =
    match expr with
    | AC c -> To_constant.layout_constant c
    | AAppOp (op, args) ->
        let op = match op.x with "==" -> "=" | _ -> op.x in
        spf "(%s %s)" op (List.split_by " " layout_typed_lit_to_smtlib2 args)
    | ATu _ -> _failatwith __FILE__ __LINE__ "unimp"
    | AProj _ -> _failatwith __FILE__ __LINE__ "unimp"
    | AVar x -> x.x
  in
  aux expr

and layout_typed_lit_to_smtlib2 expr = layout_lit_to_smtlib2 expr.x

let typed_lit_to_expr expr =
  typed_raw_term_to_expr @@ typed_lit_to_typed_raw_term expr

let lit_to_expr expr = raw_term_to_expr @@ lit_to_raw_term expr
let layout lit = Pprintast.string_of_expression @@ lit_to_expr lit
let layout_typed_lit lit = layout lit.x

let rec raw_term_to_lit e =
  match e with
  | Const c -> AC c
  | Var id -> AVar id
  | AppOp (op, args) ->
      AAppOp (op #-> layout_op, List.map typed_raw_term_to_typed_lit args)
  | App (op, args) ->
      AAppOp (op #-> layout_raw_term, List.map typed_raw_term_to_typed_lit args)
  | Tu es -> ATu (List.map typed_raw_term_to_typed_lit es)
  | _ ->
      _failatwith __FILE__ __LINE__
      @@ spf "parsing: not a op (%s)"
      @@ layout_raw_term e

and typed_raw_term_to_typed_lit expr = expr #-> raw_term_to_lit

let typed_lit_of_expr e = typed_raw_term_to_typed_lit (typed_raw_term_of_expr e)
let lit_of_expr e = (typed_lit_of_expr e).x
