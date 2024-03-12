open Z3
open Z3aux
open Lang
open Sugar

let constant_to_z3 ctx c =
  let open Constant in
  match c with
  | U | Tu _ | Dt _ ->
      _failatwith __FILE__ __LINE__ "unimp complex constant encoding"
  | B b -> bool_to_z3 ctx b
  | I i -> int_to_z3 ctx i

let rec typed_lit_to_z3 ctx lit =
  match lit.x with
  | ATu _ | AProj _ -> _failatwith __FILE__ __LINE__ "die"
  | AC c -> constant_to_z3 ctx c
  | AVar x -> tpedvar_to_z3 ctx (x.ty, x.x)
  | AAppOp (op, args) -> (
      let args = List.map (typed_lit_to_z3 ctx) args in
      match (op.x, args) with
      | "==", [ a; b ] -> Boolean.mk_eq ctx a b
      | "!=", [ a; b ] -> Boolean.mk_not ctx @@ Boolean.mk_eq ctx a b
      | "<=", [ a; b ] -> Arithmetic.mk_le ctx a b
      | ">=", [ a; b ] -> Arithmetic.mk_ge ctx a b
      | "<", [ a; b ] -> Arithmetic.mk_lt ctx a b
      | ">", [ a; b ] -> Arithmetic.mk_gt ctx a b
      | "+", [ a; b ] -> Arithmetic.mk_add ctx [ a; b ]
      | "-", [ a; b ] -> Arithmetic.mk_sub ctx [ a; b ]
      | "mod", [ a; b ] -> Arithmetic.Integer.mk_mod ctx a b
      | "*", [ a; b ] -> Arithmetic.mk_mul ctx [ a; b ]
      | "/", [ a; b ] -> Arithmetic.mk_div ctx a b
      | opname, args ->
          let argsty, retty = Nt.destruct_arr_tp op.ty in
          let func = z3func ctx opname argsty retty in
          Z3.FuncDecl.apply func args)
