open Prop.T
open Z3aux
open Z3

let make_forall ctx qv body =
  if List.length qv == 0 then body
  else
    Quantifier.expr_of_quantifier
      (Quantifier.mk_forall_const ctx qv body (Some 1) [] [] None None)

let make_exists ctx qv body =
  if List.length qv == 0 then body
  else
    Quantifier.expr_of_quantifier
      (Quantifier.mk_exists_const ctx qv body (Some 1) [] [] None None)

let z3func ctx funcname inptps outtp =
  FuncDecl.mk_func_decl ctx
    (Symbol.mk_string ctx funcname)
    (List.map (tp_to_sort ctx) inptps)
    (tp_to_sort ctx outtp)

let lit_to_z3 ctx lit =
  let rec aux = function
    | ACint n -> int_to_z3 ctx n
    | ACbool n -> bool_to_z3 ctx n
    | AVar x -> tpedvar_to_z3 ctx (x.ty, x.x)
    | AOp2 (mp, a, b) -> (
        let a = aux a in
        let b = aux b in
        match mp with
        | "==" -> Z3.Boolean.mk_eq ctx a b
        | "!=" -> Z3.Boolean.(mk_not ctx @@ mk_eq ctx a b)
        | "<=" -> Z3.Arithmetic.mk_le ctx a b
        | ">=" -> Z3.Arithmetic.mk_ge ctx a b
        | "<" -> Z3.Arithmetic.mk_lt ctx a b
        | ">" -> Z3.Arithmetic.mk_gt ctx a b
        | "+" -> Z3.Arithmetic.mk_add ctx [ a; b ]
        | "-" -> Z3.Arithmetic.mk_sub ctx [ a; b ]
        | _ -> failwith "unknown operator")
  in
  aux lit

let to_z3 ctx prop =
  let rec aux prop =
    match prop with
    | Lit lit -> lit_to_z3 ctx lit
    | Implies (p1, p2) -> Z3.Boolean.mk_implies ctx (aux p1) (aux p2)
    | Ite (p1, p2, p3) -> Z3.Boolean.mk_ite ctx (aux p1) (aux p2) (aux p3)
    | Not p -> Z3.Boolean.mk_not ctx (aux p)
    | And ps -> Z3.Boolean.mk_and ctx (List.map aux ps)
    | Or ps -> Z3.Boolean.mk_or ctx (List.map aux ps)
    | Iff (p1, p2) ->
        (* let () = *)
        (*   Printf.printf "make <=>: %s, %s \n" *)
        (*     (Expr.to_string @@ aux p1) *)
        (*     (Expr.to_string @@ aux p2) *)
        (* in *)
        Z3.Boolean.mk_iff ctx (aux p1) (aux p2)
    | Forall (u, body) ->
        make_forall ctx [ tpedvar_to_z3 ctx (u.ty, u.x) ] (aux body)
    | Exists (u, body) ->
        make_exists ctx [ tpedvar_to_z3 ctx (u.ty, u.x) ] (aux body)
    | MethodPred (mp, args) ->
        let argsty = List.map lit_get_ty args in
        let args = List.map (lit_to_z3 ctx) args in
        let func = z3func ctx mp argsty Ty_bool in
        Z3.FuncDecl.apply func args
  in
  aux prop
