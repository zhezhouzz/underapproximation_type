open Prop.Prop
open Z3aux
let to_z3 ctx prop =
  let get_ty x =
    match x.ty with
      | None -> failwith "untyped prop"
      | Some ty -> ty in
  let rec aux prop =
    match prop with
    | True -> bool_to_z3 ctx true
    | Var x ->
      tpedvar_to_z3 ctx (get_ty x, x.x)
    | Implies (p1, p2) -> Z3.Boolean.mk_implies ctx (aux p1) (aux p2)
    | Ite (p1, p2, p3) -> Z3.Boolean.mk_ite ctx (aux p1) (aux p2) (aux p3)
    | Not p -> Z3.Boolean.mk_not ctx (aux p)
    | And ps -> Z3.Boolean.mk_and ctx (List.map aux ps)
    | Or ps -> Z3.Boolean.mk_or ctx (List.map aux ps)
    | Iff (p1, p2) -> Z3.Boolean.mk_iff ctx (aux p1) (aux p2)
    | _ -> failwith "unimp"
  in
  aux prop
