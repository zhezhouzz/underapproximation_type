module NA = Languages.NormalAnormal
module T = Languages.Termlang
module StrucNA = Languages.StrucNA

let layout code =
  let code = match code.NA.x with NA.Fix (_, body) -> body | _ -> code in
  Frontend.Expr.layout @@ Term2normalanormal.to_term code

open Sugar
open Zzdatatype.Datatype

let layout_one StrucNA.{ name; body } =
  Frontend.Expr.layout @@ Term2normalanormal.to_term
  @@ NA.
       {
         ty = body.ty;
         x =
           Let
             ( [ { ty = body.ty; x = name } ],
               body,
               { ty = Languages.Normalty.Ty_unit; x = Var "function-end" } );
       }

let struct_layout code = spf "%s\n" (List.split_by "\n" layout_one code)

let subst (y, y') e =
  let open NA in
  let subst_tid id =
    if String.equal id.x y then { ty = id.ty; x = y' } else id
  in
  let rec aux e =
    match e.x with
    | Const _ -> e
    | Var id -> if String.equal id y then { ty = e.ty; x = Var y' } else e
    | Tu l -> { ty = e.ty; x = Tu (List.map subst_tid l) }
    | App (f, args) ->
        { ty = e.ty; x = App (subst_tid f, List.map subst_tid args) }
    | Lam (xs, body) -> { ty = e.ty; x = Lam (xs, aux body) }
    | Fix (f, body) -> { ty = e.ty; x = Fix (f, aux body) }
    | Let (ids, rhs, body) ->
        if List.exists (fun id -> String.equal id.x y) ids then
          { ty = e.ty; x = Let (ids, aux rhs, body) }
        else { ty = e.ty; x = Let (ids, aux rhs, aux body) }
    | Ite (id, e1, e2) -> { ty = e.ty; x = Ite (subst_tid id, aux e1, aux e2) }
    | Match (id, cases) ->
        {
          ty = e.ty;
          x =
            Match
              ( subst_tid id,
                List.map (fun case -> { case with exp = aux case.exp }) cases );
        }
  in
  let res = aux e in
  (* let () = *)
  (*   Printf.printf "[%s |-> %s]\n%s\n----\n%s\n-----\n\n" y y' (layout e) *)
  (*     (layout res) *)
  (* in *)
  res

let remove_tuple e =
  let open NA in
  let rec aux e =
    match e.x with
    | Const _ | Var _ | App _ -> e
    | Tu [ x ] -> aux { ty = x.ty; x = Var x.x }
    | Tu _ -> e
    | Lam (xs, body) -> { ty = e.ty; x = Lam (xs, aux body) }
    | Fix (f, body) -> { ty = e.ty; x = Fix (f, aux body) }
    | Let (ids, rhs, body) -> { ty = e.ty; x = Let (ids, aux rhs, aux body) }
    | Ite (id, e1, e2) -> { ty = e.ty; x = Ite (id, aux e1, aux e2) }
    | Match (id, cases) ->
        {
          ty = e.ty;
          x =
            Match
              (id, List.map (fun case -> { case with exp = aux case.exp }) cases);
        }
  in
  aux e

let remove_dummy_eq e =
  let open NA in
  let rec aux e =
    (* let () = Printf.printf "%s\n" @@ layout e in *)
    match e.x with
    | Const _ | Var _ | Tu _ | App _ -> e
    | Lam (xs, body) -> { ty = e.ty; x = Lam (xs, aux body) }
    | Fix (f, body) -> { ty = e.ty; x = Fix (f, aux body) }
    | Let (ids, rhs, body) -> (
        let rhs = aux rhs in
        match (ids, rhs.x) with
        | [ id ], Var id' ->
            (* let () = Printf.printf ">>>>> id: %s\n" id.x in *)
            aux (subst (id.x, id') body)
        | _ -> { ty = e.ty; x = Let (ids, rhs, aux body) })
    | Ite (id, e1, e2) -> { ty = e.ty; x = Ite (id, aux e1, aux e2) }
    | Match (id, cases) ->
        {
          ty = e.ty;
          x =
            Match
              (id, List.map (fun case -> { case with exp = aux case.exp }) cases);
        }
  in
  aux (remove_tuple e)

let remove_dummy_let e =
  let open NA in
  let rec aux e =
    match e.x with
    | Const _ | Var _ | Tu _ | App _ -> e
    | Lam (xs, body) -> { ty = e.ty; x = Lam (xs, aux body) }
    | Fix (f, body) -> { ty = e.ty; x = Fix (f, aux body) }
    | Let (ids, rhs, body) -> (
        let body = aux body in
        match (ids, body.x) with
        | [ id ], Var id' when String.equal id.x id' -> aux rhs
        | _ -> { ty = e.ty; x = Let (ids, aux rhs, body) })
    | Ite (id, e1, e2) -> { ty = e.ty; x = Ite (id, aux e1, aux e2) }
    | Match (id, cases) ->
        {
          ty = e.ty;
          x =
            Match
              (id, List.map (fun case -> { case with exp = aux case.exp }) cases);
        }
  in
  aux (remove_dummy_eq e)

let simplify = remove_dummy_let
