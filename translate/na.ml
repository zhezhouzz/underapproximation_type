module NA = Languages.NormalAnormal
module T = Languages.Termlang
module StrucNA = Languages.StrucNA
module Struc = Languages.Struc
open Zzdatatype.Datatype

let remove_dummy_eq e =
  let open NA in
  let rec aux_value e =
    match e.x with
    | Lit _ -> e
    | Lam (xs, body) -> { ty = e.ty; x = Lam (xs, aux body) }
    | Fix (f, body) -> { ty = e.ty; x = Fix (f, aux_value body) }
  and aux e =
    (* let () = Printf.printf "%s\n" @@ layout e in *)
    let x =
      match e.x with
      | V v ->
          let v = aux_value { ty = e.ty; x = v } in
          V v.x
      | LetApp { ret; f; args; body } ->
          LetApp { ret; f; args; body = aux body }
      | LetOp { ret; op; args; body } ->
          LetOp { ret; op; args; body = aux body }
      | LetTu { tu; args; body } -> LetTu { tu; args; body = aux body }
      | LetDeTu { tu; args; body } -> LetDeTu { tu; args; body = aux body }
      | LetVal { lhs; rhs; body } -> (
          let rhs = aux_value rhs in
          match rhs.x with
          | Lit (Var id') -> (aux (subst (lhs.x, id') body)).x
          | _ -> LetVal { lhs; rhs; body = aux body })
      | Ite { cond; e_t; e_f } -> Ite { cond; e_t = aux e_t; e_f = aux e_f }
      | Match { matched; cases } ->
          Match
            {
              matched;
              cases =
                List.map (fun case -> { case with exp = aux case.exp }) cases;
            }
    in
    { ty = e.ty; x }
  in
  aux e

let remove_dummy_let e =
  let open NA in
  (* let () = Printf.printf "start simplify\n" in *)
  let rec aux_value e =
    match e.x with
    | Lit _ -> e
    | Lam (xs, body) -> { ty = e.ty; x = Lam (xs, aux body) }
    | Fix (f, body) -> { ty = e.ty; x = Fix (f, aux_value body) }
  and aux e =
    (* let () = *)
    (*   Printf.printf "working on: %s\n" *)
    (*   @@ Frontend.Anormal.layout Term2normalanormal.to_term e *)
    (* in *)
    let x =
      match e.x with
      | V v ->
          let v = aux_value { ty = e.ty; x = v } in
          V v.x
      | LetApp { ret; f; args; body } ->
          LetApp { ret; f; args; body = aux body }
      | LetOp { ret; op; args; body } ->
          LetOp { ret; op; args; body = aux body }
      | LetTu { tu; args; body } -> LetTu { tu; args; body = aux body }
      | LetDeTu { tu; args; body } -> LetDeTu { tu; args; body = aux body }
      | LetVal { lhs; rhs; body } -> (
          let body = aux body in
          match body.x with
          | V (Lit (Var id')) when String.equal lhs.x id' ->
              let v = aux_value rhs in
              V v.x
          | _ -> LetVal { lhs; rhs = aux_value rhs; body })
      | Ite { cond; e_t; e_f } -> Ite { cond; e_t = aux e_t; e_f = aux e_f }
      | Match { matched; cases } ->
          Match
            {
              matched;
              cases =
                List.map (fun case -> { case with exp = aux case.exp }) cases;
            }
    in
    { ty = e.ty; x }
  in
  (* aux e *)
  aux (remove_dummy_eq e)

let simplify = remove_dummy_let
