module NA = Languages.NormalAnormal
module T = Languages.Termlang
module StrucNA = Languages.StrucNA
module Struc = Languages.Struc
open Zzdatatype.Datatype

let subst (y, y') e =
  let open NA in
  let subst_tid id =
    if String.equal id.x y then { ty = id.ty; x = y' } else id
  in
  let rec aux_value e =
    match e.x with
    | Const _ -> e
    | Var id -> if String.equal id y then { ty = e.ty; x = Var y' } else e
    | Lam (xs, body) -> { ty = e.ty; x = Lam (xs, aux body) }
    | Fix (f, body) -> { ty = e.ty; x = Fix (f, aux_value body) }
  and aux e =
    let x =
      match e.x with
      | V v ->
          let v = aux_value { ty = e.ty; x = v } in
          V v.x
      | LetApp { ret; f; args; body } ->
          let body = if String.equal ret.x y then aux body else body in
          LetApp { ret; f = subst_tid f; args = List.map subst_tid args; body }
      | LetVal { lhs; rhs; body } ->
          let body = if String.equal lhs.x y then aux body else body in
          LetVal { lhs; rhs = aux_value rhs; body }
      | LetTu { tu; args; body } ->
          let body = if String.equal tu.x y then aux body else body in
          LetTu { tu; args = List.map subst_tid args; body }
      | LetDeTu { tu; args; body } ->
          let body =
            if List.exists (fun x -> String.equal x.x y) args then aux body
            else body
          in
          LetTu { tu = subst_tid tu; args; body }
      | Ite (id, e1, e2) -> Ite (subst_tid id, aux e1, aux e2)
      | Match (id, cases) ->
          Match
            ( subst_tid id,
              List.map (fun case -> { case with exp = aux case.exp }) cases )
    in
    { ty = e.ty; x }
  in

  let res = aux e in
  (* let () = *)
  (*   Printf.printf "[%s |-> %s]\n%s\n----\n%s\n-----\n\n" y y' (layout e) *)
  (*     (layout res) *)
  (* in *)
  res

(* let remove_tuple e = *)
(*   let open NA in *)
(*   let rec aux e = *)
(*     match e.x with *)
(*     | Const _ | Var _ | App _ -> e *)
(*     | Tu [ x ] -> aux { ty = x.ty; x = Var x.x } *)
(*     | Tu _ -> e *)
(*     | Lam (xs, body) -> { ty = e.ty; x = Lam (xs, aux body) } *)
(*     | Fix (f, body) -> { ty = e.ty; x = Fix (f, aux body) } *)
(*     | Let (ids, rhs, body) -> { ty = e.ty; x = Let (ids, aux rhs, aux body) } *)
(*     | Ite (id, e1, e2) -> { ty = e.ty; x = Ite (id, aux e1, aux e2) } *)
(*     | Match (id, cases) -> *)
(*         { *)
(*           ty = e.ty; *)
(*           x = *)
(*             Match *)
(*               (id, List.map (fun case -> { case with exp = aux case.exp }) cases); *)
(*         } *)
(*   in *)
(*   aux e *)

(* TODO: Fix renaming *)
let remove_dummy_eq e =
  let open NA in
  let rec aux_value e =
    match e.x with
    | Const _ | Var _ -> e
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
      | LetTu { tu; args; body } -> LetTu { tu; args; body = aux body }
      | LetDeTu { tu; args; body } -> LetDeTu { tu; args; body = aux body }
      | LetVal { lhs; rhs; body } -> (
          let rhs = aux_value rhs in
          match rhs.x with
          | Var id' -> (aux (subst (lhs.x, id') body)).x
          | _ -> LetVal { lhs; rhs; body = aux body })
      | Ite (id, e1, e2) -> Ite (id, aux e1, aux e2)
      | Match (id, cases) ->
          Match
            (id, List.map (fun case -> { case with exp = aux case.exp }) cases)
    in
    { ty = e.ty; x }
  in
  aux e

let remove_dummy_let e =
  let open NA in
  (* let () = Printf.printf "start simplify\n" in *)
  let rec aux_value e =
    match e.x with
    | Const _ | Var _ -> e
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
          (* let () = Printf.printf "letapp branch\n" in *)
          LetApp { ret; f; args; body = aux body }
      | LetTu { tu; args; body } ->
          (* let () = Printf.printf "lettu branch\n" in *)
          LetTu { tu; args; body = aux body }
      | LetDeTu { tu; args; body } ->
          (* let () = Printf.printf "letdetu branch\n" in *)
          LetDeTu { tu; args; body = aux body }
      | LetVal { lhs; rhs; body } -> (
          (* let () = Printf.printf "letval branch\n" in *)
          let body = aux body in
          match body.x with
          | V (Var id') when String.equal lhs.x id' ->
              let v = aux_value rhs in
              V v.x
          | _ -> LetVal { lhs; rhs = aux_value rhs; body })
      | Ite (id, e1, e2) -> Ite (id, aux e1, aux e2)
      | Match (id, cases) ->
          Match
            (id, List.map (fun case -> { case with exp = aux case.exp }) cases)
    in
    { ty = e.ty; x }
  in
  aux e
(* aux (remove_dummy_eq e) *)

let simplify = remove_dummy_let
