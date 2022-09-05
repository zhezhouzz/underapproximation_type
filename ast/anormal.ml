module F (Typed : Type.Typed) = struct
  open Sexplib.Std
  include Typed

  type ty = t [@@deriving sexp]
  type id = Strid.T.t [@@deriving sexp]
  (* type 'a typed = { ty : ty; x : 'a } [@@deriving sexp] *)

  let tupleC = "tuple"

  type smt_lit = ConstB of bool | ConstI of int | Var of id [@@deriving sexp]

  (* NOTE: the function arguments have no quantified types *)
  type value =
    | Lam of id typed * term typed
    | Fix of id typed * value typed
    | Lit of smt_lit

  and term =
    | V of value
    | LetApp of {
        ret : id typed;
        f : id typed;
        args : id typed list;
        body : term typed;
      }
    | LetOp of {
        ret : id typed;
        op : Op.T.op;
        args : id typed list;
        body : term typed;
      }
    | LetTu of { tu : id typed; args : id typed list; body : term typed }
    | LetDeTu of { tu : id typed; args : id typed list; body : term typed }
    | LetVal of { lhs : id typed; rhs : value typed; body : term typed }
      (* branches, we will copy the continuations for branches *)
    | Ite of { cond : id typed; e_t : term typed; e_f : term typed }
    | Match of { matched : id typed; cases : case list }

  and case = { constructor : id typed; args : id list; exp : term typed }
  [@@deriving sexp]

  let make_letval x rhs body =
    { ty = body.ty; x = LetVal { lhs = { ty = rhs.ty; x }; rhs; body } }

  let value_to_term value = { ty = value.ty; x = V value.x }
  let id_to_lit (id : id typed) = { ty = id.ty; x = Var id.x }

  let id_to_value (id : id typed) =
    let v = id_to_lit id in
    { ty = v.ty; x = Lit v.x }

  let term_to_value file line e =
    match e.x with
    | V v -> { ty = e.ty; x = v }
    | _ -> Sugar._failatwith file line "not a value"

  let subst (y, y') e =
    let subst_tid id =
      if String.equal id.x y then { ty = id.ty; x = y' } else id
    in
    let aux_lit x =
      match x with
      | ConstI _ | ConstB _ -> x
      | Var id -> if String.equal id y then Var y' else x
    in
    (* let subst_tlit e = { ty = e.ty; x = aux_lit e.x } in *)
    (* let in_tlits name tlits = *)
    (*   List.exists (function Var id -> String.equal id name | _ -> false) tlits *)
    (* in *)
    let rec aux_value e =
      let x =
        match e.x with
        | Lit x -> Lit (aux_lit x)
        | Lam (xs, body) -> Lam (xs, aux body)
        | Fix (f, body) -> Fix (f, aux_value body)
      in
      { ty = e.ty; x }
    and aux e =
      let x =
        match e.x with
        | V v ->
            let v = aux_value { ty = e.ty; x = v } in
            V v.x
        | LetApp { ret; f; args; body } ->
            let body = if String.equal ret.x y then body else aux body in
            LetApp
              { ret; f = subst_tid f; args = List.map subst_tid args; body }
        | LetOp { ret; op; args; body } ->
            (* let () = *)
            (*   Printf.printf "subst_op (%s) %s -> %s\n" *)
            (*     (Zzdatatype.Datatype.List.split_by_comma (fun x -> x.x) args) *)
            (*     y y' *)
            (* in *)
            let body = if String.equal ret.x y then body else aux body in
            LetOp { ret; op; args = List.map subst_tid args; body }
        | LetVal { lhs; rhs; body } ->
            (* let () = *)
            (*   Printf.printf "subst_val (V) %s -> %s | %b\n" y y' *)
            (*     (String.equal lhs.x y) *)
            (* in *)
            let body = if String.equal lhs.x y then body else aux body in
            LetVal { lhs; rhs = aux_value rhs; body }
        | LetTu { tu; args; body } ->
            let body = if String.equal tu.x y then body else aux body in
            LetTu { tu; args = List.map subst_tid args; body }
        | LetDeTu { tu; args; body } ->
            let body =
              if List.exists (fun x -> String.equal x.x y) args then body
              else aux body
            in
            LetDeTu { tu = subst_tid tu; args; body }
        | Ite { cond; e_t; e_f } ->
            Ite { cond = subst_tid cond; e_t = aux e_t; e_f = aux e_f }
        | Match { matched; cases } ->
            Match
              {
                matched = subst_tid matched;
                cases =
                  List.map (fun case -> { case with exp = aux case.exp }) cases;
              }
      in
      { ty = e.ty; x }
    in
    let res = aux e in
    (* let () = *)
    (*   Printf.printf "[%s |-> %s]\n%s\n----\n%s\n-----\n\n" y y' (layout e) *)
    (*     (layout res) *)
    (* in *)
    res
end

module NormalAnormal = F (Normalty.Ast.NNtyped)
module OverAnormal = F (Overty.Otyped)
module UnderAnormal = F (Underty.Utyped)
