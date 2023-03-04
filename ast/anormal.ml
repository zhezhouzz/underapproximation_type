module F (Typed : Type.Typed) = struct
  open Sexplib.Std
  include Typed

  type ty = t [@@deriving sexp]
  type id = string [@@deriving sexp]

  let tupleC = "tuple"

  (* constant *)
  type smt_lit = ConstB of bool | ConstI of int [@@deriving sexp]

  (* NOTE: the function arguments have no quantified types *)
  type value =
    | Var of id typed
    | Lam of { lamarg : id typed; lambody : term typed }
    | Fix of { fixname : id typed; fstarg : id typed; lambody : term typed }
    | Lit of smt_lit
    | Exn

  and term =
    | V of value typed
    | LetApp of {
        ret : id typed;
        f : id typed;
        args : value typed list;
        body : term typed;
      }
    | LetDtConstructor of {
        ret : id typed;
        f : id typed;
        args : value typed list;
        body : term typed;
      }
    | LetOp of {
        ret : id typed;
        op : Op.T.op;
        args : value typed list;
        body : term typed;
      }
    | LetTu of { tu : id typed; args : value typed list; body : term typed }
    | LetDeTu of { tu : value typed; args : id typed list; body : term typed }
    | LetVal of { lhs : id typed; rhs : value typed; body : term typed }
      (* branches, we will copy the continuations for branches *)
    | Ite of { cond : value typed; e_t : term typed; e_f : term typed }
    | Match of { matched : value typed; cases : case list }

  and case = { constructor : id typed; args : id typed list; exp : term typed }
  [@@deriving sexp]

  let rec term_count_localvars (e : term) =
    let n =
      match e with
      | V v -> value_count_localvars v.x
      | LetApp { body; _ }
      | LetDtConstructor { body; _ }
      | LetOp { body; _ }
      | LetTu { body; _ }
      | LetVal { body; _ } ->
          1 + term_count_localvars body.x
      | LetDeTu { args; body; _ } ->
          List.length args + term_count_localvars body.x
      | Ite { e_t; e_f; _ } ->
          term_count_localvars e_t.x + term_count_localvars e_f.x
      | Match { cases; _ } ->
          List.fold_left
            (fun sum { exp; _ } -> sum + term_count_localvars exp.x)
            0 cases
    in
    (* let () = Printf.printf "%i\n" n in *)
    n

  and value_count_localvars (v : value) =
    match v with
    | Lam { lambody; _ } -> 1 + term_count_localvars lambody.x
    | Fix { lambody; _ } -> 1 + term_count_localvars lambody.x
    | Var _ | Lit _ | Exn -> 0

  let rec term_count_branches (e : term) =
    let n =
      match e with
      | V v -> 1 + value_count_branches v.x
      | LetApp { args; body; _ } ->
          List.fold_left
            (fun sum arg -> sum + value_count_branches arg.x)
            (term_count_branches body.x)
            args
      | LetDtConstructor { args; body; _ } ->
          List.fold_left
            (fun sum arg -> sum + value_count_branches arg.x)
            (term_count_branches body.x)
            args
      | LetOp { args; body; _ } ->
          List.fold_left
            (fun sum arg -> sum + value_count_branches arg.x)
            (term_count_branches body.x)
            args
      | LetTu { args; body; _ } ->
          List.fold_left
            (fun sum arg -> sum + value_count_branches arg.x)
            (term_count_branches body.x)
            args
      | LetDeTu { tu; body; _ } ->
          value_count_branches tu.x + term_count_branches body.x
      | LetVal { rhs; body; _ } ->
          value_count_branches rhs.x + term_count_branches body.x
      | Ite { cond; e_t; e_f } ->
          value_count_branches cond.x
          + term_count_branches e_t.x + term_count_branches e_f.x
      | Match { matched; cases } ->
          List.fold_left
            (fun sum { exp; _ } -> sum + term_count_branches exp.x)
            (value_count_branches matched.x)
            cases
    in
    (* let () = Printf.printf "%i\n" n in *)
    n

  and value_count_branches (v : value) =
    let n =
      match v with
      | Lam { lambody; _ } | Fix { lambody; _ } -> (
          let n = term_count_branches lambody.x in
          match lambody.x with V { x = Lam _; _ } -> n - 1 | _ -> n)
      | Var _ | Lit _ | Exn -> 0
    in
    (* let () = Printf.printf "v: %i\n" n in *)
    n

  let fix_to_lam e =
    match e.x with
    | V { x = Fix { fstarg; lambody; _ }; ty } ->
        (true, { x = V { x = Lam { lamarg = fstarg; lambody }; ty }; ty })
    | _ -> (false, e)

  let make_letval x rhs body =
    { ty = body.ty; x = LetVal { lhs = { ty = rhs.ty; x }; rhs; body } }

  let value_to_term value = { ty = value.ty; x = V value }
  (* let id_to_lit (id : id typed) = { ty = id.ty; x = Var id.x } *)

  let id_to_value (id : id typed) = { x = Var id; ty = id.ty }

  let term_to_value file line e =
    match e.x with V v -> v | _ -> Sugar._failatwith file line "not a value"

  let subst_id (y, y') e =
    let eq_tid id = String.equal id.x y in
    let subst_tid id = if eq_tid id then { ty = id.ty; x = y' } else id in
    let exists_in_tids = List.exists (fun x -> String.equal x.x y) in
    let rec aux_value e =
      let x =
        match e.x with
        | Exn | Lit _ -> e.x
        | Var id -> Var (subst_tid id)
        | Lam { lamarg; lambody } ->
            if eq_tid lamarg then e.x else Lam { lamarg; lambody = aux lambody }
        | Fix { fixname; fstarg; lambody } ->
            if exists_in_tids [ fixname; fstarg ] then e.x
            else Fix { fixname; fstarg; lambody = aux lambody }
      in
      { ty = e.ty; x }
    and aux e =
      let x =
        match e.x with
        | V v -> V (aux_value v)
        | LetApp { ret; f; args; body } ->
            let body = if eq_tid ret then body else aux body in
            LetApp
              { ret; f = subst_tid f; args = List.map aux_value args; body }
        | LetDtConstructor { ret; f; args; body } ->
            let body = if eq_tid ret then body else aux body in
            if eq_tid f then
              failwith
                "the name is the same with the datatype constructor, should \
                 not happen"
            else
              LetDtConstructor { ret; f; args = List.map aux_value args; body }
        | LetOp { ret; op; args; body } ->
            let body = if eq_tid ret then body else aux body in
            LetOp { ret; op; args = List.map aux_value args; body }
        | LetVal { lhs; rhs; body } ->
            let body = if eq_tid lhs then body else aux body in
            LetVal { lhs; rhs = aux_value rhs; body }
        | LetTu { tu; args; body } ->
            let body = if eq_tid tu then body else aux body in
            LetTu { tu; args = List.map aux_value args; body }
        | LetDeTu { tu; args; body } ->
            let body = if exists_in_tids args then body else aux body in
            LetDeTu { tu = aux_value tu; args; body }
        | Ite { cond; e_t; e_f } ->
            Ite { cond = aux_value cond; e_t = aux e_t; e_f = aux e_f }
        | Match { matched; cases } ->
            Match
              {
                matched = aux_value matched;
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

  (* HACK: handel Exn *)
  let var_exn_to_exn term =
    let rec aux e =
      let x =
        match e.x with
        | V { x = Var { x = "Exn"; _ }; ty } -> V { x = Exn; ty }
        | V _ -> e.x
        | LetApp { ret; f; args; body } ->
            LetApp { ret; f; args; body = aux body }
        | LetDtConstructor { ret; f; args; body } ->
            LetDtConstructor { ret; f; args; body = aux body }
        | LetOp { ret; op; args; body } ->
            LetOp { ret; op; args; body = aux body }
        | LetVal { lhs; rhs; body } -> LetVal { lhs; rhs; body = aux body }
        | LetTu { tu; args; body } -> LetTu { tu; args; body = aux body }
        | LetDeTu { tu; args; body } -> LetDeTu { tu; args; body = aux body }
        | Ite { cond; e_t; e_f } -> Ite { cond; e_t = aux e_t; e_f = aux e_f }
        | Match { matched; cases } ->
            Match
              {
                matched;
                cases = List.map (fun x -> { x with exp = aux x.exp }) cases;
              }
      in
      { ty = e.ty; x }
    in
    aux term
end

module NormalAnormal = struct
  include F (Normalty.Ast.NNtyped)
  open Normalty.Ast

  let recover_dt_constructor_ty (ret, args) =
    match args with
    | [] -> ret.ty
    | _ ->
        let argsty = List.map (fun x -> snd @@ x.ty) args in
        (None, T.construct_arrow_tp (argsty, snd @@ ret.ty))
end

module OverAnormal = F (Overty.Otyped)
module UnderAnormal = F (Underty.Utyped)
