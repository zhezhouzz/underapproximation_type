module T = struct
  open Sexplib.Std

  type ty = Normalty.T.t [@@deriving sexp]
  type id = Strid.T.t [@@deriving sexp]
  type 'a opttyped = { ty : ty option; x : 'a } [@@deriving sexp]
  type if_rec = bool [@@deriving sexp]

  type term =
    | Const of Value.t
    | Var of id
    | Tu of term opttyped list
    | Lam of ty * id * term opttyped
    | App of term opttyped * term opttyped list
    | Let of if_rec * (ty * id) list * term opttyped * term opttyped
    | Ite of term opttyped * term opttyped * term opttyped
    | Match of term opttyped * case list

  and case = { constructor : id; args : id list; exp : term opttyped }
  [@@deriving sexp]

  let make_untyped x = { ty = None; x }
  let make_untyped_var id = { ty = None; x = Var id }

  let make_untyped_tuple ids =
    { ty = None; x = Tu (List.map make_untyped_var ids) }

  let make_untyped_id_app (id, ids) =
    { ty = None; x = App (make_untyped_var id, List.map make_untyped_var ids) }

  let typedstr_to_var x = { ty = x.ty; x = Var x.x }
  let term_to_string_opt x = match x.x with Var name -> Some name | _ -> None

  let terms_to_strings_opt x =
    List.fold_left
      (fun l x ->
        match l with
        | None -> None
        | Some l -> (
            match term_to_string_opt x with
            | None -> None
            | Some x -> Some (l @ [ x ])))
      (Some []) x

  let erase_type term =
    let rec aux { x; _ } =
      let x =
        match x with
        | Const _ | Var _ -> x
        | Tu es -> Tu (List.map aux es)
        | Lam (ty, id, e) -> Lam (ty, id, aux e)
        | App (e, es) -> App (aux e, List.map aux es)
        | Let (if_rec, lhs, rhs, body) -> Let (if_rec, lhs, aux rhs, aux body)
        | Ite (e1, e2, e3) -> Ite (aux e1, aux e2, aux e3)
        | Match (e, cases) ->
            Match
              ( aux e,
                List.map
                  (fun { constructor; args; exp } ->
                    { constructor; args; exp = aux exp })
                  cases )
      in
      { ty = None; x }
    in
    aux term
end
