module T = struct
  open Sexplib.Std

  type ty = Smtty.T.t [@@deriving sexp]
  type 'a typed = { ty : ty; x : 'a } [@@deriving sexp]

  type arg =
    | ACint of int
    | AVar of string typed (* | AOp of string * arg * arg *)
  [@@deriving sexp]

  type t =
    | True
    | Cint of int
    | Var of string typed
    | Implies of t * t
    | Ite of t * t * t
    | Not of t
    | And of t list
    | Or of t list
    | Iff of t * t
    (* | OP2 of string * t * t *)
    | MethodPred of string * arg list
    | Forall of string typed * t
    | Exists of string typed * t
  [@@deriving sexp]

  let typed_id_eq x y =
    if String.equal x.x y.x then
      let () =
        if Smtty.T.eq (x.ty, y.ty) then () else failwith "prop naming error"
      in
      true
    else false

  let subst_typed_id t x y =
    let do_subst x y id = if typed_id_eq x id then y else id in
    let rec aux t =
      match t with
      | True | Cint _ -> t
      | Var id -> Var (do_subst x y id)
      | Implies (e1, e2) -> Implies (aux e1, aux e2)
      | Ite (e1, e2, e3) -> Ite (aux e1, aux e2, aux e3)
      | Not e -> Not (aux e)
      | And es -> And (List.map aux es)
      | Or es -> Or (List.map aux es)
      | Iff (e1, e2) -> Iff (aux e1, aux e2)
      | MethodPred (mp, args) ->
          MethodPred
            ( mp,
              List.map
                (function
                  | ACint n -> ACint n | AVar id -> AVar (do_subst x y id))
                args )
      | Forall (u, e) -> if typed_id_eq x u then t else Forall (u, aux e)
      | Exists (u, e) -> if typed_id_eq x u then t else Exists (u, aux e)
    in
    aux t

  let subst_id t x y =
    let do_subst x y id =
      if String.equal x id.x then { ty = id.ty; x = y } else id
    in
    let rec aux t =
      match t with
      | True | Cint _ -> t
      | Var id -> Var (do_subst x y id)
      | Implies (e1, e2) -> Implies (aux e1, aux e2)
      | Ite (e1, e2, e3) -> Ite (aux e1, aux e2, aux e3)
      | Not e -> Not (aux e)
      | And es -> And (List.map aux es)
      | Or es -> Or (List.map aux es)
      | Iff (e1, e2) -> Iff (aux e1, aux e2)
      | MethodPred (mp, args) ->
          MethodPred
            ( mp,
              List.map
                (function
                  | ACint n -> ACint n | AVar id -> AVar (do_subst x y id))
                args )
      | Forall (u, e) -> if String.equal u.x x then t else Forall (u, aux e)
      | Exists (u, e) -> if String.equal u.x x then t else Exists (u, aux e)
    in
    aux t

  let mk_mp_vars mp args = MethodPred (mp, List.map (fun x -> AVar x) args)

  let mk_forall_intqv x prop =
    let u = { ty = Smtty.T.Int; x } in
    Forall (u, prop u)

  let mk_exists_intqv x prop =
    let u = { ty = Smtty.T.Int; x } in
    Exists (u, prop u)

  let strict_eq t1 t2 =
    let rec aux (t1, t2) =
      match (t1, t2) with
      | True, True -> true
      | Cint n, Cint n' -> n == n'
      | Var id, Var id' -> typed_id_eq id id'
      | Implies (e1, e2), Implies (e1', e2') -> aux (e1, e1') && aux (e2, e2')
      | Ite (e1, e2, e3), Ite (e1', e2', e3') ->
          aux (e1, e1') && aux (e2, e2') && aux (e3, e3')
      | Not e, Not e' -> aux (e, e')
      | And es, And es' when List.length es = List.length es' ->
          List.for_all aux @@ List.combine es es'
      | Or es, Or es' when List.length es = List.length es' ->
          List.for_all aux @@ List.combine es es'
      | Iff (e1, e2), Iff (e1', e2') -> aux (e1, e1') && aux (e2, e2')
      | MethodPred (mp, args), MethodPred (mp', args')
        when List.length args = List.length args' ->
          String.equal mp mp'
          && List.for_all (function
               | ACint n, ACint n' -> n == n'
               | AVar id, AVar id' -> typed_id_eq id id'
               | _, _ -> false)
             @@ List.combine args args'
      | Forall (u, e), Forall (u', e') -> typed_id_eq u u' && aux (e, e')
      | Exists (u, e), Exists (u', e') -> typed_id_eq u u' && aux (e, e')
      | _, _ -> false
    in
    aux (t1, t2)
end
