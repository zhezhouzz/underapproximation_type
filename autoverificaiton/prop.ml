module SMTTy = struct
  type t = Bool | Int [@@deriving sexp]

  let eq = function Bool, Bool | Int, Int -> true | _ -> false
end

module Prop = struct
  open Sexplib.Std

  type ty = SMTTy.t [@@deriving sexp]
  type 'a typed = { ty : ty option; x : 'a } [@@deriving sexp]

  type t =
    | True
    | Var of string typed
    | Implies of t * t
    | Ite of t * t * t
    | Not of t
    | And of t list
    | Or of t list
    | Iff of t * t
    | MethodPred of string * string typed list
    | Forall of string typed * t
    | Exists of string typed * t
  [@@deriving sexp]

  let typed_id_eq (x, y) =
    if String.equal x.x y.x then
      let () =
        match (x.ty, y.ty) with
        | Some xty, Some yty ->
            if SMTTy.eq (xty, yty) then () else failwith "prop naming error"
        | _, _ -> ()
      in
      true
    else false

  let subst_typed_id t x y =
    let do_subst x y id = if typed_id_eq (x, id) then y else id in
    let rec aux t =
      match t with
      | True -> t
      | Var id -> Var (do_subst x y id)
      | Implies (e1, e2) -> Implies (aux e1, aux e2)
      | Ite (e1, e2, e3) -> Ite (aux e1, aux e2, aux e3)
      | Not e -> Not (aux e)
      | And es -> And (List.map aux es)
      | Or es -> And (List.map aux es)
      | Iff (e1, e2) -> Iff (aux e1, aux e2)
      | MethodPred (mp, args) -> MethodPred (mp, List.map (do_subst x y) args)
      | Forall (u, e) -> if typed_id_eq (x, u) then t else Forall (u, aux e)
      | Exists (u, e) -> if typed_id_eq (x, u) then t else Exists (u, aux e)
    in
    aux t

  let subst_id t x y =
    let do_subst x y id =
      if String.equal x id.x then { ty = id.ty; x = y } else id
    in
    let rec aux t =
      match t with
      | True -> t
      | Var id -> Var (do_subst x y id)
      | Implies (e1, e2) -> Implies (aux e1, aux e2)
      | Ite (e1, e2, e3) -> Ite (aux e1, aux e2, aux e3)
      | Not e -> Not (aux e)
      | And es -> And (List.map aux es)
      | Or es -> And (List.map aux es)
      | Iff (e1, e2) -> Iff (aux e1, aux e2)
      | MethodPred (mp, args) -> MethodPred (mp, List.map (do_subst x y) args)
      | Forall (u, e) -> if String.equal u.x x then t else Forall (u, aux e)
      | Exists (u, e) -> if String.equal u.x x then t else Exists (u, aux e)
    in
    aux t
end
