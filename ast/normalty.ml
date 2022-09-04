module T = struct
  open Sexplib.Std
  open Sugar

  type id = Strid.T.t [@@deriving sexp]

  type t =
    | Ty_unknown
    | Ty_var of string
    | Ty_unit
    | Ty_int
    | Ty_bool
    | Ty_list of t
    | Ty_tree of t
    | Ty_arrow of t * t
    | Ty_tuple of t list
    | Ty_constructor of (id * t list)
  [@@deriving sexp]

  let is_basic_tp = function Ty_unit | Ty_int | Ty_bool -> true | _ -> false

  let is_dt = function
    | Ty_list _ | Ty_tree _ | Ty_constructor _ -> true
    | _ -> false

  let eq x y =
    let rec aux (x, y) =
      match (x, y) with
      | Ty_unknown, Ty_unknown -> true
      | Ty_var x, Ty_var y -> String.equal x y
      | Ty_unit, Ty_unit -> true
      | Ty_int, Ty_int -> true
      | Ty_bool, Ty_bool -> true
      | Ty_list x, Ty_list y -> aux (x, y)
      | Ty_tree x, Ty_tree y -> aux (x, y)
      | Ty_arrow (x, x'), Ty_arrow (y, y') -> aux (x, y) && aux (x', y')
      | Ty_tuple xs, Ty_tuple ys ->
          if List.length xs == List.length ys then
            List.for_all aux @@ List.combine xs ys
          else false
      | Ty_constructor (id1, args1), Ty_constructor (id2, args2) ->
          String.equal id1 id2
          && List.length args1 == List.length args2
          && List.for_all2 (fun a b -> aux (a, b)) args1 args2
      | _ -> false
    in
    aux (x, y)

  let destruct_arrow_tp tp =
    let rec aux = function
      | Ty_arrow (t1, t2) ->
          let argsty, bodyty = aux t2 in
          (t1 :: argsty, bodyty)
      | ty -> ([], ty)
    in
    aux tp

  let rec construct_arrow_tp = function
    | [], retty -> retty
    | h :: t, retty -> Ty_arrow (h, construct_arrow_tp (t, retty))

  let to_smtty t =
    let aux = function
      | Ty_bool -> Autov.Smtty.Bool
      | Ty_int -> Autov.Smtty.Int
      | Ty_list _ | Ty_tree _ | Ty_constructor _ -> Autov.Smtty.Dt
      | _ -> _failatwith __FILE__ __LINE__ "not a basic type"
    in
    aux t
end

module NotatedT = struct
  open Sexplib.Std

  type t = string option * T.t [@@deriving sexp]

  let eq (a1, b1) (a2, b2) =
    match (a1, a2) with
    | None, None -> T.eq b1 b2
    | Some a1, Some a2 when String.equal a1 a2 -> T.eq b1 b2
    | _ -> false
end
