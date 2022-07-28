module T = struct
  open Sexplib.Std
  open Sugar

  type id = Strid.T.t [@@deriving sexp]

  type t =
    | Ty_unknown
    | Ty_unit
    | Ty_int
    | Ty_bool
    | Ty_list of t
    | Ty_tree of t
    | Ty_arrow of t * t
    | Ty_tuple of t list
    | Ty_constructor of (id * constructor list)

  and constructor = { dname : id; dargs : t } [@@deriving sexp]

  let is_basic_tp = function Ty_unit | Ty_int | Ty_bool -> true | _ -> false

  let eq x y =
    let rec aux (x, y) =
      match (x, y) with
      | Ty_unknown, Ty_unknown -> true
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
      | Ty_constructor _, Ty_constructor _ ->
          _failatwith __FILE__ __LINE__ "unimp"
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
      | Ty_list _ | Ty_tree _ | Ty_int -> Autov.Smtty.Int
      | _ -> _failatwith __FILE__ __LINE__ "not a basic type"
    in
    aux t
end
