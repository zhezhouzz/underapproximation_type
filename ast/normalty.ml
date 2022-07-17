module T = struct
  open Sexplib.Std

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

  let rec eq (x, y) =
    match (x, y) with
    | Ty_unknown, Ty_unknown -> true
    | Ty_unit, Ty_unit -> true
    | Ty_int, Ty_int -> true
    | Ty_bool, Ty_bool -> true
    | Ty_list x, Ty_list y -> eq (x, y)
    | Ty_tree x, Ty_tree y -> eq (x, y)
    | Ty_arrow (x, x'), Ty_arrow (y, y') -> eq (x, y) && eq (x', y')
    | Ty_tuple xs, Ty_tuple ys ->
        if List.length xs == List.length ys then
          List.for_all eq @@ List.combine xs ys
        else false
    | Ty_constructor _, Ty_constructor _ -> failwith "unimp"
    | _ -> false
end
