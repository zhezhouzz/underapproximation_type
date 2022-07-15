module T : Ast.NormalTy = struct
  open Sexplib.Std
  type id = Strid.T.t
  [@@deriving sexp]
  type t =
    | Ty_unknown
    | Ty_unit
    | Ty_int
    | Ty_bool
    | Ty_list of t
    | Ty_tree of t
    | Ty_arrow of t * t
    | Ty_tuple of (t list)
    | Ty_constructor of (id * constructor list)
  and constructor = {dname: id; dargs: t}
  [@@deriving sexp]
end
