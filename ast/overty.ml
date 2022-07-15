module T : Ast.OverTy = struct
  open Sexplib.Std
  type id = Strid.T.t
  [@@deriving sexp]
  type normalty = Normalty.T.t
  [@@deriving sexp]
  type t =
    | OverTy_base of {basename: id; normalty: normalty; constraints: unit}
    | OverTy_arrow of {argname: id; argty: t; retty: t}
    | OverTy_tuple of t list
  [@@deriving sexp]
end
