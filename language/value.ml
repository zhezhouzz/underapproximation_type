open Sexplib.Std

type t =
  | U
  | I of int
  | B of bool
  | IL of int list
  | IT of int Zzdatatype.Datatype.Tree.t
  | Tu of t list
  | NotADt
[@@deriving sexp]
