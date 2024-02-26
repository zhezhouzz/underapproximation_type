open Mtyped
open Sexplib.Std

type const =
  | U
  | B of bool
  | I of int
  | Tu of const list
  | Dt of string * const list
[@@deriving sexp]
