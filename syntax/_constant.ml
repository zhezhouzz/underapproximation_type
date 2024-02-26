open Mtyped
open Sexplib.Std

type constant =
  | U
  | B of bool
  | I of int
  | Tu of constant list
  | Dt of string * constant list
[@@deriving sexp]
