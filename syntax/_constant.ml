open Mtyped
open Sexplib.Std

type constant =
  | U
  | B of bool
  | I of int
  | CTu of constant list
  | Dt of string * constant list
[@@deriving sexp]
