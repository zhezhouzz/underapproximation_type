open Mtyped
open Sexplib.Std

type constant =
  | U
  | B of bool
  | I of int
  | Tu of constant list
  | Dt of string * constant list
[@@deriving sexp]

let compare_constant e1 e2 =
  Sexplib.Sexp.compare (sexp_of_constant e1) (sexp_of_constant e2)

let equal_constant e1 e2 =
  Sexplib.Sexp.equal (sexp_of_constant e1) (sexp_of_constant e2)
(* Generated from _constant.ml *)

let mk_const_appop op cs = (Dt (op.x, cs)) #: (snd @@ Nt.destruct_arr_tp op.ty)
