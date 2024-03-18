open Mtyped
open Sexplib.Std

type constant =
  | U
  | B of bool
  | I of int
  | Tu of constant list
  | Dt of string * constant list
[@@deriving sexp]

let rec constant_to_nt c =
  let open Nt in
  match c with
  | U -> unit_ty
  | B _ -> bool_ty
  | I _ -> int_ty
  | Tu l -> Nt.Ty_tuple (List.map constant_to_nt l)
  | Dt _ -> failwith "Not implemented"

let compare_constant e1 e2 =
  Sexplib.Sexp.compare (sexp_of_constant e1) (sexp_of_constant e2)

let equal_constant e1 e2 =
  Sexplib.Sexp.equal (sexp_of_constant e1) (sexp_of_constant e2)
(* Generated from _constant.ml *)
