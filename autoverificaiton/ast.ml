open Sexplib.Std
module Ty = Normalty.Ast.T
open Normalty.Ast.Ntyped

type ty = Ty.t [@@deriving sexp]

type lit =
  | ACint of int
  | AVar of string typed
  | AOp2 of string * lit * lit
  | ACbool of bool
[@@deriving sexp]

type t =
  | Lit of lit
  | Implies of t * t
  | Ite of t * t * t
  | Not of t
  | And of t list
  | Or of t list
  | Iff of t * t
  | MethodPred of string * lit list
  | Forall of string typed * t
  | Exists of string typed * t
[@@deriving sexp]

let mk_true = Lit (ACbool true)
let mk_false = Lit (ACbool false)
let is_op = function "+" | "-" -> true | _ -> false

let is_bop = function
  | "==" | "!=" | "<" | ">" | "<=" | ">=" -> true
  | _ -> false
