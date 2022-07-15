module SMTTy = struct
  type t =
    | Bool
    | Int
  [@@deriving sexp]
end

module Prop = struct
  open Sexplib.Std

  type ty = SMTTy.t
  [@@deriving sexp]
  type 'a typed = {ty: ty option; x: 'a}
  [@@deriving sexp]

  type t =
    | True
    | Var of string typed
    | Implies of t * t
    | Ite of t * t * t
    | Not of t
    | And of t list
    | Or of t list
    | Iff of t * t
    | Pred of string * string typed list
    | Forall of string typed list * t
    | Exists of string typed list * t
  [@@deriving sexp]
end
