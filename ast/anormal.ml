module F (Type : Type.T) = struct
  open Sexplib.Std

  type ty = Type.t [@@deriving sexp]
  type id = Strid.T.t [@@deriving sexp]
  type 'a typed = { ty : ty; x : 'a } [@@deriving sexp]

  type term =
    | Const of Value.t
    | Var of id
    | Tu of id typed list
    | Lam of id typed * term typed
    | Fix of id typed * term typed
    | App of id typed * id typed list
    | Let of id typed list * term typed * term typed
    | Ite of id typed * term typed * term typed
    | Match of id typed * case list

  and case = { constuctor : id; args : id list; exp : term typed }
  [@@deriving sexp]
end

module NormalAnormal = F (Normalty.T)
module OverAnormal = F (Overty.T)
