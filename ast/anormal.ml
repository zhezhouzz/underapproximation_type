module F (Type : Type.T) = struct
  open Sexplib.Std

  type ty = Type.t [@@deriving sexp]
  type id = Strid.T.t [@@deriving sexp]
  type 'a typed = { ty : ty; x : 'a } [@@deriving sexp]

  let tupleC = "tuple"

  type value =
    | Const of Value.t
    | Var of id
    | Lam of id typed * term typed
    | Fix of id typed * value typed

  and term =
    | V of value
    | LetApp of {
        ret : id typed;
        f : id typed;
        args : id typed list;
        body : term typed;
      }
    | LetTu of { tu : id typed; args : id typed list; body : term typed }
    | LetDeTu of { tu : id typed; args : id typed list; body : term typed }
    | LetVal of { lhs : id typed; rhs : value typed; body : term typed }
    | Ite of id typed * term typed * term typed
    | Match of id typed * case list

  and case = { constructor : id; args : id list; exp : term typed }
  [@@deriving sexp]

  let make_letval x rhs body =
    { ty = body.ty; x = LetVal { lhs = { ty = rhs.ty; x }; rhs; body } }

  let value_to_term value = { ty = value.ty; x = V value.x }
end

module NormalAnormal = F (Normalty.T)
module OverAnormal = F (Overty.T)
module UnderAnormal = F (Underty.T)
