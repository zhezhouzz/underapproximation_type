module type Id = sig
  type t
  [@@deriving sexp]
end

module type NormalTy = sig
  type id
  type t =
    | Ty_unknown
    | Ty_unit
    | Ty_int
    | Ty_bool
    | Ty_list of t
    | Ty_tree of t
    | Ty_arrow of t * t
    | Ty_tuple of (t list)
    | Ty_constructor of (id * constructor list)
  and constructor = {dname: id; dargs: t}
  [@@deriving sexp]
end

module type OverTy = sig
  type id
  type normalty
  type t =
    | OverTy_base of {basename: id; normalty: normalty; constraints: unit}
    | OverTy_arrow of {argname: id; argty: t; retty: t}
    | OverTy_tuple of t list
  [@@deriving sexp]
end

module type Value = sig
  type t =
    | U
    | I of int
    | B of bool
    | IL of int list
    | IT of int Zzdatatype.Datatype.Tree.t
    | Tu of t list
    | NotADt
  [@@deriving sexp]
end

module type Anormal = sig
  type ty

  type id

  type 'a typed

  type term =
    | Const of Value.t
    | Var of id
    | Tu of term typed list
    | Lam of id typed list * term typed
    | Fix of id typed * term typed
    | App of id typed * id typed list
    | Let of id typed list * term typed * term typed
    | Ite of id typed * term typed * term typed
    | Match of id typed * case list
  and case = {constuctor : id;
              args : id list;
              exp : term typed}
  [@@deriving sexp]
end
