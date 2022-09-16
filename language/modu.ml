open Normalty.Ast

module Signat = struct
  open Sexplib.Std

  type t_map = (string, T.t) Hashtbl.t [@@deriving sexp]
  type t = { type_decl_map : t_map; func_type_map : t_map } [@@deriving sexp]
end

module Struc = struct
  open Sexplib.Std

  type t = {
    name : string;
    if_rec : bool;
    body : Termlang.T.term Termlang.T.opttyped;
  }
  [@@deriving sexp]

  type code = t list [@@deriving sexp]
end

module StrucNA = struct
  open Sexplib.Std
  open NNtyped

  type t = { name : string; body : Anormal.NormalAnormal.term typed }
  [@@deriving sexp]

  type code = t list [@@deriving sexp]
end

module StrucOA = struct
  open Sexplib.Std
  open NNtyped

  type t = { name : string; body : Anormal.OverAnormal.term typed }
  [@@deriving sexp]

  type code = t list [@@deriving sexp]
end
