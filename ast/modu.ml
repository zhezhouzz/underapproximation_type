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

  let stat progs =
    List.map
      (fun { name; body } ->
        let num_branches = Anormal.NormalAnormal.term_count_branches body.x in
        let num_localvars = Anormal.NormalAnormal.term_count_localvars body.x in
        (name, num_branches, 1 + num_localvars))
      progs
end

module StrucOA = struct
  open Sexplib.Std
  open NNtyped

  type t = { name : string; body : Anormal.OverAnormal.term typed }
  [@@deriving sexp]

  type code = t list [@@deriving sexp]
end
