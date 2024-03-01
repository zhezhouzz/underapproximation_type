module NT = Normalty.Ntyped
open Sexplib.Std

type constructor_declaration = { constr_name : string; argsty : NT.t list }
[@@deriving sexp]
