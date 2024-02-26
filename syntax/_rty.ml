open Sexplib.Std
open Mtyped

type rty =
  | RtyBase of { ou : bool; cty : int }
  | RtyBaseArr of { argrty : rty; arg : (string[@bound]); retty : rty }
  | RtyArrArr of { argrty : rty; retty : rty }
  | RtyTuple of rty list
[@@deriving sexp]
