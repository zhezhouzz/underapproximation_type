open Sexplib.Std
open Mtyped
open Cty

type 't rty =
  | RtyBase of { ou : bool; cty : 't cty }
  | RtyBaseArr of { argcty : 't cty; arg : (string[@bound]); retty : 't rty }
  | RtyBaseDepPair of {
      argcty : 't cty;
      arg : (string[@bound]);
      retty : 't rty;
    }
  | RtyArrArr of { argrty : 't rty; retty : 't rty }
  | RtyTuple of 't rty list
[@@deriving sexp]
