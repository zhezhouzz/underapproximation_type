open Mtyped
open Sexplib.Std
open Constant
open Op

type 't raw_term =
  | Var of (('t, string) typed[@free])
  | Const of constant
  | Lam of {
      lamarg : (('t, string) typed[@bound]);
      lambody : ('t, 't raw_term) typed;
    }
  | Err
  | Let of {
      if_rec : bool;
      rhs : ('t, 't raw_term) typed;
      lhs : (('t, string) typed list[@bound]);
      letbody : ('t, 't raw_term) typed;
    }
  | App of ('t, 't raw_term) typed * ('t, 't raw_term) typed list
  | AppOp of ('t, op) typed * ('t, 't raw_term) typed list
  | Ite of
      ('t, 't raw_term) typed
      * ('t, 't raw_term) typed
      * ('t, 't raw_term) typed
  | Tu of ('t, 't raw_term) typed list
  | Match of {
      matched : ('t, 't raw_term) typed;
      match_cases : 't raw_match_case list;
    }

and 't raw_match_case =
  | Matchcase of {
      constructor : ('t, string) typed;
      args : (('t, string) typed list[@bound]);
      exp : ('t, 't raw_term) typed;
    }
[@@deriving sexp]
