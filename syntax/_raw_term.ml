open Mtyped
open Sexplib.Std

(* type 't lit = *)
(*   | AC of int *)
(*   | AVar of ((string[@free]), 't) typed *)
(*   | ATu of ('t, 't lit) typed list *)
(*   | AProj of ('t, 't lit) typed * int *)
(*   | AAppOp of (string, 't) typed * ('t, 't lit) typed list *)
(* [@@deriving sexp] *)

(* type t = U | B of bool | I of int | Tu of t list | Dt of string * t list *)
(* [@@deriving sexp] *)

type 't raw_term =
  | Var of (('t, string) typed[@free])
  | Const of int
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
  | AppOp of ('t, string) typed * ('t, 't raw_term) typed list
  | Ite of
      ('t, 't raw_term) typed
      * ('t, 't raw_term) typed
      * ('t, 't raw_term) typed
  | Tu of ('t, 't raw_term) typed list
  | Match of ('t, 't raw_term) typed * 't raw_match_case list

and 't raw_match_case =
  | Matchcase of {
      constructor : ('t, string) typed;
      args : (('t, string) typed list[@bound]);
      exp : ('t, 't raw_term) typed;
    }
[@@deriving sexp]
