open Sexplib.Std
open Mtyped
module Nt = Normalty.Ntyped
open Lit

type 't prop =
  | Lit of ('t, 't lit) typed
  | Implies of 't prop * 't prop
  | Ite of 't prop * 't prop * 't prop
  | Not of 't prop
  | And of 't prop list
  | Or of 't prop list
  | Iff of 't prop * 't prop
  | Forall of { qv : (('t, string) typed[@bound]); body : 't prop }
  | Exists of { qv : (('t, string) typed[@bound]); body : 't prop }
[@@deriving sexp]
