open Mtyped
open Sexplib.Std
open Constant
open Op

type 't value =
  | Const of constant
  | Var of (('t, string) typed[@free])
  | Lam of { lamarg : (('t, string) typed[@bound]); body : ('t, 't term) typed }
  | Fix of {
      fixname : (('t, string) typed[@bound]);
      fixarg : (('t, string) typed[@bound]);
      body : ('t, 't term) typed;
    }
  | VTu of ('t, 't value) typed list

and 't term =
  | Err
  | CVal of ('t, 't value) typed
  | LetE of {
      rhs : ('t, 't term) typed;
      lhs : (('t, string) typed[@bound]);
      body : ('t, 't term) typed;
    }
  | CLetDeTu of {
      turhs : ('t, 't value) typed;
      tulhs : (('t, string) typed list[@bound]);
      body : ('t, 't term) typed;
    }
  | CApp of { appf : ('t, 't value) typed; apparg : ('t, 't value) typed }
  | CAppOp of { op : ('t, op) typed; appopargs : ('t, 't value) typed list }
  (* branches, we will copy the continuations for branches *)
  | Match of {
      matched : ('t, 't value) typed;
      match_cases : 't match_case list;
    }

and 't match_case =
  | Matchcase of {
      constructor : ('t, string) typed;
      args : (('t, string) typed list[@bound]);
      exp : ('t, 't term) typed;
    }
[@@deriving sexp]
