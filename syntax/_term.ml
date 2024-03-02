open Mtyped
open Sexplib.Std
open Constant
open Op

type 't value =
  | VConst of constant
  | VVar of (('t, string) typed[@free])
  | VLam of {
      lamarg : (('t, string) typed[@bound]);
      body : ('t, 't term) typed;
    }
  | VFix of {
      fixname : (('t, string) typed[@bound]);
      fixarg : (('t, string) typed[@bound]);
      body : ('t, 't term) typed;
    }
  | VTu of ('t, 't value) typed list

and 't term =
  | CErr
  | CVal of ('t, 't value) typed
  | CLetE of {
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
  | CMatch of {
      matched : ('t, 't value) typed;
      match_cases : 't match_case list;
    }

and 't match_case =
  | CMatchcase of {
      constructor : ('t, string) typed;
      args : (('t, string) typed list[@bound]);
      exp : ('t, 't term) typed;
    }
[@@deriving sexp]
