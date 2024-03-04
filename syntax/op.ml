open Sexplib.Std

type op = PrimOp of string | DtConstructor of string [@@deriving sexp]

let compare_op a b = Sexplib.Sexp.compare (sexp_of_op a) (sexp_of_op b)
let compare_equal a b = Sexplib.Sexp.compare (sexp_of_op a) (sexp_of_op b)

(* NOTE: there are several constructors cannot be parsed directly from the external files *)
let dt_name_for_typectx = function
  | "()" -> "tt"
  | "::" -> "cons"
  | "[]" -> "nil"
  | _ as s -> s

let op_name_for_typectx = function
  | PrimOp name -> name
  | DtConstructor name -> dt_name_for_typectx name
