open Sexplib.Std

type op = PrimOp of string | DtConstructor of string [@@deriving sexp]

let compare_op a b = Sexplib.Sexp.compare (sexp_of_op a) (sexp_of_op b)
let compare_equal a b = Sexplib.Sexp.compare (sexp_of_op a) (sexp_of_op b)

(* NOTE: there are several constructors cannot be parsed directly from the external files *)
let dt_name_for_typectx = function
  | "()" -> "TT"
  | "::" -> "Cons"
  | "[]" -> "Nil"
  | _ as s -> s

let op_name_for_typectx = function
  | PrimOp name -> name
  | DtConstructor name -> dt_name_for_typectx name

(* a string:
   1. is in builtin_primop, then is a builtin operator.
   2. is in not builtin_primop, and with a non-lowercase alphabeta first char, then is a data constructor (include [])
   3. else, invalid
*)

let builtin_primop =
  [ "+"; "-"; "*"; "/"; ">"; ">="; "<"; "<="; "=="; "!="; "&&"; "||" ]

let is_builtin_op str = List.exists (String.equal str) builtin_primop

let is_dt_op str =
  let fst_char = String.get str 0 in
  Char.uppercase_ascii fst_char == fst_char
