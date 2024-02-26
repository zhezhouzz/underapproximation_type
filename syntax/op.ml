open Sexplib.Std

type op = PrimOp of string | DtConstructor of string [@@deriving sexp]

let compare_op a b = Sexplib.Sexp.compare (sexp_of_op a) (sexp_of_op b)
let compare_equal a b = Sexplib.Sexp.compare (sexp_of_op a) (sexp_of_op b)

open Sugar

let builtin_primop =
  [ "+"; "-"; "*"; "/"; ">"; ">="; "<"; "<="; "=="; "!="; "&&"; "||" ]

let op_of_string_opt str =
  if List.exists (String.equal str) builtin_primop then Some (PrimOp str)
  else
    let fst_char = String.get str 0 in
    if Char.lowercase_ascii fst_char != fst_char then Some (DtConstructor str)
    else None

let op_of_string str =
  match op_of_string_opt str with
  | Some op -> op
  | None -> _failatwith __FILE__ __LINE__ "unknown operator of string"

let layout = function PrimOp str -> str | DtConstructor str -> str

let pp_layout = function
  | PrimOp op -> op
  | DtConstructor "cons" -> "::"
  | DtConstructor "nil" -> "[]"
  | DtConstructor dt -> dt
