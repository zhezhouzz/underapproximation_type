open Op
open Sugar

let builtin_primop =
  [ "+"; "-"; "*"; "/"; ">"; ">="; "<"; "<="; "=="; "!="; "&&"; "||" ]

(* a string:
   1. is in builtin_primop, then is a builtin operator.
   2. is in not builtin_primop, and with a non-lowercase alphabeta first char, then is a data constructor (include [])
   3. else, invalid
*)

let is_builtin_op str = List.exists (String.equal str) builtin_primop

let is_dt_op str =
  let fst_char = String.get str 0 in
  Char.uppercase_ascii fst_char == fst_char

let string_to_op_opt str =
  if is_builtin_op str then Some (PrimOp str)
  else if is_dt_op str then Some (DtConstructor str)
  else None

let string_to_op str =
  match string_to_op_opt str with
  | Some op -> op
  | None -> _failatwith __FILE__ __LINE__ "unknown operator of string"

let _string_to_dt_op file line str =
  match string_to_op str with
  | DtConstructor op -> DtConstructor op
  | _ -> _failatwith file line "is not data constructor"

let layout_op = function PrimOp str -> str | DtConstructor str -> str
