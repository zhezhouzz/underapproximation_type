module T = struct
  open Sexplib.Std

  type op =
    | Plus
    | Minus
    | Multiply
    | Div
    | Gt
    | Ge
    | Lt
    | Le
    | Eq
    | Neq
    | And
    | Or
  [@@deriving sexp]

  type t = PrimOp of op | DtConstructor of string | External of string
  [@@deriving sexp]

  let compare a b = Sexplib.Sexp.compare (sexp_of_t a) (sexp_of_t b)

  let op_of_string = function
    | "+" -> Plus
    | "-" -> Minus
    | "*" -> Multiply
    | "/" -> Div
    | ">" -> Gt
    | ">=" -> Ge
    | "<" -> Lt
    | "<=" -> Le
    | "==" -> Eq
    | "!=" -> Neq
    | "&&" -> And
    | "||" -> Or
    | _ -> failwith "not a op"
  (* | "[]" -> Dt "[]" *)
  (* | "::" -> Dt "::" *)

  let op_of_string_opt x = try Some (op_of_string x) with _ -> None

  let op_to_string = function
    | Plus -> "+"
    | Minus -> "-"
    | Multiply -> "*"
    | Div -> "/"
    | Gt -> ">"
    | Ge -> ">="
    | Lt -> "<"
    | Le -> "<="
    | Eq -> "=="
    | Neq -> "!="
    | And -> "&&"
    | Or -> "||"
  (* | Dt dt -> dt *)

  let op_to_alias = function
    | Plus -> "plus"
    | Minus -> "minus"
    | Multiply -> "multiply"
    | Div -> "div"
    | Gt -> "gt"
    | Ge -> "ge"
    | Lt -> "lt"
    | Le -> "le"
    | Eq -> "eq"
    | Neq -> "neq"
    | And -> "and"
    | Or -> "or"
  (* | Dt "[]" -> "nil" *)
  (* | Dt "::" -> "cons" *)
  (* | Dt dt -> dt *)

  let op_of_alias = function
    | "plus" -> Plus
    | "minus" -> Minus
    | "multiply" -> Multiply
    | "div" -> Div
    | "gt" -> Gt
    | "ge" -> Ge
    | "lt" -> Lt
    | "le" -> Le
    | "eq" -> Eq
    | "neq" -> Neq
    | "and" -> And
    | "or" -> Or
    (* | "cons" -> Dt "::" *)
    (* | "nil" -> Dt "[]" *)
    | _ -> failwith "unknown primitive operators"

  let op_of_alias_opt name = try Some (op_of_alias name) with _ -> None

  let t_to_string_for_load = function
    | PrimOp op -> op_to_alias op
    | DtConstructor dt -> String.lowercase_ascii dt
    | External f -> f

  let t_to_string = function
    | PrimOp op -> op_to_string op
    | DtConstructor "cons" -> "::"
    | DtConstructor "nil" -> "[]"
    | DtConstructor dt -> dt
    | External f -> f
end
