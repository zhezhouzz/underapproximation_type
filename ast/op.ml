module T = struct
  open Sexplib.Std

  type t =
    | Plus
    | Minus
    | Gt
    | Ge
    | Lt
    | Le
    | Eq
    | Neq
    | And
    | Or
    | Dt of string
  [@@deriving sexp]

  type prim = PrimOp of t * Normalty.T.t | External of string
  [@@deriving sexp]

  let prim_dt = [ "[]"; "::" ]
  let is_prim_dt x = List.exists (String.equal x) prim_dt

  let op_of_string = function
    | "+" -> Plus
    | "-" -> Minus
    | ">" -> Gt
    | ">=" -> Ge
    | "<" -> Lt
    | "<=" -> Le
    | "==" -> Eq
    | "!=" -> Neq
    | "&&" -> And
    | "||" -> Or
    | x when is_prim_dt x -> Dt x
    | x ->
        Printf.printf "%s is not op\n" x;
        failwith "not a op"

  let op_of_string_opt x = try Some (op_of_string x) with _ -> None

  let op_to_string = function
    | Plus -> "+"
    | Minus -> "-"
    | Gt -> ">"
    | Ge -> ">="
    | Lt -> "<"
    | Le -> "<="
    | Eq -> "=="
    | Neq -> "!="
    | And -> "&&"
    | Or -> "||"
    | Dt dt -> dt

  let op_to_alias = function
    | Plus -> "plus"
    | Minus -> "minus"
    | Gt -> "gt"
    | Ge -> "ge"
    | Lt -> "lt"
    | Le -> "le"
    | Eq -> "eq"
    | Neq -> "neq"
    | And -> "and"
    | Or -> "or"
    | Dt "[]" -> "nil"
    | Dt "::" -> "cons"
    | Dt dt -> dt

  let op_of_alias = function
    | "plus" -> Plus
    | "minus" -> Minus
    | "gt" -> Gt
    | "ge" -> Ge
    | "lt" -> Lt
    | "le" -> Le
    | "eq" -> Eq
    | "neq" -> Neq
    | "and" -> And
    | "or" -> Or
    | "cons" -> Dt "::"
    | "nil" -> Dt "[]"
    | _ -> failwith "unknown primitive operators"
end
