type 'a loc = { x : 'a; loc : Lexing.position }
type lit = VarI of int | CI of int
type op = Eq | Lt | Gt | Le | Ge

type term =
  | True
  | Lit of lit
  | App of string * lit list
  | OpEq of term loc * lit
  | OpLe of lit * term loc
  | And of term loc list
  | Or of term loc list
  | Not of term loc
  | Ite of term loc * term loc * term loc
  | Let of string * term loc * term loc
