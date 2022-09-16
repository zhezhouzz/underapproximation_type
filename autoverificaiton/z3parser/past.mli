type 'a loc = { x : 'a; loc : Lexing.position }

type term =
  | True
  | OpVarEqInt of int * int
  | And of term loc list
  | Or of term loc list
  | Not of term loc
