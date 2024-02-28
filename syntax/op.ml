open Sexplib.Std

type op = PrimOp of string | DtConstructor of string [@@deriving sexp]

let compare_op a b = Sexplib.Sexp.compare (sexp_of_op a) (sexp_of_op b)
let compare_equal a b = Sexplib.Sexp.compare (sexp_of_op a) (sexp_of_op b)
