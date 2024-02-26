open Sexplib.Std

type op = PrimOp of string | DtConstructor of string [@@deriving sexp]
