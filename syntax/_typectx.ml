open Sexplib.Std
open Mtyped

type 't ctx = Typectx of ('t, string) typed list [@@deriving sexp]
