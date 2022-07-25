open Sexplib.Std

type mode = Debug of string | Release [@@deriving sexp]

type prim_path = { overp : string; underp : string; normalp : string }
[@@deriving sexp]

type config = { mode : mode; prim_path : prim_path } [@@deriving sexp]

let config : config option ref = ref None
