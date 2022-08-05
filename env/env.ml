open Sexplib.Std

type mode = Debug of string | Release [@@deriving sexp]

type prim_path = {
  normalp : string;
  overp : string;
  underp : string;
  rev_underp : string;
}
[@@deriving sexp]

type config = { mode : mode; prim_path : prim_path } [@@deriving sexp]

let config : config option ref = ref None
