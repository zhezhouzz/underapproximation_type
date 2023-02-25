open Sexplib.Std

type mode = Debug of string | Release [@@deriving sexp]

type prim_path = {
  normalp : string;
  overp : string;
  under_basicp : string;
  underp : string;
  rev_underp : string;
  type_decls : string;
  lemmas : string;
  functional_lemmas : string;
}
[@@deriving sexp]

type config = {
  mode : mode;
  logfile : string;
  resfile : string;
  all_mps : string list;
  prim_path : prim_path;
  measure : string;
}
[@@deriving sexp]

let config : config option ref = ref None
