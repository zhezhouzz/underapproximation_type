open Sexplib.Std

type mode = Debug of string | Release [@@deriving sexp]

type prim_path = {
  normalp : string;
  overp : string;
  underp : string;
  rev_underp : string;
  type_decls : string;
  lemmas : string;
  functional_lemmas : string;
}
[@@deriving sexp]

type config = { mode : mode; all_mps : string list; prim_path : prim_path }
[@@deriving sexp]

let config : config option ref = ref None
