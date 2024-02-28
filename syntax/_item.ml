open Sexplib.Std
open Mtyped
open Raw_term
open Term

type 't item =
  | MSignat of {
      type_decl_map : ('t, string) typed list;
      func_type_map : ('t, string) typed list;
    }
  | MstructRaw of {
      name : string;
      if_rec : bool;
      body : ('t, 't raw_term) typed;
    }
  | Mstruct of { name : string; if_rec : bool; body : ('t, 't term) typed }
[@@deriving sexp]
