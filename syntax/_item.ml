open Sexplib.Std
open Mtyped
open Raw_term
open Term
open Rty
open Prop
open Constructor_declaration

type 't item =
  | MTyDecl of {
      type_name : string;
      type_params : string list;
      type_decls : constructor_declaration list;
    }
  | MValDecl of ('t, string) typed
  | MMethodPred of ('t, string) typed
  | MAxiom of { name : string; prop : 't prop }
  | MFuncImpRaw of {
      name : string;
      if_rec : bool;
      body : ('t, 't raw_term) typed;
    }
  | MFuncImp of { name : string; if_rec : bool; body : ('t, 't term) typed }
  | MRty of { is_assumption : bool; name : string; rty : 't rty }
[@@deriving sexp]
