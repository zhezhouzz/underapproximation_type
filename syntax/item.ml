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
      name : ('t, string) typed;
      if_rec : bool;
      body : ('t, 't raw_term) typed;
    }
  | MFuncImp of {
      name : ('t, string) typed;
      if_rec : bool;
      body : ('t, 't term) typed;
    }
  | MRty of { is_assumption : bool; name : string; rty : 't rty }
[@@deriving sexp]

let rec fv_item (item_e : 't item) =
  match item_e with
  | MTyDecl _ -> []
  | MValDecl _ -> []
  | MMethodPred _ -> []
  | MAxiom { prop; _ } -> [] @ fv_prop prop
  | MFuncImpRaw { body; _ } -> [] @ typed_fv_raw_term body
  | MFuncImp { body; _ } -> [] @ typed_fv_term body
  | MRty { rty; _ } -> [] @ fv_rty rty

and typed_fv_item (item_e : ('t, 't item) typed) = fv_item item_e.x

let rec map_item (f : 't -> 's) (item_e : 't item) =
  match item_e with
  | MTyDecl { type_name; type_params; type_decls } ->
      MTyDecl { type_name; type_params; type_decls }
  | MValDecl _t_stringtyped0 -> MValDecl _t_stringtyped0 #=> f
  | MMethodPred _t_stringtyped0 -> MMethodPred _t_stringtyped0 #=> f
  | MAxiom { name; prop } -> MAxiom { name; prop = map_prop f prop }
  | MFuncImpRaw { name; if_rec; body } ->
      MFuncImpRaw
        { name = name #=> f; if_rec; body = typed_map_raw_term f body }
  | MFuncImp { name; if_rec; body } ->
      MFuncImp { name = name #=> f; if_rec; body = typed_map_term f body }
  | MRty { is_assumption; name; rty } ->
      MRty { is_assumption; name; rty = map_rty f rty }

and typed_map_item (f : 't -> 's) (item_e : ('t, 't item) typed) =
  item_e #=> f #-> (map_item f)

let fv_item_id e = fv_typed_id_to_id fv_item e
let typed_fv_item_id e = fv_typed_id_to_id typed_fv_item e

(* Generated from _item.ml *)

let get_rty_by_name (item_e : 't item list) (x : string) =
  let res =
    List.filter_map
      (function
        | MRty { is_assumption; name; rty } when String.equal name x ->
            Some (is_assumption, rty)
        | _ -> None)
      item_e
  in
  match res with
  | [] -> Sugar._failatwith __FILE__ __LINE__ "die"
  | [ x ] -> x
  | _ -> Sugar._failatwith __FILE__ __LINE__ "die"
