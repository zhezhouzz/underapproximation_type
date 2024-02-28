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

let rec fv_item (item_e : 't item) =
  match item_e with
  | MSignat _ -> []
  | MstructRaw { body; _ } -> [] @ typed_fv_raw_term body
  | Mstruct { body; _ } -> [] @ typed_fv_term body

and typed_fv_item (item_e : ('t, 't item) typed) = fv_item item_e.x

let rec map_item (f : 't -> 's) (item_e : 't item) =
  match item_e with
  | MSignat { type_decl_map; func_type_map } ->
      MSignat
        {
          type_decl_map = List.map (fun x -> x #=> f) type_decl_map;
          func_type_map = List.map (fun x -> x #=> f) func_type_map;
        }
  | MstructRaw { name; if_rec; body } ->
      MstructRaw { name; if_rec; body = typed_map_raw_term f body }
  | Mstruct { name; if_rec; body } ->
      Mstruct { name; if_rec; body = typed_map_term f body }

and typed_map_item (f : 't -> 's) (item_e : ('t, 't item) typed) =
  item_e #-> (map_item f)

let fv_item_id e = fv_typed_id_to_id fv_item e
let typed_fv_item_id e = fv_typed_id_to_id typed_fv_item e

(* Generated from _item.ml *)
