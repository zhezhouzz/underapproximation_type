open Sexplib.Std
open Mtyped
module Nt = Normalty.Ntyped
open Prop

type 't cty = Cty of { nty : Nt.t; phi : 't prop } [@@deriving sexp]

(* NOTE: v is default operator *)
let default_v = "v"

(* NOTE: modified *)
let rec fv_cty (cty_e : 't cty) =
  match cty_e with
  | Cty { phi; _ } ->
      let res = [] @ fv_prop phi in
      List.filter_map
        (fun x -> if String.equal default_v x.x then None else Some x)
        res

and typed_fv_cty (cty_e : ('t, 't cty) typed) = fv_cty cty_e.x

let rec subst_cty (string_x : string) f (cty_e : 't cty) =
  match cty_e with
  | Cty { nty; phi } -> Cty { nty; phi = subst_prop string_x f phi }

and typed_subst_cty (string_x : string) f (cty_e : ('t, 't cty) typed) =
  cty_e #-> (subst_cty string_x f)

let rec map_cty (f : 't -> 's) (cty_e : 't cty) =
  match cty_e with Cty { nty; phi } -> Cty { nty; phi = map_prop f phi }

and typed_map_cty (f : 't -> 's) (cty_e : ('t, 't cty) typed) =
  cty_e #=> f #-> (map_cty f)

let fv_cty_id e = fv_typed_id_to_id fv_cty e
let typed_fv_cty_id e = fv_typed_id_to_id typed_fv_cty e
let subst_cty_instance x instance e = subst_f_to_instance subst_cty x instance e

let typed_subst_cty_instance x instance e =
  subst_f_to_instance typed_subst_cty x instance e
(* Generated from _cty.ml *)

let erase_cty = function Cty { nty; _ } -> nty
