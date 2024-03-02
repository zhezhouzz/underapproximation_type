include Frontendz
include Mtyped
include Constant
include Op
include Lit
include Prop
include Cty
include Rty
include Typectx
include Raw_term
include Term
include Constructor_declaration
include Item

module FrontendRaw = struct
  let layout_constant = To_constant.layout_constant
  let layout_constants = To_constant.layout_constants
  let layout_op = To_op.layout_op
  let layout_typed_lit = To_lit.layout_typed_lit
  let layout_lit = To_lit.layout
  let layout_prop = To_prop.layout_prop
  let layout_cty = To_cty.layout_cty
  let layout_rty = To_rty.layout_rty
  let layout_raw_term = To_raw_term.layout_raw_term
  let layout_typed_raw_term = To_raw_term.layout_typed_raw_term
  let layout_item = To_item.layout_item
  let layout_structure = To_item.layout_structure
end

module FrontendTyped = struct
  open Sugar

  let force = function
    | None -> _failatwith __FILE__ __LINE__ "die"
    | Some x -> x

  let layout_constant = To_constant.layout_constant
  let layout_constants = To_constant.layout_constants
  let layout_op = To_op.layout_op

  let layout_typed_lit e =
    To_lit.layout_typed_lit e #-> force #=> (map_lit force)

  let layout_lit e = To_lit.layout @@ map_lit force e
  let layout_prop prop = To_prop.layout_prop @@ map_prop force prop
  let layout_cty cty = To_cty.layout_cty @@ map_cty force cty
  let layout_rty rty = To_rty.layout_rty @@ map_rty force rty
  let layout_raw_term e = To_raw_term.layout_raw_term @@ map_raw_term force e

  let layout_typed_raw_term e =
    To_raw_term.layout_typed_raw_term e #-> force #=> (map_term force)

  let layout_item item = To_item.layout_item @@ map_item force item

  let layout_structure s =
    To_item.layout_structure @@ List.map (map_item force) s
end

(* module Typedec = struct *)
(*   include Frontendu.Typedec *)
(*   include Typedec *)
(* end *)

(* module Struc = struct *)
(*   include Frontendu.Structure *)
(*   include Struc *)

(*   let prog_of_ocamlstruct = Frontendu.Structure.client_of_ocamlstruct *)
(*   let mps_of_ocamlstruct = Frontendu.Structure.mps_of_ocamlstruct_one *)
(* end *)

(* module NL = struct *)
(*   include NL *)

(*   let layout x = Frontendu.Expr.layout @@ Trans.nan_to_term x *)
(*   let layout_value v = layout { x = V v; ty = v.ty } *)
(*   let layout_id x = layout_value { x = Var x; ty = x.ty } *)
(* end *)

(* module StrucNA = struct *)
(*   include StrucNA *)

(*   let prog_of_ocamlstruct = Frontendu.Structure.client_of_ocamlstruct *)
(*   let layout code = Struc.layout @@ Trans.struc_nan_to_term code *)
(* end *)

(* module OT = struct *)
(*   include Frontendu.Overty *)
(*   include OT *)
(* end *)

(* module UL = struct *)
(*   include UL *)

(*   let typed_map f { ty; x } = { ty; x = f x } *)
(*   let to_ntyped NNtyped.{ x; ty } = Ntyped.{ x; ty = snd ty } *)

(*   let get_args_return_name retname body = *)
(*     let open Anormal.NormalAnormal in *)
(*     let rec aux body = *)
(*       match body.x with *)
(*       | V { x = Lam { lamarg; lambody }; _ } -> *)
(*           let args, retv = aux lambody in *)
(*           (to_ntyped lamarg :: args, retv) *)
(*       | _ -> ([], to_ntyped { x = retname; ty = body.ty }) *)
(*     in *)
(*     aux body *)
(* end *)

(* module NT = Normalty.Ntyped *)
