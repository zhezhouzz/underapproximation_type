open Ocaml5_parser
open Parsetree
open Mtyped
open Mutils
open Zzdatatype.Datatype
module Nt = Normalty.Frontend
open Item
open To_raw_term
open To_rty
open To_prop
open To_id
open Sugar

let ocaml_structure_item_to_item structure =
  match structure.pstr_desc with
  | Pstr_primitive { pval_name; pval_type; pval_prim; pval_attributes; _ } -> (
      if String.equal pval_name.txt "method_predicates" then
        let mp = List.nth pval_prim 0 in
        MMethodPred mp #: (Some (Nt.core_type_to_t pval_type))
      else
        match pval_attributes with
        | [ x ] when String.equal x.attr_name.txt "method_pred" ->
            MMethodPred pval_name.txt #: (Some (Nt.core_type_to_t pval_type))
        | _ -> MValDecl pval_name.txt #: (Some (Nt.core_type_to_t pval_type)))
  | Pstr_type (_, [ type_dec ]) -> To_type_dec.of_ocamltypedec type_dec
  | Pstr_value (flag, [ value_binding ]) -> (
      let name = id_of_pattern value_binding.pvb_pat in
      match value_binding.pvb_attributes with
      | [ x ] -> (
          match x.attr_name.txt with
          | "axiom" ->
              MAxiom { name; prop = prop_of_expr value_binding.pvb_expr }
          | "assert" ->
              MRty
                {
                  is_assumption = false;
                  name;
                  rty = rty_of_expr value_binding.pvb_expr;
                }
          | "library" ->
              MRty
                {
                  is_assumption = true;
                  name;
                  rty = rty_of_expr value_binding.pvb_expr;
                }
          | _ ->
              _failatwith __FILE__ __LINE__
                "syntax error: non known rty kind, not axiom | assert | library"
          )
      | [] ->
          let body = typed_raw_term_of_expr value_binding.pvb_expr in
          MFuncImpRaw
            {
              name =
                name
                #: (Some (Raw_term.__get_lam_term_ty __FILE__ __LINE__ body.x));
              if_rec = get_if_rec flag;
              body;
            }
      | _ -> _failatwith __FILE__ __LINE__ "wrong syntax")
  | _ -> _failatwith __FILE__ __LINE__ "translate not a func_decl"

let ocaml_structure_to_items structure =
  List.map ocaml_structure_item_to_item structure

let layout_ct_opt = function
  | Some ct -> Nt.layout ct
  | None -> _failatwith __FILE__ __LINE__ "die"

let layout_item = function
  | MTyDecl _ as item -> To_type_dec.layout_type_dec item
  | MMethodPred x ->
      spf "val[@method_predicate] %s: %s" x.x @@ layout_ct_opt x.ty
  | MValDecl x -> spf "val %s: %s" x.x @@ layout_ct_opt x.ty
  | MAxiom { name; prop } -> spf "let[@axiom] %s = %s" name (layout_prop prop)
  | MFuncImpRaw { name; if_rec; body } ->
      spf "let %s%s = %s"
        (if if_rec then "rec " else "")
        name.x
        (layout_typed_raw_term body)
  | MFuncImp { name; if_rec; _ } ->
      spf "let %s%s = %s" (if if_rec then "rec " else "") name.x "??"
  | MRty { is_assumption = false; name; rty } ->
      spf "let[@assert] %s = %s" name (layout_rty rty)
  | MRty { is_assumption = true; name; rty } ->
      spf "let[@library] %s = %s" name (layout_rty rty)

let layout_structure l = spf "%s\n" (List.split_by "\n" layout_item l)
