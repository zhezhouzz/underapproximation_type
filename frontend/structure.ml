open Ocaml_parser
open Parsetree
module L = Ast.Termlang
module S = Ast.Struc
module Type = Normalty.Frontend

let func_decl_of_ocamlstruct_one structure =
  match structure.pstr_desc with
  | Pstr_primitive { pval_name; pval_type; _ } ->
      (pval_name.txt, Type.core_type_to_t pval_type)
  | _ -> raise @@ failwith "translate not a func_decl"

let func_decl_of_ocamlstruct structures =
  List.map func_decl_of_ocamlstruct_one structures

let type_decl_of_ocamlstruct_one structure =
  match structure.pstr_desc with
  | Pstr_type (_, [ type_dec ]) -> Typedec.of_ocamltypedec type_dec
  | _ -> raise @@ failwith "translate not a type decl"

let type_decl_of_ocamlstruct structures =
  List.map type_decl_of_ocamlstruct_one structures

let client_of_ocamlstruct_one structure =
  match structure.pstr_desc with
  | Pstr_value (flag, [ value_binding ]) ->
      let name =
        match (Pat.pattern_to_slang value_binding.pvb_pat).x with
        | L.Var name -> name
        | _ -> failwith "die"
      in
      let body = Expr.expr_of_ocamlexpr value_binding.pvb_expr in
      S.{ name; if_rec = Expr.get_if_rec flag; body }
  | _ -> raise @@ failwith "translate not a function value"

let client_of_ocamlstruct structures =
  List.map client_of_ocamlstruct_one structures

open Zzdatatype.Datatype
open Sugar
open S

let layout_one { name; if_rec; body } =
  spf "let %s%s = %s" (if if_rec then "rec " else "") name (Expr.layout body)

let layout l = spf "%s\n" (List.split_by "\n" layout_one l)

let refinement_of_ocamlstruct_one t_of_ocamlexpr structure =
  match structure.pstr_desc with
  | Pstr_value (_, [ value_binding ]) ->
      let a =
        match value_binding.pvb_attributes with
        | [ x ] ->
            (* let () = Printf.printf "attr_name:%s\n" x.attr_name.txt in *)
            String.equal x.attr_name.txt "notation"
        | _ -> false
      in
      let name =
        match (Pat.pattern_to_slang value_binding.pvb_pat).x with
        | L.Var name -> name
        | _ -> failwith "die"
      in
      (* let () = Printf.printf "name:<%b>%s\n" a name in *)
      let refinement = t_of_ocamlexpr value_binding.pvb_expr in
      ((a, name), refinement)
  | _ -> raise @@ failwith "translate not a function value"

let refinement_of_ocamlstruct t_of_ocamlexpr structures =
  List.map (refinement_of_ocamlstruct_one t_of_ocamlexpr) structures

let layout_one_refinement f (name, r) = spf "⊢ %s : %s\n" name @@ f r

let layout_refinements f l =
  spf "%s\n" (List.split_by "\n" (layout_one_refinement f) l)

let layout_normals l =
  List.split_by "\n"
    (fun (name, ty) -> spf "val %s: %s" name @@ Type.layout ty)
    l
