open Ocaml_parser
open Parsetree
open Zzdatatype.Datatype

let pattern_to_string pattern =
  match pattern.ppat_desc with
  | Ppat_var ident -> ident.txt
  | _ ->
      Pprintast.pattern Format.std_formatter pattern;
      raise @@ failwith "wrong pattern name, maybe untyped"

let parse_type_declaration declare =
  let name, tp = (declare.ptype_name, declare.ptype_kind) in
  match tp with
  | Ptype_record _ ->
    failwith "unimp record type"
      (* ( name.txt, *)
      (*   Ast.T.Record *)
      (*     (List.map *)
      (*        (fun x -> (x.pld_name.txt, Type.core_type_to_t x.pld_type)) *)
      (*        l) ) *)
  | Ptype_abstract -> (
      match declare.ptype_manifest with
      | None -> failwith "undefined abstract type"
      | Some t -> (name.txt, Type.core_type_to_t t))
  | Ptype_open -> failwith "un-imp Ptype_open"
  | Ptype_variant _ -> failwith "un-imp Ptype_variant"

let parse_value_declaration declare = (declare.pval_name.txt, declare.pval_type)

open Languages.Signat

let signature_of_ocamlstructs structures =
  let signature =
    { type_decl_map = Hashtbl.create 100; func_type_map = Hashtbl.create 100 }
  in
  let f structure =
    match structure.pstr_desc with
    | Pstr_primitive declare ->
        Hashtbl.add signature.func_type_map declare.pval_name.txt
          (Type.core_type_to_t declare.pval_type)
    | Pstr_type (_, l) ->
        List.iter
          (fun declare ->
            let tpname, tp = parse_type_declaration declare in
            Hashtbl.add signature.type_decl_map tpname tp)
          l
    | _ -> failwith "un-imp signature"
  in
  List.iter f structures;
  signature
