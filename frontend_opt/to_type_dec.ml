open Ocaml5_parser
open Parsetree
open Item
open Constructor_declaration
module Type = Normalty.Frontend
open Sugar

let constructor_declaration_of_ocaml { pcd_name; pcd_args; _ } =
  let argsty =
    match pcd_args with
    | Pcstr_tuple cts -> List.map Type.core_type_to_t cts
    | _ -> failwith "unimp complex type decl"
  in
  { constr_name = pcd_name.txt; argsty }

let constructor_declaration_to_ocaml { constr_name; argsty } =
  {
    pcd_name = Location.mknoloc constr_name;
    pcd_vars = [];
    pcd_args = Pcstr_tuple (List.map Type.t_to_core_type argsty);
    pcd_res = None;
    pcd_loc = Location.none;
    pcd_attributes = [];
  }

let of_ocamltypedec { ptype_name; ptype_params; ptype_kind; ptype_manifest; _ }
    =
  match (ptype_params, ptype_kind, ptype_manifest) with
  | params, Ptype_variant cds, None ->
      let type_params =
        List.map
          (fun (ct, (_, _)) ->
            match Type.core_type_to_t ct with
            | Type.T.Ty_var name -> name
            | _ -> _failatwith __FILE__ __LINE__ "die")
          params
      in
      MTyDecl
        {
          type_name = ptype_name.txt;
          type_params;
          type_decls = List.map constructor_declaration_of_ocaml cds;
        }
  | _ -> failwith "unimp complex type decl"

let to_ocamltypedec = function
  | MTyDecl { type_name; type_params; type_decls } ->
      {
        ptype_name = Location.mknoloc type_name;
        ptype_params =
          List.map
            (fun t ->
              ( Type.t_to_core_type (Type.T.Ty_var t),
                (Asttypes.NoVariance, Asttypes.NoInjectivity) ))
            type_params;
        ptype_cstrs = [];
        ptype_kind =
          Ptype_variant (List.map constructor_declaration_to_ocaml type_decls);
        ptype_manifest = None;
        ptype_attributes = [];
        ptype_loc = Location.none;
        ptype_private = Asttypes.Public;
      }
  | _ -> _failatwith __FILE__ __LINE__ "die"

let layout_ocaml es =
  let _ = Format.flush_str_formatter () in
  Pprintast.structure Format.str_formatter
  @@ List.map
       (fun e ->
         {
           pstr_desc = Pstr_type (Asttypes.Recursive, [ e ]);
           pstr_loc = Location.none;
         })
       es;
  Format.flush_str_formatter ()

let layout_type_dec e = layout_ocaml [ to_ocamltypedec e ]
