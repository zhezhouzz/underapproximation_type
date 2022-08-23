module T = struct
  type constructor_declaration = {
    constr_name : string;
    argsty : Normalty.T.t list;
  }

  type t = { type_name : string; type_decls : constructor_declaration list }
  type user_defined_types = t list

  open Normalty.T

  let name0_to_ctype name = Ty_constructor (name, [])

  let mk_constr_types { constr_name; argsty } retty =
    (constr_name, construct_arrow_tp (argsty, retty))

  let mk_ctx es =
    List.concat
    @@ List.map
         (fun { type_name; type_decls } ->
           let retty = name0_to_ctype type_name in
           List.map (fun x -> mk_constr_types x retty) type_decls)
         es
end
