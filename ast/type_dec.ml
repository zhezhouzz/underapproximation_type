module T = struct
  module NT = Normalty.Ast.T

  type constructor_declaration = { constr_name : string; argsty : NT.t list }

  type t = {
    type_name : string;
    type_params : NT.t list;
    type_decls : constructor_declaration list;
  }

  type user_defined_types = t list

  open NT

  let name_to_ctype name type_params = Ty_constructor (name, type_params)

  let mk_constr_types { constr_name; argsty } retty =
    (constr_name, construct_arrow_tp (argsty, retty))

  let mk_ctx es =
    List.concat
    @@ List.map
         (fun { type_name; type_params; type_decls } ->
           let retty = name_to_ctype type_name type_params in
           List.map (fun x -> mk_constr_types x retty) type_decls)
         es
end
