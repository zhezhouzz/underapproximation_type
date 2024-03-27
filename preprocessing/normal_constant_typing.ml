open Lang
open Sugar
open Normal_op_typing

let rec infer_constant ctx (c : constant) =
  let open Nt in
  match c with
  | U -> Ty_unit
  | I _ -> Ty_int
  | B _ -> Ty_bool
  | Tu l -> Ty_tuple (List.map (infer_constant ctx) l)
  | Dt (op, args) ->
      let args = List.map (infer_constant ctx) args in
      let ty = get_constructor_type ctx op in
      let argsty, ty = Nt.destruct_arr_tp ty in
      if
        List.length args == List.length argsty
        && (List.for_all (fun (a, b) -> Nt.eq a b) @@ List.combine args argsty)
      then ty
      else _failatwith __FILE__ __LINE__ "wrong datatype args"
