open Language
open Sugar

let rec infer_constant (c : constant) =
  let open Nt in
  match c with
  | U -> Ty_unit
  | I _ -> Ty_int
  | B _ -> Ty_bool
  | Tu l -> Ty_tuple (List.map infer_constant l)
  | Dt _ -> _failatwith __FILE__ __LINE__ "unimp datatype instance"
