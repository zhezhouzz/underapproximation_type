open Languages.Normalty
open Languages.Op
module Prim = Abstraction.Prim

let check = function
  | Plus, [ Ty_int; Ty_int ] -> Ty_int
  | Minus, [ Ty_int; Ty_int ] -> Ty_int
  | Gt, [ Ty_int; Ty_int ] -> Ty_bool
  | Ge, [ Ty_int; Ty_int ] -> Ty_bool
  | Lt, [ Ty_int; Ty_int ] -> Ty_bool
  | Le, [ Ty_int; Ty_int ] -> Ty_bool
  | Eq, [ Ty_int; Ty_int ] -> Ty_bool
  | Neq, [ Ty_int; Ty_int ] -> Ty_bool
  | Eq, [ Ty_bool; Ty_bool ] -> Ty_bool
  | Neq, [ Ty_bool; Ty_bool ] -> Ty_bool
  | And, [ Ty_bool; Ty_bool ] -> Ty_bool
  | Or, [ Ty_bool; Ty_bool ] -> Ty_bool
  | Dt dt, argsty ->
      let ty = Prim.get_primitive_dt_normal_ty (dt, argsty) in
      snd @@ destruct_arrow_tp ty
  | op, argsty ->
      failwith
        (Sugar.spf "unknown primitive operators (%s) and arg types (%s)"
           (op_to_string op)
           (Zzdatatype.Datatype.List.split_by_comma Frontend.Type.layout argsty))
