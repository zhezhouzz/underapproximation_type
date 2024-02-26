open Sexplib.Std
open Mtyped
type 't lit =
  | AC of int 
  | AVar of ((('t, string) typed)[@free ]) 
  | ATu of ('t, 't lit) typed list 
  | AProj of ('t, 't lit) typed * int 
  | AAppOp of ('t, string) typed * ('t, 't lit) typed list [@@deriving sexp]
let rec fv_lit (lit_e : 't lit) =
  match lit_e with
  | AC _ -> []
  | AVar _t_stringtyped0 -> [] @ [_t_stringtyped0]
  | ATu _ -> []
  | AProj (_, _) -> []
  | AAppOp (_, _) -> []
and typed_fv_lit (lit_e : ('t, 't lit) typed) = fv_lit lit_e.x
let rec subst_lit (string_x : string) f (lit_e : 't lit) =
  match lit_e with
  | AC int0 -> AC int0
  | AVar _t_stringtyped0 ->
      if String.equal _t_stringtyped0.x string_x
      then f _t_stringtyped0
      else AVar _t_stringtyped0
  | ATu _t__tlittypedlist0 -> ATu _t__tlittypedlist0
  | AProj (_t__tlittyped0, int1) -> AProj (_t__tlittyped0, int1)
  | AAppOp (_t_stringtyped0, _t__tlittypedlist1) ->
      AAppOp (_t_stringtyped0, _t__tlittypedlist1)
and typed_subst_lit (string_x : string) f (lit_e : ('t, 't lit) typed) =
  lit_e #-> (subst_lit string_x f)
let rec map_lit (f : 't -> 's) (lit_e : 't lit) =
  match lit_e with
  | AC int0 -> AC int0
  | AVar _t_stringtyped0 -> AVar (_t_stringtyped0 #=> f)
  | ATu _t__tlittypedlist0 -> ATu _t__tlittypedlist0
  | AProj (_t__tlittyped0, int1) -> AProj (_t__tlittyped0, int1)
  | AAppOp (_t_stringtyped0, _t__tlittypedlist1) ->
      AAppOp ((_t_stringtyped0 #=> f), _t__tlittypedlist1)
and typed_map_lit (f : 't -> 's) (lit_e : ('t, 't lit) typed) =
  lit_e #-> (map_lit f)
let fv_lit_id e = fv_typed_id_to_id fv_lit e
let typed_fv_lit_id e = fv_typed_id_to_id typed_fv_lit e
let subst_lit_instance x instance e =
  subst_f_to_instance subst_lit x instance e
let typed_subst_lit_instance x instance e =
  subst_f_to_instance typed_subst_lit x instance e
(* Generated from _lit.ml *)

