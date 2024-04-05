open Syntax
open Frontend_opt

let some ty = Some ty
let layout_constant = To_constant.layout_constant
let layout_constants = To_constant.layout_constants
let layout_op = To_op.layout_op

let layout_typed_lit e =
  To_lit.layout_typed_lit (map_lit some e.x) #: (some e.ty)

let layout_lit e = To_lit.layout @@ map_lit some e
let layout_prop prop = To_prop.layout_prop @@ map_prop some prop
let layout_prop_to_coq prop = To_prop.layout_prop_to_coq @@ map_prop some prop

let layout_prop_to_smtlib2 prop =
  To_prop.layout_to_smtlib2 @@ map_prop some prop

let layout_cty cty = To_cty.layout_cty @@ map_cty some cty
let layout_rty rty = To_rty.layout_rty @@ map_rty some rty
let layout_raw_term e = To_raw_term.layout_raw_term @@ map_raw_term some e

let layout_typed_raw_term e =
  To_raw_term.layout_typed_raw_term (map_raw_term some e.x) #: (some e.ty)

let layout_item item = To_item.layout_item @@ map_item some item
let layout_structure s = To_item.layout_structure @@ List.map (map_item some) s

let layout_typed_term e =
  let e = Anf_to_raw_term.denormalize_term e in
  layout_typed_raw_term e

let layout_typed_value e =
  let e = Anf_to_raw_term.denormalize_value e in
  layout_typed_raw_term e

let layout_item item = layout_item @@ Anf_to_raw_term.denormalize_item item

let layout_structure s =
  layout_structure @@ Anf_to_raw_term.denormalize_structure s

(* Lit *)

let mk_typed_lit_by_id id = (AVar id) #: id.ty
let mk_typed_lit_by_const c = (AC c.x) #: c.ty

let mk_lit_eq nty (lit1, lit2) =
  let op = "==" #: (Nt.construct_arr_tp ([ nty; nty ], Nt.bool_ty)) in
  AAppOp (op, [ lit1; lit2 ])

let lit_get_mp = function
  | AAppOp (op, _) when not (is_builtin_op op.x) -> Some op.x
  | _ -> None

let typed_lit_get_mp lit = lit_get_mp lit.x

(* Prop *)
let get_cbool prop =
  match prop with Lit { x = AC (B b); _ } -> Some b | _ -> None

let mk_true = Lit (AC (B true)) #: Nt.bool_ty
let mk_false = Lit (AC (B false)) #: Nt.bool_ty
let is_true p = match get_cbool p with Some true -> true | _ -> false
let is_false p = match get_cbool p with Some false -> true | _ -> false

let eq_prop p1 p2 =
  Sexplib.Sexp.equal
    (sexp_of_prop Nt.sexp_of_t p1)
    (sexp_of_prop Nt.sexp_of_t p2)
