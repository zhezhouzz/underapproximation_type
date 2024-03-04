include Frontendz
include Mtyped
include Constant
include Op
include Lit
include Prop
include Cty
include Rty
include Typectx
include Raw_term
include Term
include Constructor_declaration
include Item

module FrontendRaw = struct
  let layout_constant = To_constant.layout_constant
  let layout_constants = To_constant.layout_constants
  let layout_op = To_op.layout_op
  let layout_typed_lit = To_lit.layout_typed_lit
  let layout_lit = To_lit.layout
  let layout_prop = To_prop.layout_prop
  let layout_cty = To_cty.layout_cty
  let layout_rty = To_rty.layout_rty
  let layout_raw_term = To_raw_term.layout_raw_term
  let layout_typed_raw_term = To_raw_term.layout_typed_raw_term
  let layout_item = To_item.layout_item
  let layout_structure = To_item.layout_structure

  (* let layout_typed_term e = *)
  (*   let e = Anf_to_raw_term.denormalize_term e in *)
  (*   let e = (map_raw_term (fun t -> Some t) e.x) #: (Some e.ty) in *)
  (*   To_raw_term.layout_typed_raw_term e *)

  (* let layout_item item = *)
  (*   layout_item *)
  (*   @@ map_item (fun t -> Some t) *)
  (*   @@ Anf_to_raw_term.denormalize_item item *)

  (* let layout_structure s = *)
  (*   layout_structure *)
  (*   @@ List.map (map_item (fun t -> Some t)) *)
  (*   @@ Anf_to_raw_term.denormalize_structure s *)
end

module FrontendTyped = struct
  let some ty = Some ty
  let layout_constant = To_constant.layout_constant
  let layout_constants = To_constant.layout_constants
  let layout_op = To_op.layout_op

  let layout_typed_lit e =
    To_lit.layout_typed_lit (map_lit some e.x) #: (some e.ty)

  let layout_lit e = To_lit.layout @@ map_lit some e
  let layout_prop prop = To_prop.layout_prop @@ map_prop some prop
  let layout_cty cty = To_cty.layout_cty @@ map_cty some cty
  let layout_rty rty = To_rty.layout_rty @@ map_rty some rty
  let layout_raw_term e = To_raw_term.layout_raw_term @@ map_raw_term some e

  let layout_typed_raw_term e =
    To_raw_term.layout_typed_raw_term (map_raw_term some e.x) #: (some e.ty)

  let layout_item item = To_item.layout_item @@ map_item some item

  let layout_structure s =
    To_item.layout_structure @@ List.map (map_item some) s

  let layout_typed_term e =
    let e = Anf_to_raw_term.denormalize_term e in
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

  (* Prop *)
  let get_cbool prop =
    match prop with Lit { x = AC (Constant.B b); _ } -> Some b | _ -> None

  let mk_true = Lit (AC (B true)) #: Nt.bool_ty
  let mk_false = Lit (AC (B false)) #: Nt.bool_ty
  let is_true p = match get_cbool p with Some true -> true | _ -> false
  let is_false p = match get_cbool p with Some false -> true | _ -> false

  let smart_and l =
    if List.exists is_false l then mk_false
    else
      match List.filter (fun p -> not (is_true p)) l with
      | [] -> mk_true
      | [ x ] -> x
      | l -> And l

  let smart_or l =
    if List.exists is_true l then mk_true
    else
      match List.filter (fun p -> not (is_false p)) l with
      | [] -> mk_false
      | [ x ] -> x
      | l -> Or l

  let smart_add_to a prop =
    match get_cbool a with
    | Some true -> prop
    | Some false -> mk_false
    | None -> (
        match prop with
        | And props -> smart_and (a :: props)
        | _ -> smart_and [ a; prop ])

  let smart_implies a prop =
    match get_cbool a with
    | Some true -> prop
    | Some false -> mk_true
    | None -> Implies (a, prop)

  let smart_sigma (qv, xprop) prop =
    match qv.ty with
    | Nt.Ty_unit -> smart_add_to xprop prop
    | _ -> Exists { qv; body = smart_add_to xprop prop }

  let smart_pi (qv, xprop) prop =
    match qv.ty with
    | Nt.Ty_unit -> smart_add_to xprop prop
    | _ -> Forall { qv; body = smart_add_to xprop prop }

  let mk_prop_var_eq_c nty (id, c) =
    let lit =
      mk_lit_eq nty
        (mk_typed_lit_by_id id #: nty, mk_typed_lit_by_const c #: nty)
    in
    Lit lit #: nty

  let mk_prop_var_eq_var nty (id, id') =
    let lit =
      mk_lit_eq nty (mk_typed_lit_by_id id #: nty, mk_typed_lit_by_id id' #: nty)
    in
    Lit lit #: nty

  (* Cty *)
  let prop_to_cty nty prop = Cty { nty; phi = prop }
  let prop_to_rty ou nty prop = RtyBase { ou; cty = prop_to_cty nty prop }

  let mk_cty_var_eq_c nty (id, c) =
    Cty { nty; phi = mk_prop_var_eq_c nty (id, c) }

  let mk_rty_var_eq_c nty (id, c) =
    RtyBase { ou = false; cty = mk_cty_var_eq_c nty (id, c) }

  let mk_cty_var_eq_var nty (id, c) =
    Cty { nty; phi = mk_prop_var_eq_var nty (id, c) }

  let mk_rty_var_eq_var nty (id, c) =
    RtyBase { ou = false; cty = mk_cty_var_eq_var nty (id, c) }

  (* Rty *)

  let map_in_retrty (f : 't rty -> 't rty) t =
    let rec aux t =
      match t with
      | RtyBase _ -> f t
      | RtyTuple ts -> RtyTuple (List.map aux ts)
      | RtyBaseArr { argcty; arg; retty } ->
          RtyBaseArr { argcty; arg; retty = aux retty }
      | RtyArrArr { argrty; retty } -> RtyArrArr { argrty; retty = aux retty }
    in
    aux t

  let map_base_in_retrty (f : 't cty -> 't cty) t =
    let rec aux t =
      match t with
      | RtyBase { ou; cty } -> RtyBase { ou; cty = f cty }
      | RtyTuple ts -> RtyTuple (List.map aux ts)
      | RtyBaseArr { argcty; arg; retty } ->
          RtyBaseArr { argcty; arg; retty = aux retty }
      | RtyArrArr { argrty; retty } -> RtyArrArr { argrty; retty = aux retty }
    in
    aux t

  let map_prop_in_retrty (f : 't prop -> 't prop) t =
    map_base_in_retrty
      (function Cty { nty; phi } -> Cty { nty; phi = f phi })
      t
end

(* module Typedec = struct *)
(*   include Frontendu.Typedec *)
(*   include Typedec *)
(* end *)

(* module Struc = struct *)
(*   include Frontendu.Structure *)
(*   include Struc *)

(*   let prog_of_ocamlstruct = Frontendu.Structure.client_of_ocamlstruct *)
(*   let mps_of_ocamlstruct = Frontendu.Structure.mps_of_ocamlstruct_one *)
(* end *)

(* module NL = struct *)
(*   include NL *)

(*   let layout x = Frontendu.Expr.layout @@ Trans.nan_to_term x *)
(*   let layout_value v = layout { x = V v; ty = v.ty } *)
(*   let layout_id x = layout_value { x = Var x; ty = x.ty } *)
(* end *)

(* module StrucNA = struct *)
(*   include StrucNA *)

(*   let prog_of_ocamlstruct = Frontendu.Structure.client_of_ocamlstruct *)
(*   let layout code = Struc.layout @@ Trans.struc_nan_to_term code *)
(* end *)

(* module OT = struct *)
(*   include Frontendu.Overty *)
(*   include OT *)
(* end *)

(* module UL = struct *)
(*   include UL *)

(*   let typed_map f { ty; x } = { ty; x = f x } *)
(*   let to_ntyped NNtyped.{ x; ty } = Ntyped.{ x; ty = snd ty } *)

(*   let get_args_return_name retname body = *)
(*     let open Anormal.NormalAnormal in *)
(*     let rec aux body = *)
(*       match body.x with *)
(*       | V { x = Lam { lamarg; lambody }; _ } -> *)
(*           let args, retv = aux lambody in *)
(*           (to_ntyped lamarg :: args, retv) *)
(*       | _ -> ([], to_ntyped { x = retname; ty = body.ty }) *)
(*     in *)
(*     aux body *)
(* end *)

(* module NT = Nt *)
