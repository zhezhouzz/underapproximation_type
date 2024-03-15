open Lang

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

let lit_is_v_eq_lit lit v =
  match lit.x with
  | AAppOp (op, [ a; b ]) when String.equal op.x "==" -> (
      match (a.x, b.x) with
      | AVar a, _ when String.equal a.x v -> Some b
      | _, AVar b when String.equal b.x v -> Some a
      | _ -> None)
  | _ -> None

let prop_is_v_eq_lit prop v =
  match prop with
  | Lit lit -> lit_is_v_eq_lit lit v
  | Iff (Lit { x = AVar var; _ }, Lit b) when String.equal var.x v -> Some b
  | Iff (Lit b, Lit { x = AVar var; _ }) when String.equal var.x v -> Some b
  | _ -> None

let smart_sigma (qv, xprop) prop =
  match qv.ty with
  | Nt.Ty_unit -> smart_add_to xprop prop
  | _ -> (
      match prop_is_v_eq_lit xprop qv.x with
      | Some lit -> subst_prop_instance qv.x lit.x prop
      | None ->
          let body = smart_add_to xprop prop in
          let fv = fv_prop body in
          if List.exists (fun y -> String.equal qv.x y.x) fv then
            Exists { qv; body }
          else body)

let smart_pi (qv, xprop) prop =
  match qv.ty with
  | Nt.Ty_unit -> smart_implies xprop prop
  | _ -> (
      match prop_is_v_eq_lit xprop qv.x with
      | Some lit -> subst_prop_instance qv.x lit.x prop
      | None ->
          let body = smart_implies xprop prop in
          let fv = fv_prop body in
          if List.exists (fun y -> String.equal qv.x y.x) fv then
            Forall { qv; body }
          else body)

let mk_prop_var_eq_c nty (id, c) =
  match c with
  | U -> mk_true
  | _ ->
      let lit =
        mk_lit_eq nty
          (mk_typed_lit_by_id id #: nty, mk_typed_lit_by_const c #: nty)
      in
      Lit lit #: nty

let mk_prop_var_eq_var nty (id, id') =
  match nty with
  | Nt.Ty_unit -> mk_true
  | _ ->
      let lit =
        mk_lit_eq nty
          (mk_typed_lit_by_id id #: nty, mk_typed_lit_by_id id' #: nty)
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

open Sugar

let mk_rty_var_eq_v (id, v) =
  match v.x with
  | VConst c -> mk_rty_var_eq_c v.ty (id, c)
  | VVar c -> mk_rty_var_eq_var v.ty (id, c.x)
  | _ -> _failatwith __FILE__ __LINE__ "die"

let union_ctys = function
  | [] -> _failatwith __FILE__ __LINE__ "die"
  | Cty { nty; phi } :: ctys ->
      if List.for_all (function Cty { nty = nty'; _ } -> Nt.eq nty nty') ctys
      then
        let phi =
          smart_or (phi :: List.map (function Cty { phi; _ } -> phi) ctys)
        in
        Cty { nty; phi }
      else _failatwith __FILE__ __LINE__ "die"

let forall_cty_to_prop = function
  | { x; ty = Cty { nty; phi } }, prop ->
      let x = x #: nty in
      let phi_x = subst_prop_instance default_v (AVar x) phi in
      smart_pi (x, phi_x) prop

let exists_cty_to_prop = function
  | { x; ty = Cty { nty; phi } }, prop ->
      let x = x #: nty in
      let phi_x = subst_prop_instance default_v (AVar x) phi in
      smart_sigma (x, phi_x) prop

let forall_cty_to_cty = function
  | x, Cty { nty; phi } -> Cty { nty; phi = forall_cty_to_prop (x, phi) }

let exists_cty_to_cty = function
  | x, Cty { nty; phi } -> Cty { nty; phi = exists_cty_to_prop (x, phi) }

let and_cty_to_cty = function
  | Cty { phi = phi_x; _ }, Cty { nty; phi } ->
      Cty { nty; phi = smart_add_to phi_x phi }

(* Rty *)

let map_in_retrty (f : 't rty -> 't rty) t =
  let rec aux t =
    match t with
    | RtyBase _ -> f t
    | RtyTuple ts -> RtyTuple (List.map aux ts)
    | RtyBaseArr { argcty; arg; retty } ->
        RtyBaseArr { argcty; arg; retty = aux retty }
    | RtyBaseDepPair { argcty; arg; retty } ->
        RtyBaseDepPair { argcty; arg; retty = aux retty }
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
    | RtyBaseDepPair { argcty; arg; retty } ->
        RtyBaseDepPair { argcty; arg; retty = aux retty }
    | RtyArrArr { argrty; retty } -> RtyArrArr { argrty; retty = aux retty }
  in
  aux t

let map_prop_in_retrty (f : 't prop -> 't prop) t =
  map_base_in_retrty (function Cty { nty; phi } -> Cty { nty; phi = f phi }) t

let union_rtys = function
  | [] -> _failatwith __FILE__ __LINE__ "die"
  | _ as rtys ->
      let ctys =
        List.map
          (function
            | RtyBase { ou = false; cty } -> cty
            | _ -> _failatwith __FILE__ __LINE__ "die")
          rtys
      in
      RtyBase { ou = false; cty = union_ctys ctys }

let exists_rty_to_cty (x, cty') =
  match x.ty with
  | RtyBase { ou = false; cty } -> exists_cty_to_cty (x.x #: cty, cty')
  | RtyArrArr _ | RtyBaseArr _ -> cty'
  | _ ->
      let () = Printf.printf "Fatal Error: %s:%s\n" x.x (layout_rty x.ty) in
      _failatwith __FILE__ __LINE__ "die"

let exists_cty_to_rty = function
  | x, RtyBase { ou = false; cty = cty' } -> exists_cty_to_cty (x, cty')
  | _ -> _failatwith __FILE__ __LINE__ "die"

let exists_rty_to_rty = function
  | x, RtyBase { ou = false; cty } ->
      RtyBase { ou = false; cty = exists_rty_to_cty (x, cty) }
  | _ -> _failatwith __FILE__ __LINE__ "die"

let exists_rtys_to_rty bindings rty =
  List.fold_right (fun x res_ty -> exists_rty_to_rty (x, res_ty)) bindings rty

let and_cty_to_rty cty1 = function
  | RtyBase { ou = false; cty } ->
      RtyBase { ou = false; cty = and_cty_to_cty (cty1, cty) }
  | _ -> _failatwith __FILE__ __LINE__ "die"

type t = Nt.t

(* uctx *)

module LinearRtyCtx = struct
  type linear_label = Available | Used | Persistent

  type lrctx = {
    builtin_ctx : t rty ctx;
    local_ctx : (linear_label * t rty) ctx;
    axioms : t prop list;
  }

  open Zzdatatype.Datatype

  let layout_linear_label = function
    | Available -> "❲1❳"
    | Used -> "❲0❳"
    | Persistent -> "❲∞❳"

  let playout_under_subtyping ctx (r1, r2) =
    To_typectx.playout_subtyping
      (To_typectx.layout_typectx layout_rty ctx)
      (layout_rty r1, layout_rty r2)

  let pprint_typectx x =
    Env.show_debug_typing (fun _ ->
        To_typectx.pprint_typectx layout_rty x;
        print_newline ())

  let pprint_linear_typectx x =
    Env.show_debug_typing (fun _ ->
        To_typectx.pprint_typectx
          (fun (label, rty) ->
            spf "%s%s" (layout_linear_label label) (layout_rty rty))
          x;
        print_newline ())

  let pprint_typectx_infer ctx (e, (r : t rty)) =
    Env.show_debug_typing (fun _ ->
        let () = Pp.printf "@{<bold>Type Infer:@}\n" in
        ctx ();
        Pp.printf "⊢ @{<hi_magenta>%s@} ⇨ " (short_str 100 e);
        Pp.printf "@{<cyan>%s@}\n\n" @@ layout_rty r)

  let pprint_typectx_judge ctx (e, (r : t rty)) =
    Env.show_debug_typing (fun _ ->
        let () = Pp.printf "@{<bold>Type Check:@}\n" in
        ctx ();
        Pp.printf "⊢ @{<hi_magenta>%s@} ⇦ " (short_str 10000 e);
        Pp.printf "@{<cyan>%s@}\n\n" @@ layout_rty r)

  let pprint_typectx_app_judge fname ctx (args, r) =
    Env.show_debug_typing (fun _ ->
        let () = Pp.printf "@{<bold>Application Type Check (%s):@}\n" fname in
        ctx ();
        Pp.printf "⊢ @{<hi_magenta>%s → ? @} ⇦ "
          (List.split_by " → "
             (fun (x, ty) -> spf "%s:%s" x (layout_rty ty))
             args);
        Pp.printf "@{<cyan>%s@}\n\n" @@ layout_rty r)

  let pprint_typectx_subtyping ctx (r1, r2) =
    Env.show_debug_typing (fun _ ->
        let () = Pp.printf "@{<bold>Subtyping Check:@}\n" in
        ctx ();
        Pp.printf "⊢ @{<hi_magenta>%s@} <: @{<cyan>%s@}\n\n" (layout_rty r1)
          (layout_rty r2))

  let pprint_typectx_nonempty ctx r1 =
    Env.show_debug_typing (fun _ ->
        let () = Pp.printf "@{<bold>None-mptyness Check:@}\n" in
        ctx ();
        Pp.printf "⊢ @{<hi_magenta>%s@} is not empty\n\n" (layout_rty r1))

  let linear_rctx_to_rctx = function
    | Typectx l -> Typectx (List.map (fun { x; ty = _, ty } -> { x; ty }) l)

  let pprint_simple_typectx_judge ctx (e, rty) =
    pprint_typectx_judge (fun () -> pprint_linear_typectx ctx.local_ctx) (e, rty)

  let pprint_simple_typectx_infer ctx (e, rty) =
    pprint_typectx_infer (fun () -> pprint_linear_typectx ctx.local_ctx) (e, rty)

  let add_to_right_label { builtin_ctx; local_ctx; axioms } x =
    { builtin_ctx; local_ctx = add_to_right local_ctx x; axioms }

  let add_to_right { builtin_ctx; local_ctx; axioms } x =
    let x =
      match x.ty with
      | RtyBaseDepPair _ -> { x = x.x; ty = (Available, x.ty) }
      | _ -> { x = x.x; ty = (Persistent, x.ty) }
    in
    { builtin_ctx; local_ctx = add_to_right local_ctx x; axioms }

  let add_to_rights lrctx l = List.fold_left add_to_right lrctx l

  let get_opt { builtin_ctx; local_ctx; _ } id =
    match get_opt local_ctx id with
    | None -> get_opt builtin_ctx id
    | Some (Used, _) -> _failatwith __FILE__ __LINE__ "Warning: used for twice"
    | Some (_, res) -> Some res

  let consume lrctx id =
    match lrctx.local_ctx with
    | Typectx l ->
        let counter = ref 0 in
        let l =
          List.map
            (function
              | { x; ty = Available, rty } when String.equal x id -> (
                  match rty with
                  | RtyBaseDepPair _ ->
                      counter := !counter + 1;
                      { x; ty = (Used, rty) }
                  | _ -> _failatwith __FILE__ __LINE__ "Warning!")
              | x -> x)
            l
        in
        if !counter == 1 then { lrctx with local_ctx = Typectx l }
        else _failatwith __FILE__ __LINE__ "die!"

  let lrctx_to_cctx pctx =
    let rec aux (pctx : (linear_label * t rty, string) typed list) uqvs =
      match List.last_destruct_opt pctx with
      | None -> uqvs
      | Some (pctx, binding) -> (
          match snd binding.ty with
          | RtyTuple _ -> _failatwith __FILE__ __LINE__ "unimp"
          | RtyBaseDepPair _ -> _failatwith __FILE__ __LINE__ "unimp"
          | RtyBaseArr _ | RtyArrArr _ -> aux pctx uqvs
          | RtyBase { ou; cty } ->
              let qt = ou_to_qt ou in
              let x = (qt, binding.x) #: cty in
              aux pctx (x :: uqvs))
    in
    match pctx with Typectx pctx -> aux pctx []

  let lrctx_to_base_tvars uctx =
    match uctx.local_ctx with
    | Typectx l ->
        List.filter_map
          (fun x ->
            match snd x.ty with
            | RtyBase { ou = true; cty } -> Some x.x #: (erase_cty cty)
            | _ -> None)
          l
end

include LinearRtyCtx
