include Frontend_opt
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
  let layout_prop_to_coq prop = To_prop.layout_prop_to_coq @@ map_prop some prop

  let layout_prop_to_smtlib2 prop =
    To_prop.layout_to_smtlib2 @@ map_prop some prop

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

  let layout_typed_value e =
    let e = Anf_to_raw_term.denormalize_value e in
    layout_typed_raw_term e

  let layout_item item = layout_item @@ Anf_to_raw_term.denormalize_item item

  let layout_structure s =
    layout_structure @@ Anf_to_raw_term.denormalize_structure s

  let layout_id_rty x = x.x ^ ":" ^ layout_rty x.ty

  (* Lit *)

  let mk_typed_lit_by_id id = (AVar id) #: id.ty
  let mk_typed_lit_by_const c = (AC c.x) #: c.ty

  let mk_lit_eq nty (lit1, lit2) =
    let op = "==" #: (Nt.construct_arr_tp ([ nty; nty ], Nt.bool_ty)) in
    AAppOp (op, [ lit1; lit2 ])

  let lit_get_mp = function
    | AAppOp (op, _) when not (Op.is_builtin_op op.x) -> Some op.x
    | _ -> None

  let typed_lit_get_mp lit = lit_get_mp lit.x

  (* Prop *)
  let get_cbool prop =
    match prop with Lit { x = AC (Constant.B b); _ } -> Some b | _ -> None

  let mk_true = Lit (AC (B true)) #: Nt.bool_ty
  let mk_false = Lit (AC (B false)) #: Nt.bool_ty
  let is_true p = match get_cbool p with Some true -> true | _ -> false
  let is_false p = match get_cbool p with Some false -> true | _ -> false

  let eq_prop p1 p2 =
    Sexplib.Sexp.equal
      (sexp_of_prop Nt.sexp_of_t p1)
      (sexp_of_prop Nt.sexp_of_t p2)

  open Zzdatatype.Datatype

  let unfold_and prop =
    let rec aux = function
      | [] -> []
      | And l :: l' -> aux (l @ l')
      | prop :: l' -> prop :: aux l'
    in
    let l = aux prop in
    List.slow_rm_dup eq_prop l

  let smart_and l =
    let l = unfold_and l in
    if List.exists is_false l then mk_false
    else
      match List.filter (fun p -> not (is_true p)) l with
      | [] -> mk_true
      | [ x ] -> x
      | l -> And l

  let unfold_or prop =
    let rec aux = function
      | [] -> []
      | Or l :: l' -> aux (l @ l')
      | prop :: l' -> prop :: aux l'
    in
    let l = aux prop in
    List.slow_rm_dup eq_prop l

  let smart_or l =
    let l = unfold_or l in
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

  let prop_get_mp prop =
    let rec aux prop_e =
      match prop_e with
      | Lit lit -> (
          match typed_lit_get_mp lit with Some mp -> [ mp ] | None -> [])
      | Implies (p1, p2) -> aux p1 @ aux p2
      | Ite (p1, p2, p3) -> aux p1 @ aux p2 @ aux p3
      | Not p1 -> aux p1
      | And ps -> List.concat (List.map aux ps)
      | Or ps -> List.concat (List.map aux ps)
      | Iff (p1, p2) -> aux p1 @ aux p2
      | Forall { body; _ } -> aux body
      | Exists { body; _ } -> aux body
    in
    List.slow_rm_dup String.equal @@ aux prop

  open Sugar

  type t = Nt.t

  let apply_pi_prop (p : t prop) (lit : (t, t lit) typed) =
    match p with
    | Forall { qv; body } ->
        if Nt.eq qv.ty lit.ty then subst_prop_instance qv.x lit.x body
        else _failatwith __FILE__ __LINE__ "die"
    | _ -> _failatwith __FILE__ __LINE__ "die"

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

  let mk_rty_var_eq_v (id, v) =
    match v.x with
    | VConst c -> mk_rty_var_eq_c v.ty (id, c)
    | VVar c -> mk_rty_var_eq_var v.ty (id, c.x)
    | _ -> _failatwith __FILE__ __LINE__ "die"

  let n_to_one_ctys prop_f = function
    | [] -> _failatwith __FILE__ __LINE__ "die"
    | Cty { nty; phi } :: ctys ->
        if
          List.for_all (function Cty { nty = nty'; _ } -> Nt.eq nty nty') ctys
        then
          let phi =
            prop_f (phi :: List.map (function Cty { phi; _ } -> phi) ctys)
          in
          Cty { nty; phi }
        else _failatwith __FILE__ __LINE__ "die"

  let union_ctys = n_to_one_ctys smart_or
  let intersect_ctys = n_to_one_ctys smart_and

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

  let map_typed_term (f : 't -> 'r) (t : ('t, 't term) typed) :
      ('r, 'r term) typed =
    { x = map_term f t.x; ty = f t.ty }

  let map_typed_value (f : 't -> 'r) (v : ('t, 't value) typed) :
      ('r, 'r value) typed =
    { x = map_value f v.x; ty = f v.ty }

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

  (* typectx *)

  (* let filter_out_builtin_typing = function *)
  (*   | Typectx uqvs -> *)
  (*       let uqvs = *)
  (*         List.filter *)
  (*           (fun x -> *)
  (*             match x.x with *)
  (*             | "dummy" -> false (\* NOTE: for synthesizer *\) *)
  (*             | x when is_dt_op x -> false *)
  (*             | _ -> true) *)
  (*           uqvs *)
  (*       in *)
  (*       Typectx uqvs *)

  open Zzdatatype.Datatype

  let playout_under_subtyping ctx (r1, r2) =
    To_typectx.playout_subtyping
      (To_typectx.layout_typectx layout_rty ctx)
      (layout_rty r1, layout_rty r2)

  let pprint_typectx x =
    Env.show_debug_typing (fun _ ->
        To_typectx.pprint_typectx layout_rty x;
        print_newline ())

  let pprint_typectx_infer ctx (e, (r : t rty)) =
    Env.show_debug_typing (fun _ ->
        let () = Pp.printf "@{<bold>Type Infer:@}\n" in
        pprint_typectx ctx;
        Pp.printf "⊢ @{<hi_magenta>%s@} ⇨ " (short_str 100 e);
        Pp.printf "@{<cyan>%s@}\n\n" @@ layout_rty r)

  let pprint_typectx_judge ctx (e, (r : t rty)) =
    Env.show_debug_typing (fun _ ->
        let () = Pp.printf "@{<bold>Type Check:@}\n" in
        pprint_typectx ctx;
        Pp.printf "⊢ @{<hi_magenta>%s@} ⇦ " (short_str 10000 e);
        Pp.printf "@{<cyan>%s@}\n\n" @@ layout_rty r)

  let pprint_typectx_app_judge fname ctx (args, r) =
    Env.show_debug_typing (fun _ ->
        let () = Pp.printf "@{<bold>Application Type Check (%s):@}\n" fname in
        pprint_typectx ctx;
        Pp.printf "⊢ @{<hi_magenta>%s → ? @} ⇦ "
          (List.split_by " → "
             (fun (x, ty) -> spf "%s:%s" x (layout_rty ty))
             args);
        Pp.printf "@{<cyan>%s@}\n\n" @@ layout_rty r)

  let pprint_typectx_subtyping ctx (r1, r2) =
    Env.show_debug_typing (fun _ ->
        let () = Pp.printf "@{<bold>Subtyping Check:@}\n" in
        pprint_typectx ctx;
        Pp.printf "⊢ @{<hi_magenta>%s@} <: @{<cyan>%s@}\n\n" (layout_rty r1)
          (layout_rty r2))

  let pprint_typectx_nonempty ctx r1 =
    Env.show_debug_typing (fun _ ->
        let () = Pp.printf "@{<bold>None-mptyness Check:@}\n" in
        pprint_typectx ctx;
        Pp.printf "⊢ @{<hi_magenta>%s@} is not empty\n\n" (layout_rty r1))

  (* let pprint_typectx_q uqvs eqvs pre body = *)
  (*   Env.show_debug_queries (fun _ -> *)
  (*       let () = Pp.printf "@{<bold>Query:@}\n" in *)
  (*        Quantified.print_qt_ uqvs eqvs; *)
  (*       Pp.printf "\n@{<cyan>%s@} @{<bold>=>@}\n@{<hi_magenta>%s@}\n" *)
  (*         (Autov.playout_prop pre) (Autov.playout_prop body)) *)
  (* (Autov.coq_layout_prop pre) *)
  (* (Autov.coq_layout_prop body) *)

  (* uctx *)

  type uctx = {
    builtin_ctx : t rty ctx;
    local_ctx : t rty ctx;
    axioms : t prop list;
  }

  let pprint_simple_typectx_judge ctx (e, rty) =
    (* pprint_typectx_judge (filter_out_builtin_typing ctx) (e, rty) *)
    pprint_typectx_judge ctx.local_ctx (e, rty)

  let pprint_simple_typectx_infer ctx (e, rty) =
    (* pprint_typectx_infer (filter_out_builtin_typing ctx) (e, rty) *)
    pprint_typectx_infer ctx.local_ctx (e, rty)

  let add_to_right { builtin_ctx; local_ctx; axioms } x =
    { builtin_ctx; local_ctx = add_to_right local_ctx x; axioms }

  let add_to_rights { builtin_ctx; local_ctx; axioms } x =
    { builtin_ctx; local_ctx = add_to_rights local_ctx x; axioms }

  let get_opt { builtin_ctx; local_ctx; _ } id =
    match get_opt local_ctx id with
    | None -> get_opt builtin_ctx id
    | Some res -> Some res
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
