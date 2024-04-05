open Syntax
open Typedlang
open Propfunc
open Ctyfunc
open Zzdatatype.Datatype
open Sugar

type t = Nt.t

let map_in_retrty (f : 't rty -> 't rty) t =
  let rec aux t =
    match t with
    | RtyBase _ -> f t
    | RtyInter (rty1, rty2) -> RtyInter (aux rty1, aux rty2)
    | RtyBaseArr { argcty; arg; retty } ->
        RtyBaseArr { argcty; arg; retty = aux retty }
    | RtyBaseDepPair { argcty; arg; retty } ->
        RtyBaseDepPair { argcty; arg; retty = aux retty }
    | RtyGhostArr { argnty; arg; retty } ->
        RtyGhostArr { argnty; arg; retty = aux retty }
    | RtyArrArr { argrty; retty } -> RtyArrArr { argrty; retty = aux retty }
  in
  aux t

let map_base_in_retrty (f : 't cty -> 't cty) t =
  let rec aux t =
    match t with
    | RtyBase { ou; cty } -> RtyBase { ou; cty = f cty }
    | RtyInter (rty1, rty2) -> RtyInter (aux rty1, aux rty2)
    | RtyBaseArr { argcty; arg; retty } ->
        RtyBaseArr { argcty; arg; retty = aux retty }
    | RtyBaseDepPair { argcty; arg; retty } ->
        RtyBaseDepPair { argcty; arg; retty = aux retty }
    | RtyGhostArr { argnty; arg; retty } ->
        RtyGhostArr { argnty; arg; retty = aux retty }
    | RtyArrArr { argrty; retty } -> RtyArrArr { argrty; retty = aux retty }
  in
  aux t

let map_prop_in_retrty (f : 't prop -> 't prop) t =
  map_base_in_retrty (function Cty { nty; phi } -> Cty { nty; phi = f phi }) t

let _desugar_rty_ret_under rty =
  let rec aux (res : t rty -> t rty) = function
    | RtyBase { ou = Fa; cty } -> res (RtyBase { ou = Fa; cty })
    | RtyBase { ou = Ex; cty = Cty { nty; phi } } ->
        let default_res = Rename.unique default_res in
        let phi =
          smart_implies
            (subst_prop_instance default_v (AVar default_res #: nty) phi)
            (mk_prop_var_eq_var nty (default_v, default_res))
        in
        let retty = res (RtyBase { ou = Fa; cty = Cty { nty; phi } }) in
        RtyGhostArr { argnty = nty; arg = default_res; retty }
    | RtyBaseArr { argcty; arg; retty } ->
        aux (fun retty -> res (RtyBaseArr { argcty; arg; retty })) retty
    | RtyBaseDepPair { argcty; arg; retty } ->
        aux (fun retty -> res (RtyBaseDepPair { argcty; arg; retty })) retty
    | RtyArrArr { argrty; retty } ->
        aux (fun retty -> res (RtyArrArr { argrty; retty })) retty
    | RtyInter _trtylist0 -> _failatwith __FILE__ __LINE__ "unimp"
    | RtyGhostArr { argnty; arg; retty } ->
        RtyGhostArr { argnty; arg; retty = aux res retty }
  in
  aux (fun rty -> rty) rty

let desugar_rty_ret_under rty =
  match erase_rty rty with
  | Nt.Ty_arrow _ -> _desugar_rty_ret_under rty
  | _ -> rty

let intersect_rtys = function
  | [] -> _failatwith __FILE__ __LINE__ "die"
  | [ rty ] -> rty
  | rtys ->
      let len = List.length rtys in
      let len_lit = (AC (I len)) #: Nt.Ty_int in
      (* let v_lit = (AVar default_v #: Nt.Ty_int) #: Nt.Ty_int in *)
      let arg = Rename.unique default_res in
      let arg_lit = (AVar arg #: Nt.Ty_int) #: Nt.Ty_int in
      let phi =
        List.fold_left apply_pi_prop
          (Env.get_statements_by_name "template_forall_n_v_in_0_to_n")
          [ len_lit; arg_lit ]
      in
      let rtys = List.map _desugar_rty_ret_under rtys in
      let gvars, ctys =
        List.fold_left
          (fun (gvars, ctys) rty ->
            let gvars', rty = extract_ghost_vars rty in
            match rty with
            | RtyBase { ou = Fa; cty } -> (gvars @ gvars', ctys @ [ cty ])
            | _ -> _failatwith __FILE__ __LINE__ "die")
          ([], []) rtys
      in
      let ctys =
        List.mapi
          (fun i cty ->
            let i_lit = (AC (I i)) #: Nt.Ty_int in
            let prop' =
              List.fold_left apply_pi_prop
                (Env.get_statements_by_name "forall_a_i_a_eq_i")
                [ arg_lit; i_lit ]
            in
            map_phi_in_cty (smart_add_to prop') cty)
          ctys
      in
      let retty =
        RtyBase
          {
            ou = Fa;
            cty = map_phi_in_cty (smart_implies phi) @@ union_ctys ctys;
          }
      in
      let gvars = (arg #: Nt.Ty_int) :: gvars in
      construct_ghost_vars gvars retty

let rec pack_rty_to_rty = function
  | x, RtyGhostArr { argnty; arg; retty } ->
      RtyGhostArr { argnty; arg; retty = pack_rty_to_rty (x, retty) }
  | x, RtyBase { ou = Fa; cty } -> (
      match erase_rty x.ty with
      | Nt.Ty_arrow _ -> RtyBase { ou = Fa; cty }
      | _ -> (
          match x.ty with
          | RtyBase { ou = Fa; cty = cty_x } ->
              RtyBase { ou = Fa; cty = exists_cty_to_cty (x.x #: cty_x, cty) }
          | RtyBase { ou = Ex; cty = Cty { nty; phi } } ->
              let phi = subst_prop_instance default_v (AVar x.x #: nty) phi in
              RtyGhostArr
                {
                  argnty = nty;
                  arg = x.x;
                  retty =
                    RtyBase
                      { ou = Fa; cty = map_phi_in_cty (smart_implies phi) cty };
                }
          | _ ->
              let () =
                Printf.printf "Fatal Error: %s:%s\n" x.x (layout_rty x.ty)
              in
              _failatwith __FILE__ __LINE__ "die"))
  | _ -> _failatwith __FILE__ __LINE__ "die"

let pack_rtys_to_rty bindings rty =
  List.fold_right (fun x res_ty -> pack_rty_to_rty (x, res_ty)) bindings rty

let and_cty_to_rty cty1 = function
  | RtyBase { ou; cty } -> RtyBase { ou; cty = and_cty_to_cty (cty1, cty) }
  | _ -> _failatwith __FILE__ __LINE__ "die"
