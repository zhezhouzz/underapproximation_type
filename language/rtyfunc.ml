open Syntax
open Typedlang
open Propfunc
open Ctyfunc
open Zzdatatype.Datatype
open Sugar

type t = Nt.t

let mk_rty_var_eq_c ou nty (id, c) =
  RtyBase { ou; cty = mk_cty_var_eq_c nty (id, c); er = mk_false }

let mk_rty_var_eq_var ou nty (id, c) =
  RtyBase { ou; cty = mk_cty_var_eq_var nty (id, c); er = mk_false }

let mk_rty_var_eq_v ou nty (id, v) =
  match v.x with
  | VConst c -> mk_rty_var_eq_c ou nty (id, c)
  | VVar c -> mk_rty_var_eq_var ou nty (id, c.x)
  | _ -> _failatwith __FILE__ __LINE__ "die"

let cty_to_rty ou cty = RtyBase { ou; cty; er = mk_false }

let prop_to_rty ou nty prop =
  RtyBase { ou; cty = prop_to_cty nty prop; er = mk_false }

let prop_to_er_rty ou nty prop =
  RtyBase { ou; cty = prop_to_cty nty mk_false; er = prop }

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
    | RtyBase { ou; cty; er } -> RtyBase { ou; cty = f cty; er }
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
    | RtyBase { ou = Fa; cty; er } -> res (RtyBase { ou = Fa; cty; er })
    | RtyBase { ou = Ex; cty = Cty { nty; phi }; er } ->
        let default_res = Rename.unique default_res in
        let phi =
          smart_implies
            (subst_prop_instance default_v (AVar default_res #: nty) phi)
            (mk_prop_var_eq_var nty (default_v, default_res))
        in
        let retty = res (RtyBase { ou = Fa; cty = Cty { nty; phi }; er }) in
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

(* let rec intersect_rtys = function *)
(*   | [] -> _failatwith __FILE__ __LINE__ "die" *)
(*   | [ rty ] -> rty *)
(*   | rty1 :: rtys' -> *)
(*       let rty2 = intersect_rtys rtys' in *)
(*       let arg = Rename.unique default_res in *)
(*       let arg_lit = (AVar arg #: Nt.Ty_bool) #: Nt.Ty_bool in *)
(*       (\* let rty1, rty2 = map2 _desugar_rty_ret_under (rty1, rty2) in *\) *)
(*       (\* let rty1, rty2 = map2 _desugar_rty_ret_under (rty1, rty2) in *\) *)
(*       (\* let destr rty = *\) *)
(*       (\*   let gvars', rty = extract_ghost_vars rty in *\) *)
(*       (\*   match rty with *\) *)
(*       (\*   | RtyBase { ou = Fa; cty; er } -> (gvars', cty, er) *\) *)
(*       (\*   | _ -> _failatwith __FILE__ __LINE__ "die" *\) *)
(*       (\* in *\) *)
(*       (\* let (gvars1, cty1, er1), (gvars2, cty2, er2) = map2 destr (rty1, rty2) in *\) *)
(*       let cty1 = map_phi_in_cty (smart_add_to (Lit arg_lit)) cty1 in *)
(*       let cty2 = map_phi_in_cty (smart_add_to (Not (Lit arg_lit))) cty2 in *)
(*       (\* let er1 = (smart_add_to (Lit arg_lit)) er1 in *\) *)
(*       (\* let er2 = (smart_add_to (Not (Lit arg_lit))) er2 in *\) *)
(*       let retty = *)
(*         RtyBase { ou = Ex; cty = union_ctys [ cty1; cty2 ]; er = mk_false } *)
(*       in *)
(*       retty *)
(* (\* let gvars = (arg #: Nt.Ty_bool) :: (gvars1 @ gvars2) in *\) *)
(* (\* construct_ghost_vars gvars retty *\) *)

let rec intersect_rtys = function
  | [] -> _failatwith __FILE__ __LINE__ "die"
  | [ rty ] -> rty
  | rty1 :: rtys' -> (
      let rty2 = intersect_rtys rtys' in
      match (rty1, rty2) with
      | RtyBase { ou = Ex; cty = cty1; _ }, RtyBase { ou = Ex; cty = cty2; _ }
        ->
          RtyBase { ou = Ex; cty = union_ctys [ cty1; cty2 ]; er = mk_false }
      | _, _ -> _failatwith __FILE__ __LINE__ "die")

(* let gvars = (arg #: Nt.Ty_bool) :: (gvars1 @ gvars2) in *)
(* construct_ghost_vars gvars retty *)

let rec pack_rty_to_rty = function
  | x, RtyGhostArr { argnty; arg; retty } ->
      RtyGhostArr { argnty; arg; retty = pack_rty_to_rty (x, retty) }
  | x, RtyBase { ou = Ex; cty; er } -> (
      match erase_rty x.ty with
      | Nt.Ty_arrow _ -> RtyBase { ou = Ex; cty; er }
      | _ -> (
          match x.ty with
          | RtyBase { ou = Ex; cty = cty_x; er = er_x } when is_false er_x ->
              RtyBase
                { ou = Ex; cty = exists_cty_to_cty (x.x #: cty_x, cty); er }
          | _ ->
              let () =
                Printf.printf "Fatal Error: %s:%s\n" x.x (layout_rty x.ty)
              in
              _failatwith __FILE__ __LINE__ "die"))
  | x, rty ->
      let () =
        Printf.printf "Ex (%s: %s).%s\n" x.x (layout_rty x.ty) (layout_rty rty)
      in
      _failatwith __FILE__ __LINE__ "die"

(* let rec pack_rty_to_rty = function *)
(*   | x, RtyGhostArr { argnty; arg; retty } -> *)
(*       RtyGhostArr { argnty; arg; retty = pack_rty_to_rty (x, retty) } *)
(*   | x, RtyBase { ou; cty; er } -> ( *)
(*       match erase_rty x.ty with *)
(*       | Nt.Ty_arrow _ -> RtyBase { ou = Fa; cty; er } *)
(*       | _ -> ( *)
(*           match x.ty with *)
(*           | RtyBase { ou = ou'; cty = cty_x; er = er_x } when is_false er_x  -> *)
(*               RtyBase *)
(*                 { ou = Fa; cty = exists_cty_to_cty (x.x #: cty_x, cty); er } *)
(*           | RtyBase { ou = Ex; cty = Cty { nty; phi }; er = er_x } *)
(*             when is_false er_x -> *)
(*               let phi = subst_prop_instance default_v (AVar x.x #: nty) phi in *)
(*               RtyGhostArr *)
(*                 { *)
(*                   argnty = nty; *)
(*                   arg = x.x; *)
(*                   retty = *)
(*                     RtyBase *)
(*                       { *)
(*                         ou = Fa; *)
(*                         cty = map_phi_in_cty (smart_implies phi) cty; *)
(*                         er; *)
(*                       }; *)
(*                 } *)
(*           | _ -> *)
(*               let () = *)
(*                 Printf.printf "Fatal Error: %s:%s\n" x.x (layout_rty x.ty) *)
(*               in *)
(*               _failatwith __FILE__ __LINE__ "die")) *)
(*   | _ -> _failatwith __FILE__ __LINE__ "die" *)

let pack_rtys_to_rty bindings rty =
  List.fold_right (fun x res_ty -> pack_rty_to_rty (x, res_ty)) bindings rty

let and_cty_to_rty cty1 = function
  | RtyBase { ou; cty; er } ->
      RtyBase { ou; cty = and_cty_to_cty (cty1, cty); er }
  | _ -> _failatwith __FILE__ __LINE__ "die"

let alpha_renaming_rty_in_scope (scope : string list) (rty : t rty) =
  let scope = List.slow_rm_dup String.equal scope in
  let renaming name =
    if List.exists (String.equal name) scope then Some (Rename.unique name)
    else None
  in
  let rec aux = function
    | RtyBase _ as rty -> rty
    | RtyGhostArr { argnty; arg; retty } -> (
        let retty = aux retty in
        match renaming arg with
        | None -> RtyGhostArr { argnty; arg; retty }
        | Some arg' ->
            RtyGhostArr
              {
                argnty;
                arg = arg';
                retty = subst_rty_instance arg (AVar arg' #: argnty) retty;
              })
    | RtyBaseArr { argcty; arg; retty } -> (
        let retty = aux retty in
        match renaming arg with
        | None -> RtyBaseArr { argcty; arg; retty }
        | Some arg' ->
            RtyBaseArr
              {
                argcty;
                arg = arg';
                retty =
                  subst_rty_instance arg (AVar arg' #: (erase_cty argcty)) retty;
              })
    | RtyBaseDepPair { argcty; arg; retty } -> (
        let retty = aux retty in
        match renaming arg with
        | None -> RtyBaseDepPair { argcty; arg; retty }
        | Some arg' ->
            RtyBaseDepPair
              {
                argcty;
                arg = arg';
                retty =
                  subst_rty_instance arg (AVar arg' #: (erase_cty argcty)) retty;
              })
    | RtyArrArr { argrty; retty } -> RtyArrArr { argrty; retty = aux retty }
    | RtyInter (rty1, rty2) -> RtyInter (aux rty1, aux rty2)
  in
  aux rty

let destruct_over_rty_to_ctys_params rty =
  let rec aux = function
    | RtyBase _ as rty -> ([], rty)
    | RtyBaseArr { argcty; arg; retty } ->
        let params, rty = aux retty in
        ((arg #: argcty) :: params, rty)
    | RtyArrArr { retty; _ } -> aux retty
    | _ -> _failatwith __FILE__ __LINE__ "die"
  in
  aux rty

let unknown = "φ"

(* let mk_unknowns rty = *)
(*   let args, rty = destruct_over_rty_to_ctys_params rty in *)
(*   let rec aux  *)
