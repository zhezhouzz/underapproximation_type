open Syntax
open Frontend_opt
open Typedlang
open Tyctx
(* open Zzdatatype.Datatype *)

type t = Nt.t

(* open Sugar *)

type rctx = {
  builtin_ctx : t rty ctx;
  local_ctx : t rty ctx;
  axioms : t prop list;
}

let pprint_typectx x =
  Env.show_debug_typing (fun _ ->
      To_typectx.pprint_typectx layout_rty x;
      print_newline ())

let pprint_linear_typectx x =
  Env.show_debug_typing (fun _ ->
      To_typectx.pprint_typectx layout_rty x;
      print_newline ())

let to_ctx_list = function Typectx l -> l

let pprint_simple_typectx_judge ctx (e, rty) =
  pprint_typectx_judge (fun () -> pprint_linear_typectx ctx.local_ctx) (e, rty)

let pprint_simple_typectx_infer ctx (e, rty) =
  pprint_typectx_infer (fun () -> pprint_linear_typectx ctx.local_ctx) (e, rty)

let add_to_right_label { builtin_ctx; local_ctx; axioms } x =
  { builtin_ctx; local_ctx = add_to_right local_ctx x; axioms }

let map_in_local_ctx { builtin_ctx; local_ctx; axioms } f =
  { builtin_ctx; local_ctx = f local_ctx; axioms }

let add_to_right ctx x =
  map_in_local_ctx ctx (fun local_ctx -> add_to_right local_ctx x)

let add_to_rights ctx l =
  map_in_local_ctx ctx (fun local_ctx -> add_to_rights local_ctx l)

let add_to_left ctx x =
  map_in_local_ctx ctx (fun local_ctx -> add_to_left local_ctx x)

let add_to_lefts ctx l =
  map_in_local_ctx ctx (fun local_ctx -> add_to_lefts local_ctx l)

let get_opt { builtin_ctx; local_ctx; _ } id =
  match get_opt local_ctx id with
  | None -> get_opt builtin_ctx id
  | Some res -> Some res

let rctx_to_cctx pctx = ctx_list_to_cctx (to_ctx_list pctx)

let rctx_to_base_tvars uctx =
  ctx_list_to_base_tvars (to_ctx_list uctx.local_ctx)

let update_rty_by_name ctx name f =
  map_in_local_ctx ctx (fun ctx -> update_rty_by_name ctx name f)

let flip_by_name ctx name = update_rty_by_name ctx name flip_base

open Zzdatatype.Datatype
open Sugar

let pack_under_ctx ctx (cty : t cty) =
  let get_fv (cty : t cty) = List.map _get_x @@ fv_cty cty in
  let rec aux (ctx : (t rty, string) typed list) cty =
    let fvs = get_fv cty in
    match fvs with
    | [] -> ([], cty)
    | _ -> (
        match List.last_destruct_opt ctx with
        | None -> Sugar._failatwith __FILE__ __LINE__ "die"
        | Some (ctx, x) ->
            if List.exists (String.equal x.x) fvs then
              let self_fvs = List.map _get_x @@ fv_rty x.ty in
              let cty =
                match x.ty with
                | RtyBase { ou = Ex; cty = x_cty; _ } ->
                    Ctyfunc.exists_cty_to_cty (x.x #: x_cty, cty)
                | _ ->
                    let () = Printf.printf "%s:%s\n" x.x (layout_rty x.ty) in
                    _failatwith __FILE__ __LINE__ "die"
              in
              match self_fvs with
              | [] ->
                  let res, cty = aux ctx cty in
                  (res @ [ x.x ], cty)
              | _ -> aux ctx cty
            else aux ctx cty)
  in
  match ctx.local_ctx with Typectx ctx -> aux ctx cty

(* let track_original_randomness_from_fvs ctx fvs = *)
(*   let rec aux ctx fvs = *)
(*     (\* let _ = pprint_typectx (Typectx ctx) in *\) *)
(*     (\* let _ = Printf.printf "%s\n" (StrList.to_string fvs) in *\) *)
(*     match fvs with *)
(*     | [] -> [] *)
(*     | _ -> ( *)
(*         match List.last_destruct_opt ctx with *)
(*         | None -> Sugar._failatwith __FILE__ __LINE__ "die" *)
(*         | Some (ctx, x) -> *)
(*             if List.exists (String.equal x.x) fvs then *)
(*               let fvs = List.remove_elt String.equal x.x fvs in *)
(*               let self_fvs = List.map _get_x @@ fv_rty x.ty in *)
(*               match self_fvs with *)
(*               | [] -> aux ctx fvs @ [ x.x ] *)
(*               | _ -> aux ctx (fvs @ self_fvs) *)
(*             else aux ctx fvs) *)
(*   in *)
(*   match ctx.local_ctx with Typectx ctx -> aux ctx fvs *)

let track_original_randomness_from_cty ctx cty =
  let origins, cty = pack_under_ctx ctx cty in
  (List.fold_left flip_by_name ctx origins, cty)

let consume_rty ctx = function
  | RtyBase { ou = Ex; cty; er } ->
      let ctx, cty = track_original_randomness_from_cty ctx cty in
      (ctx, RtyBase { ou = Ex; cty; er })
  | _ -> Sugar._failatwith __FILE__ __LINE__ "die"
