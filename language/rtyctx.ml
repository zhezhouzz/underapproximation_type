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

(* NOTE: pack_and_flip (x:[p1(v)], y:[p2(x, v)])([p3(y, v)]) =
   (x:{p1(v)}, y:{p2(x, v)})([exists x.p1(x) /\ exists y.p2(x, y) /\ p3(y, v)]) *)

let pack_and_flip ctx (cty : t cty) =
  let get_fv (cty : t cty) = List.map _get_x @@ fv_cty cty in
  let rec aux (ctx : (t rty, string) typed list) cty =
    let fvs = get_fv cty in
    match fvs with
    | [] -> (ctx, cty)
    | _ -> (
        match List.last_destruct_opt ctx with
        | None -> (ctx, cty)
        | Some (ctx, x) when List.exists (String.equal x.x) fvs -> (
            match x.ty with
            | RtyBase { ou = Fa; _ } ->
                let ctx, cty = aux ctx cty in
                (ctx @ [ x ], cty)
            | RtyBase { ou = Ex; cty = x_cty; er } ->
                let cty = Ctyfunc.exists_cty_to_cty (x.x #: x_cty, cty) in
                let x = x.x #: (RtyBase { ou = Fa; cty = x_cty; er }) in
                let ctx, cty = aux ctx cty in
                (ctx @ [ x ], cty)
            | _ ->
                let () = Printf.printf "%s:%s\n" x.x (layout_rty x.ty) in
                _failatwith __FILE__ __LINE__ "die")
        | Some (ctx, x) ->
            let ctx, cty = aux ctx cty in
            (ctx @ [ x ], cty))
  in
  match ctx.local_ctx with
  | Typectx local_ctx ->
      let local_ctx, cty = aux local_ctx cty in
      ({ ctx with local_ctx = Typectx local_ctx }, cty)

(* let track_original_randomness_from_cty ctx cty = *)
(*   let origins, cty = pack_and_flip ctx cty in *)
(*   (List.fold_left flip_by_name ctx origins, cty) *)

let consume_rty ctx = function
  | RtyBase { ou = Ex; cty; er } ->
      let ctx, cty = pack_and_flip ctx cty in
      (ctx, RtyBase { ou = Ex; cty; er })
  | _ -> Sugar._failatwith __FILE__ __LINE__ "die"
