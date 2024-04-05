open Syntax
open Typedlang
open Frontend_opt
open Tyctx
open Zzdatatype.Datatype

type t = Nt.t

open Sugar

type linear_label = Available | Used | Persistent

type lrctx = {
  builtin_ctx : t rty ctx;
  local_ctx : (linear_label * t rty) ctx;
  axioms : t prop list;
}

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

let to_ctx_list = function
  | Typectx l -> List.map (fun { x; ty = _, ty } -> { x; ty }) l

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

let lrctx_to_cctx pctx = ctx_list_to_cctx (to_ctx_list pctx)

let lrctx_to_base_tvars uctx =
  ctx_list_to_base_tvars (to_ctx_list uctx.local_ctx)
