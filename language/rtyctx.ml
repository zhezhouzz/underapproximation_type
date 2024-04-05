open Syntax
open Frontend_opt
open Typedlang
open Tyctx
open Zzdatatype.Datatype

type t = Nt.t

(* open Sugar *)

type lrctx = {
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

let add_to_right { builtin_ctx; local_ctx; axioms } x =
  { builtin_ctx; local_ctx = add_to_right local_ctx x; axioms }

let add_to_rights lrctx l = List.fold_left add_to_right lrctx l

let get_opt { builtin_ctx; local_ctx; _ } id =
  match get_opt local_ctx id with
  | None -> get_opt builtin_ctx id
  | Some res -> Some res

let lrctx_to_cctx pctx = ctx_list_to_cctx (to_ctx_list pctx)

let lrctx_to_base_tvars uctx =
  ctx_list_to_base_tvars (to_ctx_list uctx.local_ctx)
