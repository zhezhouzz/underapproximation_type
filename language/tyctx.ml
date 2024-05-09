open Syntax
open Typedlang
open Zzdatatype.Datatype

type t = Nt.t

open Sugar

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

let pprint_typectx_overlaptyping ctx (r1, r2) =
  Env.show_debug_typing (fun _ ->
      let () = Pp.printf "@{<bold>Overlaping Check:@}\n" in
      ctx ();
      Pp.printf "⊢ @{<hi_magenta>%s@} ⋐ @{<cyan>%s@}\n\n" (layout_rty r1)
        (layout_rty r2))

let pprint_typectx_nonempty ctx r1 =
  Env.show_debug_typing (fun _ ->
      let () = Pp.printf "@{<bold>None-mptyness Check:@}\n" in
      ctx ();
      Pp.printf "⊢ @{<hi_magenta>%s@} is not empty\n\n" (layout_rty r1))

let ctx_list_to_cctx pctx =
  let rec aux (pctx : (t rty, string) typed list) uqvs =
    match List.last_destruct_opt pctx with
    | None -> uqvs
    | Some (pctx, binding) -> (
        match binding.ty with
        | RtyBaseDepPair _ | RtyBaseArr _ | RtyArrArr _ -> aux pctx uqvs
        | RtyGhostArr _ -> (
            match erase_rty binding.ty with
            | Nt.Ty_arrow _ -> aux pctx uqvs
            | _ -> _failatwith __FILE__ __LINE__ "die")
        | RtyBase { ou; cty; _ } ->
            let x = (ou, binding.x) #: cty in
            aux pctx (x :: uqvs))
  in
  aux pctx []

let ctx_list_to_base_tvars l =
  List.filter_map
    (fun x ->
      match x.ty with
      | RtyBase { ou = Fa; cty; _ } -> Some x.x #: (erase_cty cty)
      | _ -> None)
    l

let update_rty_by_name (ctx : t rty ctx) name f =
  match ctx with
  | Typectx l ->
      Typectx
        (List.map
           (fun x -> if String.equal x.x name then x.x #: (f x.ty) else x)
           l)
