module T = Autov.Smtty
module NT = Languages.Normalty
open Sugar
open Languages.SMTtyped
open Languages.SMTSimpleTypectx
open Zzdatatype.Datatype

let infer_id ctx name =
  (* let () = *)
  (*   Printf.printf "[infer_id] ctx: %s\n" @@ List.split_by_comma fst ctx *)
  (* in *)
  match get_opt ctx name.x with
  | None -> failwith @@ spf "free variable (%s) in refinement type" name.x
  | Some (_, ty) -> { ty; x = name.x }

let rec infer_lit ctx lit =
  let open Autov.Prop in
  match lit with
  | AVar id -> AVar (infer_id ctx id)
  | ACint _ | ACbool _ -> lit
  | AOp2 (mp, a, b) ->
      let a = infer_lit ctx a in
      let b = infer_lit ctx b in
      if T.eq (lit_get_ty a) T.Int && T.eq (lit_get_ty b) T.Int && is_op mp then
        AOp2 (mp, a, b)
      else _failatwith __FILE__ __LINE__ ""

let infer_prop ctx t =
  let open Autov.Prop in
  let rec aux ctx t =
    match t with
    | Lit lit -> Lit (infer_lit ctx lit)
    | Implies (e1, e2) -> Implies (aux ctx e1, aux ctx e2)
    | Ite (e1, e2, e3) -> Ite (aux ctx e1, aux ctx e2, aux ctx e3)
    | Not e -> Not (aux ctx e)
    | And es -> And (List.map (aux ctx) es)
    | Or es -> Or (List.map (aux ctx) es)
    | Iff (e1, e2) -> Iff (aux ctx e1, aux ctx e2)
    | MethodPred (mp, args) -> MethodPred (mp, List.map (infer_lit ctx) args)
    | Forall (u, e) ->
        let ctx = add_to_right ctx (u.ty, u.x) in
        Forall (u, aux ctx e)
    | Exists (u, e) ->
        (* let () = Printf.printf "ADD %s\n" u.x in *)
        let ctx = add_to_right ctx (u.ty, u.x) in
        Exists (u, aux ctx e)
  in
  aux ctx t

module Ntyped = Languages.Ntyped

let infer uqvs t =
  (* let () = Printf.printf "infer: %s\n" @@ Frontend.Underty.pretty_layout t in *)
  let open Languages.Underty in
  let rec aux ctx = function
    | UnderTy_base { basename; normalty; prop } ->
        let ctx = add_to_right ctx (NT.to_smtty normalty, basename) in
        UnderTy_base { basename; normalty; prop = infer_prop ctx prop }
    | UnderTy_arrow { argname; hidden_vars; argty; retty } ->
        (* let () = Printf.printf "do arrow\n" in *)
        let argty =
          aux
            (List.fold_left add_to_right ctx
            @@ List.map
                 (fun x -> (NT.to_smtty x.Ntyped.ty, x.Ntyped.x))
                 hidden_vars)
            argty
        in
        let ctx = add_to_right ctx (NT.to_smtty @@ erase argty, argname) in
        (* let () = *)
        (*   Printf.printf "[infer] ctx: %s\n" @@ List.split_by_comma fst ctx *)
        (* in *)
        UnderTy_arrow { argname; hidden_vars; argty; retty = aux ctx retty }
    | UnderTy_tuple ts -> UnderTy_tuple (List.map (aux ctx) ts)
  in
  let to_ctx qvs = List.map Ntyped.(fun x -> (x.x, NT.to_smtty x.ty)) qvs in
  aux (to_ctx uqvs) t
