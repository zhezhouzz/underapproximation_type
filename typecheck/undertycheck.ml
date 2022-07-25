module T = Autov.Smtty
module NT = Languages.Normalty
open Languages.Underty

let infer_id ctx name =
  let open Autov.Prop in
  match Typectx.find_opt ctx name.x with
  | None -> failwith "free variable in refinement type"
  | Some ty -> { ty; x = name.x }

let infer_prop ctx t =
  let open Autov.Prop in
  let rec aux ctx t =
    match t with
    | True | Cint _ -> t
    | Var id -> Var (infer_id ctx id)
    | Implies (e1, e2) -> Implies (aux ctx e1, aux ctx e2)
    | Ite (e1, e2, e3) -> Ite (aux ctx e1, aux ctx e2, aux ctx e3)
    | Not e -> Not (aux ctx e)
    | And es -> And (List.map (aux ctx) es)
    | Or es -> Or (List.map (aux ctx) es)
    | Iff (e1, e2) -> Iff (aux ctx e1, aux ctx e2)
    | MethodPred (mp, args) ->
        MethodPred
          ( mp,
            List.map
              (function
                | AVar id -> AVar (infer_id ctx id) | ACint n -> ACint n)
              args )
    | Forall (u, e) ->
        let ctx = Typectx.overlap ctx (u.ty, u.x) in
        Forall (u, aux ctx e)
    | Exists (u, e) ->
        let ctx = Typectx.overlap ctx (u.ty, u.x) in
        Exists (u, aux ctx e)
  in
  aux ctx t

let infer t =
  let rec aux ctx = function
    | UnderTy_base { basename; normalty; prop } ->
        let () =
          Printf.printf "\t>>: (%s: %s ===> %s )\n" basename
            (Frontend.Type.layout normalty)
            (T.layout @@ NT.to_smtty normalty)
        in
        let ctx = Typectx.overlap ctx (NT.to_smtty normalty, basename) in
        UnderTy_base { basename; normalty; prop = infer_prop ctx prop }
    | UnderTy_arrow { argname; argty; retty } ->
        let argty = aux ctx argty in
        let ctx = Typectx.overlap ctx (NT.to_smtty @@ erase argty, argname) in
        UnderTy_arrow { argname; argty; retty = aux ctx retty }
    | UnderTy_tuple ts -> UnderTy_tuple (List.map (aux ctx) ts)
  in
  aux [] t
