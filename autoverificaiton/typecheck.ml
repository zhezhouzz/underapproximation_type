open Prop.T
open Zzdatatype.Datatype
module T = Smtty.T

let get_ty ctx name =
  match StrMap.find_opt ctx name with
  | None -> failwith "prop type check no such variable in ctx"
  | Some ty -> ty

let type_infer prop =
  let rec aux ctx prop =
    match prop with
    | True -> prop
    | Var { ty = Some _; _ } -> prop
    | Var { ty = None; x } -> Var { ty = Some (get_ty ctx x); x }
    | Implies (e1, e2) -> Implies (aux ctx e1, aux ctx e2)
    | Ite (e1, e2, e3) -> Ite (aux ctx e1, aux ctx e2, aux ctx e3)
    | Not e -> Not (aux ctx e)
    | And es -> And (List.map (aux ctx) es)
    | Or es -> And (List.map (aux ctx) es)
    | Iff (e1, e2) -> Iff (aux ctx e1, aux ctx e2)
    | MethodPred (mp, args) ->
        MethodPred
          ( mp,
            List.map
              (fun x ->
                match x with
                | { ty = Some _; _ } -> x
                | { ty = None; x } -> { ty = Some (get_ty ctx x); x })
              args )
    | Forall (uty, u, e) -> Forall (uty, u, aux (StrMap.add u uty ctx) e)
    | Exists (uty, u, e) -> Exists (uty, u, aux (StrMap.add u uty ctx) e)
  in
  aux StrMap.empty prop
