open Zzdatatype.Datatype
open Prop.T

let fv prop =
  let rec aux m t =
    match t with
    | True -> m
    | Cint _ -> m
    | Var id -> StrMap.add id.x () m
    | Implies (e1, e2) -> List.fold_left aux m [ e1; e2 ]
    | Ite (e1, e2, e3) -> List.fold_left aux m [ e1; e2; e3 ]
    | Not e -> aux m e
    | And es -> List.fold_left aux m es
    | Or es -> List.fold_left aux m es
    | Iff (e1, e2) -> List.fold_left aux m [ e1; e2 ]
    | MethodPred (_, args) ->
        List.fold_left
          (fun m id ->
            match id with ACint _ -> m | AVar id -> StrMap.add id.x () m)
          m args
    | Forall (u, e) -> StrMap.remove u.x @@ aux m e
    | Exists (u, e) -> StrMap.remove u.x @@ aux m e
  in
  StrMap.to_key_list @@ aux StrMap.empty prop
