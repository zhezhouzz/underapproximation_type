open Zzdatatype.Datatype
open Prop.T

let rec lit_fv m t =
  match t with
  | ACint _ -> m
  | AVar id -> StrMap.add id.x () m
  | AOp2 (_, a, b) -> lit_fv (lit_fv m a) b

let _add_fv m prop =
  let rec aux m t =
    match t with
    | True -> m
    | Lit lit -> lit_fv m lit
    | Implies (e1, e2) -> List.fold_left aux m [ e1; e2 ]
    | Ite (e1, e2, e3) -> List.fold_left aux m [ e1; e2; e3 ]
    | Not e -> aux m e
    | And es -> List.fold_left aux m es
    | Or es -> List.fold_left aux m es
    | Iff (e1, e2) -> List.fold_left aux m [ e1; e2 ]
    | MethodPred (_, args) -> List.fold_left lit_fv m args
    | Forall (u, e) -> StrMap.remove u.x @@ aux m e
    | Exists (u, e) -> StrMap.remove u.x @@ aux m e
  in
  aux m prop

let add_fv fv prop =
  StrMap.to_key_list
  @@ _add_fv (StrMap.from_kv_list @@ List.map (fun name -> (name, ())) fv) prop

let fv prop = add_fv [] prop
