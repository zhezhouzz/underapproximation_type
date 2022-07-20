open Zzdatatype.Datatype
open Sugar

type 'a t = (string * 'a) list

let find_opt (ctx : 'a t) id : 'a option =
  let* _, t = List.find_opt (fun (id', _) -> String.equal id id') ctx in
  Some t

let find_opt_normal ctx id =
  try Some (Primitive.get_primitive_ty id) with _ -> find_opt ctx id

let get_ty_normal (ctx : 'a t) id : 'a =
  match find_opt_normal ctx id with
  | None -> failwith @@ Sugar.spf "no such name (%s) in the type context" id
  | Some ty -> ty

let get_ty (ctx : 'a t) id : 'a =
  match find_opt ctx id with
  | None -> failwith @@ Sugar.spf "no such name (%s) in the type context" id
  | Some ty -> ty

let overlap ctx (ty, id) =
  let rec aux = function
    | [] -> [ (id, ty) ]
    | (id', ty') :: t ->
        if String.equal id id' then (id', ty) :: t else (id', ty') :: aux t
  in
  aux ctx
