open Zzdatatype.Datatype
open Sugar

type 'a t = (string * 'a) list

let find_opt_ ctx id =
  let* _, t = List.find_opt (fun (id', _) -> String.equal id id') ctx in
  Some t

let find_opt ctx id =
  try Some (Primitive.get_primitive_ty id) with _ -> find_opt_ ctx id

let overlap ctx (ty, id) =
  let rec aux = function
    | [] -> [ (id, ty) ]
    | (id', ty') :: t ->
        if String.equal id id' then (id', ty) :: t else (id', ty') :: aux t
  in
  aux ctx
