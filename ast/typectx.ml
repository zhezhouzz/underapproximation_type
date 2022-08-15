open Zzdatatype.Datatype
open Sugar

type 'a t = (string * 'a) list

let empty = []
let add_to_left (ty, name) ctx = (name, ty) :: ctx
let exists ctx name = List.exists (fun (x, _) -> String.equal x name) ctx

let get_opt (ctx : 'a t) id : 'a option =
  let* _, t = List.find_opt (fun (id', _) -> String.equal id id') ctx in
  Some t

let get_ty (ctx : 'a t) id : 'a =
  match get_opt ctx id with
  | None -> failwith @@ Sugar.spf "no such name (%s) in the type context" id
  | Some ty -> ty

let add_to_right ctx (ty, id) =
  if exists ctx id then _failatwith __FILE__ __LINE__ "" else ctx @ [ (id, ty) ]
(* let rec aux = function *)
(*   | [] -> [ (id, ty) ] *)
(*   | (id', ty') :: t -> *)
(*       if String.equal id id' then (id', ty) :: t else aux t @ [ (id', ty') ] *)
(* in *)
(* aux ctx *)

let add_to_rights ctx l = List.fold_left add_to_right ctx l

let pretty_layout f ctx =
  List.split_by ";\n" (fun (name, ty) -> Printf.sprintf "%s:%s" name (f ty)) ctx

let subtract ctx ctx' =
  let rec aux eq = function
    | l, [] -> l
    | [], _ -> _failatwith __FILE__ __LINE__ ""
    | h1 :: t1, h2 :: t2 ->
        if eq h1 h2 then aux eq (t1, t2) else aux eq (t1, h2 :: t2)
  in
  aux (fun (x, _) (y, _) -> String.equal x y) (ctx, ctx')

let fold_right = List.fold_right
let filter_map = List.filter_map

let fv f l =
  let fv = List.concat @@ List.map (fun (id, t) -> id :: f t) l in
  List.slow_rm_dup String.equal fv

let update ctx (id, f) =
  let counter = ref 0 in
  let ctx =
    List.map
      (fun (x, ty) ->
        if String.equal x id then (
          counter := !counter + 1;
          (x, f ty))
        else (x, ty))
      ctx
  in
  if !counter != 1 then failwith "type ctx update error" else ctx
