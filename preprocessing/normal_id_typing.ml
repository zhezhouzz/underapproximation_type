open Language
open Sugar

type t = Nt.t

let _unify_opt file line t1 t2 =
  match (t1, t2) with
  | _, None -> t1
  | None, _ -> t2
  | Some t1, Some t2 -> Some (Nt._type_unify file line t1 t2)

let bi_typed_id_infer (ctx : t ctx) (x : (t option, string) typed) :
    (t, string) typed =
  match _unify_opt __FILE__ __LINE__ (get_opt ctx x.x) x.ty with
  | Some ty -> { ty; x = x.x }
  | None ->
      let layout_ct_opt = function None -> "none" | Some ty -> Nt.layout ty in
      let () =
        Printf.printf "(%s: %s) =? %s\n" x.x
          (layout_ct_opt (get_opt ctx x.x))
          (layout_ct_opt x.ty)
      in
      _failatwith __FILE__ __LINE__ "die"

let bi_typed_id_check (ctx : t ctx) (x : (t option, string) typed) (ty : t) :
    (t, string) typed =
  let x = bi_typed_id_infer ctx x in
  let ty = Nt._type_unify __FILE__ __LINE__ x.ty ty in
  { ty; x = x.x }
