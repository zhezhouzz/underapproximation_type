open Language
open Normal_id_typing
open Normal_constant_typing
open Sugar

type t = Nt.t

let rec bi_typed_lit_check (ctx : t ctx) (lit : (t option, t option lit) typed)
    (ty : t) : (t, t lit) typed =
  match (lit.x, ty) with
  | AC _, _ | AVar _, _ ->
      let lit = bi_typed_lit_infer ctx lit in
      let _ = Nt._type_unify __FILE__ __LINE__ lit.ty ty in
      lit.x #: ty
  | ATu l, Nt.Ty_tuple tys ->
      let l =
        List.map (fun (x, ty) -> bi_typed_lit_check ctx x ty)
        @@ _safe_combine __FILE__ __LINE__ l tys
      in
      (ATu l) #: ty
  | AProj _, _ -> _failatwith __FILE__ __LINE__ "unimp"
  | AAppOp (mp, args), _ ->
      let mp = bi_typed_id_infer ctx mp in
      let args' = List.map (bi_typed_lit_infer ctx) args in
      (* let _ = Printf.printf "lit: %s\n" (To_lit.layout_typed_lit lit) in *)
      let mp_ty =
        Nt._type_unify __FILE__ __LINE__ mp.ty
          (Nt.construct_arr_tp (List.map _get_ty args', ty))
      in
      let mp = mp.x #: mp_ty in
      let argsty, _ = Nt.destruct_arr_tp mp_ty in
      let args =
        List.map (fun (x, ty) -> bi_typed_lit_check ctx x ty)
        @@ _safe_combine __FILE__ __LINE__ args argsty
      in
      (AAppOp (mp, args)) #: ty
  | _, _ -> _failatwith __FILE__ __LINE__ "lit type error"

and bi_typed_lit_infer (ctx : t ctx) (lit : (t option, t option lit) typed) :
    (t, t lit) typed =
  match lit.x with
  | AVar id ->
      let id =
        match id.ty with
        | None -> bi_typed_id_infer ctx id
        | Some ty ->
            let _ = failwith "endsd" in
            id.x #: ty
      in
      (AVar id) #: id.ty
  | AC c -> (
      match lit.ty with
      | None -> (AC c) #: (infer_constant c)
      | Some ty -> (AC c) #: ty)
  | ATu l ->
      let l = List.map (bi_typed_lit_infer ctx) l in
      let ty = Nt.mk_tuple (List.map _get_ty l) in
      (ATu l) #: ty
  | AProj _ -> _failatwith __FILE__ __LINE__ "unimp"
  | AAppOp (mp, args) ->
      let mp = bi_typed_id_infer ctx mp in
      let args' = List.map (bi_typed_lit_infer ctx) args in
      let mp_ty =
        Nt._type_unify __FILE__ __LINE__ mp.ty
          (Nt.construct_arr_tp (List.map _get_ty args', Ty_unknown))
      in
      let mp = mp.x #: mp_ty in
      let argsty, retty = Nt.destruct_arr_tp mp_ty in
      let args =
        List.map (fun (x, ty) -> bi_typed_lit_check ctx x ty)
        @@ _safe_combine __FILE__ __LINE__ args argsty
      in
      (AAppOp (mp, args)) #: retty
