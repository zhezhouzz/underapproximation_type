open Language
open FrontendTyped

(* open Zzdatatype.Datatype *)
open Sugar

(* open Subtyping *)
open Termcheck

type t = Nt.t

let layout_ty = Nt.layout
let inferred_result = ref None

let get_inferred_result () =
  match !inferred_result with
  | None -> _failatwith __FILE__ __LINE__ "die"
  | Some res -> res

let rec partial_value_type_infer (uctx : uctx) (a : (t, t value) typed)
    (rty : t rty) : (t rty, t rty value) typed option =
  let res =
    match (a.x, rty) with
    | VLam { lamarg; body }, RtyBaseArr { argcty; arg; retty } ->
        let retty = subst_rty_instance arg (AVar lamarg) retty in
        let argrty = RtyBase { ou = true; cty = argcty } in
        let* body =
          partial_term_type_infer
            (add_to_right uctx lamarg.x #: argrty)
            body retty
        in
        let lamarg = lamarg.x #: argrty in
        let rty = RtyBaseArr { argcty; arg = lamarg.x; retty = body.ty } in
        Some (VLam { lamarg; body }) #: rty
    | VLam { lamarg; body }, RtyArrArr { argrty; retty } ->
        let* body =
          partial_term_type_infer
            (add_to_right uctx lamarg.x #: argrty)
            body retty
        in
        let lamarg = lamarg.x #: argrty in
        let rty = RtyArrArr { argrty; retty = body.ty } in
        Some (VLam { lamarg; body }) #: rty
    | VLam _, _ -> _failatwith __FILE__ __LINE__ ""
    | VFix { fixname; fixarg; body }, RtyBaseArr { argcty; arg; retty } ->
        let rec_constraint_cty = apply_rec_arg fixarg in
        let rty' =
          let a = { x = Rename.unique fixarg.x; ty = fixarg.ty } in
          RtyBaseArr
            {
              argcty = intersect_ctys [ argcty; rec_constraint_cty ];
              arg = a.x;
              retty = subst_rty_instance arg (AVar a) retty;
            }
        in
        let binding = fixarg.x #: (RtyBase { ou = true; cty = argcty }) in
        let retty = subst_rty_instance arg (AVar fixarg) retty in
        let* body' =
          partial_term_type_infer
            (add_to_rights uctx [ binding; fixname.x #: rty' ])
            body retty
        in
        (* let a = { x = Rename.unique fixarg.x; ty = fixarg.ty } in *)
        (* let prop = Checkaux.make_order_constraint fixarg a in *)
        (* let retty_a = subst_rty_instance arg (AVar a) retty in *)
        (* let rty_a = RtyBaseArr { argcty; arg = a.x; retty = retty_a } in *)
        (* let rty_a = map_prop_in_retrty (smart_add_to prop) rty_a in *)
        (* let binding = fixarg.x #: (RtyBase { ou = true; cty = argcty }) in *)
        (* (\* let retty = subst_rty_instance arg (AVar a) retty in *\) *)
        (* (\* let body = (subst_term_instance fixarg.x (VVar a) body.x) #: body.ty in *\) *)
        (* let uctx' = add_to_rights uctx [ binding; fixname.x #: rty_a ] in *)
        (* let* body' = partial_term_type_infer uctx' body retty in *)
        let rty = RtyBaseArr { argcty; arg = fixarg.x; retty = body'.ty } in
        Some
          (VFix
             {
               fixname = fixname.x #: rty;
               fixarg = fixname.x #: binding.ty;
               body = body';
             })
          #: rty
    | _ -> Some (value_type_infer uctx a)
  in
  let () =
    match res with
    | Some res -> pprint_simple_typectx_infer uctx (layout_typed_value a, res.ty)
    | None -> ()
  in
  res

and partial_term_type_infer (uctx : uctx) (a : (t, t term) typed) (rty : t rty)
    : (t rty, t rty term) typed option =
  match a.x with
  | CVal v ->
      let* v = partial_value_type_infer uctx v rty in
      Some (CVal v) #: v.ty
  | _ ->
      (* NOTE: the first type not a value (function body) *)
      let* a = term_type_infer uctx a in
      let inferred_rty = Infer_prop.abductive_infer_rty uctx a.ty rty in
      let () = Printf.printf "inferred_rty: %s\n" (layout_rty inferred_rty) in
      let () = inferred_result := Some inferred_rty in
      Some a
(* | _ -> term_type_infer uctx a *)
