open Language
open Sugar

let make_order_constraint a x =
  let a = (AVar a) #: a.ty in
  let x = (AVar x) #: x.ty in
  let lt = "<" #: Nt.(construct_arr_tp ([ Ty_int; Ty_int ], Ty_bool)) in
  let geq = ">=" #: Nt.(construct_arr_tp ([ Ty_int; Ty_int ], Ty_bool)) in
  let ty = Nt._type_unify __FILE__ __LINE__ a.ty x.ty in
  match ty with
  | Nt.Ty_int ->
      And
        [
          Lit (AAppOp (lt, [ x; a ])) #: Nt.Ty_bool;
          Lit (AAppOp (geq, [ x; (AC (I 0)) #: Nt.Ty_int ])) #: Nt.Ty_bool;
        ]
  | _ -> _failatwith __FILE__ __LINE__ "unimp"

let typed_value_to_typed_lit file line v =
  match v.x with
  | VConst c -> (AC c) #: v.ty
  | VVar c -> (AVar c.x #: v.ty) #: v.ty
  | _ -> _failatwith file line "die"

(* open Zzdatatype.Datatype *)

(* let check_empty_hidden file line hvs = *)
(*   if List.length hvs != 0 then *)
(*     _failatwith file line "do not allow hidden variables" *)
(*   else () *)

(* let erase_check_mk_id file line id underfty = *)
(*   let ty = unify file line underfty (snd id.NL.ty) in *)
(*   UL.{ ty; x = id.x } *)

(* let merge_case_tys tys = *)
(*   let ty = UT.disjunct_list tys in *)
(*   ty *)

(* let term_subtyping_check file line uctx UL.{ x; ty } t2 = *)
(*   let () = Undersub.subtyping_check file line uctx.ctx ty t2 in *)
(*   UL.{ x; ty = t2 } *)

(* let term_subtyping_check_opt file line uctx UL.{ x; ty } t2 = *)
(*   if Undersub.subtyping_check_bool file line uctx.ctx ty t2 then *)
(*     Some UL.{ x; ty = t2 } *)
(*   else None *)

(* let ut_eq_to_ut_underctx (uctx : uctx) t = *)
(*   match t with *)
(*   | MMT.UtNormal t -> t *)
(*   | MMT.UtCopy id -> ( *)
(*       match Typectx.get_ty uctx.ctx id.NTyped.x with *)
(*       | Ot _ -> make_basic_from_eq_var id *)
(*       | Ut (UtNormal idty) -> *)
(*           if NT.is_basic_tp id.ty then make_basic_from_eq_var id *)
(*           else if NT.is_dt id.ty then *)
(*             (\* HACK: assume the dt type will not change; when there is not pattern matching *\) *)
(*             if true then idty *)
(*             else *)
(*               let all_mps = *)
(*                 match !Env.config with *)
(*                 | None -> _failatwith __FILE__ __LINE__ "" *)
(*                 | Some config -> config.all_mps *)
(*               in *)
(*               let () = *)
(*                 Env.show_debug_debug @@ fun _ -> *)
(*                 Pp.printf "all_mps: %s\n" @@ StrList.to_string all_mps *)
(*               in *)
(*               Dt_eq.make_eq_type all_mps id *)
(*           else idty *)
(*       | _ -> _failatwith __FILE__ __LINE__ "") *)

(* let subtyping_check file line uctx t1 t2 = *)
(*   let t1 = ut_eq_to_ut_underctx uctx t1 in *)
(*   Undersub.subtyping_check file line uctx.ctx t1 t2 *)

(* let subtyping_check_bool file line uctx t1 t2 = *)
(*   let t1 = ut_eq_to_ut_underctx uctx t1 in *)
(*   Undersub.subtyping_check_bool file line uctx.ctx t1 t2 *)

(* let close_ids ids ty = Typectx.close_by_diff_ ids ty *)

(* let make_order_constraint a x ty = *)
(*   let open NT in *)
(*   match ty with *)
(*   | Ty_int -> *)
(*       P.( *)
(*         And *)
(*           [ *)
(*             MethodPred ("<", [ AVar { x; ty }; AVar { x = a; ty } ]); *)
(*             MethodPred (">=", [ AVar { x; ty }; ACint 0 ]); *)
(*           ]) *)
(*   | _ -> _failatwith __FILE__ __LINE__ "unimp" *)

(* let dt_expand f argsty = *)
(*   let measure = *)
(*     match !Env.config with *)
(*     | None -> _failatwith __FILE__ __LINE__ "" *)
(*     | Some c -> c.measure *)
(*   in *)
(*   let argsty = *)
(*     List.concat *)
(*     @@ List.map *)
(*          (fun uty -> *)
(*            if NT.is_dt (MMT.ut_erase_ uty) then *)
(*              match uty with *)
(*              | UtNormal _ -> _failatwith __FILE__ __LINE__ "unimp" *)
(*              | UtCopy id -> *)
(*                  let sizeargty = *)
(*                    UT.make_basic_from_prop NT.Ty_int (fun v -> *)
(*                        P.(MethodPred (measure, [ AVar id; AVar v ]))) *)
(*                  in *)
(*                  [ MMT.UtNormal sizeargty; uty ] *)
(*            else [ uty ]) *)
(*          argsty *)
(*   in *)
(*   let fnty = *)
(*     let _, retnty = NT.destruct_arr_tp (snd f.NL.ty) in *)
(*     let argsnty = List.map MMT.ut_erase_ argsty in *)
(*     NT.construct_arr_tp (argsnty, retnty) *)
(*   in *)
(*   (fnty, argsty) *)
