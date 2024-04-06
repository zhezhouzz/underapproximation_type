open Language
open Sugar
open Rctx
open Subtyping

(* let make_order_constraint a x = *)
(*   let a = (AVar a) #: a.ty in *)
(*   let x = (AVar x) #: x.ty in *)
(*   let lt = "<" #: Nt.(construct_arr_tp ([ Ty_int; Ty_int ], Ty_bool)) in *)
(*   let geq = ">=" #: Nt.(construct_arr_tp ([ Ty_int; Ty_int ], Ty_bool)) in *)
(*   let ty = Nt._type_unify __FILE__ __LINE__ a.ty x.ty in *)
(*   match ty with *)
(*   | Nt.Ty_int -> *)
(*       And *)
(*         [ *)
(*           Lit (AAppOp (lt, [ x; a ])) #: Nt.Ty_bool; *)
(*           Lit (AAppOp (geq, [ x; (AC (I 0)) #: Nt.Ty_int ])) #: Nt.Ty_bool; *)
(*         ] *)
(*   | _ -> _failatwith __FILE__ __LINE__ "unimp" *)

let typed_value_to_typed_lit file line v =
  match v.x with
  | VConst c -> (AC c) #: v.ty
  | VVar c -> (AVar c.x #: v.ty) #: v.ty
  | _ -> _failatwith file line "die"

let alpha_renaming_rty_term rctx e rty =
  alpha_renaming_rty_in_scope (stale_ctx rctx.local_ctx @ stale_term e.x) rty

let alpha_renaming_rty_value rctx e rty =
  alpha_renaming_rty_in_scope (stale_ctx rctx.local_ctx @ stale_value e.x) rty

let layout_ty = Nt.layout
let _rec_arg : t prop option ref = ref None
let init_rec_arg x = _rec_arg := Some x

let apply_rec_arg arg =
  let p = Env.get_statements_by_name "rec_arg" in
  let arg = (AVar arg) #: arg.ty in
  let param = (AVar default_v #: Nt.int_ty) #: Nt.int_ty in
  let phi = List.fold_left apply_pi_prop p [ arg; param ] in
  Cty { nty = Nt.int_ty; phi }

let _warinning_subtyping_error file line (rty1, rty2) =
  Env.show_debug_typing @@ fun _ ->
  Pp.printf "@{<bold>Type Error at [%s::%i]:@} %s <: %s\n" file line
    (layout_rty rty1) (layout_rty rty2)

let _warinning_subtyping_emptyness_error file line rty1 =
  Env.show_debug_typing @@ fun _ ->
  Pp.printf "@{<bold>Type Error at [%s::%i]:@} %s is empty type\n" file line
    (layout_rty rty1)

let _warinning_typing_error file line (str, rty) =
  Env.show_debug_typing @@ fun _ ->
  Pp.printf "@{<bold>Type Error at [%s::%i]:@} %s : %s\n" file line str
    (layout_rty rty)

let sub_rty_bool rctx (t1, t2) =
  let _ =
    Tyctx.pprint_typectx_subtyping
      (fun () -> pprint_linear_typectx rctx.local_ctx)
      (t1, t2)
  in
  Subrty.sub_rty_bool rctx (t1, t2)

let is_nonempty_rty rctx t1 =
  let _ =
    Tyctx.pprint_typectx_nonempty
      (fun () -> pprint_linear_typectx rctx.local_ctx)
      t1
  in
  true

let _id_type_infer file line (rctx : rctx) (id : string) : t rty =
  match get_opt rctx id with
  | None -> _failatwith file line (spf "cannot find %s in type context" id)
  | Some res -> res

let const_type_infer nty (c : constant) =
  match c with
  | U -> prop_to_rty Fa Nt.unit_ty mk_true
  | _ -> mk_rty_var_eq_c Fa nty (default_v, c)
