open Languages
open Sugar
module P = Autov.Prop

let check_empty_hidden file line hvs =
  if List.length hvs != 0 then
    _failatwith file line "do not allow hidden variables"
  else ()

open Zzdatatype.Datatype

let unify file line underfty normalty =
  let open UT in
  let open Ntyped in
  (* let () = *)
  (*   Pp.printf "|_ %s _| ~> %s <=== %s\n" *)
  (*     (Frontend.Underty.pretty_layout underfty) *)
  (*     (NT.layout @@ UT.erase underfty) *)
  (*     (NT.layout normalty) *)
  (* in *)
  let rec aux m = function
    | UnderTy_base { basename; normalty; prop }, nt ->
        let m, normalty = NT._type_unify_ file line m normalty nt in
        let prop = P.type_update m prop in
        UnderTy_base { basename; normalty; prop }
    | UnderTy_poly_arrow { argname; argnty; retty }, Ty_arrow (t1, t2) ->
        let m, argnty = NT._type_unify_ file line m argnty t1 in
        let retty = aux m (retty, t2) in
        UnderTy_poly_arrow { argname; argnty; retty }
    | UnderTy_arrow { argname; argty; retty }, Ty_arrow (t1, t2) ->
        let argty = aux m (argty, t1) in
        let m = StrMap.add argname (erase argty) m in
        let retty = aux m (retty, t2) in
        UnderTy_arrow { argname; argty; retty }
    | UnderTy_tuple bindings, Ty_tuple ts ->
        let l = _safe_combine file line bindings ts in
        let bindings, _ =
          List.fold_left
            (fun (bindings, m) ((x, ty), nt) ->
              let ty = aux m (ty, nt) in
              let m = StrMap.add x (erase ty) m in
              (bindings @ [ (x, ty) ], m))
            ([], m) l
        in
        UnderTy_tuple bindings
    | _, _ -> _failatwith file line "unify"
  in
  aux StrMap.empty (underfty, normalty)

(* let erase_check file line (underfty, normalty) = *)
(*   (\* let () = *\) *)
(*   (\*   Pp.printf "|_ %s _| ~> %s = %s\n" *\) *)
(*   (\*     (UT.pretty_layout underfty) *\) *)
(*   (\*     (NT.layout @@ UT.erase underfty) *\) *)
(*   (\*     (NT.layout @@ snd normalty) *\) *)
(*   (\* in *\) *)
(*   let _ = _check_equality file line NT.eq (UT.erase underfty) (snd normalty) in *)
(*   () *)

let erase_check_mk_id file line id underfty =
  (* let () = *)
  (*   Pp.printf "|_ %s _| ~> %s = %s\n" *)
  (*     (Frontend.Underty.layout underfty) *)
  (*     (Frontend.Type.layout @@ UT.erase underfty) *)
  (*     (Frontend.Type.layout id.NL.ty) *)
  (* in *)
  let ty = unify file line underfty (snd id.NL.ty) in
  UL.{ ty; x = id.x }
(* let _ = _check_equality file line NT.eq (UT.erase underfty) (snd id.NL.ty) in *)
(* UL.{ ty = underfty; x = id.x } *)

let subtyping_check = Undersub.subtyping_check
let subtyping_check_bool = Undersub.subtyping_check_bool

let term_subtyping_check file line ctx UL.{ x; ty } t2 =
  let () = Undersub.subtyping_check file line ctx ty t2 in
  UL.{ x; ty = t2 }

(* let subtyping_check__with_hidden_vars = *)
(*   Undersub.subtyping_check_with_hidden_vars *)

let merge_case_tys tys =
  (* let () = *)
  (*   List.iteri *)
  (*     (fun i ty -> *)
  (*       Pp.printf "@{<bold>Case(%i) ty@}: %s\n" i @@ UT.pretty_layout ty) *)
  (*     tys *)
  (* in *)
  let ty = UT.disjunct_list tys in
  (* let () = *)
  (*   Pp.printf "@{<bold>Merged ty@}: %s\n" @@ Frontend.Underty.pretty_layout ty *)
  (* in *)
  ty

(* let close_term_by_diff ctx' ctx UL.{ ty; x } = *)
(*   let _ = UL.{ x; ty = UnderTypectx.close_by_diff ctx' ctx ty } in *)
(*   _failatwith __FILE__ __LINE__ "unimp" *)

(* let () = *)
(*   Pp.printf "@{<bold>Close:@}\n"; *)
(*   Frontend.Typectx.pretty_print ctx'; *)
(*   Pp.printf "@{<bold>-@}\n"; *)
(*   Frontend.Typectx.pretty_print ctx; *)
(*   Pp.printf "@{<bold>=@}\n"; *)
(*   Frontend.Typectx.pretty_print *)
(*     { *)
(*       qvs = []; *)
(*       qbody = *)
(*         List.map (fun (b, (name, t)) -> (spf "|%b|%s" b name, t)) *)
(*         @@ Languages.UnderTypectx.subtract ctx'.qbody ctx.qbody; *)
(*     } *)
(* in *)
(* module MultiTypectx = Languages.MultiUnderTypectx *)
module Nctx = Languages.UTSimpleTypectx
module Typectx = Languages.UnderTypectx
(* open Abstraction *)

type uctx = { ctx : Nctx.t; nctx : Nctx.t; libctx : Typectx.t }

let id_type_infer (uctx : uctx) (id : NL.id NL.typed) : UL.id UL.typed =
  let ty =
    try Nctx.get_ty uctx.ctx id.x with _ -> Typectx.get_ty uctx.libctx id.x
  in
  erase_check_mk_id __FILE__ __LINE__ id ty

let id_type_check (uctx : uctx) (id : NL.id NL.typed) (ty : UT.t) :
    NL.id UL.typed =
  let id = id_type_infer uctx id in
  let () = subtyping_check __FILE__ __LINE__ uctx.ctx id.UL.ty ty in
  UL.{ x = id.x; ty }

let lit_type_infer (uctx : uctx) (lit : NL.smt_lit NL.typed) :
    UL.smt_lit UL.typed =
  let open NL in
  let open UT in
  match lit.x with
  | ConstI n -> { ty = make_basic_from_const_int n; x = ConstI n }
  | ConstB b -> { ty = make_basic_from_const_bool b; x = ConstB b }
  | Var id ->
      UL.(typed_map (fun x -> Var x))
      @@ id_type_infer uctx { ty = lit.ty; x = id }
