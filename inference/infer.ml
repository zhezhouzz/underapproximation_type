open Languages
module Typectx = UnderTypectx
module P = Autov.Prop
open Ntyped
open Zzdatatype.Datatype
(* open Abstraction *)

type infer_ctx = {
  qv : string typed list;
  args : string typed list;
  mps : string list;
  features : (string * string typed) list;
}

open Typecheck
open Infer_ctx
open Bv
open Sugar

let get_fvs infer_ctx model =
  let fv_tab =
    Autov.Func_interp.get_fvs infer_ctx.features
      (List.map (fun x -> x.x) (get_inpout infer_ctx))
      (List.map (fun x -> x.x) infer_ctx.qvs)
      model
  in
  fv_tab

type chekck_result = Succ of UT.t | Failed of Z3.Model.model

let phi_init infer_ctx =
  let settings = List.map (fun _ -> [ true; false ]) infer_ctx.features in
  let all = List.choose_list_list settings in
  Hashtbl.of_seq @@ Seq.map (fun x -> (x, ())) @@ List.to_seq all

let phi_to_prop infer_ctx phi =
  match List.of_seq @@ Hashtbl.to_seq_keys phi with
  | [] -> P.mk_false
  | bvs ->
      let props = List.map (bv_to_prop infer_ctx.features) bvs in
      P.Or props

let neg_phi_to_prop infer_ctx phi =
  match phi with
  | [] -> P.mk_true
  | bvs ->
      let props =
        List.map (fun x -> P.Not (bv_to_prop infer_ctx.features x)) bvs
      in
      P.And props

let phi_rule_out_bv phi bv =
  if Hashtbl.mem phi bv then Hashtbl.remove phi bv
  else _failatwith __FILE__ __LINE__ ""

let post_shrink infer_ctx nctx prog uty =
  let rec aux neg_phi =
    let neg_phi_prop = neg_phi_to_prop infer_ctx neg_phi in
    let () =
      Pp.printf "@{<yellow>neg_phi:@} %s\n"
      @@ Autov.pretty_layout_prop (bv_to_prop_merge infer_ctx neg_phi)
    in
    let () =
      Printf.printf "prop: %s\n" @@ Autov.pretty_layout_prop neg_phi_prop
    in
    let _ =
      match
        Autov.check []
          (P.Iff (neg_phi_prop, P.Not (bv_to_prop_merge infer_ctx neg_phi)))
      with
      | None -> ()
      | Some _ -> _failatwith __FILE__ __LINE__ "bad"
    in
    let uty =
      UT.work_on_retty
        (fun x -> String.equal x infer_ctx.retv.x)
        ( (fun prop ->
            And
              [
                prop;
                P.topu_to_prop
                  (infer_ctx.qvs, P.Not (bv_to_prop_merge infer_ctx neg_phi));
              ]),
          fun _ -> _failatwith __FILE__ __LINE__ "" )
        uty
    in
    let () = Printf.printf "uty: %s\n" @@ UT.pretty_layout uty in
    let res =
      try
        let _ = Undercheck.type_check nctx prog uty in
        Succ uty
      with Autov.FailWithModel (_, model) -> Failed model
    in
    match res with
    | Succ uty -> uty
    | Failed model ->
        let bvs = get_fvs infer_ctx model in
        let bv = choose_bv bvs in
        aux (bv :: neg_phi)
  in
  aux []

module SNA = Languages.StrucNA
module Nctx = Simpletypectx.UTSimpleTypectx

let struc_post_shrink infer_ctx_file l notations (r : (string * UT.t) list) =
  let open SNA in
  List.fold_lefti
    (fun tab id ((name' : string), ty) ->
      let id = id + 1 in
      let () = Pp.printf "@{<bold>Task %i:@}\n" id in
      match List.find_opt (fun { name; _ } -> String.equal name name') l with
      | None -> _failatwith __FILE__ __LINE__ "does not provide source code"
      | Some { body; _ } -> (
          let notations_ctx =
            Nctx.(
              List.fold_left
                (fun ctx (name, ty) -> add_to_right ctx (ty, name))
                empty notations)
          in
          try
            let args, retv = UT.get_inpout ty in
            let infer_ctx = load infer_ctx_file args retv in
            let uty = post_shrink infer_ctx notations_ctx body ty in
            tab @ [ (id, name', uty, Check_false.check infer_ctx uty) ]
          with e ->
            Pp.printf "@{<bold>@{<red>Task %i, Fatal Error@}@}\n" id;
            raise e))
    [] r
