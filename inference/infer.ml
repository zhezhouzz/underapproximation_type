open Languages
module Typectx = UnderTypectx
module P = Autov.Prop
open Ntyped
open Zzdatatype.Datatype
(* open Abstraction *)
(* open Sugar *)

type infer_ctx = {
  qv : string typed list;
  args : string typed list;
  mps : string list;
  features : (string * string typed) list;
}

type mp_maps = (string * string typed) list

open Typecheck

(* let get_bv model m = *)
(*   let m_z3 = *)
(*     List.map *)
(*       (fun (mp, args) -> *)
(*         Autov.get_mp_app model (mp, List.map (fun x -> x.x) args)) *)
(*       m *)
(*   in *)
(*   m_z3 *)

(* let default_m = *)
(*   let open Ntyped in *)
(*   [ *)
(*     ("mem", [ { ty = Ty_int; x = "v" }; { ty = Ty_int; x = "u" } ]); *)
(*     ("mem", [ { ty = Ty_int; x = "s1" }; { ty = Ty_int; x = "u" } ]); *)
(*     ("mem", [ { ty = Ty_int; x = "s2" }; { ty = Ty_int; x = "u" } ]); *)
(*   ] *)

(* open UL *)

(* let rec_infer ctx (f : string UL.typed) (body : NL.value NL.typed) typecheck : *)
(*     value typed = *)
(*   let m = default_m in *)
(*   let rec loop props = *)
(*     let _ = Pp.printf "@{<bold>Iter %i:@}\n" (List.length props) in *)
(*     let f = *)
(*       { x = f.x; ty = UT.map_on_retty (fun x -> P.And (props @ [ x ])) f.ty } *)
(*     in *)
(*     let ctx' = Typectx.add_to_right ctx f in *)
(*     try *)
(*       let body = typecheck ctx' body f.ty in *)
(*       { ty = body.ty; x = Fix (f, body) } *)
(*     with Autov.FailWithModel (msg, model) -> *)
(*       let () = Pp.printf "@{<orange>Application failed:@}%s\n" msg in *)
(*       let bv' = get_bv model m in *)
(*       let prop = bv_to_prop m bv' in *)
(*       loop (prop :: props) *)
(*   in *)
(*   loop [] *)
open Infer_ctx

let get_fvs infer_ctx model =
  let fv_tab =
    Autov.Func_interp.get_fvs infer_ctx.features
      (List.map (fun x -> x.x) infer_ctx.args)
      (List.map (fun x -> x.x) infer_ctx.qvs)
      model
  in
  fv_tab

let bv_to_prop m l =
  let open P in
  let prop =
    And
      (List.map (fun ((mp, args), b) ->
           let prop = MethodPred (mp, List.map (fun x -> AVar x) args) in
           if b then prop else Not prop)
      @@ List.combine m l)
  in
  prop

open Sexplib.Std

type bv = bool list [@@deriving sexp]

open Sugar

let choose_bv bvs =
  let max = ref None in
  let () =
    List.iter
      (fun (bv, score) ->
        match !max with
        | None -> max := Some (score, [ bv ])
        | Some (max_score, bvs) ->
            if max_score == score then max := Some (score, bv :: bvs)
            else if max_score > score then ()
            else max := Some (score, [ bv ]))
      bvs
  in
  match !max with
  | None -> _failatwith __FILE__ __LINE__ ""
  | Some (_, bv) ->
      let bvs =
        List.sort
          (fun a b ->
            Sexplib.Sexp.compare
              (sexp_of_list sexp_of_bool a)
              (sexp_of_list sexp_of_bool b))
          bv
      in
      List.nth bvs 0

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
      P.topu_to_prop (infer_ctx.qvs, P.Or props)

let neg_phi_to_prop infer_ctx phi =
  match phi with
  | [] -> P.mk_true
  | bvs ->
      let props =
        List.map (fun x -> P.Not (bv_to_prop infer_ctx.features x)) bvs
      in
      P.topu_to_prop (infer_ctx.qvs, P.And props)

let phi_rule_out_bv phi bv =
  if Hashtbl.mem phi bv then Hashtbl.remove phi bv
  else _failatwith __FILE__ __LINE__ ""

let post_shrink infer_ctx nctx prog uty =
  let rec aux neg_phi =
    let neg_phi_prop = neg_phi_to_prop infer_ctx neg_phi in
    (* let () = *)
    (*   List.iter *)
    (*     (fun bv -> *)
    (*       Pp.printf "@{<yellow>%s@}\n" @@ List.split_by_comma string_of_bool bv) *)
    (*     neg_phi *)
    (* in *)
    let () =
      Printf.printf "prop: %s\n" @@ Autov.pretty_layout_prop neg_phi_prop
    in
    let uty =
      UT.work_on_retty
        (fun x -> String.equal x "v")
        ( (fun prop -> And [ prop; neg_phi_prop ]),
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
            let args = UL.get_args_return_name "v" body in
            let infer_ctx = load infer_ctx_file args in
            let uty = post_shrink infer_ctx notations_ctx body ty in
            tab @ [ (id, name', uty) ]
          with e ->
            Pp.printf "@{<bold>@{<red>Task %i, Fatal Error@}@}\n" id;
            raise e))
    [] r
