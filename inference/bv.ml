(* open Languages *)

(* module Typectx = UnderTypectx *)
module P = Autov.Prop
open Zzdatatype.Datatype
open Sexplib.Std
open Sugar
open Infer_ctx

type bv = bool list [@@deriving sexp]

let bv_tab_partition tab =
  let tab_t = Hashtbl.create 100 in
  let tab_f = Hashtbl.create 100 in
  let () =
    Hashtbl.iter
      (fun bv () ->
        match bv with
        | [] -> _failatwith __FILE__ __LINE__ ""
        | true :: bv -> Hashtbl.add tab_t bv ()
        | false :: bv -> Hashtbl.add tab_f bv ())
      tab
  in
  (tab_t, tab_f)

let featrue_to_prop (mp, args) =
  P.MethodPred (mp, List.map (fun x -> P.AVar x) args)

let bv_to_prop m l =
  let open P in
  let prop =
    And
      (List.map (fun (feature, b) ->
           let prop = featrue_to_prop feature in
           if b then prop else Not prop)
      @@ List.combine m l)
  in
  prop

let sanity_check infer_ctx bvs prop =
  List.iter
    (fun bv ->
      let bv_prop = bv_to_prop infer_ctx.features bv in
      match Autov.check [] (P.Implies (bv_prop, prop)) with
      | None -> ()
      | Some _ ->
          let () =
            Pp.printf "bv: %s\nprop: %s\n"
              (Autov.pretty_layout_prop bv_prop)
              (Autov.pretty_layout_prop prop)
          in
          _failatwith __FILE__ __LINE__ "")
    bvs

let bv_to_prop_merge infer_ctx bvs =
  let tab =
    Hashtbl.of_seq @@ List.to_seq @@ List.map (fun bv -> (bv, ())) bvs
  in
  let rec aux features tab =
    match features with
    | [] -> _failatwith __FILE__ __LINE__ ""
    | feature :: features ->
        let tab_t, tab_f = bv_tab_partition tab in
        let make tab' =
          if Hashtbl.length tab' == 0 then P.mk_false
          else if Hashtbl.length tab' == Core.Int.pow 2 (List.length features)
          then P.mk_true
          else aux features tab'
        in
        let cond = featrue_to_prop feature in
        P.peval @@ P.Ite (cond, make tab_t, make tab_f)
  in
  let prop = aux infer_ctx.features tab in
  let () = sanity_check infer_ctx bvs prop in
  prop

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
            -Sexplib.Sexp.compare
               (sexp_of_list sexp_of_bool a)
               (sexp_of_list sexp_of_bool b))
          bv
      in
      List.nth bvs 0

let layout_bv bv = List.split_by_comma string_of_bool bv

let layout_bvs infer_ctx bvs =
  let bvs =
    List.sort
      (fun a b ->
        -Sexplib.Sexp.compare
           (sexp_of_list sexp_of_bool (fst a))
           (sexp_of_list sexp_of_bool (fst b)))
      bvs
  in
  Pp.printf "@{<bold>%s@}\n"
  @@ List.split_by_comma layout_feature infer_ctx.features;
  List.iter
    (fun (bv, n) -> Pp.printf "@{<yellow>%s@}: %i\n" (layout_bv bv) n)
    bvs
