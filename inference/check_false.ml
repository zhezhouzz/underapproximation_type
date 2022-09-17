open Languages
module Typectx = UnderTypectx
module P = Autov.Prop
open Ntyped
open Zzdatatype.Datatype

(* open Typecheck *)
open Infer_ctx
open Bv
open Sugar

let close_post uty =
  let open UT in
  let rec aux uty =
    match uty with
    | UnderTy_base { prop; _ } -> prop
    | UnderTy_arrow { argname; argty; retty } -> (
        match
          (List.exists (String.equal argname) @@ fv retty, is_base_type argty)
        with
        | true, true ->
            let x, _, xprop = assume_base __FILE__ __LINE__ argty in
            And [ P.subst_id xprop x argname; aux retty ]
        | true, false -> _failatwith __FILE__ __LINE__ ""
        | false, _ -> aux retty)
    | _ -> _failatwith __FILE__ __LINE__ ""
  in
  aux uty

let mk_false_post uty =
  let open UT in
  let rec aux uty =
    match uty with
    | UnderTy_base { basename; normalty; _ } ->
        UnderTy_base { basename; normalty; prop = P.mk_false }
    | UnderTy_arrow { argname; argty; retty } ->
        UnderTy_arrow { argname; argty; retty = aux retty }
    | _ -> _failatwith __FILE__ __LINE__ ""
  in
  aux uty

type res = IsFalse | IsNotFalse

let print_res = function
  | IsFalse ->
      Pp.printf "@{<bold>@{<red>Trivial Result:@}@} reduced to false.\n"
  | IsNotFalse -> Pp.printf "@{<bold>@{<green>Valid Result:@}@} reachable.\n"

let check infer_ctx uty =
  let false_uty = mk_false_post uty in
  let prop = close_post uty in
  let prop_false = close_post false_uty in
  let dts = List.filter (fun x -> is_dt x.ty) (get_inpout infer_ctx) in
  (* let _ = Pp.printf "dts: %s\n" @@ List.split_by_comma (fun x -> x.x) dts in *)
  let lemmas = Lemma.lemmas_with_dts (Abstraction.Prim.lemmas_to_pres ()) dts in
  (* let () = *)
  (*   List.iter *)
  (*     (fun lemma -> *)
  (*       Pp.printf "@{<bold>Lemma:@} %s\n" @@ Autov.pretty_layout_prop lemma) *)
  (*     lemmas *)
  (* in *)
  match Autov.check lemmas (P.Implies (prop, prop_false)) with
  | None -> IsFalse
  | _ -> IsNotFalse
