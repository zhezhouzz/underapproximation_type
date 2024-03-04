open Languagez
open FrontendTyped
open Zzdatatype.Datatype
open Sugar

let close_rtyped_to_prop x prop =
  match x.ty with
  | Cty { nty; phi } -> (
      let qt, x = x.x in
      let x = x #: nty in
      match qt with
      | Normalty.Connective.Fa -> smart_pi (x, phi) prop
      | Normalty.Connective.Ex -> smart_sigma (x, phi) prop)

let close_rtypeds_to_prop l prop = List.fold_right close_rtyped_to_prop l prop

let layout_qt = function
  | Normalty.Connective.Fa -> "∀"
  | Normalty.Connective.Ex -> "∃"

let layout_qv { x = qt, x; ty } =
  spf "%s%s:{%s}" (layout_qt qt) x @@ layout_cty ty

let aux_sub_cty uqvs cty1 cty2 =
  match (cty1, cty2) with
  | Cty { nty = nty1; phi = phi1 }, Cty { nty = nty2; phi = phi2 } ->
      let nty = Nt._type_unify __FILE__ __LINE__ nty1 nty2 in
      let () =
        Env.show_debug_queries @@ fun _ ->
        Printf.printf "uqvs: %s\n" @@ List.split_by_comma layout_qv uqvs
      in
      let query = close_rtypeds_to_prop uqvs (smart_implies phi1 phi2) in
      (* let () = Printf.printf "query: %s\n" (layout_prop query) in *)
      let query =
        match nty with
        | Nt.Ty_unit -> query
        | _ -> Forall { qv = default_v #: nty; body = query }
      in
      let () =
        Env.show_debug_queries @@ fun _ ->
        Printf.printf "query: %s\n" (layout_prop query)
      in
      let fvs = fv_prop query in
      let () =
        _assert __FILE__ __LINE__
          (spf "the cty query has free variables %s"
             (List.split_by_comma
                (function { x; ty } -> spf "%s:%s" x (Nt.layout ty))
                fvs))
          (0 == List.length fvs)
      in
      (* TODO: Axioms *)
      Backend.Smtquery.check_bool mk_true query

type qt = Normalty.Connective.qt
type t = Nt.t

let sub_cty (pctx : t rty ctx) (cty1, cty2) =
  let rec aux (pctx : (t rty, string) typed list) uqvs cty1 cty2 =
    match List.last_destruct_opt pctx with
    | None -> aux_sub_cty uqvs cty1 cty2
    | Some (pctx, binding) -> (
        match binding.ty with
        | RtyTuple _ -> _failatwith __FILE__ __LINE__ "unimp"
        | RtyBaseArr _ | RtyArrArr _ -> aux pctx uqvs cty1 cty2
        | RtyBase { ou; cty } ->
            let qt = ou_to_qt ou in
            let x = (qt, binding.x) #: cty in
            aux pctx (x :: uqvs) cty1 cty2)
  in
  match pctx with Typectx pctx -> aux pctx [] cty1 cty2

let sub_cty_bool (pctx : t rty ctx) (cty1, cty2) = sub_cty pctx (cty1, cty2)

let is_bot_cty pctx cty =
  sub_cty_bool pctx (cty, prop_to_cty (erase_cty cty) mk_false)

let is_bot_rty pctx rty =
  match rty with
  | RtyBase { ou = false; cty } -> is_bot_cty pctx cty
  | _ -> false
