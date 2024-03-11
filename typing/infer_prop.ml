open Language
open FrontendTyped
open Sugar
open Inference
open Feature

type t = Nt.t

let abductive_infer_subtyping_query ~(features : t lit list)
    ~(verifier : t prop -> bool) =
  match Cegis.cegis features verifier with
  | None -> _failatwith __FILE__ __LINE__ "end"
  | Some res -> res

let abductive_infer_cty uctx cty1 cty2 =
  let vars =
    match uctx.local_ctx with
    | Typectx l ->
        List.filter_map
          (fun x ->
            match x.ty with
            | RtyBase { ou = true; _ } -> Some x.x #: (erase_rty x.ty)
            | _ -> None)
          l
  in
  match cty1 with
  | Cty { nty; phi } ->
      let v = default_v #: nty in
      let vars = v :: vars in
      let features = mk_features (Feature.get_template ()) vars in
      let verifier prop =
        let phi = smart_or [ prop; phi ] in
        let cty1 = Cty { nty; phi } in
        let res = Subtyping.Subcty.sub_cty_bool uctx (cty1, cty2) in
        let () =
          Env.show_debug_queries @@ fun _ ->
          Pp.printf "@{<bold>@{<orange>Verifier:@} %b@}\n" res
        in
        res
      in
      let phi = abductive_infer_subtyping_query ~features ~verifier in
      Cty { nty; phi }

let abductive_infer_rty uctx rty1 rty2 =
  match (rty1, rty2) with
  | RtyBase { ou = false; cty = cty1 }, RtyBase { ou = false; cty = cty2 } ->
      let cty = abductive_infer_cty uctx cty1 cty2 in
      RtyBase { ou = false; cty }
  | _, _ -> _failatwith __FILE__ __LINE__ "unimp"
