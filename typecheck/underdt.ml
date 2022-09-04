open Autov.Prop
open Abstraction
module NT = Languages.Normalty
module LA = Languages.Lemma

(* let mk_mapping apps  *)

let instantiate_lemmas lemma vars =
  let eqvs = lemma.LA.qvs in
  let uqvs = lemma.LA.qbody.qvs in
  ()

let dts_to_bits (uqs, eqs, prop) =
  let dt_eqs, basic_eqs = List.partition (fun x -> NT.is_dt x.ty) eqs in
  let lemmas = Prim.lemmas_to_pres () in

  let dt_eqs, prop = Autov.uqv_encoding (List.map (fun x -> x.x) dt_eqs) prop in
  let prop =
    List.fold_right (fun qv prop -> P.(Exists (qv, prop))) dt_eqs prop
  in
  List.fold_right (fun x prop -> P.Forall (x, prop)) uqs
  @@ List.fold_right (fun x prop -> P.Exists (x, prop)) basic_eqs prop
