open Languages.Lemma
open Sugar

let pretty_layout lemma =
  (* let to_strings = List.map (fun x -> x.Languages.SMTtyped.x) in *)
  spf "%s%s%s."
    (Quantified.mk_eqs lemma.qvs)
    (Quantified.mk_uqs lemma.qbody.qvs)
    (Autov.pretty_layout_prop lemma.qbody.qbody)
