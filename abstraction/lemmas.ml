open Zzdatatype.Datatype

(* let rev_tab_names = [ ("rev_intlistnil", "[]"); ("rev_intlistcons", "::") ] *)
let m : Languages.Lemma.t StrMap.t option ref = ref None

let make_m m (refinements : (string * Languages.Lemma.t) list) =
  let l = List.map (fun (name, ty) -> (make_key (name, ty), ty)) refinements in
  m :=
    Some
      (StrMap.from_kv_list
      @@ List.map
           (fun (k, v) -> (Core.Sexp.to_string @@ Op.sexp_of_prim k, v))
           l)
