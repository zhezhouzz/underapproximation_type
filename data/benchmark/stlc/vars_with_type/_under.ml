external method_predicates : t = "typing_var"

let[@library] vars_with_type_size =
  let gamma_size = (v >= 0 : [%v: int]) [@over] in
  let gamma = (size v gamma_size : [%v: stlc_tyctx]) [@over] in
  let offset = (v >= 0 : [%v: int]) [@over] in
  let tau = (true : [%v: stlc_ty]) [@over] in
  (typing_var gamma v tau && is_var_in_range offset gamma_size
    : [%v: stlc_term])
    [@under]

let vars_with_type =
  let gamma = (true : [%v: stlc_tyctx]) [@over] in
  let tau = (true : [%v: stlc_ty]) [@over] in
  (typing_var gamma v tau : [%v: stlc_term]) [@under]
