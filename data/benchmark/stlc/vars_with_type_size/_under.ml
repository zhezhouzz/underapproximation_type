external method_predicates : t = "size" "typing_var" "is_var_in_range"

let[@library] type_eq =
  let tau_a = (true : [%v: stlc_ty]) [@over] in
  let tau_b = (true : [%v: stlc_ty]) [@over] in
  (iff v (type_eq_spec tau_a tau_b) : [%v: bool]) [@under]

let vars_with_type_size =
  let gamma_size = (v >= 0 : [%v: int]) [@over] in
  let gamma = (size v gamma_size : [%v: stlc_tyctx]) [@over] in
  let offset = (v >= 0 : [%v: int]) [@over] in
  let tau = (true : [%v: stlc_ty]) [@over] in
  (typing_var gamma v tau && is_var_in_range offset gamma_size
    : [%v: stlc_term])
    [@under]
