external method_predicates : t = "typing" "no_app" "size_app" "typing_var"

let[@library] vars_with_type =
  let gamma = (true : [%v: stlc_tyctx]) [@over] in
  let tau = (true : [%v: stlc_ty]) [@over] in
  (typing_var gamma v tau : [%v: stlc_term]) [@under]

let or_var_in_typectx =
  let a = (true : [%v: stlc_term]) [@over] in
  let tau = (true : [%v: stlc_ty]) [@over] in
  let gamma = (true : [%v: stlc_tyctx]) [@over] in
  ((implies (typing gamma a tau) (typing gamma v tau)
   && implies (no_app a) (no_app v)
   && implies (no_app a) (no_app v)
   && fun (u : [%forall: int]) -> implies (size_app a u) (size_app a v))
   || typing_var gamma v tau
    : [%v: stlc_term])
    [@under]
