external method_predicates : t = "is_const" "is_const_eq"

let[@assert] gen_const =
  let a = (true : [%v: unit]) [@over] in
  (is_const v : [%v: stlc_term]) [@under]
