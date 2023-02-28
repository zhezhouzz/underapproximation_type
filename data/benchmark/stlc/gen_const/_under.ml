external method_predicates : t = "size" "is_const"

let gen_const =
  let a = (true : [%v: unit]) [@over] in
  (is_const v : [%v: stlc_term]) [@under]
