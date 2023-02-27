external method_predicates : t = "len"

let[@library] int_range =
  let x = (true : [%v: int]) [@over] in
  let y = (true : [%v: int]) [@over] in
  (x <= v && v <= y : [%v: int]) [@under]

let[@library] int_gen =
  let _ = (true : [%v: unit]) [@over] in
  (true : [%v: int]) [@under]

let[@library] bool_gen =
  let _ = (true : [%v: unit]) [@over] in
  (true : [%v: bool]) [@under]

let leftisthp_gen =
  let s = (0 <= v : [%v: int]) [@over] in
  (len v s : [%v: int leftisthp]) [@under]
