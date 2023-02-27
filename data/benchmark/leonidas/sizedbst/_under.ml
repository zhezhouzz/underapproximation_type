external method_predicates : t = "rng" "mem" "sorted" "<="

let[@library] int_range =
  let a = (true : [%v: int]) [@over] in
  let b = (1 + a < v : [%v: int]) [@over] in
  (a < v && v < b : [%v: int]) [@under]

let[@library] bool_gen =
  let _ = (true : [%v: unit]) [@over] in
  (true : [%v: bool]) [@under]

let size_bst_gen =
  let d = (0 <= v : [%v: int]) [@over] in
  let s = (d <= v : [%v: int]) [@over] in
  let lo = (true : [%v: int]) [@over] in
  let hi = (v == lo + d : [%v: int]) [@over] in
  (fun (u : [%forall: int]) ->
     implies (mem v u) (lo < u && u < hi) && sorted v && rng v d
    : [%v: int tree])
    [@under]
