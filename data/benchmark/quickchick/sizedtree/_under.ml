external method_predicates : t = "len" "<="

let[@assert] sized_tree_gen =
  let s = (0 <= v : [%v: int]) [@over] in
  (fun (u : int) -> implies (len v u) (0 <= u && u <= s)
    : [%v: int tree])
    [@under]
