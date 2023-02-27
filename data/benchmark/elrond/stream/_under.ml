external method_predicates : t = "len" "<="

let[@library] int_gen =
  let _ = (true : [%v: unit]) [@over] in
  (true : [%v: int]) [@under]

let[@library] bool_gen =
  let _ = (true : [%v: unit]) [@over] in
  (true : [%v: bool]) [@under]

let stream_gen =
  let s = (0 <= v : [%v: int]) [@over] in
  (fun (u : [%forall: int]) -> implies (len v u) (0 <= u && u <= s)
    : [%v: int stream])
    [@under]
