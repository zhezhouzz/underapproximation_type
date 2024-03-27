val list_gen : int -> int list

let[@library] list_gen =
  let a = (true : [%v: int]) [@over] in
  (len v a : [%v: int list]) [@under]

let batchedq_gen (sizel : int) : int batchedq = Err

let[@assert] batchedq_gen =
  let s = (v >= 0 : [%v: int]) [@over] in
  (batchedq_len v s : [%v: int batchedq]) [@under]
