val stream_gen : int -> int stream

let[@library] stream_gen =
  let s = (0 <= v : [%v: int]) [@over] in
  (stream_len v s : [%v: int stream]) [@under]

let bankersq_gen (lenf : int) : int bankersq =
  let (lenr : int) = int_range_inc 0 lenf in
  let (f : int stream) = stream_gen lenf in
  let (r : int stream) = stream_gen lenr in
  Bankersq (lenf, f, lenr, r)

let[@assert] bankersq_gen =
  let s = (v >= 0 : [%v: int]) [@over] in
  (bankersq_len v s : [%v: int bankersq]) [@under]
