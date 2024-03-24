let[@library] ( == ) =
  let a = (true : [%v: int]) [@over] in
  let b = (true : [%v: int]) [@over] in
  (iff v (a == b) : [%v: bool]) [@under]

let[@library] ( != ) =
  let a = (true : [%v: int]) [@over] in
  let b = (true : [%v: int]) [@over] in
  (iff v (a != b) : [%v: bool]) [@under]

let[@library] ( < ) =
  let a = (true : [%v: int]) [@over] in
  let b = (true : [%v: int]) [@over] in
  (iff v (a < b) : [%v: bool]) [@under]

let[@library] ( > ) =
  let a = (true : [%v: int]) [@over] in
  let b = (true : [%v: int]) [@over] in
  (iff v (a > b) : [%v: bool]) [@under]

let[@library] ( <= ) =
  let a = (true : [%v: int]) [@over] in
  let b = (true : [%v: int]) [@over] in
  (iff v (a <= b) : [%v: bool]) [@under]

let[@library] ( >= ) =
  let a = (true : [%v: int]) [@over] in
  let b = (true : [%v: int]) [@over] in
  (iff v (a >= b) : [%v: bool]) [@under]

let[@library] ( + ) =
  let a = (true : [%v: int]) [@over] in
  let b = (true : [%v: int]) [@over] in
  (v == a + b : [%v: int]) [@under]

let[@library] ( - ) =
  let a = (true : [%v: int]) [@over] in
  let b = (true : [%v: int]) [@over] in
  (v == a - b : [%v: int]) [@under]

let[@library] TT = (true : [%v: unit]) [@under]
let[@library] True = (v : [%v: bool]) [@under]
let[@library] False = (not v : [%v: bool]) [@under]
let[@library] Nil = (emp v : [%v: int list]) [@under]

let[@library] Cons =
  let x = (true : [%v: int]) [@over] in
  let xs = (true : [%v: int list]) [@over] in
  (hd v x && tl v xs : [%v: int list]) [@under]

let[@library] list_mem =
  let xs = (true : [%v: int list]) [@over] in
  let x = (true : [%v: int]) [@over] in
  (v == list_mem xs x : [%v: bool]) [@under]

let[@library] Leaf = (leaf v : [%v: int tree]) [@under]

let[@library] Node =
  let x = (true : [%v: int]) [@over] in
  let lt = (true : [%v: int tree]) [@over] in
  let rt = (true : [%v: int tree]) [@over] in
  (root v x && lch v lt && rch v rt : [%v: int tree]) [@under]

let[@library] Rbtleaf = (rb_leaf v : [%v: int rbtree]) [@under]

let[@library] Rbtnode =
  let c = (true : [%v: bool]) [@over] in
  let lt = (true : [%v: int rbtree]) [@over] in
  let x = (true : [%v: int]) [@over] in
  let rt = (true : [%v: int rbtree]) [@over] in
  (rb_root_color v c && rb_root v x && rb_lch v lt && rb_rch v rt
    : [%v: int rbtree])
    [@under]

let[@library] Streamnil = (stream_emp v : [%v: int stream]) [@under]

let[@library] Streamlazycons =
  let x = (true : [%v: int]) [@over] in
  let xs = (true : [%v: int stream lazyty]) [@over] in
  (stream_hd v x && stream_tl v (forc xs) : [%v: int stream]) [@under]

let[@library] Lazyty =
  let x = (true : [%v: int stream]) [@over] in
  (forc v == x : [%v: int stream lazyty]) [@under]

let[@library] Bankersq =
  let lenf = (v >= 0 : [%v: int]) [@over] in
  let f = (stream_len v lenf : [%v: int stream]) [@over] in
  let lenr = (v >= 0 && v <= lenf : [%v: int]) [@over] in
  let r = (stream_len v lenr : [%v: int stream]) [@over] in
  (bankersq1 v lenf && bankersq2 v f && bankersq3 v lenr && bankersq4 v r
    : [%v: int bankersq])
    [@under]

let[@library] Batchedq =
  let f = (true : [%v: int list]) [@over] in
  let r = (true : [%v: int list]) [@over] in
  (batchedq1 v f && batchedq2 v r : [%v: int batchedq]) [@under]

let[@library] Lhpleaf = (leftisthp_leaf v : [%v: int leftisthp]) [@under]

let[@library] Lhpnode =
  let r = (true : [%v: int]) [@over] in
  let x = (true : [%v: int]) [@over] in
  let lt = (true : [%v: int leftisthp]) [@over] in
  let rt = (true : [%v: int leftisthp]) [@over] in
  (leftisthp_rank v r && leftisthp_root v x && leftisthp_lch v lt
   && leftisthp_rch v rt
    : [%v: int leftisthp])
    [@under]

(* the built-in random generators *)

let[@library] int_range =
  let a = (true : [%v: int]) [@over] in
  let b = (1 + a < v : [%v: int]) [@over] in
  (a < v && v < b : [%v: int]) [@under]

let[@library] bool_gen =
  let _ = (true : [%v: unit]) [@over] in
  (true : [%v: bool]) [@under]

let[@library] int_gen =
  let _ = (true : [%v: unit]) [@over] in
  (true : [%v: int]) [@under]

let[@library] nat_gen =
  let _ = (true : [%v: unit]) [@over] in
  (v >= 0 : [%v: int]) [@under]

let[@library] int_range_inc =
  let a = (true : [%v: int]) [@over] in
  let b = (a <= v : [%v: int]) [@over] in
  (a <= v && v <= b : [%v: int]) [@under]

let[@library] int_range_inex =
  let a = (true : [%v: int]) [@over] in
  let b = (a <= v : [%v: int]) [@over] in
  (a <= v && v < b : [%v: int]) [@under]

let[@library] increment =
  let n = (true : [%v: int]) [@over] in
  (v == n + 1 : [%v: int]) [@under]

let[@library] decrement =
  let n = (true : [%v: int]) [@over] in
  (v == n - 1 : [%v: int]) [@under]

let[@library] lt_eq_one =
  let s = (true : [%v: int]) [@over] in
  (iff v (s <= 1) && iff (not v) (s > 1) : [%v: bool]) [@under]

(* uniquel  *)

let[@library] gt_eq_int_gen =
  let x = (true : [%v: int]) [@over] in
  (true : [%v: int]) [@under]

let[@library] sizecheck =
  let x = (true : [%v: int]) [@over] in
  (iff v (x == 0) && iff (not v) (x > 0) : [%v: bool]) [@under]

let[@library] subs =
  let s = (true : [%v: int]) [@over] in
  (v == s - 1 : [%v: int]) [@under]

let[@library] dummy = (true : [%v: unit]) [@under]
