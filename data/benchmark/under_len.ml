let[@library] nil = (len v 0 : [%v: int list]) [@under]

let[@library] cons =
  let _ = (true : [%v: int]) [@under] in
  let s = (v >= 0 : [%v: int]) [@over] in
  let _ = (len v s : [%v: int list]) [@under] in
  (fun (u : [%forall: int]) -> implies (u == s + 1) (len v u)
    : [%v: int list])
    [@under]

let[@library] batchedq =
  let s1 = (v >= 0 : [%v: int]) [@over] in
  let _ = (len v s1 : [%v: int list]) [@under] in
  let s2 = (v >= 0 : [%v: int]) [@over] in
  let _ =
    (fun (u : [%forall: int]) -> implies (0 <= u && u <= s1) (len v u)
      : [%v: int list])
      [@over]
  in
  (len v s1 : [%v: int batchedq]) [@under]

let[@library] leaf = (len v 0 : [%v: int tree]) [@under]

let[@library] node =
  let _ = (true : [%v: int]) [@under] in
  let sizel = (v >= 0 : [%v: int]) [@over] in
  let _ = (fun (u : [%forall: int]) -> len v sizel : [%v: int tree]) [@under] in
  let sizer = (v == sizel : [%v: int]) [@over] in
  let _ = (fun (u : [%forall: int]) -> len v sizer : [%v: int tree]) [@under] in
  (fun (u : [%forall: int]) -> implies (u == sizel + 1) (len v u)
    : [%v: int tree])
    [@under]

(* color black *)
let[@library] rbtleaf = (len v 0 : [%v: int rbtree]) [@under]

let[@library] rbtnode =
  let c = (not v : [%v: bool]) [@over] in
  let sizel = (v >= 0 : [%v: int]) [@over] in
  let _ =
    (len v sizel && implies (sizel == 0) (hdcolor v true)
      : [%v: int rbtree])
      [@under]
  in
  let _ = (true : [%v: int]) [@under] in
  let sizer = (v == sizel : [%v: int]) [@over] in
  let _ =
    (len v sizer && implies (sizer == 0) (hdcolor v true)
      : [%v: int rbtree])
      [@under]
  in
  (fun (u : [%forall: int]) ->
     hdcolor v false && implies (u == sizel + 1) (len v u)
    : [%v: int rbtree])
    [@under]

(* color red *)
let[@library] rbtnode =
  let c = (v : [%v: bool]) [@over] in
  let sizel = (v >= 0 : [%v: int]) [@over] in
  let _ = (len v sizel && hdcolor v false : [%v: int rbtree]) [@under] in
  let _ = (true : [%v: int]) [@under] in
  let sizer = (v == sizel : [%v: int]) [@over] in
  let _ = (len v sizer && hdcolor v false : [%v: int rbtree]) [@under] in
  (hdcolor v true && len v sizel : [%v: int rbtree]) [@under]

(* heap *)
let[@library] hempty = (len v 0 : [%v: int heap]) [@under]

let[@library] hnode =
  let root = (true : [%v: int]) [@over] in
  let sizel = (v >= 0 : [%v: int]) [@over] in
  let _ =
    (fun (u : [%forall: int]) ->
       len v sizel && heap v && implies (hd v u) (u <= root)
      : [%v: int heap])
      [@under]
  in
  let sizer = (v == sizel : [%v: int]) [@over] in
  let _ =
    (fun (u : [%forall: int]) ->
       len v sizer && heap v && implies (hd v u) (u <= root)
      : [%v: int heap])
      [@under]
  in
  (fun (u : [%forall: int]) ->
     hd v root && heap v && implies (u == sizel + 1) (len v u)
    : [%v: int heap])
    [@under]

(* stream *)
let[@library] lazyty =
  let s = (v >= 0 : [%v: int]) [@over] in
  let _ = (len v s : [%v: int stream]) [@under] in
  (len v s : [%v: int stream lazyty]) [@under]

let[@library] streamnil = (len v 0 : [%v: int stream]) [@under]

let[@library] streamlazycons =
  let _ = (true : [%v: int]) [@under] in
  let s = (v >= 0 : [%v: int]) [@over] in
  let _ = (len v s : [%v: int stream lazyty]) [@under] in
  (fun (u : [%forall: int]) -> implies (u == s + 1) (len v u)
    : [%v: int stream])
    [@under]

(* bankersq *)
let[@library] bankersq =
  let lenf = (v >= 0 : [%v: int]) [@over] in
  let s1 = (v == lenf : [%v: int]) [@over] in
  let _ = (len v s1 : [%v: int stream]) [@under] in
  let _ = (v >= 0 && v <= lenf : [%v: int]) [@under] in
  let s2 = (v >= 0 : [%v: int]) [@over] in
  let _ =
    (fun (u : [%forall: int]) -> implies (0 <= u && u <= lenf) (len v u)
      : [%v: int stream])
      [@under]
  in
  (len v lenf : [%v: int bankersq]) [@under]

(* leftisthp *)
let[@library] lhpleaf = (len v 0 : [%v: int leftisthp]) [@under]

let[@library] lhpnode =
  let _ = (true : [%v: int]) [@under] in
  let sizel = (v >= 0 : [%v: int]) [@over] in
  let _ = (len v sizel : [%v: int leftisthp]) [@under] in
  let sizer = (v >= 0 : [%v: int]) [@over] in
  let _ =
    (fun (u : [%forall: int]) -> implies (0 <= u && u <= sizel) (len v u)
      : [%v: int leftisthp])
      [@under]
  in
  let _ = (v == sizer : [%v: int]) [@under] in
  (fun (u : [%forall: int]) -> implies (u == sizel + 1) (len v u)
    : [%v: int leftisthp])
    [@under]
