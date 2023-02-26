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
      [@under]
  in
  (len v s1 : [%v: int batchedq]) [@under]

(* color black *)
let[@library] rbtleaf = (numblack v 0 && noredred v : [%v: int rbtree]) [@under]

let[@library] rbtnode =
  let c = (not v : [%v: bool]) [@over] in
  let sizel = (v >= 0 : [%v: int]) [@over] in
  let _ =
    (numblack v sizel && noredred v && implies (sizel == 0) (hdcolor v true)
      : [%v: int rbtree])
      [@under]
  in
  let _ = (true : [%v: int]) [@under] in
  let sizer = (v == sizel : [%v: int]) [@over] in
  let _ =
    (numblack v sizer && noredred v && implies (sizer == 0) (hdcolor v true)
      : [%v: int rbtree])
      [@under]
  in
  (fun (u : [%forall: int]) ->
     hdcolor v false && implies (u == sizel + 1) (numblack v u && noredred v)
    : [%v: int rbtree])
    [@under]

(* color red *)
let[@library] rbtnode =
  let c = (v : [%v: bool]) [@over] in
  let sizel = (v >= 0 : [%v: int]) [@over] in
  let _ =
    (numblack v sizel && noredred v && hdcolor v false
      : [%v: int rbtree])
      [@under]
  in
  let _ = (true : [%v: int]) [@under] in
  let sizer = (v == sizel : [%v: int]) [@over] in
  let _ =
    (numblack v sizer && noredred v && hdcolor v false
      : [%v: int rbtree])
      [@under]
  in
  (hdcolor v true && numblack v sizel && noredred v : [%v: int rbtree]) [@under]
