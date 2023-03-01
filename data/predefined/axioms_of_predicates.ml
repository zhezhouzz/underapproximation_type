(* stlc const *)
let stlc_const1 (tm : [%forall: stlc_term]) (u : [%exists: int]) =
  implies (is_const tm) (is_const_eq tm u && u >= 0)

let stlc_const2 (tm : [%forall: stlc_term]) (u : [%forall: int]) =
  implies (is_const_eq tm u && u >= 0) (is_const tm)

(* stlc type_eq *)

let stlc_type1 (t1 : [%forall: stlc_ty]) (t2 : [%forall: stlc_ty])
    (u : [%exists: int]) =
  implies (type_eq_spec t1 t2) (ty_size t1 u && ty_size t2 u)

(* let stlc_type2 (t1 : [%forall: stlc_ty]) (t2 : [%forall: stlc_ty]) *)
(*     (u : [%exists: int]) = *)
(*   implies (type_eq_spec t1 t2) (ty_size t1 u && ty_size t2 u) *)

(* int list *)
let il1 (l : [%forall: int list]) (u : [%forall: int]) =
  implies (len l 0) (not (mem l u))

let il2 (l : [%forall: int list]) (u : [%forall: int]) =
  implies (hd l u) (mem l u)

let il3 (l : [%forall: int list]) (u : [%exists: int]) =
  implies (not (len l 0)) (mem l u)

let il4 (l : [%forall: int list]) (u : [%exists: int]) =
  implies (not (len l 0)) (hd l u)

let il5 (l : [%forall: int list]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (hd l u && hd l w) (u == w)

let il6 (l : [%forall: int list]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (ord l u w || ord l w u) (mem l u && mem l w)

let il7 (l : [%forall: int list]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (mem l u && mem l w && not (u == w)) (ord l u w || ord l w u)

let il8 (l : [%forall: int list]) (u : [%forall: int]) =
  implies (len l u) (u >= 0)

let il9 (l : [%forall: int list]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (len l u && len l w) (u == w)

let il10 (l : [%forall: int list]) (u : [%exists: int]) = len l u

let il11 (l : [%forall: int list]) (u : [%forall: int]) =
  implies (rng l 0) (not (mem l u))

let il12 (l : [%forall: int list]) (u : [%exists: int]) =
  implies (not (rng l 0)) (mem l u)

let il13 (l : [%forall: int list]) (u : [%exists: int]) =
  implies (not (rng l 0)) (hd l u)

let il14 (l : [%forall: int list]) (u : [%forall: int]) =
  implies (rng l u) (u >= 0)

let il15 (l : [%forall: int list]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (rng l u && rng l w) (u == w)

let il16 (l : [%forall: int list]) (u : [%exists: int]) = rng l u

(* int list (unique list) *)

let iul1 (l : [%forall: int ulist]) (u : [%forall: int]) =
  implies (len l 0) (not (mem l u))

let iul2 (l : [%forall: int ulist]) (u : [%forall: int]) =
  implies (hd l u) (mem l u)

let iul3 (l : [%forall: int ulist]) (u : [%exists: int]) =
  implies (not (len l 0)) (mem l u)

let iul4 (l : [%forall: int ulist]) (u : [%exists: int]) =
  implies (not (len l 0)) (hd l u)

let iul5 (l : [%forall: int ulist]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (hd l u && hd l w) (u == w)

let iul6 (l : [%forall: int ulist]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (ord l u w || ord l w u) (mem l u && mem l w)

let iul7 (l : [%forall: int ulist]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (mem l u && mem l w && not (u == w)) (ord l u w || ord l w u)

let iul8 (l : [%forall: int ulist]) (u : [%forall: int]) =
  implies (len l u) (u >= 0)

let iul9 (l : [%forall: int ulist]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (len l u && len l w) (u == w)

let iul10 (l : [%forall: int ulist]) (u : [%exists: int]) = len l u

let iul11 (l : [%forall: int ulist]) (u : [%forall: int]) =
  implies (rng l 0) (not (mem l u))

let iul12 (l : [%forall: int ulist]) (u : [%exists: int]) =
  implies (not (rng l 0)) (mem l u)

let iul13 (l : [%forall: int ulist]) (u : [%exists: int]) =
  implies (not (rng l 0)) (hd l u)

let iul14 (l : [%forall: int ulist]) (u : [%forall: int]) =
  implies (rng l u) (u >= 0)

let iul15 (l : [%forall: int ulist]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (rng l u && rng l w) (u == w)

let iul16 (l : [%forall: int ulist]) (u : [%exists: int]) = rng l u

(* int tree *)

let it1 (l : [%forall: int tree]) (u : [%forall: int]) =
  implies (len l 0) (not (mem l u))

let it2 (l : [%forall: int tree]) (u : [%forall: int]) =
  implies (hd l u) (mem l u)

let it3 (l : [%forall: int tree]) (u : [%exists: int]) =
  implies (not (len l 0)) (mem l u)

let it4 (l : [%forall: int tree]) (u : [%exists: int]) =
  implies (not (len l 0)) (hd l u)

let it5 (l : [%forall: int tree]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (hd l u && hd l w) (u == w)

let it6 (l : [%forall: int tree]) (u : [%forall: int]) =
  implies (len l 0) (sorted l)

let it8 (l : [%forall: int tree]) (u : [%forall: int]) =
  implies (len l u) (u >= 0)

let it9 (l : [%forall: int tree]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (len l u && len l w) (u == w)

let it10 (l : [%forall: int tree]) (u : [%exists: int]) = len l u

let it11 (l : [%forall: int tree]) (u : [%forall: int]) =
  implies (rng l 0) (not (mem l u))

let it12 (l : [%forall: int tree]) (u : [%exists: int]) =
  implies (not (rng l 0)) (mem l u)

let it13 (l : [%forall: int tree]) (u : [%exists: int]) =
  implies (not (rng l 0)) (hd l u)

let it14 (l : [%forall: int tree]) (u : [%forall: int]) =
  implies (rng l u) (u >= 0)

let it15 (l : [%forall: int tree]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (rng l u && rng l w) (u == w)

let it16 (l : [%forall: int tree]) (u : [%exists: int]) = rng l u
let it17 (l : [%forall: int tree]) = implies (rng l 0) (sorted l)

(* int tree (complete) *)

let ict1 (l : [%forall: int ctree]) (u : [%forall: int]) =
  implies (len l 0) (not (mem l u))

let ict2 (l : [%forall: int ctree]) (u : [%forall: int]) =
  implies (hd l u) (mem l u)

let ict3 (l : [%forall: int ctree]) (u : [%exists: int]) =
  implies (not (len l 0)) (mem l u)

let ict4 (l : [%forall: int ctree]) (u : [%exists: int]) =
  implies (not (len l 0)) (hd l u)

let ict5 (l : [%forall: int ctree]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (hd l u && hd l w) (u == w)

let ict6 (l : [%forall: int ctree]) (u : [%forall: int]) =
  implies (len l 0) (sorted l)

let ict8 (l : [%forall: int ctree]) (u : [%forall: int]) =
  implies (len l u) (u >= 0)

let ict9 (l : [%forall: int ctree]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (len l u && len l w) (u == w)

let ict10 (l : [%forall: int ctree]) (u : [%exists: int]) = len l u

let ict11 (l : [%forall: int ctree]) (u : [%forall: int]) =
  implies (rng l 0) (not (mem l u))

let ict12 (l : [%forall: int ctree]) (u : [%exists: int]) =
  implies (not (rng l 0)) (mem l u)

let ict13 (l : [%forall: int ctree]) (u : [%exists: int]) =
  implies (not (rng l 0)) (hd l u)

let ict14 (l : [%forall: int ctree]) (u : [%forall: int]) =
  implies (rng l u) (u >= 0)

let ict15 (l : [%forall: int ctree]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (rng l u && rng l w) (u == w)

let ict16 (l : [%forall: int ctree]) (u : [%exists: int]) = rng l u
let ict17 (l : [%forall: int ctree]) = implies (rng l 0) (sorted l)

(* rbtree *)

let rbt0 (l : [%forall: int rbtree]) (u : [%forall: int]) =
  implies (numblack l u) (noredred l)

let rbt1 (l : [%forall: int rbtree]) =
  implies (numblack l 0 && noredred l) (not (hdcolor l false))

let rbt2 (l : [%forall: int rbtree]) (u : [%forall: int]) =
  implies
    (numblack l u && noredred l && u > 0)
    (hdcolor l true || hdcolor l false)

let rbt3 (l : [%forall: int rbtree]) =
  implies (not (hdcolor l true || hdcolor l false)) (numblack l 0 && noredred l)

let rbt4 (l : [%forall: int rbtree]) =
  implies (hdcolor l false) (not (numblack l 0 && noredred l))

let rbt5 (l : [%forall: int rbtree]) = not (hdcolor l true && hdcolor l false)

let rbt8 (l : [%forall: int rbtree]) (u : [%forall: int]) =
  implies (numblack l u && noredred l) (u >= 0)

let rbt9 (l : [%forall: int rbtree]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (numblack l u && numblack l w && noredred l) (u == w)

(* int heap *)

let iheap1 (l : [%forall: int heap]) (u : [%forall: int]) =
  implies (len l 0) (not (mem l u))

let iheap2 (l : [%forall: int heap]) (u : [%forall: int]) =
  implies (hd l u) (mem l u)

let iheap3 (l : [%forall: int heap]) (u : [%exists: int]) =
  implies (not (len l 0)) (mem l u)

let iheap4 (l : [%forall: int heap]) (u : [%exists: int]) =
  implies (not (len l 0)) (hd l u)

let iheap5 (l : [%forall: int heap]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (hd l u && hd l w) (u == w)

let iheap6 (l : [%forall: int heap]) (u : [%forall: int]) =
  implies (len l 0) (heap l)

let iheap8 (l : [%forall: int heap]) (u : [%forall: int]) =
  implies (len l u) (u >= 0)

let iheap9 (l : [%forall: int heap]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (len l u && len l w) (u == w)

let iheap10 (l : [%forall: int heap]) (u : [%exists: int]) = len l u

(* int set *)

let iset1 (l : [%forall: int set]) (u : [%forall: int]) =
  implies (len l 0) (not (mem l u))

let iset2 (l : [%forall: int set]) (u : [%forall: int]) =
  implies (hd l u) (mem l u)

let iset3 (l : [%forall: int set]) (u : [%exists: int]) =
  implies (not (len l 0)) (mem l u)

let iset4 (l : [%forall: int set]) (u : [%exists: int]) =
  implies (not (len l 0)) (hd l u)

let iset5 (l : [%forall: int set]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (hd l u && hd l w) (u == w)

let iset6 (l : [%forall: int set]) (u : [%forall: int]) =
  implies (len l 0) (sorted l)

let iset8 (l : [%forall: int set]) (u : [%forall: int]) =
  implies (len l u) (u >= 0)

let iset9 (l : [%forall: int set]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (len l u && len l w) (u == w)

let iset10 (l : [%forall: int set]) (u : [%exists: int]) = len l u

let iset11 (l : [%forall: int set]) (u : [%forall: int]) =
  implies (rng l 0) (not (mem l u))

let iset12 (l : [%forall: int set]) (u : [%exists: int]) =
  implies (not (rng l 0)) (mem l u)

let iset13 (l : [%forall: int set]) (u : [%exists: int]) =
  implies (not (rng l 0)) (hd l u)

let iset14 (l : [%forall: int set]) (u : [%forall: int]) =
  implies (rng l u) (u >= 0)

let iset15 (l : [%forall: int set]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (rng l u && rng l w) (u == w)

let iset16 (l : [%forall: int set]) (u : [%exists: int]) = rng l u
let iset17 (l : [%forall: int set]) = implies (rng l 0) (sorted l)

(* int batchedq *)

let ibatchedq8 (l : [%forall: int batchedq]) (u : [%forall: int]) =
  implies (len l u) (u >= 0)

let ibatchedq9 (l : [%forall: int batchedq]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (len l u && len l w) (u == w)

let ibatchedq10 (l : [%forall: int batchedq]) (u : [%exists: int]) = len l u

(* int stream *)
let istream1 (l : [%forall: int stream]) (u : [%forall: int]) =
  implies (len l 0) (not (mem l u))

let istream2 (l : [%forall: int stream]) (u : [%forall: int]) =
  implies (hd l u) (mem l u)

let istream3 (l : [%forall: int stream]) (u : [%exists: int]) =
  implies (not (len l 0)) (mem l u)

let istream4 (l : [%forall: int stream]) (u : [%exists: int]) =
  implies (not (len l 0)) (hd l u)

let istream5 (l : [%forall: int stream]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (hd l u && hd l w) (u == w)

let istream6 (l : [%forall: int stream]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (ord l u w || ord l w u) (mem l u && mem l w)

let istream7 (l : [%forall: int stream]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (mem l u && mem l w && not (u == w)) (ord l u w || ord l w u)

let istream8 (l : [%forall: int stream]) (u : [%forall: int]) =
  implies (len l u) (u >= 0)

let istream9 (l : [%forall: int stream]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (len l u && len l w) (u == w)

let istream10 (l : [%forall: int stream]) (u : [%exists: int]) = len l u

let istream11 (l : [%forall: int stream]) (u : [%forall: int]) =
  implies (rng l 0) (not (mem l u))

let istream12 (l : [%forall: int stream]) (u : [%exists: int]) =
  implies (not (rng l 0)) (mem l u)

let istream13 (l : [%forall: int stream]) (u : [%exists: int]) =
  implies (not (rng l 0)) (hd l u)

let istream14 (l : [%forall: int stream]) (u : [%forall: int]) =
  implies (rng l u) (u >= 0)

let istream15 (l : [%forall: int stream]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (rng l u && rng l w) (u == w)

let istream16 (l : [%forall: int stream]) (u : [%exists: int]) = rng l u

(* int stream lazyty *)
let istreamlazy1 (l : [%forall: int stream lazyty]) (u : [%forall: int]) =
  implies (len l 0) (not (mem l u))

let istreamlazy2 (l : [%forall: int stream lazyty]) (u : [%forall: int]) =
  implies (hd l u) (mem l u)

let istreamlazy3 (l : [%forall: int stream lazyty]) (u : [%exists: int]) =
  implies (not (len l 0)) (mem l u)

let istreamlazy4 (l : [%forall: int stream lazyty]) (u : [%exists: int]) =
  implies (not (len l 0)) (hd l u)

let istreamlazy5 (l : [%forall: int stream lazyty]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (hd l u && hd l w) (u == w)

let istreamlazy6 (l : [%forall: int stream lazyty]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (ord l u w || ord l w u) (mem l u && mem l w)

let istreamlazy7 (l : [%forall: int stream lazyty]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (mem l u && mem l w && not (u == w)) (ord l u w || ord l w u)

let istreamlazy8 (l : [%forall: int stream lazyty]) (u : [%forall: int]) =
  implies (len l u) (u >= 0)

let istreamlazy9 (l : [%forall: int stream lazyty]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (len l u && len l w) (u == w)

let istreamlazy10 (l : [%forall: int stream lazyty]) (u : [%exists: int]) =
  len l u

let istreamlazy11 (l : [%forall: int stream lazyty]) (u : [%forall: int]) =
  implies (rng l 0) (not (mem l u))

let istreamlazy12 (l : [%forall: int stream lazyty]) (u : [%exists: int]) =
  implies (not (rng l 0)) (mem l u)

let istreamlazy13 (l : [%forall: int stream lazyty]) (u : [%exists: int]) =
  implies (not (rng l 0)) (hd l u)

let istreamlazy14 (l : [%forall: int stream lazyty]) (u : [%forall: int]) =
  implies (rng l u) (u >= 0)

let istreamlazy15 (l : [%forall: int stream lazyty]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (rng l u && rng l w) (u == w)

let istreamlazy16 (l : [%forall: int stream lazyty]) (u : [%exists: int]) =
  rng l u

(* int bankersq *)

let ibankersq8 (l : [%forall: int bankersq]) (u : [%forall: int]) =
  implies (len l u) (u >= 0)

let ibankersq9 (l : [%forall: int bankersq]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (len l u && len l w) (u == w)

let ibankersq10 (l : [%forall: int bankersq]) (u : [%exists: int]) = len l u

(* int leftisthp *)

let ileftisthp8 (l : [%forall: int leftisthp]) (u : [%forall: int]) =
  implies (len l u) (u >= 0)

let ileftisthp9 (l : [%forall: int leftisthp]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (len l u && len l w) (u == w)

let ileftisthp10 (l : [%forall: int leftisthp]) (u : [%exists: int]) = len l u

(* int unbset *)

let unbset1 (l : [%forall: int unbset]) (u : [%forall: int]) =
  implies (len l 0) (not (mem l u))

let unbset2 (l : [%forall: int unbset]) (u : [%forall: int]) =
  implies (hd l u) (mem l u)

let unbset3 (l : [%forall: int unbset]) (u : [%exists: int]) =
  implies (not (len l 0)) (mem l u)

let unbset4 (l : [%forall: int unbset]) (u : [%exists: int]) =
  implies (not (len l 0)) (hd l u)

let unbset5 (l : [%forall: int unbset]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (hd l u && hd l w) (u == w)

let unbset6 (l : [%forall: int unbset]) (u : [%forall: int]) =
  implies (len l 0) (sorted l)

let unbset8 (l : [%forall: int unbset]) (u : [%forall: int]) =
  implies (len l u) (u >= 0)

let unbset9 (l : [%forall: int unbset]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (len l u && len l w) (u == w)

let unbset10 (l : [%forall: int unbset]) (u : [%exists: int]) = len l u

let unbset11 (l : [%forall: int unbset]) (u : [%forall: int]) =
  implies (rng l 0) (not (mem l u))

let unbset12 (l : [%forall: int unbset]) (u : [%exists: int]) =
  implies (not (rng l 0)) (mem l u)

let unbset13 (l : [%forall: int unbset]) (u : [%exists: int]) =
  implies (not (rng l 0)) (hd l u)

let unbset14 (l : [%forall: int unbset]) (u : [%forall: int]) =
  implies (rng l u) (u >= 0)

let unbset15 (l : [%forall: int unbset]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (rng l u && rng l w) (u == w)

let unbset16 (l : [%forall: int unbset]) (u : [%exists: int]) = rng l u
let unbset17 (l : [%forall: int unbset]) = implies (rng l 0) (sorted l)
