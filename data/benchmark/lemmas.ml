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

(* rbtree *)
let rbt1 (l : [%forall: int rbtree]) =
  implies (numblack l 0) (not (hdcolor l false))

let rbt2 (l : [%forall: int rbtree]) (u : [%forall: int]) =
  implies (numblack l u && u > 0) (hdcolor l true || hdcolor l false)

let rbt3 (l : [%forall: int rbtree]) =
  implies (not (hdcolor l true || hdcolor l false)) (numblack l 0)

let rbt4 (l : [%forall: int rbtree]) =
  implies (hdcolor l false) (not (numblack l 0))

let rbt5 (l : [%forall: int rbtree]) = not (hdcolor l true && hdcolor l false)

let rbt8 (l : [%forall: int rbtree]) (u : [%forall: int]) =
  implies (numblack l u) (u >= 0)

let rbt9 (l : [%forall: int rbtree]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (numblack l u && numblack l w) (u == w)

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
