(* stlc const *)
let[@axiom] stlc_const1 (tm : stlc_term) ((u [@exists]) : int) =
  implies (is_const tm) (is_const_eq tm u && u >= 0)

let[@axiom] stlc_const2 (tm : stlc_term) (u : int) =
  implies (is_const_eq tm u && u >= 0) (is_const tm)

(* stlc type_eq *)

let[@axiom] stlc_type1 (t1 : stlc_ty) (t2 : stlc_ty) ((u [@exists]) : int) =
  implies (type_eq_spec t1 t2) (ty_size t1 u && ty_size t2 u)

let[@axiom] stlc_type2 (t1 : stlc_ty) ((u [@exists]) : int) = ty_size t1 u
let[@axiom] stlc_type3 (t1 : stlc_ty) (u : int) = implies (ty_size t1 u) (u >= 0)

let[@axiom] stlc_type4 (t1 : stlc_ty) (u : int) (w : int) =
  implies (ty_size t1 u && ty_size t1 w) (u == w)

let[@axiom] stlc_type5 (t1 : stlc_ty) (t2 : stlc_ty) (u : int) =
  implies (type_eq_spec t1 t2) (iff (ty_size t1 u) (ty_size t2 u))

let[@axiom] stlc_type6 (t1 : stlc_ty) (t2 : stlc_ty) (u : int) =
  implies
    (not (type_eq_spec t1 t2))
    (implies (ty_size t1 u) (not (ty_size t2 u))
    && implies (ty_size t2 u) (not (ty_size t1 u)))

let[@axiom] stlc_type7 (t1 : stlc_ty) (t2 : stlc_ty) (u : int) (w : int) =
  implies (is_ty_post t1 t2 && ty_size t1 u && ty_size t2 w) (u == w + 1)

let[@axiom] stlc_type8 (t1 : stlc_ty) (t2 : stlc_ty) (u : int) (w : int) =
  implies (is_ty_pre t1 t2 && ty_size t1 u && ty_size t2 w) (u == w + 1)

let[@axiom] stlc_type9 (t1 : stlc_ty) ((u [@exists]) : int) = size t1 u
let[@axiom] stlc_type10 (t1 : stlc_ty) (u : int) = implies (size t1 u) (u >= 0)

let[@axiom] stlc_type11 (t1 : stlc_ty) (u : int) (w : int) =
  implies (size t1 u && size t1 w) (u == w)

let[@axiom] stlc_type12 (t1 : stlc_ty) (u : int) (w : int) =
  implies (ty_size t1 u) (ty_size t1 w)

(* stlc tyctx *)

let[@axiom] stlc_gamma_size1 (t1 : stlc_tyctx) ((u [@exists]) : int) =
  gamma_size t1 u

let[@axiom] stlc_gamma_size2 (t1 : stlc_tyctx) (u : int) =
  implies (gamma_size t1 u) (u >= 0)

let[@axiom] stlc_gamma_size3 (t1 : stlc_tyctx) (u : int) (w : int) =
  implies (gamma_size t1 u && gamma_size t1 w) (u == w)

let[@axiom] stlc_gamma_size4 (t1 : stlc_tyctx) (t2 : stlc_tyctx) (u : int)
    (w : int) =
  implies (is_tyctx_tl t1 t2 && gamma_size t1 u && gamma_size t2 w) (u == w + 1)

let[@axiom] stlc_gamma_size5 (t : stlc_term) (s : int) (u : int) (w : int) =
  implies (is_var_in_range t s u && w == s + 1) (is_var_in_range t s w)

let[@axiom] stlc_gamma_size6 (gamma : stlc_tyctx) (t : stlc_term)
    (tau : stlc_ty) (s : int) (u : int) =
  implies
    (typing_var gamma t tau && u >= 0 && is_var_in_range t u s
   && gamma_size gamma s)
    (is_id_eq t u)

let[@axiom] stlc_gamma_size7 (t : stlc_term) (u : int) =
  implies (is_id_eq t u) (u >= 0)

let[@axiom] stlc_gamma_size8 (t : stlc_term) ((u [@exists]) : int) =
  is_id_eq t u

let[@axiom] stlc_gamma_size9 (t : stlc_term) ((u [@exists]) : int)
    ((w [@exists]) : int) =
  is_var_in_range t u w

let[@axiom] stlc_gamma_size10 (gamma : stlc_tyctx) (t : stlc_term)
    (tau : stlc_ty) (u : int) (w : int) =
  implies
    (is_var_in_range t u w && gamma_size gamma w && u == 0)
    (typing_var gamma t tau)

(* let[@axiom] stlc_gamma_size11 (gamma : stlc_tyctx) (t : stlc_term) *)
(*     (tau : stlc_ty) = *)
(*   typing_var gamma t tau *)

let[@axiom] stlc_gamma_size11 (t : stlc_term) (u : int) = is_id_eq t u

let[@axiom] stlc_gamma_size12 (t : stlc_term) (gamma : int) (tau : int)
    (w : int) =
  is_var_in_range t 0 w

(* typing *)

let[@axiom] stlc_typing1 (gamma : stlc_tyctx) (t : stlc_term) (tau : stlc_ty) =
  implies (typing gamma t tau) (typing_var gamma t tau)

let[@axiom] stlc_typing2 (a : stlc_term) (v : stlc_term) (u : int) =
  implies
    (implies (no_app a) (no_app v) && implies (size_app a u) (size_app v u))
    (a == v)

let[@axiom] stlc_typing3 (gamma : stlc_tyctx) (v : stlc_term) (tau : stlc_ty) =
  iff
    (no_app v && typing gamma v tau)
    (is_const v || is_abs v || typing_var gamma v tau)

(* let[@axiom] stlc_typing4 (t1 : stlc_ty) ((u[@exists]) : int) = size_app t1 u *)

(* let[@axiom] stlc_typing5 (t1 : stlc_ty) (u : int) = *)
(*   implies (size_app t1 u) (u >= 0) *)

(* let[@axiom] stlc_typing6 (t1 : stlc_ty) (u : int) *)
(*     (w : int) = *)
(*   implies (size_app t1 u && size_app t1 w) (u == w) *)

let[@axiom] stlc_typing7 (a : stlc_term) (v : stlc_term) ((u [@exists]) : int) =
  implies
    (implies (no_app a) (no_app v) && implies (size_app a u) (size_app v u))
    (a == v)

(* dec_pair *)

let[@axiom] stlc_dec_pair1 (tau : stlc_ty) (dec : int) (num_app : int) =
  implies (dec_pair tau dec num_app && not (num_app == 0)) (dec > 0)

(* int list *)

let[@axiom] il1 (l : int list) (u : int) = implies (len l 0) (not (mem l u))
let[@axiom] il2 (l : int list) (u : int) = implies (hd l u) (mem l u)

let[@axiom] il3 (l : int list) ((u [@exists]) : int) =
  implies (not (len l 0)) (mem l u)

let[@axiom] il4 (l : int list) ((u [@exists]) : int) =
  implies (not (len l 0)) (hd l u)

let[@axiom] il5 (l : int list) (u : int) (w : int) =
  implies (hd l u && hd l w) (u == w)

let[@axiom] il6 (l : int list) (u : int) (w : int) =
  implies (ord l u w || ord l w u) (mem l u && mem l w)

let[@axiom] il7 (l : int list) (u : int) (w : int) =
  implies (mem l u && mem l w && not (u == w)) (ord l u w || ord l w u)

let[@axiom] il8 (l : int list) (u : int) = implies (len l u) (u >= 0)

let[@axiom] il9 (l : int list) (u : int) (w : int) =
  implies (len l u && len l w) (u == w)

let[@axiom] il10 (l : int list) ((u [@exists]) : int) = len l u
let[@axiom] il11 (l : int list) (u : int) = implies (rng l 0) (not (mem l u))

let[@axiom] il12 (l : int list) ((u [@exists]) : int) =
  implies (not (rng l 0)) (mem l u)

let[@axiom] il13 (l : int list) ((u [@exists]) : int) =
  implies (not (rng l 0)) (hd l u)

let[@axiom] il14 (l : int list) (u : int) = implies (rng l u) (u >= 0)

let[@axiom] il15 (l : int list) (u : int) (w : int) =
  implies (rng l u && rng l w) (u == w)

let[@axiom] il16 (l : int list) ((u [@exists]) : int) = rng l u

(* int list (unique list) *)

let[@axiom] iul1 (l : int ulist) (u : int) = implies (len l 0) (not (mem l u))
let[@axiom] iul2 (l : int ulist) (u : int) = implies (hd l u) (mem l u)

let[@axiom] iul3 (l : int ulist) ((u [@exists]) : int) =
  implies (not (len l 0)) (mem l u)

let[@axiom] iul4 (l : int ulist) ((u [@exists]) : int) =
  implies (not (len l 0)) (hd l u)

let[@axiom] iul5 (l : int ulist) (u : int) (w : int) =
  implies (hd l u && hd l w) (u == w)

let[@axiom] iul6 (l : int ulist) (u : int) (w : int) =
  implies (ord l u w || ord l w u) (mem l u && mem l w)

let[@axiom] iul7 (l : int ulist) (u : int) (w : int) =
  implies (mem l u && mem l w && not (u == w)) (ord l u w || ord l w u)

let[@axiom] iul8 (l : int ulist) (u : int) = implies (len l u) (u >= 0)

let[@axiom] iul9 (l : int ulist) (u : int) (w : int) =
  implies (len l u && len l w) (u == w)

let[@axiom] iul10 (l : int ulist) ((u [@exists]) : int) = len l u
let[@axiom] iul11 (l : int ulist) (u : int) = implies (rng l 0) (not (mem l u))

let[@axiom] iul12 (l : int ulist) ((u [@exists]) : int) =
  implies (not (rng l 0)) (mem l u)

let[@axiom] iul13 (l : int ulist) ((u [@exists]) : int) =
  implies (not (rng l 0)) (hd l u)

let[@axiom] iul14 (l : int ulist) (u : int) = implies (rng l u) (u >= 0)

let[@axiom] iul15 (l : int ulist) (u : int) (w : int) =
  implies (rng l u && rng l w) (u == w)

let[@axiom] iul16 (l : int ulist) ((u [@exists]) : int) = rng l u

(* int tree *)

let[@axiom] it1 (l : int tree) (u : int) = implies (len l 0) (not (mem l u))
let[@axiom] it2 (l : int tree) (u : int) = implies (hd l u) (mem l u)

let[@axiom] it3 (l : int tree) ((u [@exists]) : int) =
  implies (not (len l 0)) (mem l u)

let[@axiom] it4 (l : int tree) ((u [@exists]) : int) =
  implies (not (len l 0)) (hd l u)

let[@axiom] it5 (l : int tree) (u : int) (w : int) =
  implies (hd l u && hd l w) (u == w)

let[@axiom] it6 (l : int tree) (u : int) = implies (len l 0) (sorted l)
let[@axiom] it8 (l : int tree) (u : int) = implies (len l u) (u >= 0)

let[@axiom] it9 (l : int tree) (u : int) (w : int) =
  implies (len l u && len l w) (u == w)

let[@axiom] it10 (l : int tree) ((u [@exists]) : int) = len l u
let[@axiom] it11 (l : int tree) (u : int) = implies (rng l 0) (not (mem l u))

let[@axiom] it12 (l : int tree) ((u [@exists]) : int) =
  implies (not (rng l 0)) (mem l u)

let[@axiom] it13 (l : int tree) ((u [@exists]) : int) =
  implies (not (rng l 0)) (hd l u)

let[@axiom] it14 (l : int tree) (u : int) = implies (rng l u) (u >= 0)

let[@axiom] it15 (l : int tree) (u : int) (w : int) =
  implies (rng l u && rng l w) (u == w)

let[@axiom] it16 (l : int tree) ((u [@exists]) : int) = rng l u
let[@axiom] it17 (l : int tree) = implies (rng l 0) (sorted l)

(* int tree (complet[@axiom]e) *)

let[@axiom] ict1 (l : int ctree) (u : int) = implies (len l 0) (not (mem l u))
let[@axiom] ict2 (l : int ctree) (u : int) = implies (hd l u) (mem l u)

let[@axiom] ict3 (l : int ctree) ((u [@exists]) : int) =
  implies (not (len l 0)) (mem l u)

let[@axiom] ict4 (l : int ctree) ((u [@exists]) : int) =
  implies (not (len l 0)) (hd l u)

let[@axiom] ict5 (l : int ctree) (u : int) (w : int) =
  implies (hd l u && hd l w) (u == w)

let[@axiom] ict6 (l : int ctree) (u : int) = implies (len l 0) (sorted l)
let[@axiom] ict8 (l : int ctree) (u : int) = implies (len l u) (u >= 0)

let[@axiom] ict9 (l : int ctree) (u : int) (w : int) =
  implies (len l u && len l w) (u == w)

let[@axiom] ict10 (l : int ctree) ((u [@exists]) : int) = len l u
let[@axiom] ict11 (l : int ctree) (u : int) = implies (rng l 0) (not (mem l u))

let[@axiom] ict12 (l : int ctree) ((u [@exists]) : int) =
  implies (not (rng l 0)) (mem l u)

let[@axiom] ict13 (l : int ctree) ((u [@exists]) : int) =
  implies (not (rng l 0)) (hd l u)

let[@axiom] ict14 (l : int ctree) (u : int) = implies (rng l u) (u >= 0)

let[@axiom] ict15 (l : int ctree) (u : int) (w : int) =
  implies (rng l u && rng l w) (u == w)

let[@axiom] ict16 (l : int ctree) ((u [@exists]) : int) = rng l u
let[@axiom] ict17 (l : int ctree) = implies (rng l 0) (sorted l)

(* rbtree *)

let[@axiom] rbt0 (l : int rbtree) (u : int) =
  implies (numblack l u) (noredred l)

let[@axiom] rbt1 (l : int rbtree) =
  implies (numblack l 0 && noredred l) (not (hdcolor l false))

let[@axiom] rbt2 (l : int rbtree) (u : int) =
  implies
    (numblack l u && noredred l && u > 0)
    (hdcolor l true || hdcolor l false)

let[@axiom] rbt3 (l : int rbtree) =
  implies (not (hdcolor l true || hdcolor l false)) (numblack l 0 && noredred l)

let[@axiom] rbt4 (l : int rbtree) =
  implies (hdcolor l false) (not (numblack l 0 && noredred l))

let[@axiom] rbt5 (l : int rbtree) = not (hdcolor l true && hdcolor l false)

let[@axiom] rbt8 (l : int rbtree) (u : int) =
  implies (numblack l u && noredred l) (u >= 0)

let[@axiom] rbt9 (l : int rbtree) (u : int) (w : int) =
  implies (numblack l u && numblack l w && noredred l) (u == w)

(* int heap *)

let[@axiom] iheap1 (l : int heap) (u : int) = implies (len l 0) (not (mem l u))
let[@axiom] iheap2 (l : int heap) (u : int) = implies (hd l u) (mem l u)

let[@axiom] iheap3 (l : int heap) ((u [@exists]) : int) =
  implies (not (len l 0)) (mem l u)

let[@axiom] iheap4 (l : int heap) ((u [@exists]) : int) =
  implies (not (len l 0)) (hd l u)

let[@axiom] iheap5 (l : int heap) (u : int) (w : int) =
  implies (hd l u && hd l w) (u == w)

let[@axiom] iheap6 (l : int heap) (u : int) = implies (len l 0) (heap l)
let[@axiom] iheap8 (l : int heap) (u : int) = implies (len l u) (u >= 0)

let[@axiom] iheap9 (l : int heap) (u : int) (w : int) =
  implies (len l u && len l w) (u == w)

let[@axiom] iheap10 (l : int heap) ((u [@exists]) : int) = len l u

(* int set *)

let[@axiom] iset1 (l : int set) (u : int) = implies (len l 0) (not (mem l u))
let[@axiom] iset2 (l : int set) (u : int) = implies (hd l u) (mem l u)

let[@axiom] iset3 (l : int set) ((u [@exists]) : int) =
  implies (not (len l 0)) (mem l u)

let[@axiom] iset4 (l : int set) ((u [@exists]) : int) =
  implies (not (len l 0)) (hd l u)

let[@axiom] iset5 (l : int set) (u : int) (w : int) =
  implies (hd l u && hd l w) (u == w)

let[@axiom] iset6 (l : int set) (u : int) = implies (len l 0) (sorted l)
let[@axiom] iset8 (l : int set) (u : int) = implies (len l u) (u >= 0)

let[@axiom] iset9 (l : int set) (u : int) (w : int) =
  implies (len l u && len l w) (u == w)

let[@axiom] iset10 (l : int set) ((u [@exists]) : int) = len l u
let[@axiom] iset11 (l : int set) (u : int) = implies (rng l 0) (not (mem l u))

let[@axiom] iset12 (l : int set) ((u [@exists]) : int) =
  implies (not (rng l 0)) (mem l u)

let[@axiom] iset13 (l : int set) ((u [@exists]) : int) =
  implies (not (rng l 0)) (hd l u)

let[@axiom] iset14 (l : int set) (u : int) = implies (rng l u) (u >= 0)

let[@axiom] iset15 (l : int set) (u : int) (w : int) =
  implies (rng l u && rng l w) (u == w)

let[@axiom] iset16 (l : int set) ((u [@exists]) : int) = rng l u
let[@axiom] iset17 (l : int set) = implies (rng l 0) (sorted l)

(* int batchedq *)

let[@axiom] ibatchedq8 (l : int batchedq) (u : int) = implies (len l u) (u >= 0)

let[@axiom] ibatchedq9 (l : int batchedq) (u : int) (w : int) =
  implies (len l u && len l w) (u == w)

let[@axiom] ibatchedq10 (l : int batchedq) ((u [@exists]) : int) = len l u

(* int stream *)
let[@axiom] istream1 (l : int stream) (u : int) =
  implies (len l 0) (not (mem l u))

let[@axiom] istream2 (l : int stream) (u : int) = implies (hd l u) (mem l u)

let[@axiom] istream3 (l : int stream) ((u [@exists]) : int) =
  implies (not (len l 0)) (mem l u)

let[@axiom] istream4 (l : int stream) ((u [@exists]) : int) =
  implies (not (len l 0)) (hd l u)

let[@axiom] istream5 (l : int stream) (u : int) (w : int) =
  implies (hd l u && hd l w) (u == w)

let[@axiom] istream6 (l : int stream) (u : int) (w : int) =
  implies (ord l u w || ord l w u) (mem l u && mem l w)

let[@axiom] istream7 (l : int stream) (u : int) (w : int) =
  implies (mem l u && mem l w && not (u == w)) (ord l u w || ord l w u)

let[@axiom] istream8 (l : int stream) (u : int) = implies (len l u) (u >= 0)

let[@axiom] istream9 (l : int stream) (u : int) (w : int) =
  implies (len l u && len l w) (u == w)

let[@axiom] istream10 (l : int stream) ((u [@exists]) : int) = len l u

let[@axiom] istream11 (l : int stream) (u : int) =
  implies (rng l 0) (not (mem l u))

let[@axiom] istream12 (l : int stream) ((u [@exists]) : int) =
  implies (not (rng l 0)) (mem l u)

let[@axiom] istream13 (l : int stream) ((u [@exists]) : int) =
  implies (not (rng l 0)) (hd l u)

let[@axiom] istream14 (l : int stream) (u : int) = implies (rng l u) (u >= 0)

let[@axiom] istream15 (l : int stream) (u : int) (w : int) =
  implies (rng l u && rng l w) (u == w)

let[@axiom] istream16 (l : int stream) ((u [@exists]) : int) = rng l u

(* int stream lazyty *)
let[@axiom] istreamlazy1 (l : int stream lazyty) (u : int) =
  implies (len l 0) (not (mem l u))

let[@axiom] istreamlazy2 (l : int stream lazyty) (u : int) =
  implies (hd l u) (mem l u)

let[@axiom] istreamlazy3 (l : int stream lazyty) ((u [@exists]) : int) =
  implies (not (len l 0)) (mem l u)

let[@axiom] istreamlazy4 (l : int stream lazyty) ((u [@exists]) : int) =
  implies (not (len l 0)) (hd l u)

let[@axiom] istreamlazy5 (l : int stream lazyty) (u : int) (w : int) =
  implies (hd l u && hd l w) (u == w)

let[@axiom] istreamlazy6 (l : int stream lazyty) (u : int) (w : int) =
  implies (ord l u w || ord l w u) (mem l u && mem l w)

let[@axiom] istreamlazy7 (l : int stream lazyty) (u : int) (w : int) =
  implies (mem l u && mem l w && not (u == w)) (ord l u w || ord l w u)

let[@axiom] istreamlazy8 (l : int stream lazyty) (u : int) =
  implies (len l u) (u >= 0)

let[@axiom] istreamlazy9 (l : int stream lazyty) (u : int) (w : int) =
  implies (len l u && len l w) (u == w)

let[@axiom] istreamlazy10 (l : int stream lazyty) ((u [@exists]) : int) =
  len l u

let[@axiom] istreamlazy11 (l : int stream lazyty) (u : int) =
  implies (rng l 0) (not (mem l u))

let[@axiom] istreamlazy12 (l : int stream lazyty) ((u [@exists]) : int) =
  implies (not (rng l 0)) (mem l u)

let[@axiom] istreamlazy13 (l : int stream lazyty) ((u [@exists]) : int) =
  implies (not (rng l 0)) (hd l u)

let[@axiom] istreamlazy14 (l : int stream lazyty) (u : int) =
  implies (rng l u) (u >= 0)

let[@axiom] istreamlazy15 (l : int stream lazyty) (u : int) (w : int) =
  implies (rng l u && rng l w) (u == w)

let[@axiom] istreamlazy16 (l : int stream lazyty) ((u [@exists]) : int) =
  rng l u

(* int bankersq *)

let[@axiom] ibankersq8 (l : int bankersq) (u : int) = implies (len l u) (u >= 0)

let[@axiom] ibankersq9 (l : int bankersq) (u : int) (w : int) =
  implies (len l u && len l w) (u == w)

let[@axiom] ibankersq10 (l : int bankersq) ((u [@exists]) : int) = len l u

(* int leftisthp *)

let[@axiom] ileftisthp8 (l : int leftisthp) (u : int) =
  implies (len l u) (u >= 0)

let[@axiom] ileftisthp9 (l : int leftisthp) (u : int) (w : int) =
  implies (len l u && len l w) (u == w)

let[@axiom] ileftisthp10 (l : int leftisthp) ((u [@exists]) : int) = len l u

(* int unbset *)

let[@axiom] unbset1 (l : int unbset) (u : int) =
  implies (len l 0) (not (mem l u))

let[@axiom] unbset2 (l : int unbset) (u : int) = implies (hd l u) (mem l u)

let[@axiom] unbset3 (l : int unbset) ((u [@exists]) : int) =
  implies (not (len l 0)) (mem l u)

let[@axiom] unbset4 (l : int unbset) ((u [@exists]) : int) =
  implies (not (len l 0)) (hd l u)

let[@axiom] unbset5 (l : int unbset) (u : int) (w : int) =
  implies (hd l u && hd l w) (u == w)

let[@axiom] unbset6 (l : int unbset) (u : int) = implies (len l 0) (sorted l)
let[@axiom] unbset8 (l : int unbset) (u : int) = implies (len l u) (u >= 0)

let[@axiom] unbset9 (l : int unbset) (u : int) (w : int) =
  implies (len l u && len l w) (u == w)

let[@axiom] unbset10 (l : int unbset) ((u [@exists]) : int) = len l u

let[@axiom] unbset11 (l : int unbset) (u : int) =
  implies (rng l 0) (not (mem l u))

let[@axiom] unbset12 (l : int unbset) ((u [@exists]) : int) =
  implies (not (rng l 0)) (mem l u)

let[@axiom] unbset13 (l : int unbset) ((u [@exists]) : int) =
  implies (not (rng l 0)) (hd l u)

let[@axiom] unbset14 (l : int unbset) (u : int) = implies (rng l u) (u >= 0)

let[@axiom] unbset15 (l : int unbset) (u : int) (w : int) =
  implies (rng l u && rng l w) (u == w)

let[@axiom] unbset16 (l : int unbset) ((u [@exists]) : int) = rng l u
let[@axiom] unbset17 (l : int unbset) = implies (rng l 0) (sorted l)
