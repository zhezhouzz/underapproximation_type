let il_mem (l : [%forall: int list]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (u == w) (implies (mem l u) (mem l w))

let il_hd (l : [%forall: int list]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (u == w) (implies (hd l u) (hd l w))

let il_ord (l : [%forall: int list]) (u : [%forall: int]) (w : [%forall: int])
    (p : [%forall: int]) (q : [%forall: int]) =
  implies (u == w && p == q) (implies (ord l u p) (ord l w q))

let it_mem (l : [%forall: int_tree]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (u == w) (implies (mem l u) (mem l w))

let it_hd (l : [%forall: int_tree]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (u == w) (implies (hd l u) (hd l w))

(* leftisthp *)

let lhp_mem (l : [%forall: int leftisthp]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (u == w) (implies (mem l u) (mem l w))

let lhp_hd (l : [%forall: int leftisthp]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (u == w) (implies (hd l u) (hd l w))

(* pairinghp *)

let php_mem (l : [%forall: int pairinghp]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (u == w) (implies (mem l u) (mem l w))

let php_hd (l : [%forall: int pairinghp]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (u == w) (implies (hd l u) (hd l w))

(* binomialhp *)

let bhp_mem (l : [%forall: int binomialhp]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (u == w) (implies (mem l u) (mem l w))

let bhp_hd (l : [%forall: int binomialhp]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (u == w) (implies (hd l u) (hd l w))

let bhpl_mem (l : [%forall: int binomialhp list]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (u == w) (implies (mem l u) (mem l w))

let bhpl_hd (l : [%forall: int binomialhp list]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (u == w) (implies (hd l u) (hd l w))

(* skewhp *)

let shp_mem (l : [%forall: int skewhp]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (u == w) (implies (mem l u) (mem l w))

let shp_hd (l : [%forall: int skewhp]) (u : [%forall: int]) (w : [%forall: int])
    =
  implies (u == w) (implies (hd l u) (hd l w))

let shpl_mem (l : [%forall: int skewhp list]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (u == w) (implies (mem l u) (mem l w))

let shpl_hd (l : [%forall: int skewhp list]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (u == w) (implies (hd l u) (hd l w))

(* splayhp *)

let sphp_mem (l : [%forall: int splayhp]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (u == w) (implies (mem l u) (mem l w))

let sphp_hd (l : [%forall: int splayhp]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (u == w) (implies (hd l u) (hd l w))

(* unbset *)

let us_mem (l : [%forall: int unbset]) (u : [%forall: int]) (w : [%forall: int])
    =
  implies (u == w) (implies (mem l u) (mem l w))

let us_hd (l : [%forall: int unbset]) (u : [%forall: int]) (w : [%forall: int])
    =
  implies (u == w) (implies (hd l u) (hd l w))
