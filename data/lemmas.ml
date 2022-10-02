(* int list *)

let il1 (l : [%forall: int list]) (u : [%forall: int]) =
  implies (empty l) (not (mem l u))

let il2 (l : [%forall: int list]) (u : [%forall: int]) =
  implies (hd l u) (mem l u)

let il3 (l : [%forall: int list]) (u : [%exists: int]) =
  implies (not (empty l)) (mem l u)

let il4 (l : [%forall: int list]) (u : [%exists: int]) =
  implies (not (empty l)) (hd l u)

let il5 (l : [%forall: int list]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (hd l u && hd l w) (u == w)

let il6 (l : [%forall: int list]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (ord l u w) (mem l u && mem l w)

let il7 (l : [%forall: int list]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (mem l u && mem l w && u != w) (ord l u w || ord l w u)

(* int tree *)
let it1 (l : [%forall: int tree]) (u : [%forall: int]) =
  implies (empty l) (not (mem l u))

let it2 (l : [%forall: int tree]) (u : [%forall: int]) =
  implies (hd l u) (mem l u)

let it3 (l : [%forall: int tree]) (u : [%exists: int]) =
  implies (not (empty l)) (mem l u)

let it4 (l : [%forall: int tree]) (u : [%exists: int]) =
  implies (not (empty l)) (hd l u)

let it5 (l : [%forall: int tree]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (hd l u && hd l w) (u == w)

(* int leftisthp *)
let lhp1 (l : [%forall: int leftisthp]) (u : [%forall: int]) =
  implies (empty l) (not (mem l u))

let lhp2 (l : [%forall: int leftisthp]) (u : [%forall: int]) =
  implies (hd l u) (mem l u)

let lhp3 (l : [%forall: int leftisthp]) (u : [%exists: int]) =
  implies (not (empty l)) (mem l u)

let lhp4 (l : [%forall: int leftisthp]) (u : [%exists: int]) =
  implies (not (empty l)) (hd l u)

let lhp5 (l : [%forall: int leftisthp]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (hd l u && hd l w) (u == w)

(* int pairinghp *)
let php1 (l : [%forall: int pairinghp]) (u : [%forall: int]) =
  implies (empty l) (not (mem l u))

let php2 (l : [%forall: int pairinghp]) (u : [%forall: int]) =
  implies (hd l u) (mem l u)

let php3 (l : [%forall: int pairinghp]) (u : [%exists: int]) =
  implies (not (empty l)) (mem l u)

let php4 (l : [%forall: int pairinghp]) (u : [%exists: int]) =
  implies (not (empty l)) (hd l u)

let php5 (l : [%forall: int pairinghp]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (hd l u && hd l w) (u == w)

(* int pairinghp list *)
let phpl1 (l : [%forall: int pairinghp list]) (u : [%forall: int]) =
  implies (empty l) (not (mem l u))

let phpl2 (l : [%forall: int pairinghp list]) (u : [%forall: int]) =
  implies (hd l u) (mem l u)

let phpl3 (l : [%forall: int pairinghp list]) (u : [%exists: int]) =
  implies (not (empty l)) (mem l u)

let phpl4 (l : [%forall: int pairinghp list]) (u : [%exists: int]) =
  implies (not (empty l)) (hd l u)

let phpl5 (l : [%forall: int pairinghp list]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (hd l u && hd l w) (u == w)

(* int binomialhp *)
let bhp1 (l : [%forall: int binomialhp]) (u : [%forall: int]) =
  implies (empty l) (not (mem l u))

let bhp2 (l : [%forall: int binomialhp]) (u : [%forall: int]) =
  implies (hd l u) (mem l u)

let bhp3 (l : [%forall: int binomialhp]) (u : [%exists: int]) =
  implies (not (empty l)) (mem l u)

let bhp4 (l : [%forall: int binomialhp]) (u : [%exists: int]) =
  implies (not (empty l)) (hd l u)

let bhp5 (l : [%forall: int binomialhp]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (hd l u && hd l w) (u == w)

(* int binomialhp list *)
let bhpl1 (l : [%forall: int binomialhp list]) (u : [%forall: int]) =
  implies (empty l) (not (mem l u))

let bhpl2 (l : [%forall: int binomialhp list]) (u : [%forall: int]) =
  implies (hd l u) (mem l u)

let bhpl3 (l : [%forall: int binomialhp list]) (u : [%exists: int]) =
  implies (not (empty l)) (mem l u)

let bhpl4 (l : [%forall: int binomialhp list]) (u : [%exists: int]) =
  implies (not (empty l)) (hd l u)

let bhpl5 (l : [%forall: int binomialhp list]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (hd l u && hd l w) (u == w)

(* int rbset *)
let rbs1 (l : [%forall: int rbset]) (u : [%forall: int]) =
  implies (empty l) (not (mem l u))

let rbs2 (l : [%forall: int rbset]) (u : [%forall: int]) =
  implies (hd l u) (mem l u)

let rbs3 (l : [%forall: int rbset]) (u : [%exists: int]) =
  implies (not (empty l)) (mem l u)

let rbs4 (l : [%forall: int rbset]) (u : [%exists: int]) =
  implies (not (empty l)) (hd l u)

let rbs5 (l : [%forall: int rbset]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (hd l u && hd l w) (u == w)

(* int skewhp *)
let shp1 (l : [%forall: int skewhp]) (u : [%forall: int]) =
  implies (empty l) (not (mem l u))

let shp2 (l : [%forall: int skewhp]) (u : [%forall: int]) =
  implies (hd l u) (mem l u)

let shp3 (l : [%forall: int skewhp]) (u : [%exists: int]) =
  implies (not (empty l)) (mem l u)

let shp4 (l : [%forall: int skewhp]) (u : [%exists: int]) =
  implies (not (empty l)) (hd l u)

let shp5 (l : [%forall: int skewhp]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (hd l u && hd l w) (u == w)

(* int skewhp list *)
let shpl1 (l : [%forall: int skewhp list]) (u : [%forall: int]) =
  implies (empty l) (not (mem l u))

let shpl2 (l : [%forall: int skewhp list]) (u : [%forall: int]) =
  implies (hd l u) (mem l u)

let shpl3 (l : [%forall: int skewhp list]) (u : [%exists: int]) =
  implies (not (empty l)) (mem l u)

let shpl4 (l : [%forall: int skewhp list]) (u : [%exists: int]) =
  implies (not (empty l)) (hd l u)

let shpl5 (l : [%forall: int skewhp list]) (u : [%forall: int])
    (w : [%forall: int]) =
  implies (hd l u && hd l w) (u == w)

(* int splayhp *)
let sphp1 (l : [%forall: int splayhp]) (u : [%forall: int]) =
  implies (empty l) (not (mem l u))

let sphp2 (l : [%forall: int splayhp]) (u : [%forall: int]) =
  implies (hd l u) (mem l u)

let sphp3 (l : [%forall: int splayhp]) (u : [%exists: int]) =
  implies (not (empty l)) (mem l u)

let sphp4 (l : [%forall: int splayhp]) (u : [%exists: int]) =
  implies (not (empty l)) (hd l u)

let sphp5 (l : [%forall: int splayhp]) (u : [%forall: int]) (w : [%forall: int])
    =
  implies (hd l u && hd l w) (u == w)

(* int unbset *)
let us1 (l : [%forall: int unbset]) (u : [%forall: int]) =
  implies (empty l) (not (mem l u))

let us2 (l : [%forall: int unbset]) (u : [%forall: int]) =
  implies (hd l u) (mem l u)

let us3 (l : [%forall: int unbset]) (u : [%exists: int]) =
  implies (not (empty l)) (mem l u)

let us4 (l : [%forall: int unbset]) (u : [%exists: int]) =
  implies (not (empty l)) (hd l u)

let us5 (l : [%forall: int unbset]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (hd l u && hd l w) (u == w)
