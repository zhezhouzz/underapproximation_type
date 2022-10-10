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

(* int tree *)
(* let it1 (l : [%forall: int tree]) (u : [%forall: int]) = *)
(*   implies (len l 0) (not (mem l u)) *)

(* let it2 (l : [%forall: int tree]) (u : [%forall: int]) = *)
(*   implies (hd l u) (mem l u) *)

(* let it3 (l : [%forall: int tree]) (u : [%exists: int]) = *)
(*   implies (not (len l 0)) (mem l u) *)

(* let it4 (l : [%forall: int tree]) (u : [%exists: int]) = *)
(*   implies (not (len l 0)) (hd l u) *)

(* let it5 (l : [%forall: int tree]) (u : [%forall: int]) (w : [%forall: int]) = *)
(*   implies (hd l u && hd l w) (u == w) *)

(* let it6 (l : [%forall: int tree]) (u : [%forall: int]) (w : [%forall: int]) = *)
(*   implies (ord l u w || ord l w u) (mem l u && mem l w) *)

(* let it7 (l : [%forall: int tree]) (u : [%forall: int]) (w : [%forall: int]) = *)
(*   implies (mem l u && mem l w && not (u == w)) (ord l u w || ord l w u) *)

(* let it8 (l : [%forall: int tree]) (u : [%forall: int]) = *)
(*   implies (len l u) (u >= 0) *)

(* let it9 (l : [%forall: int tree]) (u : [%forall: int]) (w : [%forall: int]) = *)
(*   implies (len l u && len l w) (u == w) *)

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
