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
  iff (ord l u w || ord l w u) (mem l u && mem l w)

let il7 (l : [%forall: int list]) (u : [%forall: int]) =
  implies (len l u) (u >= 0)

let il8 (l : [%forall: int list]) = implies (len l 0) (empty l)

let il9 (l : [%forall: int list]) (u : [%forall: int]) (w : [%forall: int]) =
  implies (len l u && len l w) (u == w)
