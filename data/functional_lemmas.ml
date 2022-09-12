let mem_list (l : [%forall: int list]) (u : [%exists: int]) (w : [%exists: int])
    =
  implies (u == w) (implies (mem l u) (mem l w))

let hd_list (l : [%forall: int list]) (u : [%exists: int]) (w : [%exists: int])
    =
  implies (u == w) (implies (hd l u) (hd l w))
