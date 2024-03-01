let[@axiom] il_mem (l : int list) (u : int) (w : int) =
  implies (u == w) (implies (mem l u) (mem l w))

let[@axiom] il_hd (l : int list) (u : int) (w : int) =
  implies (u == w) (implies (hd l u) (hd l w))

let[@axiom] il_len (l : int list) (u : int) (w : int) =
  implies (u == w) (implies (len l u) (len l w))
