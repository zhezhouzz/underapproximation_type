let list_gen_wrong (len_gen : unit -> int) (elem_gen : unit -> int) (r : unit) =
  let len = len_gen () in
  let elem = elem_gen () in
  let rec aux (n : int) = if n == 0 then [] else elem :: aux (n - 1) in
  aux len

let list_gen_correct (len_gen : unit -> int) (elem_gen : unit -> int) (r : unit)
    =
  let len = len_gen () in
  let rec aux n = if n == 0 then [] else elem_gen () :: aux (n - 1) in
  aux len

let rec bst_gen (low : int) (high : int) =
  if low + 1 >= high then Leaf
  else
    Leaf
    <+>
    let x = random (low + 1) (high - 1) in
    let l = bst_gen low x in
    let r = bst_gen x high in
    Node (x, l, r)

let rec bst_gen (size : int) (low : int) (high : int) =
  if size == 0 then Leaf
  else if low + 1 >= high then Leaf
  else
    Leaf
    <+>
    let x = random (low + 1) (high - 1) in
    let l = bst_gen (size - 1) low x in
    let r = bst_gen (size - 1) x high in
    Node (x, l, r)

let bst_gen (r : unit) : int tree =
  let low = nat_gen () in
  let high = nat_gen () in
  let rec aux [%n: nat] [%m: nat] (low : [%(v:int): v = n]) (high : [%(v:int): v = n + m])
  : [%(v:int): forall u, mem v u => n <= u /\ u <= m]
                        =
    if low + 1 >= high then Leaf
    else
      Leaf
      <+>
      let x = random (low + 1) (high - 1) in
      let l = aux low x in
      let r = aux x high in
      Node (x, l, r)
  in
  aux low high
