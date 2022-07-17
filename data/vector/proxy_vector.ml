let create (capacity : int) (dummy : int) =
  let x0 = List.nil in
  let x1 = (dummy, x0) in
  let x2 = ref x1 in
  x2

let copy (a : t ref) (b : t ref) : unit =
  let a' = !a in
  let x0 = fst a in
  let x1 = snd a in
  let x3 l h =
    let x31 = List.nil in
    let x32 = List.cons h x31 in
    let x33 = List.cons l x32 in
    x33
  in
  let x4 = List.fold_left x3 x2 x1 in
  let b' = !b in
  let x5 = fst b in
  let x6 = (x5, x4) in
  let x7 = ref x6 in
  x7

let merge_list (a : t ref) (b : t ref) : unit =
  let a' = !a in
  let b' = !b in
  let x0 = fst a' in
  let x1 = snd a' in
  let x2 = fst b' in
  let x3 = snd b' in
  let x4 = List.concat x1 x2 in
  let x5 = List.nil in
  let x6 = (x0, x4) in
  let x7 = (x2, x5) in
  let x8 = a := x6 in
  let x9 = b := x7 in
  x9
