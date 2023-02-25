let rec snoc (lenf : int) (f : int list) (lenr : int) (r : int list) (x : int) :
    int * int list * int * int list =
  let (lenr1 : int) = lenr + 1 in
  let (r1 : int list) = x :: r in
  let (b : bool) = lenr1 <= lenf in
  if b then (lenf, f, lenr1, r1)
  else
    let (r2 : int list) = reverse r1 in
    let (f1 : int list) = concat f r2 in
    let (lenf1 : int) = lenf + lenr1 in
    let (lenr2 : int) = 0 in
    let (r3 : int list) = [] in
    (lenf1, f1, lenr2, r3)
