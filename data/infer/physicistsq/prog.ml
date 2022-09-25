let check (w : int list) (lenf : int) (f : int list) (lenr : int) (r : int list)
    : int list * int * int list * int * int list =
  let (b : bool) = lenr <= lenf in
  if b then
    match w with
    | [] -> (f, lenf, f, lenr, r)
    | h1 :: t1 -> (w, lenf, f, lenr, r)
  else
    match f with
    | [] ->
        let (f1 : int list) = rev r in
        let (f2 : int list) = append f f1 in
        let (lenf1 : int) = lenf + lenr in
        let (lenr1 : int) = 0 in
        let (r1 : int list) = [] in
        (f1, lenf1, f1, lenr1, r1)
    | h2 :: t2 ->
        let (f3 : int list) = rev r in
        let (f4 : int list) = append f f3 in
        let (lenf2 : int) = lenf + lenr in
        let (lenr2 : int) = 0 in
        let (r2 : int list) = [] in
        (f, lenf2, f4, lenr2, r2)
