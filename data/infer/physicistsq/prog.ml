let check (w : int list) (lenf : int) (f : int list) (lenr : int) (r : int list)
    : int * int list * int * int list =
  if lenr <= lenf then
    match w with
    | [] -> (f, lenf, f, lenr, r)
    | h1 :: t1 -> (w, lenf, f, lenr, r)
  else
    match f with
    | [] ->
        let f1 = rev r in
        let f2 = append f f1 in
        let lenf1 = lenf + lenr in
        let lenr1 = 0 in
        let r1 = [] in
        (f1, lenf1, f1, lenr1, r1)
    | h2 :: t2 ->
        let f3 = rev r in
        let f4 = append f f3 in
        let lenf2 = lenf + lenr in
        let lenr2 = 0 in
        let r2 = [] in
        (f, lenf2, f4, lenr2, r2)
