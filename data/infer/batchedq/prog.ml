let tail (f : int list) (r : int list) : int list * int list =
  match f with
  | [] -> Exn
  | x :: f1 -> (
      match f1 with
      | [] ->
          let (r1 : int list) = rev r in
          (r1, f1)
      | y :: f2 -> (f1, r))
