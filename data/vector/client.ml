let pre (capacity : int) (dummy : int) (a1 : Vector.t) =
  (0 < capacity || capacity == 0) && max_bound v

let client (capacity : int) (dummy : int) (a1 : Vector.t ref) =
  let (x0 : Vector.t ref) = Vector.create dummy capacity in
  let (x1 : unit) = Vector.copy a1 x0 in
  let (x2 : unit) = Vector.merge_right a1 x0 in
  x

let post (capacity : int) (dummy : int) (a1 : Vector.t ref) (a1' : Vector.t ref)
    (nu : Vector.t ref) =
  point_to nu_ref nu && len_zero nu && len_sum !a1 !nu !a1' && disj a1' nu
