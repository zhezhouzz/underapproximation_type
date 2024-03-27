let rec leftisthp_gen (s : int) : int leftisthp =
  if s == 0 then Lhpleaf else Err

let[@assert] leftisthp_gen =
  let s = (0 <= v : [%v: int]) [@over] in
  (leftisthp_depth v s : [%v: int leftisthp]) [@under]
