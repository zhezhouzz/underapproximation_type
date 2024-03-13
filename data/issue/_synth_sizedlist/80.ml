let rec goal (size : int) : int list =
  if sizecheck size then []
  else if bool_gen () then
    subs (gt_eq_int_gen size) :: subs size :: goal (subs size)
  else [ subs (gt_eq_int_gen (subs size)) ]

let[@assert] goal =
  let s = (0 <= v : [%v: int]) [@over] in
  (fun ((n [@exists]) : int) -> len v n && n <= s : [%v: int list]) [@under]
