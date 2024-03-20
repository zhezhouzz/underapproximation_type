let rec sized_list_gen (s : int) : int list =
  if sizecheck s then []
  else if bool_gen () then []
  else int_gen () :: sized_list_gen (subs s)

let[@assert] sized_list_gen =
  let s = (0 <= v : [%v: int]) [@over] in
  (fun ((n [@exists]) : int) -> len v n && n <= s : [%v: int list]) [@under]
