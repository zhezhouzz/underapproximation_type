let rec sorted_list_gen (size : int) (x : int) : int list =
  let (b : bool) = size == 0 in
  if b then []
  else
    let (y : int) = int_gen () in
    if x <= y then
      let (size2 : int) = size - 1 in
      let (l : int list) = sorted_list_gen size2 y in
      let (l2 : int list) = x :: l in
      l2
    else Exn

(* let[@assert] sorted_list_gen = *)
(*   let s = (0 <= v : [%v: int]) [@over] in *)
(*   let x = (true : [%v: int]) [@over] in *)
(*   (lenF v == s : [%v: int list]) [@under] *)

let[@assert] sorted_list_gen =
  let s = (0 <= v : [%v: int]) [@over] in
  let x = (true : [%v: int]) [@over] in
  (len v s && sorted v && (emp v || hd v x) : [%v: int list]) [@under]
