let rec insert (s : elem list) (x : elem) : elem list =
  match s with
  | [] -> [ x ]
  | h :: t -> if elem_eq x h then s else x :: insert t h

(* let[@assert] insert = *)
(*   let (i [@ghost]) = (0 < v : [%v: int]) [@over] in *)
(*   let s = (uniq v && len v (i - 1) : [%v: elem list]) [@under] in *)
(*   let x = (not (list_mem s v) : [%v: elem]) [@under] in *)
(*   (uniq v && len v i : [%v: elem list]) [@under] *)

let[@assert] insert =
  let (i [@ghost]) = (0 < v : [%v: int]) [@over] in
  let s = (uniq v && len v (i - 1) : [%v: elem list]) [@under] in
  let x = (true : [%v: elem]) [@under] in
  (uniq v && len v i : [%v: elem list]) [@under]
