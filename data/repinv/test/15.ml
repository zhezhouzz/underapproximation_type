let insert (n : int) (s : int list) (x : int) : int list =
  match s with
  | [] -> [ x ]
  | h :: t ->
      if x == h then h :: t
      else if x < h then x :: h :: t
      else h :: insert (n - 1) t x

let[@assert] insert =
  let n = (v >= 0 : [%v: int]) [@over] in
  let s = (len v n : [%v: int list]) [@under] in
  let x = (true : [%v: int]) [@over] in
  (uniq v : [%v: int list]) [@over]

(* let[@disprove] insert = *)
(*   let s = (uniq v : [%v: int list]) [@over] in *)
(*   let x = (true : [%v: int]) [@over] in *)
(*   (uniq v : [%v: int list]) [@over] *)
