let foo (n : int) : int = if n > 0 then 1 else 2

let[@assert] foo =
  let n = (true : [%v: int]) in
  (v == 1 : [%v: int]) [@over]

(* let[@assert] foo = *)
(*   let n = (true : [%v: int]) in *)
(*   (v == 2 : [%v: int]) [@over] *)

(* let[@assert] foo = *)
(*   let n = (true : [%v: int]) in *)
(*   (v == 1 || v == 2 : [%v: int]) [@over] *)

(* let[@assert] foo = *)
(*   let n = (true : [%v: int]) in *)
(*   (v == 1 || v == 2 : [%v: int]) [@under] *)
