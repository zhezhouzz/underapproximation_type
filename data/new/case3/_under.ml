let nat_gen =
  let (dummy : [%under: unit -> int]) =
    let (x : [%over: unit]) = (true : [%v: unit]) in
    (true : [%v: int])
  in
  let (y : [%over: unit]) = (true : [%v: unit]) in
  (v >= 0 : [%v: int])

(* should fail *)

(* let nat_gen = *)
(*   let (dummy : [%under: unit -> int]) = *)
(*     let (x : [%over: unit]) = (true : [%v: unit]) in *)
(*     (v >= 5 : [%v: int]) *)
(*   in *)
(*   let (y : [%over: unit]) = (true : [%v: unit]) in *)
(*   (v >= 0 : [%v: int]) *)
