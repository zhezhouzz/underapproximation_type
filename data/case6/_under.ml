(* let tail = *)
(*   let x = (v : int) true in *)
(*   let l = (v : int list) true in *)
(*   (v : int list) (fun (u : [%forall: int]) -> implies (mem v u) (u == x)) *)

(* let tail = *)
(*   let x = (v : int) true in *)
(*   let l = (v : int list) true in *)
(*   (v : int list) (fun (u : [%forall: int]) -> not (mem v u)) *)
(* Should fail *)

(* let tail = *)
(*   let x = (v : int) true in *)
(*   let l = (v : int list) true in *)
(*   (v : int list) (not (mem v x)) *)

(* let tail = *)
(*   let x = (v : int) true in *)
(*   let l = (v : int list) true in *)
(*   (v : int list) (fun (u : [%forall: int]) -> mem v u) *)

(* let tail = *)
(*   let x = (v : int) true in *)
(*   let l = (v : int list) true in *)
(*   (v : int list) (fun (u : [%forall: int]) -> implies (x == u) (not (mem v u))) *)

(* let tail = *)
(*   let x = (v : int) true in *)
(*   let l = (v : int list) true in *)
(*   (v : int list) (not (mem v x)) *)

(* (Pre) *)

let tail =
  let x = (v : int) true in
  let l = (v : int list) (fun (w : [%forall: int]) -> false) in
  (v : int list) (fun (u : [%forall: int]) -> not (mem v u))

(* (Infer) *)

(* let tail = *)
(*   let x = (v : int) true in *)
(*   let l = (v : int list) true in *)
(*   (v : int list) (mem v x) *)

(* let tail = *)
(*   let x = (v : int) true in *)
(*   let l = (v : int list) true in *)
(*   (v : int list) (fun (u : [%forall: int]) -> *)
(*       (mem v u && x == u) || (mem v u && x != u) || ((not (mem v u)) && x != u)) *)

(* let tail = *)
(*   let x = (v : int) true in *)
(*   let l = (v : int list) true in *)
(*   (v : int list) (fun (u : [%forall: int]) -> *)
(*       (mem v u && x == u) || (mem v u && x != u)) *)

(* let tail = *)
(*   let x = (v : int) true in *)
(*   let l = (v : int list) true in *)
(*   (v : int list) (fun (u : [%forall: int]) -> *)
(*       ((not (mem v u)) && x != u) || (mem v u && x != u)) *)

(* let tail = *)
(*   let x = (v : int) true in *)
(*   let l = (v : int list) true in *)
(*   (v : int list) (fun (u : [%forall: int]) -> *)
(*       (mem v u && x == u) || ((not (mem v u)) && x != u)) *)

(* [true; false] *)

(* let tail = *)
(*   let x = (v : int) true in *)
(*   let l = (v : int list) true in *)
(*   (v : int list) (fun (w : [%exists: int]) (u : [%forall: int]) -> *)
(*       (mem v w && x == w) && (mem v u || not (x == u))) *)

(* [false; false] *)

(* let tail = *)
(*   let x = (v : int) true in *)
(*   let l = (v : int list) true in *)
(*   (v : int list) (fun (u : [%forall: int]) -> (not (mem v u)) && x != u) *)

(* [ true; true ] *)

(* let tail = *)
(*   let x = (v : int) true in *)
(*   let l = (v : int list) true in *)
(*   (v : int list) (fun (u : [%forall: int]) -> mem v u && x == u) *)
