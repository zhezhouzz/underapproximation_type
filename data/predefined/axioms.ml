(* int list *)

(* let[@axiom] il8 (l : int list) (u : int) = implies (len l u) (u >= 0) *)

(* let[@axiom] il17 (l : int list) (n : int) (m : int) = *)
(*   implies (len l n && len l m) (n == m) *)

(* let[@axiom] il18 (n : int) ((l [@exists]) : int list) = *)
(*   implies (n >= 0) (len l n) *)

(* let[@axiom] il19 (x : int) (l1 : int list) (l2 : int list) (n : int) = *)
(*   implies (consB x l1 l2) (iff (len l1 n) (len l2 (n + 1))) *)

(* let[@axiom] il20 (x : int) (l : int list) (l1 : int list) (l2 : int list) = *)
(*   implies (consB x l l1 && consB x l l2) (l1 == l2) *)

(* let[@axiom] il21 (x1 : int) (x2 : int) (l1 : int list) (l2 : int list) *)
(*     (l : int list) = *)
(*   implies (consB x1 l1 l && consB x2 l2 l) (x1 == x2 && l1 == l2) *)

(* let[@axiom] il22 (x : int) (l1 : int list) (l2 : int list) = *)
(*   implies (consB x l1 l2) (not (len l2 0)) *)

let[@axiom] il8 (l : int list) = lenF l >= 0

let[@axiom] il18 (n : int) ((l [@exists]) : int list) =
  implies (n >= 0) (lenF l == n)

let[@axiom] il19 (x : int) (l : int list) = 1 + lenF l == lenF (consF x l)
(* let[@axiom] il20 ((l [@exists]) : int list) = lenF l == 0 *)

let[@axiom] il21 (x1 : int) (x2 : int) (l1 : int list) (l2 : int list) =
  implies (consF x1 l1 == consF x2 l2) (x1 == x2 && l1 == l2)

let[@axiom] il22 (x : int) (l : int list) = lenF (consF x l) > 0

let[@axiom] il23 (l : int list) ((x [@exists]) : int)
    ((l1 [@exists]) : int list) =
  implies (lenF l > 0) (l == consF x l1)
