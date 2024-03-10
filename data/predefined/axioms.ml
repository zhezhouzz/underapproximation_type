(* int list *)

(** list basic *)

let[@axiom] list_emp_no_hd (l : int list) (x : int) =
  (emp l) #==> (not (hd l x))

let[@axiom] list_emp_no_tl (l : int list) (l1 : int list) =
  (emp l) #==> (not (tl l l1))

let[@axiom] list_no_emp_exists_tl (l : int list) ((l1 [@exists]) : int list) =
  (not (emp l)) #==> (tl l l1)

let[@axiom] list_no_emp_exists_hd (l : int list) ((x [@exists]) : int) =
  (not (emp l)) #==> (hd l x)

let[@axiom] list_hd_no_emp (l : int list) (x : int) =
  (hd l x) #==> (not (emp l))

let[@axiom] list_tl_no_emp (l : int list) (l1 : int list) =
  (tl l l1) #==> (not (emp l))

(** len *)

let[@axiom] list_len_geq_0 (l : int list) (n : int) = (len l n) #==> (n > 0)
let[@axiom] list_emp_len_0 (l : int list) = iff (emp l) (len l 0)
let[@axiom] list_ex_len (l : int list) ((n [@exists]) : int) = len l n

let[@axiom] list_tl_len_plus_1 (l : int list) (l1 : int list) (n : int) =
  (tl l l1) #==> (iff (len l1 n) (len l (n + 1)))

(* let[@axiom] il18 (n : int) ((l [@exists]) : int list) = *)
(*   implies (n >= 0) (lenF l == n) *)

(* let[@axiom] il4 (l : int list) ((u [@exists]) : int) = *)
(*   implies (lenF l > 0) (hd l u) *)

(** list_mem *)

let[@axiom] list_hd_is_mem (l : int list) (u : int) =
  (hd l u) #==> (list_mem l u)

let[@axiom] list_emp_no_mem (l : int list) (u : int) =
  (emp l) #==> (not (list_mem l u))

(* let[@axiom] il3 (l : int list) ((u [@exists]) : int) = *)
(*   implies (lenF l > 0) (list_mem l u) *)

let[@axiom] list_tl_mem (l : int list) (l1 : int list) (u : int) =
  (tl l l1 && list_mem l1 u) #==> (list_mem l u)

let[@axiom] list_cons_mem (l : int list) (l1 : int list) (u : int) =
  (tl l l1 && list_mem l u) #==> (list_mem l1 u || hd l u)

(** sorted *)

let[@axiom] list_emp_sorted (l : int list) = (emp l) #==> (sorted l)

let[@axiom] list_tl_sorted (l : int list) (l1 : int list) =
  (tl l l1 && sorted l) #==> (sorted l1)

let[@axiom] list_hd_sorted (l : int list) (l1 : int list) (x : int) (y : int) =
  (tl l l1 && sorted l) #==> (emp l1 || ((hd l1 y && hd l x) #==> (x <= y)))

(* let[@axiom] il26 (x : int) (l : int list) (u : int) = *)
(*   implies (sorted (consF x l)) (sorted l && implies (list_mem l u) (x <= u)) *)

(* let[@axiom] il28 (x : int) (l : int list) (u : int) = *)
(*   implies (sorted (consF x l)) (implies (hd l u) (x <= u) && sorted l) *)

(* let[@axiom] il27 (l : int list) = implies (lenF l == 0) (sorted l) *)

(* list_min *)

(* let[@axiom] il28 (u : int) = list_min (0 : int list) u *)
(* let[@axiom] il29 (l : int list) ((x [@exists]) : int) = list_min l x *)

(* let[@axiom] il26 (x : int) (l : int list) = *)
(*   iff (sorted (consF x l)) (sorted l && list_min l x) *)

(* let[@axiom] il30 (x : int) (n : int) ((l [@exists]) : int list) = *)
(*   implies (n >= 0) (list_min l x && lenF l == n) *)
