(** int list *)

(** basic *)

(* let[@axiom] list_emp_no_hd (l : int list) (x : int) = *)
(*   (emp l) #==> (not (hd l x)) *)

(* let[@axiom] list_emp_no_tl (l : int list) (l1 : int list) = *)
(*   (emp l) #==> (not (tl l l1)) *)

(* let[@axiom] list_emp_ex ((l [@exists]) : int list) = emp l *)

(* let[@axiom] list_no_emp_exists_tl (l : int list) ((l1 [@exists]) : int list) = *)
(*   (not (emp l)) #==> (tl l l1) *)

(* let[@axiom] list_no_emp_exists_hd (l : int list) ((x [@exists]) : int) = *)
(*   (not (emp l)) #==> (hd l x) *)

(* let[@axiom] list_hd_no_emp (l : int list) (x : int) = *)
(*   (hd l x) #==> (not (emp l)) *)

(* let[@axiom] list_tl_no_emp (l : int list) (l1 : int list) = *)
(*   (tl l l1) #==> (not (emp l)) *)

let[@axiom] list_ex_single_uniq (h : int) ((l [@exists]) : int list)
    ((t [@exists]) : int list) =
  hd l h && tl l t && emp t && len l 1 && uniq l

let[@axiom] list_ex_emp_uniq ((l [@exists]) : int list) = emp l && uniq l

let[@axiom] list_single_uniq_destruct (l : int list) ((h [@exists]) : int)
    ((t [@exists]) : int list) =
  (len l 1 && uniq l) #==> (hd l h && tl l t && emp t)

let[@axiom] list_nonemp_uniq_destruct (l : int list) ((h [@exists]) : int)
    ((t [@exists]) : int list) =
  ((not (emp l)) && uniq l) #==> (hd l h && tl l t && uniq t)

(** len *)

let[@axiom] list_len_0_is_emp (l : int list) = (len l 0) #==> (emp l)

(** list_mem *)

(* let[@axiom] list_hd_is_mem (l : int list) (u : int) = *)
(*   (hd l u) #==> (list_mem l u) *)

(* let[@axiom] list_emp_no_mem (l : int list) (u : int) = *)
(*   (emp l) #==> (not (list_mem l u)) *)

(* let[@axiom] list_tl_mem (l : int list) (l1 : int list) (u : int) = *)
(*   (tl l l1 && list_mem l1 u) #==> (list_mem l u) *)

(* let[@axiom] list_cons_mem (l : int list) (l1 : int list) (u : int) = *)
(*   (tl l l1 && list_mem l u) #==> (list_mem l1 u || hd l u) *)

(** unique *)

(* let[@axiom] list_emp_unique (l : int list) = (emp l) #==> (uniq l) *)
(* let[@axiom] list_emp_ex ((l [@exists]) : int list) = uniq l *)

(* let[@axiom] list_tl_unique (l : int list) (l1 : int list) = *)
(*   (tl l l1 && uniq l) #==> (uniq l1) *)

(* let[@axiom] list_hd_unique (l : int list) (l1 : int list) (x : int) = *)
(*   (tl l l1 && uniq l && hd l1 x) #==> (not (list_mem l1 x)) *)

(* let[@axiom] list_unique_hd_tl (l : int list) (l1 : int list) = *)
(*   (tl l l1 && emp l1) #==> (uniq l) *)

(* let[@axiom] list_unique_hd_tl (l : int list) (x : int) (l1 : int list) = *)
(*   (hd l x && tl l l1 && uniq l1 && not (list_mem l1 x)) #==> (uniq l) *)
