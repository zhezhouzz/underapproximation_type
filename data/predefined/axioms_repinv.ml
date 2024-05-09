(** int list *)

(** len *)

let[@axiom] list_destruct_non_emp (l : int list) ((h [@exists]) : int)
    ((t [@exists]) : int list) =
  (not (emp l)) #==> (hd l h && tl l t)

let[@axiom] list_len_not_zero_not_emp (l : int list) (i : int) =
  (len l i && not (i == 0)) #==> (not (emp l))

let[@axiom] list_len_zero_is_emp (l : int list) = (len l 0) #==> (emp l)

let[@axiom] list_len_tl_len2 (l : int list) (l' : int list) (i : int) =
  (len l i && tl l l') #==> (len l' (i - 1))

let[@axiom] list_sorted_tl_sorted (l : int list) (l' : int list) =
  (sorted l && tl l l') #==> (sorted l')

let[@axiom] list_sorted_fst_second_lt (l : int list) (h : int) (t : int list)
    (h' : int) =
  (sorted l && hd l h && tl l t && hd t h') #==> (h < h')

(** uniq *)

let[@axiom] list_uniq_destruct_non_emp (l : int list) ((h [@exists]) : int)
    ((t [@exists]) : int list) =
  ((not (emp l)) && uniq l) #==> (hd l h && tl l t)

let[@axiom] list_uniq_not_mem (l : int list) ((x [@exists]) : int) =
  (uniq l) #==> (not (list_mem l x))

let[@axiom] list_uniq_tl_uniq (l : int list) (l' : int list) =
  (uniq l && tl l l') #==> (uniq l')

let[@axiom] list_uniq_hd_not_in_tl (l : int list) (h : int) (l' : int list) =
  (uniq l && tl l l' && hd l h) #==> (not (list_mem l' h))

let[@axiom] list_uniq_fst_second_not_eq (l : int list) (h : int) (t : int list)
    (h' : int) =
  (uniq l && hd l h && tl l t && hd t h') #==> (not (h == h'))

let[@axiom] list_mem_hd_or_tl (l : int list) (h : int) (t : int list) (u : int)
    =
  (list_mem l u && hd l h && tl l t) #==> (list_mem t u || u == h)

let[@axiom] list_mem_tl_also_l (l : int list) (t : int list) (u : int) =
  (list_mem t u && tl l t) #==> (list_mem l u)

let[@axiom] list_hd_is_mem (l : int list) (u : int) =
  (hd l u) #==> (list_mem l u)
