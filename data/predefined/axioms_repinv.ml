(** int list *)

(** len *)

let[@axiom] list_len_leq_zero (l : int list) (n : int) = (len l n) #==> (0 <= n)

let[@axiom] list_destruct_non_emp (l : int list) ((h [@exists]) : int)
    ((t [@exists]) : int list) =
  (not (emp l)) #==> (hd l h && tl l t)

let[@axiom] list_len_not_zero_not_emp (l : int list) (i : int) =
  (len l i && not (i == 0)) #==> (not (emp l))

let[@axiom] list_len_zero_is_emp (l : int list) = (len l 0) #==> (emp l)

let[@axiom] list_len_tl_len (l : int list) (l' : int list) (i : int) =
  (len l (i + 1) && tl l l') #==> (len l' i)

let[@axiom] list_len_tl_len2 (l : int list) (l' : int list) (i : int) =
  (len l i && tl l l') #==> (len l' (i - 1))

let[@axiom] list_sorted_tl_sorted (l : int list) (l' : int list) =
  (sorted l && tl l l') #==> (sorted l')

let[@axiom] list_sorted_fst_second_lt (l : int list) (h : int) (t : int list)
    (h' : int) =
  (sorted l && hd l h && tl l t && hd t h') #==> (h < h')

(** uniq *)

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

(** union *)

let[@axiom] list_singleton_list_sorted (l : int list) =
  (len l 1) #==> (sorted l)

(* let[@axiom] list_singleton_list_uniq (l : int list) = (len l 1) #==> (uniq l) *)

let[@axiom] list_singleton_list_ex (h : int) ((l [@exists]) : int list)
    ((l' [@exists]) : int list) =
  len l 1 && hd l h && tl l l' && len l' 0

let[@axiom] list_len_lenlte (l : int list) (n : int) (m : int) =
  (len l n && n <= m) #==> (lenlte l m)

(** tree basic *)

(* let[@axiom] tree_num_node_exists (tr : int tree) ((n [@exists]) : int) = *)
(*   num_node tr n *)

let[@axiom] tree_destruct_non_leaf (tr : int tree) ((y [@exists]) : int)
    ((l [@exists]) : int tree) ((r [@exists]) : int tree) ((nl [@exists]) : int)
    ((nr [@exists]) : int) =
  (not (leaf tr))
  #==> (root tr y && lch tr l && rch tr r && num_node l nl && num_node r nr)

let[@axiom] tree_num_node_gte_zero (tr : int tree) (n : int) =
  (num_node tr n) #==> (0 <= n)

let[@axiom] tree_num_node_gt_zero_is_not_leaf (tr : int tree) (n : int) =
  (num_node tr n && n > 0) #==> (not (leaf tr))

let[@axiom] tree_num_node_1_ch_leaf (tr : int tree) (tr' : int tree) =
  (num_node tr 1 && (lch tr tr' || rch tr tr')) #==> (leaf tr')

let[@axiom] tree_leaf_num_node_zero (tr : int tree) =
  (leaf tr) #==> (num_node tr 0)

let[@axiom] tree_num_node_zero_leaf (tr : int tree) =
  (num_node tr 0) #==> (leaf tr)

let[@axiom] tree_num_node_ch_sum_plus_1 (tr : int tree) (l : int tree)
    (r : int tree) (n : int) (nl : int) (nr : int) =
  (lch tr l && rch tr r && num_node tr n && num_node l nl && num_node r nr)
  #==> (n == 1 + nl + nr)

let[@axiom] tree_leaf_bst (tr : int tree) = (leaf tr) #==> (bst tr)

let[@axiom] tree_bst_lch_bst (tr : int tree) (tr' : int tree) =
  (bst tr && lch tr tr') #==> (bst tr')

let[@axiom] tree_bst_rch_bst (tr : int tree) (tr' : int tree) =
  (bst tr && rch tr tr') #==> (bst tr')

let[@axiom] tree_bst_destruct_botright_non_leaf (tr : int tree)
    ((tr' [@exists]) : int tree) ((y [@exists]) : int) =
  ((not (leaf tr)) && bst tr) #==> (botright tr tr' y)

let[@axiom] tree_bst_destruct_botright_non_leaf (tr : int tree)
    ((tr' [@exists]) : int tree) ((x [@exists]) : int) ((y [@exists]) : int)
    ((l [@exists]) : int tree) ((r [@exists]) : int tree) ((nl [@exists]) : int)
    ((nr [@exists]) : int) =
  ((not (leaf tr)) && bst tr)
  #==> (botright tr tr' x
       && (not (leaf tr'))
          #==> (root tr' y && lch tr' l && rch tr' r && num_node l nl
              && num_node r nr))

let[@axiom] tree_bst_botright_rest_bst (tr : int tree) (tr' : int tree)
    (y : int) =
  (bst tr && botright tr tr' y) #==> (bst tr')

let[@axiom] tree_bst_botright_rest_bst_num_node_minus_1 (tr : int tree)
    (tr' : int tree) (y : int) (n : int) =
  (num_node tr n && botright tr tr' y) #==> (num_node tr' (n - 1))

let[@axiom] tree_bst_botright_num_node_1 (tr : int tree) (tr' : int tree)
    (y : int) (n : int) =
  (num_node tr 1 && botright tr tr' y) #==> (root tr y)

let[@axiom] tree_leaf_num_node_eq_zero (tr : int tree) (n : int) =
  (leaf tr && num_node tr n) #==> (n == 0)

let[@axiom] tree_num_node_one_any_leaf_child (tr : int tree) (tr' : int tree) =
  (num_node tr 1 && leaf tr') #==> (lch tr tr' && rch tr tr')

let[@axiom] tree_bst_botright_not_root (tr : int tree) (tr' : int tree)
    (y : int) =
  (botright tr tr' y) #==> (not (root tr' y))

let[@axiom] tree_bst_botright_when_lt_root_of_rest (tr : int tree)
    (tr' : int tree) (y : int) (x : int) (l : int tree) (r' : int tree)
    (l' : int tree) (nl' : int) =
  (botright tr tr' y && root tr' x && lch tr' l' && rch tr' r' && y < x
 && lch tr l && num_node l' nl')
  #==> (root tr x && rch tr r' && num_node l (nl' + 1))

let[@axiom] tree_bst_botright_when_gt_root_of_rest (tr : int tree)
    (tr' : int tree) (y : int) (x : int) (r : int tree) (r' : int tree)
    (l' : int tree) (nr' : int) =
  (botright tr tr' y && root tr' x && lch tr' l' && rch tr' r' && x < y
 && rch tr r && num_node r' nr')
  #==> (root tr x && lch tr l' && num_node r (nr' + 1))
