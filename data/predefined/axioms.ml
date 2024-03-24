(** int list *)

(** basic *)

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

let[@axiom] list_len_geq_0 (l : int list) (n : int) = (len l n) #==> (n >= 0)
(* let[@axiom] list_len_0_emp (l : int list) = (emp l) #==> (len l 0) *)

let[@axiom] list_emp_len_0 (l : int list) (n : int) =
  (emp l && len l n) #==> (n == 0)

let[@axiom] list_positive_len_is_not_emp (l : int list) (n : int) =
  (len l n && n > 0) #==> (not (emp l))

let[@axiom] list_tl_len_plus_1 (l : int list) (l1 : int list) (n : int) =
  (tl l l1) #==> (iff (len l1 n) (len l (n + 1)))

(** list_mem *)

let[@axiom] list_hd_is_mem (l : int list) (u : int) =
  (hd l u) #==> (list_mem l u)

let[@axiom] list_emp_no_mem (l : int list) (u : int) =
  (emp l) #==> (not (list_mem l u))

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

(** unique *)

let[@axiom] list_emp_unique (l : int list) = (emp l) #==> (uniq l)

let[@axiom] list_tl_unique (l : int list) (l1 : int list) =
  (tl l l1 && uniq l) #==> (uniq l1)

let[@axiom] list_hd_unique (l : int list) (l1 : int list) (x : int) =
  (tl l l1 && uniq l && hd l1 x) #==> (not (list_mem l1 x))

(** int tree *)

(** basic *)

let[@axiom] tree_leaf_no_root (l : int tree) (x : int) =
  (leaf l) #==> (not (root l x))

let[@axiom] tree_leaf_no_ch (l : int tree) (l1 : int tree) =
  (leaf l) #==> (not (lch l l1 || rch l l1))

let[@axiom] tree_no_leaf_exists_ch (l : int tree) ((l1 [@exists]) : int tree)
    ((l2 [@exists]) : int tree) =
  (not (leaf l)) #==> (lch l l1 && rch l l2)

let[@axiom] tree_no_leaf_exists_root (l : int tree) ((x [@exists]) : int) =
  (not (leaf l)) #==> (root l x)

let[@axiom] tree_root_no_leaf (l : int tree) (x : int) =
  (root l x) #==> (not (leaf l))

let[@axiom] tree_ch_no_leaf (l : int tree) (l1 : int tree) =
  (lch l l1 || rch l l1) #==> (not (leaf l))

(** depth *)

let[@axiom] tree_depth_geq_0 (l : int tree) (n : int) = (depth l n) #==> (n >= 0)

let[@axiom] tree_leaf_depth_0 (l : int tree) (n : int) =
  (leaf l && depth l n) #==> (n == 0)

let[@axiom] tree_positive_depth_is_not_leaf (l : int tree) (n : int) =
  (depth l n && n > 0) #==> (not (leaf l))

let[@axiom] tree_ch_depth_ex (l : int tree) (l1 : int tree) (n : int)
    ((n1 [@exists]) : int) =
  ((lch l l1 || rch l l1) && depth l n) #==> (depth l1 n1)

let[@axiom] tree_ch_depth_minus_1 (l : int tree) (l1 : int tree) (n : int)
    (n1 : int) =
  ((lch l l1 || rch l l1) && depth l n && depth l1 n1) #==> (n1 <= n - 1)

(** tree_mem *)

let[@axiom] tree_root_mem (l : int tree) (x : int) =
  (root l x) #==> (tree_mem l x)

let[@axiom] tree_mem_lch_mem (l : int tree) (l1 : int tree) (x : int) =
  (lch l l1 && tree_mem l1 x) #==> (tree_mem l x)

let[@axiom] tree_mem_rch_mem (l : int tree) (l1 : int tree) (x : int) =
  (rch l l1 && tree_mem l1 x) #==> (tree_mem l x)

(** bst *)

let[@axiom] tree_leaf_bst (l : int tree) = (leaf l) #==> (bst l)

let[@axiom] tree_bst_lch_bst (l : int tree) (l1 : int tree) =
  (lch l l1 && bst l) #==> (bst l1)

let[@axiom] tree_bst_rch_bst (l : int tree) (l1 : int tree) =
  (rch l l1 && bst l) #==> (bst l1)

let[@axiom] tree_bst_lch_mem_lt_root (l : int tree) (l1 : int tree) (x : int)
    (y : int) =
  (bst l && lch l l1 && root l x && tree_mem l1 y) #==> (y < x)

let[@axiom] tree_bst_rch_mem_gt_root (l : int tree) (l1 : int tree) (x : int)
    (y : int) =
  (bst l && rch l l1 && root l x && tree_mem l1 y) #==> (x < y)

(** heap *)

let[@axiom] tree_heap_lch_heap (l : int tree) (l1 : int tree) =
  (lch l l1 && heap l) #==> (heap l1)

let[@axiom] tree_heap_rch_heap (l : int tree) (l1 : int tree) =
  (rch l l1 && heap l) #==> (heap l1)

let[@axiom] tree_heap_root_lt_lch_root (l : int tree) (l1 : int tree) (x : int)
    (y : int) =
  (heap l && lch l l1 && root l x && root l1 y) #==> (y < x)

let[@axiom] tree_heap_root_rt_rch_root (l : int tree) (l1 : int tree) (x : int)
    (y : int) =
  (heap l && rch l l1 && root l x && root l1 y) #==> (y < x)

(** complete *)

let[@axiom] tree_complete_lch_complete (l : int tree) (l1 : int tree) =
  (lch l l1 && complete l) #==> (complete l1)

let[@axiom] tree_complete_rch_complete (l : int tree) (l1 : int tree) =
  (rch l l1 && complete l) #==> (complete l1)

let[@axiom] tree_complete_lch_depth_minus_1 (l : int tree) (l1 : int tree)
    (n : int) =
  (lch l l1 && complete l && depth l n) #==> (depth l1 (n - 1))

let[@axiom] tree_complete_rch_depth_minus_1 (l : int tree) (l1 : int tree)
    (n : int) =
  (rch l l1 && complete l && depth l n) #==> (depth l1 (n - 1))

(** int stream *)

let[@axiom] stream_stream_emp_no_stream_hd (l : int stream) (x : int) =
  (stream_emp l) #==> (not (stream_hd l x))

let[@axiom] stream_stream_emp_no_stream_tl (l : int stream) (l1 : int stream) =
  (stream_emp l) #==> (not (stream_tl l l1))

let[@axiom] stream_no_stream_emp_exists_stream_tl (l : int stream)
    ((l1 [@exists]) : int stream) =
  (not (stream_emp l)) #==> (stream_tl l l1)

let[@axiom] stream_no_stream_emp_exists_stream_hd (l : int stream)
    ((x [@exists]) : int) =
  (not (stream_emp l)) #==> (stream_hd l x)

let[@axiom] stream_stream_hd_no_stream_emp (l : int stream) (x : int) =
  (stream_hd l x) #==> (not (stream_emp l))

let[@axiom] stream_stream_tl_no_stream_emp (l : int stream) (l1 : int stream) =
  (stream_tl l l1) #==> (not (stream_emp l))

(** stream_len *)

let[@axiom] stream_stream_len_geq_0 (l : int stream) (n : int) =
  (stream_len l n) #==> (n >= 0)

let[@axiom] stream_stream_len_leq_0_emp_stream (l : int stream) (n : int) =
  (stream_len l n && n <= 0) #==> (stream_emp l)

let[@axiom] stream_stream_emp_stream_len_0 (l : int stream) (n : int) =
  (stream_emp l && stream_len l n) #==> (n == 0)

let[@axiom] stream_positive_stream_len_is_not_stream_emp (l : int stream)
    (n : int) =
  (stream_len l n && n > 0) #==> (not (stream_emp l))

let[@axiom] stream_stream_tl_stream_len_plus_1 (l : int stream)
    (l1 : int stream) (n : int) =
  (stream_tl l l1) #==> (iff (stream_len l1 n) (stream_len l (n + 1)))

(** bankersq *)

let[@axiom] bankersq_destruct (q : int bankersq) ((lenf [@exists]) : int)
    ((f [@exists]) : int stream) ((lenr [@exists]) : int)
    ((r [@exists]) : int stream) =
  lenr >= 0 && lenr <= lenf && stream_len f lenf && stream_len r lenr
  && bankersq1 q lenf && bankersq2 q f && bankersq3 q lenr && bankersq4 q r

let[@axiom] bankersq1_len (q : int bankersq) (n : int) (m : int) =
  (bankersq1 q n && bankersq_len q m) #==> (n == m)

(** batchedq *)

let[@axiom] batchedq_destruct (q : int batchedq) ((lenf [@exists]) : int)
    ((f [@exists]) : int list) ((lenr [@exists]) : int)
    ((r [@exists]) : int list) =
  batchedq1 q f && batchedq2 q r && lenr >= 0 && len f lenf && len r lenr

let[@axiom] batchedq_f_geq_r (q : int batchedq) (lenf : int) (f : int list)
    (lenr : int) (r : int list) =
  (batchedq1 q f && batchedq2 q r && len f lenf && len r lenr)
  #==> (lenf >= lenr)

let[@axiom] batchedq1_len (q : int batchedq) (f : int list) (n : int) =
  (batchedq1 q f) #==> (iff (batchedq_len q n) (len f n))

(** int leafisthp *)

(** basic *)

let[@axiom] leftisthp_leftisthp_leaf_no_leftisthp_root (l : int leftisthp)
    (x : int) =
  (leftisthp_leaf l) #==> (not (leftisthp_root l x))

let[@axiom] leftisthp_leftisthp_leaf_no_ch (l : int leftisthp)
    (l1 : int leftisthp) =
  (leftisthp_leaf l) #==> (not (leftisthp_lch l l1 || leftisthp_rch l l1))

let[@axiom] leftisthp_no_leftisthp_leaf_exists_ch (l : int leftisthp)
    ((l1 [@exists]) : int leftisthp) ((l2 [@exists]) : int leftisthp)
    ((r [@exists]) : int) =
  (not (leftisthp_leaf l))
  #==> (leftisthp_lch l l1 && leftisthp_rch l l2 && leftisthp_rank l r
       && leftisthp_depth l2 (r - 1)
       && r > 0)

let[@axiom] leftisthp_no_leftisthp_leaf_exists_leftisthp_root
    (l : int leftisthp) ((x [@exists]) : int) =
  (not (leftisthp_leaf l)) #==> (leftisthp_root l x)

let[@axiom] leftisthp_leftisthp_root_no_leftisthp_leaf (l : int leftisthp)
    (x : int) =
  (leftisthp_root l x) #==> (not (leftisthp_leaf l))

let[@axiom] leftisthp_ch_no_leftisthp_leaf (l : int leftisthp)
    (l1 : int leftisthp) =
  (leftisthp_lch l l1 || leftisthp_rch l l1) #==> (not (leftisthp_leaf l))

(** leftisthp_depth *)

let[@axiom] leftisthp_leftisthp_depth_0_leftisthp_leaf (l : int leftisthp) =
  (leftisthp_depth l 0) #==> (leftisthp_leaf l)

let[@axiom] leftisthp_right_depth_leq_depth (l : int leftisthp) (n : int)
    (r : int) =
  (leftisthp_depth l n && leftisthp_rank l r) #==> (r <= n)

let[@axiom] leftisthp_right_depth_leq_depth (l : int leftisthp)
    (l1 : int leftisthp) (r : int) =
  (leftisthp_rch l l1 && leftisthp_rank l r) #==> (leftisthp_depth l1 (r - 1))

let[@axiom] leftisthp_leftisthp_depth_geq_0 (l : int leftisthp) (n : int) =
  (leftisthp_depth l n) #==> (n >= 0)

let[@axiom] leftisthp_leftisthp_leaf_leftisthp_depth_0 (l : int leftisthp)
    (n : int) =
  (leftisthp_leaf l && leftisthp_depth l n) #==> (n == 0)

let[@axiom] leftisthp_positive_leftisthp_depth_is_not_leftisthp_leaf
    (l : int leftisthp) (n : int) =
  (leftisthp_depth l n && not (n == 0)) #==> (not (leftisthp_leaf l))

let[@axiom] leftisthp_leftisthp_depth_ch_leftisthp_depth_minus_1
    (tr : int leftisthp) (tr1 : int leftisthp) (n : int) =
  (leftisthp_lch tr tr1)
  #==> (iff (leftisthp_depth tr (n + 1)) (leftisthp_depth tr1 n))

(** int rbtree *)

(** basic *)

let[@axiom] rbtree_rb_leaf_no_rb_root (l : int rbtree) (x : int) =
  (rb_leaf l) #==> (not (rb_root l x))

let[@axiom] rbtree_rb_leaf_no_rb_root_color (l : int rbtree) (x : bool) =
  (rb_leaf l) #==> (not (rb_root_color l x))

let[@axiom] rbtree_rb_leaf_no_ch (l : int rbtree) (l1 : int rbtree) =
  (rb_leaf l) #==> (not (rb_lch l l1 || rb_rch l l1))

let[@axiom] rbtree_no_rb_leaf_exists_ch (l : int rbtree)
    ((l1 [@exists]) : int rbtree) ((l2 [@exists]) : int rbtree) =
  (not (rb_leaf l)) #==> (rb_lch l l1 && rb_rch l l2)

let[@axiom] rbtree_no_rb_leaf_exists_rb_root (l : int rbtree)
    ((x [@exists]) : int) =
  (not (rb_leaf l)) #==> (rb_root l x)

let[@axiom] rbtree_no_rb_leaf_exists_rb_root_color (l : int rbtree)
    ((x [@exists]) : bool) =
  (not (rb_leaf l)) #==> (rb_root_color l x)

let[@axiom] rbtree_rb_root_no_rb_leaf (l : int rbtree) (x : int) =
  (rb_root l x) #==> (not (rb_leaf l))

let[@axiom] rbtree_rb_root_color_no_rb_leaf (l : int rbtree) (x : bool) =
  (rb_root_color l x) #==> (not (rb_leaf l))

let[@axiom] rbtree_ch_no_rb_leaf (l : int rbtree) (l1 : int rbtree) =
  (rb_lch l l1 || rb_rch l l1) #==> (not (rb_leaf l))

(** num_black *)

let[@axiom] rbtree_num_black_geq_0 (l : int rbtree) (n : int) =
  (num_black l n) #==> (n >= 0)

let[@axiom] rbtree_rb_leaf_num_black_0 (l : int rbtree) (n : int) =
  (rb_leaf l && num_black l n) #==> (n == 0)

let[@axiom] rbtree_positive_num_black_is_not_rb_leaf (l : int rbtree) (n : int)
    =
  (num_black l n && n > 0) #==> (not (rb_leaf l))

let[@axiom] rbtree_ch_num_black_plus_1 (l : int rbtree) (l1 : int rbtree)
    (n : int) ((n1 [@exists]) : int) =
  ((rb_lch l l1 || rb_rch l l1) && num_black l n)
  #==> (num_black l1 n1 && n1 <= n - 1)

let[@axiom] num_black_root_black_lt_minus_1 (v : int rbtree) (lt : int rbtree)
    (h : int) =
  (rb_root_color v false && num_black v h && rb_lch v lt)
  #==> (num_black lt (h - 1))

let[@axiom] num_black_root_black_rt_minus_1 (v : int rbtree) (rt : int rbtree)
    (h : int) =
  (rb_root_color v false && num_black v h && rb_rch v rt)
  #==> (num_black rt (h - 1))

let[@axiom] num_black_root_red_lt_same (v : int rbtree) (lt : int rbtree)
    (h : int) =
  (rb_root_color v true && num_black v h && rb_lch v lt) #==> (num_black lt h)

let[@axiom] num_black_root_red_rt_same (v : int rbtree) (rt : int rbtree)
    (h : int) =
  (rb_root_color v true && num_black v h && rb_rch v rt) #==> (num_black rt h)

let[@axiom] num_black_root_black_0_lt_leaf (v : int rbtree) (lt : int rbtree)
    (h : int) =
  (num_black v 0 && rb_lch v lt) #==> (rb_leaf lt)

let[@axiom] num_black_root_black_0_rt_leaf (v : int rbtree) (rt : int rbtree)
    (h : int) =
  (num_black v 0 && rb_rch v rt) #==> (rb_leaf rt)

let[@axiom] num_black_root_black_0_rt_red (v : int rbtree) (rt : int rbtree)
    (h : int) =
  (num_black v 0 && rb_rch v rt) #==> (rb_root_color v true)

let[@axiom] no_red_red_lt (v : int rbtree) (lt : int rbtree) =
  (no_red_red v && rb_lch v lt) #==> (no_red_red lt)

let[@axiom] no_red_red_rt (v : int rbtree) (rt : int rbtree) =
  (no_red_red v && rb_rch v rt) #==> (no_red_red rt)

let[@axiom] no_red_red_root_red_lt_not_red (v : int rbtree) (lt : int rbtree) =
  (no_red_red v && rb_lch v lt && rb_root_color v true)
  #==> (not (rb_root_color lt true))

let[@axiom] no_red_red_root_red_rt_not_red (v : int rbtree) (rt : int rbtree) =
  (no_red_red v && rb_rch v rt && rb_root_color v true)
  #==> (not (rb_root_color rt true))

let[@axiom] black_lt_black_num_black_gt_1 (v : int rbtree) (lt : int rbtree)
    (h : int) =
  (num_black v h && rb_lch v lt && rb_root_color v false
 && rb_root_color lt false)
  #==> (h > 1)

let[@axiom] black_rt_black_num_black_gt_1 (v : int rbtree) (rt : int rbtree)
    (h : int) =
  (num_black v h && rb_rch v rt && rb_root_color v false
 && rb_root_color rt false)
  #==> (h > 1)
