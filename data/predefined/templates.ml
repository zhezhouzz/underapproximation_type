let[@axiom] template_eq_0 (x : int) = x == 0
let[@axiom] template_lt (a : int) (b : int) = a < b
let[@axiom] template_leq_1 (a : int) = a <= 1
let[@axiom] template_emp (l : int list) = emp l
let[@axiom] template_sorted (l : int list) = sorted l
let[@axiom] template_leaf (l : int tree) = leaf l
(* let[@axiom] template_rb_leaf (v : int rbtree) = rb_leaf v *)
(* let[@axiom] template_no_red_red (v : int rbtree) = no_red_red v *)
(* let[@axiom] template_red_root (v : int rbtree) = rb_root_color v true *)
(* let[@axiom] template_black_root (v : int rbtree) = rb_root_color v false *)
(* let[@axiom] template_num_black (v : int rbtree) (n : int) = num_black v n *)
(* let[@axiom] template_num_black_0 (v : int rbtree) = num_black v 0 *)
(* let[@axiom] template_stream_emp (l : int stream) = stream_emp l *)
(* let[@axiom] template_leftisthp_leaf (l : int leftisthp) = leftisthp_leaf l *)
(* let[@axiom] template_no_app (e : stlc_term) = num_app e 0 *)
(* let[@axiom] template_is_const (e : stlc_term) = is_const e *)
(* let[@axiom] template_is_var (e : stlc_term) = is_var e *)
(* let[@axiom] template_is_abs (e : stlc_term) = is_abs e *)
(* let[@axiom] template_is_app (e : stlc_term) = is_app e *)
