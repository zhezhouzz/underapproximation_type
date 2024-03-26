let[@axiom] rec_arg (arg : int) (param : int) = 0 <= param && param < arg
let[@axiom] template_eq_0 (x : int) = x == 0
let[@axiom] template_lt (a : int) (b : int) = a < b
let[@axiom] template_leq_1 (a : int) = a <= 1
let[@axiom] template_emp (l : int list) = emp l
let[@axiom] template_sorted (l : int list) = sorted l
let[@axiom] template_leaf (l : int tree) = leaf l
let[@axiom] template_rb_leaf (v : int rbtree) = rb_leaf v
let[@axiom] template_no_red_red (v : int rbtree) = no_red_red v
let[@axiom] template_red_root (v : int rbtree) = rb_root_color v true
let[@axiom] template_black_root (v : int rbtree) = rb_root_color v false
let[@axiom] template_num_black (v : int rbtree) (n : int) = num_black v n
let[@axiom] template_num_black_0 (v : int rbtree) = num_black v 0
