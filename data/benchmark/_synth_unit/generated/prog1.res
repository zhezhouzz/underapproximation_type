[Loading type decls]:
type 'a pairinghp =
  | Phpleaf 
  | Phpnode of 'a * 'a pairinghp list 
type 'a tree =
  | Leaf 
  | Node of 'a * 'a tree * 'a tree 
type 'a heap =
  | Hempty 
  | Hnode of 'a * 'a heap * 'a heap 
type 'a set =
  | Sempty 
  | Snode of 'a * 'a set * 'a set 
type 'a binomialhp =
  | Bhpleaf 
  | Bhpnode of int * 'a * 'a binomialhp list 
type 'a rbtree =
  | Rbtleaf 
  | Rbtnode of bool * 'a rbtree * 'a * 'a rbtree 
type 'a skewhp =
  | Shpnode of int * 'a * 'a list * 'a skewhp list 
type 'a splayhp =
  | Sphpleaf 
  | Sphpnode of 'a splayhp * 'a * 'a splayhp 
type 'a unbset =
  | Usleaf 
  | Usnode of 'a * 'a unbset * 'a unbset 
type 'a batchedq =
  | Batchedq of 'a list * 'a list 
type 'a lazyty =
  | Lazyty of 'a 
type 'a stream =
  | Streamnil 
  | Streamlazycons of 'a * 'a stream lazyty 
type 'a bankersq =
  | Bankersq of int * 'a stream * int * 'a stream 
type 'a leftisthp =
  | Lhpleaf 
  | Lhpnode of 'a * 'a leftisthp * 'a leftisthp * int 

[Load ocaml program]:
let rec goal (size : int) (x0 : int) =
  (if sizecheck size then [] else (subs size) :: x0 :: (goal (subs size) x0) : 
  int list)

has ext: (size : int)
has ext: (x0 : int)
[Before type check]:
let rec goal = fun (size : int) ->
  fun (x0 : int) ->
    (if sizecheck size
     then nil 
     else cons (subs size) (cons x0 (goal (subs size) x0)) : int list)


[Typed program]:
let rec goal = (fun (size : int) ->
   (fun (x0 : int) ->
      (if ((sizecheck : int -> bool) (size : int) : bool)
       then ((nil : int list)  : i
...
t) : int list) : int list) : int list) : int ->
                                                                 int list) : 
int -> int -> int list)


[Typed A-normal from]:
let rec goal = (fun (size : int) ->
   (fun (x0 : int) ->
      (let ((x!0 : bool)) = ((sizecheck : int -> bool) (size : int) : bool) in
       (if (x
...
)) : int list) : int list) : int list) : int ->
                                                                 int list) : 
int -> int -> int list)


[1mTask 1:[0m
[1mcheck against with:[0m s:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((len v s) ∧ (∀ u, ((mem v u) => (u == x))))]
[1mType Check:[0m
[32m∅
[39m⊢ [95m(fun (size : int) ->
   (fun (x0 : int) ->
      (let ((x!0 : bool)) = ((sizecheck : int -> bool) (size : int) : bool) in
       (if (x!0 : bool)
        then
          (let ((x!1 : int list)) = ((nil : int list)  : int list) in
           (x!1 : int list) : int list)
        else
          ((let ((x!2 : int)) = ((subs : int -> int) (size : int) : int) in
            (let ((x!3 : int list)) =
               ((goal : int -> int -> int list) (x!2 : int) (x0 : int) : 
               int list) in
             (let ((x!4 : int list)) =
                ((cons : int -> int list -> int list) (x0 : int)
                   (x!3 : int list) : int list) in
              (let ((x!5 : int)) = ((subs : int -> int) (size : int) : int) in
               (let ((x!6 : int list)) =
                  ((cons : int -> int list -> int list) (x!5 : int)
                     (x!4 : int list) : int list) in
                (x!6 : int list) : int list) : int list) : int list) : 
              int list)) : int list) : int list) : int list) : int ->
                                                                 int list) : 
int -> int -> int list)[39m ⇦ [36ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m

[1mType Check:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,
⊢ [95m(fun (x0 : int) ->
   (let ((x!0 : bool)) = ((sizecheck : int -> bool) (size!0 : int) : bool) in
    (if (x!0 : bool)
     then
       (let ((x!1 : int list)) = ((nil : int list)  : int list) in
        (x!1 : int list) : int list)
     else
       ((let ((x!2 : int)) = ((subs : int -> int) (size!0 : int) : int) in
         (let ((x!3 : int list)) =
            ((goal : int -> int -> int list) (x!2 : int) (x0 : int) : 
            int list) in
          (let ((x!4 : int list)) =
             ((cons : int -> int list -> int list) (x0 : int)
                (x!3 : int list) : int list) in
           (let ((x!5 : int)) = ((subs : int -> int) (size!0 : int) : int) in
            (let ((x!6 : int list)) =
               ((cons : int -> int list -> int list) (x!5 : int)
                  (x!4 : int list) : int list) in
             (x!6 : int list) : int list) : int list) : int list) : int list)) : 
       int list) : int list) : int list) : int -> int list)[39m ⇦ [36mx:{v:int | ⊤}→[v:int list | ((len v size!0) ∧ (∀ u, ((mem v u) => (u == x))))][39m

[1mType Check:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,
⊢ [95m(let ((x!0 : bool)) = ((sizecheck : int -> bool) (size!0 : int) : bool) in
 (if (x!0 : bool)
  then
    (let ((x!1 : int list)) = ((nil : int list)  : int list) in
     (x!1 : int list) : int list)
  else
    ((let ((x!2 : int)) = ((subs : int -> int) (size!0 : int) : int) in
      (let ((x!3 : int list)) =
         ((goal : int -> int -> int list) (x!2 : int) (x0 : int) : int list) in
       (let ((x!4 : int list)) =
          ((cons : int -> int list -> int list) (x0 : int) (x!3 : int list) : 
          int list) in
        (let ((x!5 : int)) = ((subs : int -> int) (size!0 : int) : int) in
         (let ((x!6 : int list)) =
            ((cons : int -> int list -> int list) (x!5 : int)
               (x!4 : int list) : int list) in
          (x!6 : int list) : int list) : int list) : int list) : int list)) : 
    int list) : int list) : int list)[39m ⇦ [36m[v:int list | ((len v size!0) ∧ (∀ u, ((mem v u) => (u == x0))))][39m

infer sizecheck
infer size!0
[1mApplication Type Check (sizecheck):[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,
⊢ [95ma!0:[v = size!0] → ? [39m ⇦ [36mx:{v:int | ⊤}→[v:bool | ((v <=> (x == 0)) ∧ ((¬ v) <=> (x > 0)))][39m

[1mSubtyping Check:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,
⊢ [95m[v:int | ⊤][39m <: [36m[v:int | (v == size!0)][39m

[1mQuery:[0m
[33m∀ size!0, ∀ x0, ∀ v, [39m[32m[39m
[36m((size!0 >= 0) ∧ (size!0 == v))[39m [1m=>[0m
[95m⊤[39m
[1mraw:[0m vc_head(2); vc_body(1)
[1madd_lemma:[0m vc_head(2); vc_body(1)
[1mwithout_dt:[0m 6
to_Z3: 0.0001s
[1mSolving time: 0.02[0m
[1mLet LHS:[0m x!0 => [v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))]
[1mType Check:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,
⊢ [95m(if (x!0 : bool)
 then
   (let ((x!1 : int list)) = ((nil : int list)  : int list) in
    (x!1 : int list) : int list)
 else
   ((let ((x!2 : int)) = ((subs : int -> int) (size!0 : int) : int) in
     (let ((x!3 : int list)) =
        ((goal : int -> int -> int list) (x!2 : int) (x0 : int) : int list) in
      (let ((x!4 : int list)) =
         ((cons : int -> int list -> int list) (x0 : int) (x!3 : int list) : 
         int list) in
       (let ((x!5 : int)) = ((subs : int -> int) (size!0 : int) : int) in
        (let ((x!6 : int list)) =
           ((cons : int -> int list -> int list) (x!5 : int) (x!4 : int list) : 
           int list) in
         (x!6 : int list) : int list) : int list) : int list) : int list)) : 
   int list) : int list)[39m ⇦ [36m[v:int list | ((len v size!0) ∧ (∀ u, ((mem v u) => (u == x0))))][39m

infer x!0
infer subs
infer size!0
[1mApplication Type Check (subs):[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,
⊢ [95ma!1:[v = size!0] → ? [39m ⇦ [36ms:{v:int | ⊤}→[v:int | (v == (s - 1))][39m

[1mSubtyping Check:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,
⊢ [95m[v:int | ⊤][39m <: [36m[v:int | (v == size!0)][39m

[1mQuery:[0m
[33m∀ size!0, ∀ x0, ∀ v, [39m[32m[39m
[36m((size!0 >= 0) ∧ (∃ x!0, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ (size!0 == v))))[39m [1m=>[0m
[95m(∃ x!0, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0)))[39m
[1mraw:[0m vc_head(13); vc_body(11)
[1madd_lemma:[0m vc_head(13); vc_body(11)
[1mwithout_dt:[0m 27
to_Z3: 0.0001s
[1mSolving time: 0.02[0m
[1mLet LHS:[0m x!2 => [v:int | (v == (size!0 - 1))]
infer goal
infer x!2
infer x0
[1mApplication Type Check (goal):[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int | (v == (size!0 - 1))][39m,
⊢ [95ma!2:[v = x!2] → a!3:[v = x0] → ? [39m ⇦ [36ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m

[1mSubtyping Check:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int | (v == (size!0 - 1))][39m,
⊢ [95m[v:int | (v >= 0)][39m <: [36m[v:int | (v == x!2)][39m

[1mQuery:[0m
[33m∀ size!0, ∀ x0, ∀ v, [39m[32m[39m
[36m((size!0 >= 0) ∧ (∃ x!0, (∃ x!2, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ ((size!0 - 1) == x!2) ∧ (v == x!2)))))[39m [1m=>[0m
[95m(∃ x!0, (∃ x!2, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ ((size!0 - 1) == x!2) ∧ (v >= 0))))[39m
[1mraw:[0m vc_head(14); vc_body(13)
[1madd_lemma:[0m vc_head(14); vc_body(13)
[1mwithout_dt:[0m 30
to_Z3: 0.0001s
[1mSolving time: 0.02[0m
[1mSubtyping Check:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int | (v == (size!0 - 1))][39m,a!2:[32m[v = x!2][39m,
⊢ [95m[v:int | ⊤][39m <: [36m[v:int | (v == x0)][39m

[1mQuery:[0m
[33m∀ size!0, ∀ x0, ∀ v, [39m[32m[39m
[36m((size!0 >= 0) ∧ (∃ x!0, (∃ x!2, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ ((size!0 - 1) == x!2) ∧ (v == x0)))))[39m [1m=>[0m
[95m(∃ x!0, (∃ x!2, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ ((size!0 - 1) == x!2))))[39m
[1mraw:[0m vc_head(14); vc_body(12)
[1madd_lemma:[0m vc_head(14); vc_body(12)
[1mwithout_dt:[0m 29
to_Z3: 0.0001s
[1mSolving time: 0.02[0m
[1mLet LHS:[0m x!3 => [v:int list | ((x!2 < size!0) ∧ (x!2 >= 0) ∧ (len v x!2) ∧ (∀ u, ((mem v u) => (u == x0))))]
infer x0
infer x!3
[1mApplication Type Check (cons):[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int | (v == (size!0 - 1))][39m,x!3:[32m[v:int list | ((x!2 < size!0) ∧ (x!2 >= 0) ∧ (len v x!2) ∧ (∀ u, ((mem v u) => (u == x0))))][39m,
⊢ [95ma!4:[v = x0] → a!5:[v:int | (len x!3 v)] → a!6:[v = x!3] → ? [39m ⇦ [36mh:{v:int | ⊤}→s:{v:int | (v >= 0)}→[v:int list | ((len v s) ∧ (∀ u, ((mem v u) => (u == h))))]→[v:int list | (∀ u, (((u == (s + 1)) => (len v u)) ∧ ((mem v u) => (u == h))))][39m

[1mSubtyping Check:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int | (v == (size!0 - 1))][39m,x!3:[32m[v:int list | ((x!2 < size!0) ∧ (x!2 >= 0) ∧ (len v x!2) ∧ (∀ u, ((mem v u) => (u == x0))))][39m,
⊢ [95m[v:int | ⊤][39m <: [36m[v:int | (v == x0)][39m

[1mQuery:[0m
[33m∀ size!0, ∀ x0, ∀ v, [39m[32m[39m
[36m((size!0 >= 0) ∧ (∃ x!0, (∃ x!2, (∃ x!3, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ ((size!0 - 1) == x!2) ∧ (x!2 < size!0) ∧ (x!2 >= 0) ∧ (len x!3 x!2) ∧ (∀ u, ((mem x!3 u) => (u == x0))) ∧ (v == x0))))))[39m [1m=>[0m
[95m(∃ x!0, (∃ x!2, (∃ x!3, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ ((size!0 - 1) == x!2) ∧ (x!2 < size!0) ∧ (x!2 >= 0) ∧ (len x!3 x!2) ∧ (∀ u, ((mem x!3 u) => (u == x0)))))))[39m
[1mraw:[0m vc_head(21); vc_body(19)
[1madd_lemma:[0m vc_head(36); vc_body(614)
[1mwithout_dt:[0m 1751
to_Z3: 0.0058s
[1mSolving time: 0.06[0m
[1mSubtyping Check:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int | (v == (size!0 - 1))][39m,x!3:[32m[v:int list | ((x!2 < size!0) ∧ (x!2 >= 0) ∧ (len v x!2) ∧ (∀ u, ((mem v u) => (u == x0))))][39m,a!4:[32m[v = x0][39m,
⊢ [95m[v:int | (v >= 0)][39m <: [36m[v:int | (len x!3 v)][39m

[1mQuery:[0m
[33m∀ size!0, ∀ x0, ∀ v, [39m[32m[39m
[36m((size!0 >= 0) ∧ (∃ x!0, (∃ x!2, (∃ x!3, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ ((size!0 - 1) == x!2) ∧ (x!2 < size!0) ∧ (x!2 >= 0) ∧ (len x!3 x!2) ∧ (∀ u, ((mem x!3 u) => (u == x0))) ∧ (len x!3 v))))))[39m [1m=>[0m
[95m(∃ x!0, (∃ x!2, (∃ x!3, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ ((size!0 - 1) == x!2) ∧ (x!2 < size!0) ∧ (x!2 >= 0) ∧ (len x!3 x!2) ∧ (∀ u, ((mem x!3 u) => (u == x0))) ∧ (v >= 0)))))[39m
[1mraw:[0m vc_head(21); vc_body(20)
[1madd_lemma:[0m vc_head(36); vc_body(615)
[1mwithout_dt:[0m 1752
to_Z3: 0.0056s
[1mSolving time: 0.06[0m
[1mSubtyping Check:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int | (v == (size!0 - 1))][39m,x!3:[32m[v:int list | ((x!2 < size!0) ∧ (x!2 >= 0) ∧ (len v x!2) ∧ (∀ u, ((mem v u) => (u == x0))))][39m,a!4:[32m[v = x0][39m,a!5:[32m[v:int | (len x!3 v)][39m,
⊢ [95m[v:int list | ((x!2 < size!0) ∧ (x!2 >= 0) ∧ (len v x!2) ∧ (∀ u, ((mem v u) => (u == x0))))][39m <: [36m[v:int list | ((len v a!5) ∧ (∀ u, ((mem v u) => (u == a!4))))][39m

[1mQuery:[0m
[33m∀ size!0, ∀ x0, ∀ v, [39m[32m[39m
[36m((size!0 >= 0) ∧ (∃ x!0, (∃ x!2, (∃ x!3, (∃ a!5, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ ((size!0 - 1) == x!2) ∧ (x!2 < size!0) ∧ (x!2 >= 0) ∧ (len x!3 x!2) ∧ (∀ u, ((mem x!3 u) => (u == x0))) ∧ (len x!3 a!5) ∧ (len v a!5) ∧ (∀ u, ((mem v u) => (u == x0)))))))))[39m [1m=>[0m
[95m(∃ x!0, (∃ x!2, (∃ x!3, (∃ a!5, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ ((size!0 - 1) == x!2) ∧ (len x!3 x!2) ∧ (∀ u, ((mem x!3 u) => (u == x0))) ∧ (len x!3 a!5) ∧ (x!2 < size!0) ∧ (x!2 >= 0) ∧ (len v x!2) ∧ (∀ u, ((mem v u) => (u == x0))))))))[39m
[1mraw:[0m vc_head(26); vc_body(25)
[1madd_lemma:[0m vc_head(56); vc_body(988)
[1mwithout_dt:[0m 3775
to_Z3: 0.0139s
[1mSolving time: 0.15[0m
[1mConsume variable a!6[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int | (v == (size!0 - 1))][39m,x!3:[32m[v:int list | ((x!2 < size!0) ∧ (x!2 >= 0) ∧ (len v x!2) ∧ (∀ u, ((mem v u) => (u == x0))))][39m,a!4:[32m[v = x0][39m,a!5:[32m[v:int | (len x!3 v)][39m,a!6:[32m⟬ [v = x!3] ⟭[39m,
[1mLet LHS:[0m x!4 => [v:int list | (∃ a!5, ((len x!3 a!5) ∧ (∀ u, (((u == (a!5 + 1)) => (len v u)) ∧ ((mem v u) => (u == x0))))))]
infer subs
infer size!0
[1mApplication Type Check (subs):[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int | (v == (size!0 - 1))][39m,x!3:[32m[v:int list | ((x!2 < size!0) ∧ (x!2 >= 0) ∧ (len v x!2) ∧ (∀ u, ((mem v u) => (u == x0))))][39m,x!4:[32m[v:int list | (∃ a!5, ((len x!3 a!5) ∧ (∀ u, (((u == (a!5 + 1)) => (len v u)) ∧ ((mem v u) => (u == x0))))))][39m,
⊢ [95ma!8:[v = size!0] → ? [39m ⇦ [36ms:{v:int | ⊤}→[v:int | (v == (s - 1))][39m

[1mSubtyping Check:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int | (v == (size!0 - 1))][39m,x!3:[32m[v:int list | ((x!2 < size!0) ∧ (x!2 >= 0) ∧ (len v x!2) ∧ (∀ u, ((mem v u) => (u == x0))))][39m,x!4:[32m[v:int list | (∃ a!5, ((len x!3 a!5) ∧ (∀ u, (((u == (a!5 + 1)) => (len v u)) ∧ ((mem v u) => (u == x0))))))][39m,
⊢ [95m[v:int | ⊤][39m <: [36m[v:int | (v == size!0)][39m

[1mQuery:[0m
[33m∀ size!0, ∀ x0, ∀ v, [39m[32m[39m
[36m((size!0 >= 0) ∧ (∃ x!0, (∃ x!2, (∃ x!3, (∃ x!4, (∃ a!5, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ ((size!0 - 1) == x!2) ∧ (x!2 < size!0) ∧ (x!2 >= 0) ∧ (len x!3 x!2) ∧ (∀ u, ((mem x!3 u) => (u == x0))) ∧ (len x!3 a!5) ∧ (∀ u, ((((1 + a!5) == u) => (len x!4 u)) ∧ ((mem x!4 u) => (u == x0)))) ∧ (size!0 == v))))))))[39m [1m=>[0m
[95m(∃ x!0, (∃ x!2, (∃ x!3, (∃ x!4, (∃ a!5, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ ((size!0 - 1) == x!2) ∧ (x!2 < size!0) ∧ (x!2 >= 0) ∧ (len x!3 x!2) ∧ (∀ u, ((mem x!3 u) => (u == x0))) ∧ (len x!3 a!5) ∧ (∀ u, ((((1 + a!5) == u) => (len x!4 u)) ∧ ((mem x!4 u) => (u == x0))))))))))[39m
[1mraw:[0m vc_head(29); vc_body(27)
[1madd_lemma:[0m vc_head(59); vc_body(2729)
[1mwithout_dt:[0m 8229
to_Z3: 0.0542s
[1mSolving time: 0.64[0m
[1mLet LHS:[0m x!5 => [v:int | (v == (size!0 - 1))]
infer x!5
infer x!4
[1mApplication Type Check (cons):[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int | (v == (size!0 - 1))][39m,x!3:[32m[v:int list | ((x!2 < size!0) ∧ (x!2 >= 0) ∧ (len v x!2) ∧ (∀ u, ((mem v u) => (u == x0))))][39m,x!4:[32m[v:int list | (∃ a!5, ((len x!3 a!5) ∧ (∀ u, (((u == (a!5 + 1)) => (len v u)) ∧ ((mem v u) => (u == x0))))))][39m,x!5:[32m[v:int | (v == (size!0 - 1))][39m,
⊢ [95ma!10:[v = x!5] → a!11:[v:int | (len x!4 v)] → a!12:[v = x!4] → ? [39m ⇦ [36mh:{v:int | ⊤}→s:{v:int | (v >= 0)}→[v:int list | ((len v s) ∧ (∀ u, ((mem v u) => (u == h))))]→[v:int list | (∀ u, (((u == (s + 1)) => (len v u)) ∧ ((mem v u) => (u == h))))][39m

[1mSubtyping Check:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int | (v == (size!0 - 1))][39m,x!3:[32m[v:int list | ((x!2 < size!0) ∧ (x!2 >= 0) ∧ (len v x!2) ∧ (∀ u, ((mem v u) => (u == x0))))][39m,x!4:[32m[v:int list | (∃ a!5, ((len x!3 a!5) ∧ (∀ u, (((u == (a!5 + 1)) => (len v u)) ∧ ((mem v u) => (u == x0))))))][39m,x!5:[32m[v:int | (v == (size!0 - 1))][39m,
⊢ [95m[v:int | ⊤][39m <: [36m[v:int | (v == x!5)][39m

[1mQuery:[0m
[33m∀ size!0, ∀ x0, ∀ v, [39m[32m[39m
[36m((size!0 >= 0) ∧ (∃ x!0, (∃ x!2, (∃ x!3, (∃ x!4, (∃ a!5, (∃ x!5, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ ((size!0 - 1) == x!2) ∧ (x!2 < size!0) ∧ (x!2 >= 0) ∧ (len x!3 x!2) ∧ (∀ u, ((mem x!3 u) => (u == x0))) ∧ (len x!3 a!5) ∧ (∀ u, ((((1 + a!5) == u) => (len x!4 u)) ∧ ((mem x!4 u) => (u == x0)))) ∧ ((size!0 - 1) == x!5) ∧ (v == x!5)))))))))[39m [1m=>[0m
[95m(∃ x!0, (∃ x!2, (∃ x!3, (∃ x!4, (∃ a!5, (∃ x!5, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ ((size!0 - 1) == x!2) ∧ (x!2 < size!0) ∧ (x!2 >= 0) ∧ (len x!3 x!2) ∧ (∀ u, ((mem x!3 u) => (u == x0))) ∧ (len x!3 a!5) ∧ (∀ u, ((((1 + a!5) == u) => (len x!4 u)) ∧ ((mem x!4 u) => (u == x0)))) ∧ ((size!0 - 1) == x!5))))))))[39m
[1mraw:[0m vc_head(30); vc_body(28)
[1madd_lemma:[0m vc_head(60); vc_body(3352)
[1mwithout_dt:[0m 10253
to_Z3: 0.0791s
[1mSolving time: 0.89[0m
[1mSubtyping Check:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int | (v == (size!0 - 1))][39m,x!3:[32m[v:int list | ((x!2 < size!0) ∧ (x!2 >= 0) ∧ (len v x!2) ∧ (∀ u, ((mem v u) => (u == x0))))][39m,x!4:[32m[v:int list | (∃ a!5, ((len x!3 a!5) ∧ (∀ u, (((u == (a!5 + 1)) => (len v u)) ∧ ((mem v u) => (u == x0))))))][39m,x!5:[32m[v:int | (v == (size!0 - 1))][39m,a!10:[32m[v = x!5][39m,
⊢ [95m[v:int | (v >= 0)][39m <: [36m[v:int | (len x!4 v)][39m

[1mQuery:[0m
[33m∀ size!0, ∀ x0, ∀ v, [39m[32m[39m
[36m((size!0 >= 0) ∧ (∃ x!0, (∃ x!2, (∃ x!3, (∃ x!4, (∃ a!5, (∃ x!5, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ ((size!0 - 1) == x!2) ∧ (x!2 < size!0) ∧ (x!2 >= 0) ∧ (len x!3 x!2) ∧ (∀ u, ((mem x!3 u) => (u == x0))) ∧ (len x!3 a!5) ∧ (∀ u, ((((1 + a!5) == u) => (len x!4 u)) ∧ ((mem x!4 u) => (u == x0)))) ∧ ((size!0 - 1) == x!5) ∧ (len x!4 v)))))))))[39m [1m=>[0m
[95m(∃ x!0, (∃ x!2, (∃ x!3, (∃ x!4, (∃ a!5, (∃ x!5, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ ((size!0 - 1) == x!2) ∧ (x!2 < size!0) ∧ (x!2 >= 0) ∧ (len x!3 x!2) ∧ (∀ u, ((mem x!3 u) => (u == x0))) ∧ (len x!3 a!5) ∧ (∀ u, ((((1 + a!5) == u) => (len x!4 u)) ∧ ((mem x!4 u) => (u == x0)))) ∧ ((size!0 - 1) == x!5) ∧ (v >= 0))))))))[39m
[1mraw:[0m vc_head(30); vc_body(29)
[1madd_lemma:[0m vc_head(60); vc_body(3353)
[1mwithout_dt:[0m 10254
to_Z3: 0.0782s
[1mSolving time: 0.86[0m
[1mSubtyping Check:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int | (v == (size!0 - 1))][39m,x!3:[32m[v:int list | ((x!2 < size!0) ∧ (x!2 >= 0) ∧ (len v x!2) ∧ (∀ u, ((mem v u) => (u == x0))))][39m,x!4:[32m[v:int list | (∃ a!5, ((len x!3 a!5) ∧ (∀ u, (((u == (a!5 + 1)) => (len v u)) ∧ ((mem v u) => (u == x0))))))][39m,x!5:[32m[v:int | (v == (size!0 - 1))][39m,a!10:[32m[v = x!5][39m,a!11:[32m[v:int | (len x!4 v)][39m,
⊢ [95m[v:int list | (∃ a!5, ((len x!3 a!5) ∧ (∀ u, (((u == (a!5 + 1)) => (len v u)) ∧ ((mem v u) => (u == x0))))))][39m <: [36m[v:int list | ((len v a!11) ∧ (∀ u, ((mem v u) => (u == a!10))))][39m

[1mQuery:[0m
[33m∀ size!0, ∀ x0, ∀ v, [39m[32m[39m
[36m((size!0 >= 0) ∧ (∃ x!0, (∃ x!2, (∃ x!3, (∃ x!4, (∃ a!5, (∃ x!5, (∃ a!11, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ ((size!0 - 1) == x!2) ∧ (x!2 < size!0) ∧ (x!2 >= 0) ∧ (len x!3 x!2) ∧ (∀ u, ((mem x!3 u) => (u == x0))) ∧ (len x!3 a!5) ∧ (∀ u, ((((1 + a!5) == u) => (len x!4 u)) ∧ ((mem x!4 u) => (u == x0)))) ∧ ((size!0 - 1) == x!5) ∧ (len x!4 a!11) ∧ (len v a!11) ∧ (∀ u, ((mem v u) => (u == x!5))))))))))))[39m [1m=>[0m
[95m(∃ x!0, (∃ x!2, (∃ x!3, (∃ x!4, (∃ a!15, (∃ x!5, (∃ a!11, (∃ a!5, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ ((size!0 - 1) == x!2) ∧ (x!2 < size!0) ∧ (x!2 >= 0) ∧ (len x!3 x!2) ∧ (∀ u, ((mem x!3 u) => (u == x0))) ∧ (len x!3 a!15) ∧ (∀ u, ((((1 + a!15) == u) => (len x!4 u)) ∧ ((mem x!4 u) => (u == x0)))) ∧ ((size!0 - 1) == x!5) ∧ (len x!4 a!11) ∧ (len x!3 a!5) ∧ (∀ u, ((((1 + a!5) == u) => (len v u)) ∧ ((mem v u) => (u == x0)))))))))))))[39m
[1mraw:[0m vc_head(35); vc_body(37)
[1madd_lemma:[0m vc_head(80); vc_body(4928)
[1mwithout_dt:[0m 17659
to_Z3: 0.1462s
[1mSolving time: 0.44[0m
model:
(define-fun u!90 () Int
  5921)
(define-fun x!3 ()
...
not (= x!0 7)) (= x!0 5) (= (k!10320 x!1) 5385))))
[38;2;255;165;0mUnder Type Check failed:[39m[typecheck/undercheck.ml:241] Subtyping check: rejected by the verifier
[1mType Infer:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int | (v == (size!0 - 1))][39m,x!3:[32m[v:int list | ((x!2 < size!0) ∧ (x!2 >= 0) ∧ (len v x!2) ∧ (∀ u, ((mem v u) => (u == x0))))][39m,x!4:[32m[v:int list | (∃ a!5, ((len x!3 a!5) ∧ (∀ u, (((u == (a!5 + 1)) => (len v u)) ∧ ((mem v u) => (u == x0))))))][39m,x!5:[32m[v:int | (v == (size!0 - 1))][39m,
⊢ [95m(let ((x!6 : int list)) =
   ((cons : int -> int l
...
) : 
   int list) in
 (x!6 : int list) : int list)[39m ⇨ [36m[v:int list | ⊥][39m

[1mType Infer:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int | (v == (size!0 - 1))][39m,x!3:[32m[v:int list | ((x!2 < size!0) ∧ (x!2 >= 0) ∧ (len v x!2) ∧ (∀ u, ((mem v u) => (u == x0))))][39m,x!4:[32m[v:int list | (∃ a!5, ((len x!3 a!5) ∧ (∀ u, (((u == (a!5 + 1)) => (len v u)) ∧ ((mem v u) => (u == x0))))))][39m,
⊢ [95m(let ((x!5 : int)) = ((subs : int -> int) (size!0 
...
ist) in
  (x!6 : int list) : int list) : int list)[39m ⇨ [36m[v:int list | (∃ x!5, ((x!5 == (size!0 - 1)) ∧ ⊥))][39m

[1mType Infer:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int | (v == (size!0 - 1))][39m,x!3:[32m[v:int list | ((x!2 < size!0) ∧ (x!2 >= 0) ∧ (len v x!2) ∧ (∀ u, ((mem v u) => (u == x0))))][39m,
⊢ [95m(let ((x!4 : int list)) =
   ((cons : int -> int l
...
!6 : int list) : int list) : int list) : int list)[39m ⇨ [36m[v:int list | (∃ x!4, (∃ a!5, (∃ x!5, (((len x!3 a!5) ∧ (∀ u, (((u == (a!5 + 1)) => (len x!4 u)) ∧ ((mem x!4 u) => (u == x0))))) ∧ ((x!5 == (size!0 - 1)) ∧ ⊥)))))][39m

[1mType Infer:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int | (v == (size!0 - 1))][39m,
⊢ [95m(let ((x!3 : int list)) =
   ((goal : int -> int -
...
t) : int list) : int list) : int list) : int list)[39m ⇨ [36m[v:int list | (∃ x!3, (∃ x!4, (∃ a!5, (∃ x!5, (((x!2 < size!0) ∧ (x!2 >= 0) ∧ (len x!3 x!2) ∧ (∀ u, ((mem x!3 u) => (u == x0)))) ∧ (((len x!3 a!5) ∧ (∀ u, (((u == (a!5 + 1)) => (len x!4 u)) ∧ ((mem x!4 u) => (u == x0))))) ∧ ((x!5 == (size!0 - 1)) ∧ ⊥)))))))][39m

[1mType Infer:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,
⊢ [95m(let ((x!2 : int)) = ((subs : int -> int) (size!0 
...
) : int list) : int list) : int list) : 
int list)[39m ⇨ [36m[v:int list | (∃ x!2, (∃ x!3, (∃ x!4, (∃ a!5, (∃ x!5, ((x!2 == (size!0 - 1)) ∧ (((x!2 < size!0) ∧ (x!2 >= 0) ∧ (len x!3 x!2) ∧ (∀ u, ((mem x!3 u) => (u == x0)))) ∧ (((len x!3 a!5) ∧ (∀ u, (((u == (a!5 + 1)) => (len x!4 u)) ∧ ((mem x!4 u) => (u == x0))))) ∧ ((x!5 == (size!0 - 1)) ∧ ⊥)))))))))][39m

[1mApplication Type Check (nil):[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!1:[32m[v:bool | x!0][39m,
⊢ [95m → ? [39m ⇦ [36m[v:int list | (len v 0)][39m

[1mLet LHS:[0m x!1 => [v:int list | (len v 0)]
infer x!1
[1mType Infer:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!1:[32m[v:bool | x!0][39m,x!1:[32m[v:int list | (len v 0)][39m,
⊢ [95m(x!1 : int list)[39m ⇨ [36m[v:int list | (len v 0)][39m

[1mType Infer:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!1:[32m[v:bool | x!0][39m,
⊢ [95m(let ((x!1 : int list)) = ((nil : int list)  : int list) in (x!1 : int list) : 
int list)[39m ⇨ [36m[v:int list | (∃ x!1, ((len x!1 0) ∧ (len v 0)))][39m

[1mType Infer:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,
⊢ [95m(if (x!0 : bool)
 then
   (let ((x!1 : int list)) 
...
int list) : int list)) : 
   int list) : int list)[39m ⇨ [36m[v:int list | (∃ b!2, (∃ x!41, (∃ b!3, (∃ x!42, (∃ x!43, (∃ x!44, (∃ a!19, (∃ x!45, (x!0 ∧ (len x!41 0) ∧ (len v 0))))))))))][39m

[1mSubtyping Check:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,
⊢ [95m[v:int list | (∃ b!2, (∃ x!41, (∃ b!3, (∃ x!42, (∃ x!43, (∃ x!44, (∃ a!19, (∃ x!45, (x!0 ∧ (len x!41 0) ∧ (len v 0))))))))))][39m <: [36m[v:int list | ((len v size!0) ∧ (∀ u, ((mem v u) => (u == x0))))][39m

[1mQuery:[0m
[33m∀ size!0, ∀ x0, ∀ v, [39m[32m[39m
[36m((size!0 >= 0) ∧ (∃ x!0, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (len v size!0) ∧ (∀ u, ((mem v u) => (u == x0))))))[39m [1m=>[0m
[95m(∃ x!0, (∃ x!41, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ x!0 ∧ (len x!41 0) ∧ (len v 0))))[39m
[1mraw:[0m vc_head(15); vc_body(12)
[1madd_lemma:[0m vc_head(30); vc_body(329)
[1mwithout_dt:[0m 920
to_Z3: 0.0026s
[1mSolving time: 0.03[0m
model:
(define-fun u!103 () Int
  7720)
(define-fun x!0 (
...
x!0 Int) (x!1 Int)) Bool
  (= (k!10632 x!1) 7722))
[38;2;255;165;0mUnder Type Check failed:[39m[typecheck/undercheck.ml:447] Subtyping check: rejected by the verifier
[1m[31mTask 1, type check failed[39m[0m
& goal
 $3$ & $10$ & $3$ & $12$ & $(10, 8)$ & $3.35(0.28)$
