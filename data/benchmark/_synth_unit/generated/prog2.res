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
  (if sizecheck size then [] else (subs size) :: x0 :: (goal size x0) : 
  int list)

has ext: (size : int)
has ext: (x0 : int)
[Before type check]:
let rec goal = fun (size : int) ->
  fun (x0 : int) ->
    (if sizecheck size
     then nil 
     else cons (subs size) (cons x0 (goal size x0)) : int list)


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
                                                                 int ->
                                                                    int list)


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
          ((let ((x!2 : int list)) =
              ((goal : int -> int -> int list) (size : int) (x0 : int) : 
              int list) in
            (let ((x!3 : int list)) =
               ((cons : int -> int list -> int list) (x0 : int)
                  (x!2 : int list) : int list) in
             (let ((x!4 : int)) = ((subs : int -> int) (size : int) : int) in
              (let ((x!5 : int list)) =
                 ((cons : int -> int list -> int list) (x!4 : int)
                    (x!3 : int list) : int list) in
               (x!5 : int list) : int list) : int list) : int list)) : 
          int list) : int list) : int list) : int -> int list) : int ->
                                                                   int ->
                                                                    int list)[39m ⇦ [36ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m

[1mType Check:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,
⊢ [95m(fun (x0 : int) ->
   (let ((x!0 : bool)) = ((sizecheck : int -> bool) (size!0 : int) : bool) in
    (if (x!0 : bool)
     then
       (let ((x!1 : int list)) = ((nil : int list)  : int list) in
        (x!1 : int list) : int list)
     else
       ((let ((x!2 : int list)) =
           ((goal : int -> int -> int list) (size!0 : int) (x0 : int) : 
           int list) in
         (let ((x!3 : int list)) =
            ((cons : int -> int list -> int list) (x0 : int) (x!2 : int list) : 
            int list) in
          (let ((x!4 : int)) = ((subs : int -> int) (size!0 : int) : int) in
           (let ((x!5 : int list)) =
              ((cons : int -> int list -> int list) (x!4 : int)
                 (x!3 : int list) : int list) in
            (x!5 : int list) : int list) : int list) : int list)) : int list) : 
      int list) : int list) : int -> int list)[39m ⇦ [36mx:{v:int | ⊤}→[v:int list | ((len v size!0) ∧ (∀ u, ((mem v u) => (u == x))))][39m

[1mType Check:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,
⊢ [95m(let ((x!0 : bool)) = ((sizecheck : int -> bool) (size!0 : int) : bool) in
 (if (x!0 : bool)
  then
    (let ((x!1 : int list)) = ((nil : int list)  : int list) in
     (x!1 : int list) : int list)
  else
    ((let ((x!2 : int list)) =
        ((goal : int -> int -> int list) (size!0 : int) (x0 : int) : 
        int list) in
      (let ((x!3 : int list)) =
         ((cons : int -> int list -> int list) (x0 : int) (x!2 : int list) : 
         int list) in
       (let ((x!4 : int)) = ((subs : int -> int) (size!0 : int) : int) in
        (let ((x!5 : int list)) =
           ((cons : int -> int list -> int list) (x!4 : int) (x!3 : int list) : 
           int list) in
         (x!5 : int list) : int list) : int list) : int list)) : int list) : 
   int list) : int list)[39m ⇦ [36m[v:int list | ((len v size!0) ∧ (∀ u, ((mem v u) => (u == x0))))][39m

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
[1mSolving time: 0.01[0m
[1mLet LHS:[0m x!0 => [v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))]
[1mType Check:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,
⊢ [95m(if (x!0 : bool)
 then
   (let ((x!1 : int list)) = ((nil : int list)  : int list) in
    (x!1 : int list) : int list)
 else
   ((let ((x!2 : int list)) =
       ((goal : int -> int -> int list) (size!0 : int) (x0 : int) : int list) in
     (let ((x!3 : int list)) =
        ((cons : int -> int list -> int list) (x0 : int) (x!2 : int list) : 
        int list) in
      (let ((x!4 : int)) = ((subs : int -> int) (size!0 : int) : int) in
       (let ((x!5 : int list)) =
          ((cons : int -> int list -> int list) (x!4 : int) (x!3 : int list) : 
          int list) in
        (x!5 : int list) : int list) : int list) : int list)) : int list) : 
int list)[39m ⇦ [36m[v:int list | ((len v size!0) ∧ (∀ u, ((mem v u) => (u == x0))))][39m

infer x!0
infer goal
infer size!0
infer x0
[1mApplication Type Check (goal):[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,
⊢ [95ma!1:[v = size!0] → a!2:[v = x0] → ? [39m ⇦ [36ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m

[1mSubtyping Check:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,
⊢ [95m[v:int | (v >= 0)][39m <: [36m[v:int | (v == size!0)][39m

[1mQuery:[0m
[33m∀ size!0, ∀ x0, ∀ v, [39m[32m[39m
[36m((size!0 >= 0) ∧ (∃ x!0, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ (size!0 == v))))[39m [1m=>[0m
[95m(∃ x!0, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ (v >= 0)))[39m
[1mraw:[0m vc_head(13); vc_body(12)
[1madd_lemma:[0m vc_head(13); vc_body(12)
[1mwithout_dt:[0m 28
to_Z3: 0.0001s
[1mSolving time: 0.01[0m
[1mSubtyping Check:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,a!1:[32m[v = size!0][39m,
⊢ [95m[v:int | ⊤][39m <: [36m[v:int | (v == x0)][39m

[1mQuery:[0m
[33m∀ size!0, ∀ x0, ∀ v, [39m[32m[39m
[36m((size!0 >= 0) ∧ (∃ x!0, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ (v == x0))))[39m [1m=>[0m
[95m(∃ x!0, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0)))[39m
[1mraw:[0m vc_head(13); vc_body(11)
[1madd_lemma:[0m vc_head(13); vc_body(11)
[1mwithout_dt:[0m 27
to_Z3: 0.0000s
[1mSolving time: 0.01[0m
[1mLet LHS:[0m x!2 => [v:int list | ((size!0 < size!0) ∧ (size!0 >= 0) ∧ (len v size!0) ∧ (∀ u, ((mem v u) => (u == x0))))]
infer x0
infer x!2
[1mApplication Type Check (cons):[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int list | ((size!0 < size!0) ∧ (size!0 >= 0) ∧ (len v size!0) ∧ (∀ u, ((mem v u) => (u == x0))))][39m,
⊢ [95ma!3:[v = x0] → a!4:[v:int | (len x!2 v)] → a!5:[v = x!2] → ? [39m ⇦ [36mh:{v:int | ⊤}→s:{v:int | (v >= 0)}→[v:int list | ((len v s) ∧ (∀ u, ((mem v u) => (u == h))))]→[v:int list | (∀ u, (((u == (s + 1)) => (len v u)) ∧ ((mem v u) => (u == h))))][39m

[1mSubtyping Check:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int list | ((size!0 < size!0) ∧ (size!0 >= 0) ∧ (len v size!0) ∧ (∀ u, ((mem v u) => (u == x0))))][39m,
⊢ [95m[v:int | ⊤][39m <: [36m[v:int | (v == x0)][39m

[1mQuery:[0m
[33m∀ size!0, ∀ x0, ∀ v, [39m[32m[39m
[36m((size!0 >= 0) ∧ (∃ x!0, (∃ x!2, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ (size!0 < size!0) ∧ (size!0 >= 0) ∧ (len x!2 size!0) ∧ (∀ u, ((mem x!2 u) => (u == x0))) ∧ (v == x0)))))[39m [1m=>[0m
[95m(∃ x!0, (∃ x!2, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ (size!0 < size!0) ∧ (size!0 >= 0) ∧ (len x!2 size!0) ∧ (∀ u, ((mem x!2 u) => (u == x0))))))[39m
[1mraw:[0m vc_head(20); vc_body(18)
[1madd_lemma:[0m vc_head(34); vc_body(433)
[1mwithout_dt:[0m 1188
to_Z3: 0.0038s
[1mSolving time: 0.02[0m
[1mSubtyping Check:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int list | ((size!0 < size!0) ∧ (size!0 >= 0) ∧ (len v size!0) ∧ (∀ u, ((mem v u) => (u == x0))))][39m,a!3:[32m[v = x0][39m,
⊢ [95m[v:int | (v >= 0)][39m <: [36m[v:int | (len x!2 v)][39m

[1mQuery:[0m
[33m∀ size!0, ∀ x0, ∀ v, [39m[32m[39m
[36m((size!0 >= 0) ∧ (∃ x!0, (∃ x!2, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ (size!0 < size!0) ∧ (size!0 >= 0) ∧ (len x!2 size!0) ∧ (∀ u, ((mem x!2 u) => (u == x0))) ∧ (len x!2 v)))))[39m [1m=>[0m
[95m(∃ x!0, (∃ x!2, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ (size!0 < size!0) ∧ (size!0 >= 0) ∧ (len x!2 size!0) ∧ (∀ u, ((mem x!2 u) => (u == x0))) ∧ (v >= 0))))[39m
[1mraw:[0m vc_head(20); vc_body(19)
[1madd_lemma:[0m vc_head(34); vc_body(434)
[1mwithout_dt:[0m 1189
to_Z3: 0.0038s
[1mSolving time: 0.02[0m
[1mSubtyping Check:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int list | ((size!0 < size!0) ∧ (size!0 >= 0) ∧ (len v size!0) ∧ (∀ u, ((mem v u) => (u == x0))))][39m,a!3:[32m[v = x0][39m,a!4:[32m[v:int | (len x!2 v)][39m,
⊢ [95m[v:int list | ((size!0 < size!0) ∧ (size!0 >= 0) ∧ (len v size!0) ∧ (∀ u, ((mem v u) => (u == x0))))][39m <: [36m[v:int list | ((len v a!4) ∧ (∀ u, ((mem v u) => (u == a!3))))][39m

[1mQuery:[0m
[33m∀ size!0, ∀ x0, ∀ v, [39m[32m[39m
[36m((size!0 >= 0) ∧ (∃ x!0, (∃ x!2, (∃ a!4, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ (size!0 < size!0) ∧ (size!0 >= 0) ∧ (len x!2 size!0) ∧ (∀ u, ((mem x!2 u) => (u == x0))) ∧ (len x!2 a!4) ∧ (len v a!4) ∧ (∀ u, ((mem v u) => (u == x0))))))))[39m [1m=>[0m
[95m(∃ x!0, (∃ x!2, (∃ a!4, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ (len x!2 size!0) ∧ (∀ u, ((mem x!2 u) => (u == x0))) ∧ (len x!2 a!4) ∧ (size!0 < size!0) ∧ (size!0 >= 0) ∧ (len v size!0) ∧ (∀ u, ((mem v u) => (u == x0)))))))[39m
[1mraw:[0m vc_head(25); vc_body(24)
[1madd_lemma:[0m vc_head(54); vc_body(753)
[1mwithout_dt:[0m 2788
to_Z3: 0.0092s
[1mSolving time: 0.02[0m
[1mConsume variable a!5[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int list | ((size!0 < size!0) ∧ (size!0 >= 0) ∧ (len v size!0) ∧ (∀ u, ((mem v u) => (u == x0))))][39m,a!3:[32m[v = x0][39m,a!4:[32m[v:int | (len x!2 v)][39m,a!5:[32m⟬ [v = x!2] ⟭[39m,
[1mLet LHS:[0m x!3 => [v:int list | (∃ a!4, ((len x!2 a!4) ∧ (∀ u, (((u == (a!4 + 1)) => (len v u)) ∧ ((mem v u) => (u == x0))))))]
infer subs
infer size!0
[1mApplication Type Check (subs):[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int list | ((size!0 < size!0) ∧ (size!0 >= 0) ∧ (len v size!0) ∧ (∀ u, ((mem v u) => (u == x0))))][39m,x!3:[32m[v:int list | (∃ a!4, ((len x!2 a!4) ∧ (∀ u, (((u == (a!4 + 1)) => (len v u)) ∧ ((mem v u) => (u == x0))))))][39m,
⊢ [95ma!7:[v = size!0] → ? [39m ⇦ [36ms:{v:int | ⊤}→[v:int | (v == (s - 1))][39m

[1mSubtyping Check:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int list | ((size!0 < size!0) ∧ (size!0 >= 0) ∧ (len v size!0) ∧ (∀ u, ((mem v u) => (u == x0))))][39m,x!3:[32m[v:int list | (∃ a!4, ((len x!2 a!4) ∧ (∀ u, (((u == (a!4 + 1)) => (len v u)) ∧ ((mem v u) => (u == x0))))))][39m,
⊢ [95m[v:int | ⊤][39m <: [36m[v:int | (v == size!0)][39m

[1mQuery:[0m
[33m∀ size!0, ∀ x0, ∀ v, [39m[32m[39m
[36m((size!0 >= 0) ∧ (∃ x!0, (∃ x!2, (∃ x!3, (∃ a!4, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ (size!0 < size!0) ∧ (size!0 >= 0) ∧ (len x!2 size!0) ∧ (∀ u, ((mem x!2 u) => (u == x0))) ∧ (len x!2 a!4) ∧ (∀ u, ((((1 + a!4) == u) => (len x!3 u)) ∧ ((mem x!3 u) => (u == x0)))) ∧ (size!0 == v)))))))[39m [1m=>[0m
[95m(∃ x!0, (∃ x!2, (∃ x!3, (∃ a!4, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ (size!0 < size!0) ∧ (size!0 >= 0) ∧ (len x!2 size!0) ∧ (∀ u, ((mem x!2 u) => (u == x0))) ∧ (len x!2 a!4) ∧ (∀ u, ((((1 + a!4) == u) => (len x!3 u)) ∧ ((mem x!3 u) => (u == x0)))))))))[39m
[1mraw:[0m vc_head(28); vc_body(26)
[1madd_lemma:[0m vc_head(57); vc_body(2170)
[1mwithout_dt:[0m 6428
to_Z3: 0.0453s
[1mSolving time: 0.06[0m
[1mLet LHS:[0m x!4 => [v:int | (v == (size!0 - 1))]
infer x!4
infer x!3
[1mApplication Type Check (cons):[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int list | ((size!0 < size!0) ∧ (size!0 >= 0) ∧ (len v size!0) ∧ (∀ u, ((mem v u) => (u == x0))))][39m,x!3:[32m[v:int list | (∃ a!4, ((len x!2 a!4) ∧ (∀ u, (((u == (a!4 + 1)) => (len v u)) ∧ ((mem v u) => (u == x0))))))][39m,x!4:[32m[v:int | (v == (size!0 - 1))][39m,
⊢ [95ma!9:[v = x!4] → a!10:[v:int | (len x!3 v)] → a!11:[v = x!3] → ? [39m ⇦ [36mh:{v:int | ⊤}→s:{v:int | (v >= 0)}→[v:int list | ((len v s) ∧ (∀ u, ((mem v u) => (u == h))))]→[v:int list | (∀ u, (((u == (s + 1)) => (len v u)) ∧ ((mem v u) => (u == h))))][39m

[1mSubtyping Check:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int list | ((size!0 < size!0) ∧ (size!0 >= 0) ∧ (len v size!0) ∧ (∀ u, ((mem v u) => (u == x0))))][39m,x!3:[32m[v:int list | (∃ a!4, ((len x!2 a!4) ∧ (∀ u, (((u == (a!4 + 1)) => (len v u)) ∧ ((mem v u) => (u == x0))))))][39m,x!4:[32m[v:int | (v == (size!0 - 1))][39m,
⊢ [95m[v:int | ⊤][39m <: [36m[v:int | (v == x!4)][39m

[1mQuery:[0m
[33m∀ size!0, ∀ x0, ∀ v, [39m[32m[39m
[36m((size!0 >= 0) ∧ (∃ x!0, (∃ x!2, (∃ x!3, (∃ a!4, (∃ x!4, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ (size!0 < size!0) ∧ (size!0 >= 0) ∧ (len x!2 size!0) ∧ (∀ u, ((mem x!2 u) => (u == x0))) ∧ (len x!2 a!4) ∧ (∀ u, ((((1 + a!4) == u) => (len x!3 u)) ∧ ((mem x!3 u) => (u == x0)))) ∧ ((size!0 - 1) == x!4) ∧ (v == x!4))))))))[39m [1m=>[0m
[95m(∃ x!0, (∃ x!2, (∃ x!3, (∃ a!4, (∃ x!4, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ (size!0 < size!0) ∧ (size!0 >= 0) ∧ (len x!2 size!0) ∧ (∀ u, ((mem x!2 u) => (u == x0))) ∧ (len x!2 a!4) ∧ (∀ u, ((((1 + a!4) == u) => (len x!3 u)) ∧ ((mem x!3 u) => (u == x0)))) ∧ ((size!0 - 1) == x!4)))))))[39m
[1mraw:[0m vc_head(29); vc_body(27)
[1madd_lemma:[0m vc_head(58); vc_body(2729)
[1mwithout_dt:[0m 8228
to_Z3: 0.0627s
[1mSolving time: 0.08[0m
[1mSubtyping Check:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int list | ((size!0 < size!0) ∧ (size!0 >= 0) ∧ (len v size!0) ∧ (∀ u, ((mem v u) => (u == x0))))][39m,x!3:[32m[v:int list | (∃ a!4, ((len x!2 a!4) ∧ (∀ u, (((u == (a!4 + 1)) => (len v u)) ∧ ((mem v u) => (u == x0))))))][39m,x!4:[32m[v:int | (v == (size!0 - 1))][39m,a!9:[32m[v = x!4][39m,
⊢ [95m[v:int | (v >= 0)][39m <: [36m[v:int | (len x!3 v)][39m

[1mQuery:[0m
[33m∀ size!0, ∀ x0, ∀ v, [39m[32m[39m
[36m((size!0 >= 0) ∧ (∃ x!0, (∃ x!2, (∃ x!3, (∃ a!4, (∃ x!4, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ (size!0 < size!0) ∧ (size!0 >= 0) ∧ (len x!2 size!0) ∧ (∀ u, ((mem x!2 u) => (u == x0))) ∧ (len x!2 a!4) ∧ (∀ u, ((((1 + a!4) == u) => (len x!3 u)) ∧ ((mem x!3 u) => (u == x0)))) ∧ ((size!0 - 1) == x!4) ∧ (len x!3 v))))))))[39m [1m=>[0m
[95m(∃ x!0, (∃ x!2, (∃ x!3, (∃ a!4, (∃ x!4, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ (size!0 < size!0) ∧ (size!0 >= 0) ∧ (len x!2 size!0) ∧ (∀ u, ((mem x!2 u) => (u == x0))) ∧ (len x!2 a!4) ∧ (∀ u, ((((1 + a!4) == u) => (len x!3 u)) ∧ ((mem x!3 u) => (u == x0)))) ∧ ((size!0 - 1) == x!4) ∧ (v >= 0)))))))[39m
[1mraw:[0m vc_head(29); vc_body(28)
[1madd_lemma:[0m vc_head(58); vc_body(2730)
[1mwithout_dt:[0m 8229
to_Z3: 0.0637s
[1mSolving time: 0.09[0m
[1mSubtyping Check:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int list | ((size!0 < size!0) ∧ (size!0 >= 0) ∧ (len v size!0) ∧ (∀ u, ((mem v u) => (u == x0))))][39m,x!3:[32m[v:int list | (∃ a!4, ((len x!2 a!4) ∧ (∀ u, (((u == (a!4 + 1)) => (len v u)) ∧ ((mem v u) => (u == x0))))))][39m,x!4:[32m[v:int | (v == (size!0 - 1))][39m,a!9:[32m[v = x!4][39m,a!10:[32m[v:int | (len x!3 v)][39m,
⊢ [95m[v:int list | (∃ a!4, ((len x!2 a!4) ∧ (∀ u, (((u == (a!4 + 1)) => (len v u)) ∧ ((mem v u) => (u == x0))))))][39m <: [36m[v:int list | ((len v a!10) ∧ (∀ u, ((mem v u) => (u == a!9))))][39m

[1mQuery:[0m
[33m∀ size!0, ∀ x0, ∀ v, [39m[32m[39m
[36m((size!0 >= 0) ∧ (∃ x!0, (∃ x!2, (∃ x!3, (∃ a!4, (∃ x!4, (∃ a!10, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ (size!0 < size!0) ∧ (size!0 >= 0) ∧ (len x!2 size!0) ∧ (∀ u, ((mem x!2 u) => (u == x0))) ∧ (len x!2 a!4) ∧ (∀ u, ((((1 + a!4) == u) => (len x!3 u)) ∧ ((mem x!3 u) => (u == x0)))) ∧ ((size!0 - 1) == x!4) ∧ (len x!3 a!10) ∧ (len v a!10) ∧ (∀ u, ((mem v u) => (u == x!4)))))))))))[39m [1m=>[0m
[95m(∃ x!0, (∃ x!2, (∃ x!3, (∃ a!14, (∃ x!4, (∃ a!10, (∃ a!4, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (¬ x!0) ∧ (size!0 < size!0) ∧ (size!0 >= 0) ∧ (len x!2 size!0) ∧ (∀ u, ((mem x!2 u) => (u == x0))) ∧ (len x!2 a!14) ∧ (∀ u, ((((1 + a!14) == u) => (len x!3 u)) ∧ ((mem x!3 u) => (u == x0)))) ∧ ((size!0 - 1) == x!4) ∧ (len x!3 a!10) ∧ (len x!2 a!4) ∧ (∀ u, ((((1 + a!4) == u) => (len v u)) ∧ ((mem v u) => (u == x0))))))))))))[39m
[1mraw:[0m vc_head(34); vc_body(36)
[1madd_lemma:[0m vc_head(78); vc_body(4165)
[1mwithout_dt:[0m 14744
to_Z3: 0.1315s
[1mSolving time: 0.16[0m
[1mConsume variable a!11[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int list | ((size!0 < size!0) ∧ (size!0 >= 0) ∧ (len v size!0) ∧ (∀ u, ((mem v u) => (u == x0))))][39m,x!3:[32m[v:int list | (∃ a!4, ((len x!2 a!4) ∧ (∀ u, (((u == (a!4 + 1)) => (len v u)) ∧ ((mem v u) => (u == x0))))))][39m,x!4:[32m[v:int | (v == (size!0 - 1))][39m,a!9:[32m[v = x!4][39m,a!10:[32m[v:int | (len x!3 v)][39m,a!11:[32m⟬ [v = x!3] ⟭[39m,
[1mLet LHS:[0m x!5 => [v:int list | (∃ a!10, ((len x!3 a!10) ∧ (∀ u, (((u == (a!10 + 1)) => (len v u)) ∧ ((mem v u) => (u == x!4))))))]
infer x!5
[1mType Infer:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int list | ((size!0 < size!0) ∧ (size!0 >= 0) ∧ (len v size!0) ∧ (∀ u, ((mem v u) => (u == x0))))][39m,x!3:[32m[v:int list | (∃ a!4, ((len x!2 a!4) ∧ (∀ u, (((u == (a!4 + 1)) => (len v u)) ∧ ((mem v u) => (u == x0))))))][39m,x!4:[32m[v:int | (v == (size!0 - 1))][39m,x!5:[32m[v:int list | (∃ a!10, ((len x!3 a!10) ∧ (∀ u, (((u == (a!10 + 1)) => (len v u)) ∧ ((mem v u) => (u == x!4))))))][39m,
⊢ [95m(x!5 : int list)[39m ⇨ [36m[v:int list | (∃ a!10, ((len x!3 a!10) ∧ (∀ u, (((u == (a!10 + 1)) => (len v u)) ∧ ((mem v u) => (u == x!4))))))][39m

[1mType Infer:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int list | ((size!0 < size!0) ∧ (size!0 >= 0) ∧ (len v size!0) ∧ (∀ u, ((mem v u) => (u == x0))))][39m,x!3:[32m[v:int list | (∃ a!4, ((len x!2 a!4) ∧ (∀ u, (((u == (a!4 + 1)) => (len v u)) ∧ ((mem v u) => (u == x0))))))][39m,x!4:[32m[v:int | (v == (size!0 - 1))][39m,
⊢ [95m(let ((x!5 : int list)) =
   ((cons : int -> int l
...
) : 
   int list) in
 (x!5 : int list) : int list)[39m ⇨ [36m[v:int list | (∃ x!5, (∃ a!18, (∃ a!10, (((len x!3 a!18) ∧ (∀ u, (((u == (a!18 + 1)) => (len x!5 u)) ∧ ((mem x!5 u) => (u == x!4))))) ∧ ((len x!3 a!10) ∧ (∀ u, (((u == (a!10 + 1)) => (len v u)) ∧ ((mem v u) => (u == x!4)))))))))][39m

[1mType Infer:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int list | ((size!0 < size!0) ∧ (size!0 >= 0) ∧ (len v size!0) ∧ (∀ u, ((mem v u) => (u == x0))))][39m,x!3:[32m[v:int list | (∃ a!4, ((len x!2 a!4) ∧ (∀ u, (((u == (a!4 + 1)) => (len v u)) ∧ ((mem v u) => (u == x0))))))][39m,
⊢ [95m(let ((x!4 : int)) = ((subs : int -> int) (size!0 
...
ist) in
  (x!5 : int list) : int list) : int list)[39m ⇨ [36m[v:int list | (∃ x!4, (∃ x!5, (∃ a!18, (∃ a!10, ((x!4 == (size!0 - 1)) ∧ (((len x!3 a!18) ∧ (∀ u, (((u == (a!18 + 1)) => (len x!5 u)) ∧ ((mem x!5 u) => (u == x!4))))) ∧ ((len x!3 a!10) ∧ (∀ u, (((u == (a!10 + 1)) => (len v u)) ∧ ((mem v u) => (u == x!4)))))))))))][39m

[1mType Infer:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,x!2:[32m[v:int list | ((size!0 < size!0) ∧ (size!0 >= 0) ∧ (len v size!0) ∧ (∀ u, ((mem v u) => (u == x0))))][39m,
⊢ [95m(let ((x!3 : int list)) =
   ((cons : int -> int l
...
!5 : int list) : int list) : int list) : int list)[39m ⇨ [36m[v:int list | (∃ x!3, (∃ a!4, (∃ x!4, (∃ x!5, (∃ a!18, (∃ a!10, (((len x!2 a!4) ∧ (∀ u, (((u == (a!4 + 1)) => (len x!3 u)) ∧ ((mem x!3 u) => (u == x0))))) ∧ ((x!4 == (size!0 - 1)) ∧ (((len x!3 a!18) ∧ (∀ u, (((u == (a!18 + 1)) => (len x!5 u)) ∧ ((mem x!5 u) => (u == x!4))))) ∧ ((len x!3 a!10) ∧ (∀ u, (((u == (a!10 + 1)) => (len v u)) ∧ ((mem v u) => (u == x!4))))))))))))))][39m

[1mType Infer:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,b!0:[32m[v:bool | (¬ x!0)][39m,
⊢ [95m(let ((x!2 : int list)) =
   ((goal : int -> int -
...
t) : int list) : int list) : int list) : int list)[39m ⇨ [36m[v:int list | (∃ x!2, (∃ x!3, (∃ a!4, (∃ x!4, (∃ x!5, (∃ a!18, (∃ a!10, (((size!0 < size!0) ∧ (size!0 >= 0) ∧ (len x!2 size!0) ∧ (∀ u, ((mem x!2 u) => (u == x0)))) ∧ (((len x!2 a!4) ∧ (∀ u, (((u == (a!4 + 1)) => (len x!3 u)) ∧ ((mem x!3 u) => (u == x0))))) ∧ ((x!4 == (size!0 - 1)) ∧ (((len x!3 a!18) ∧ (∀ u, (((u == (a!18 + 1)) => (len x!5 u)) ∧ ((mem x!5 u) => (u == x!4))))) ∧ ((len x!3 a!10) ∧ (∀ u, (((u == (a!10 + 1)) => (len v u)) ∧ ((mem v u) => (u == x!4))))))))))))))))][39m

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
 : int list) : int list)) : int list) : 
int list)[39m ⇨ [36m[v:int list | (∃ b!2, (∃ x!30, (∃ b!3, (∃ x!31, (∃ x!32, (∃ a!19, (∃ x!33, (∃ x!34, (∃ a!20, (∃ a!21, ((x!0 ∧ (len x!30 0) ∧ (len v 0)) ∨ ((¬ x!0) ∧ (size!0 < size!0) ∧ (size!0 >= 0) ∧ (len x!31 size!0) ∧ (∀ u, ((mem x!31 u) => (u == x0))) ∧ (len x!31 a!19) ∧ (∀ u, ((((1 + a!19) == u) => (len x!32 u)) ∧ ((mem x!32 u) => (u == x0)))) ∧ ((size!0 - 1) == x!33) ∧ (len x!32 a!20) ∧ (∀ u, ((((1 + a!20) == u) => (len x!34 u)) ∧ ((mem x!34 u) => (u == x!33)))) ∧ (len x!32 a!21) ∧ (∀ u, ((((1 + a!21) == u) => (len v u)) ∧ ((mem v u) => (u == x!33))))))))))))))))][39m

[1mSubtyping Check:[0m
size!0:[32m{v:int | (v >= 0)}[39m,goal:[32ms:{v:int | (v >= 0)}→x:{v:int | ⊤}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (len v s) ∧ (∀ u, ((mem v u) => (u == x))))][39m,x0:[32m{v:int | ⊤}[39m,x!0:[32m[v:bool | ((v <=> (size!0 == 0)) ∧ ((¬ v) <=> (size!0 > 0)))][39m,
⊢ [95m[v:int list | (∃ b!2, (∃ x!30, (∃ b!3, (∃ x!31, (∃ x!32, (∃ a!19, (∃ x!33, (∃ x!34, (∃ a!20, (∃ a!21, ((x!0 ∧ (len x!30 0) ∧ (len v 0)) ∨ ((¬ x!0) ∧ (size!0 < size!0) ∧ (size!0 >= 0) ∧ (len x!31 size!0) ∧ (∀ u, ((mem x!31 u) => (u == x0))) ∧ (len x!31 a!19) ∧ (∀ u, ((((1 + a!19) == u) => (len x!32 u)) ∧ ((mem x!32 u) => (u == x0)))) ∧ ((size!0 - 1) == x!33) ∧ (len x!32 a!20) ∧ (∀ u, ((((1 + a!20) == u) => (len x!34 u)) ∧ ((mem x!34 u) => (u == x!33)))) ∧ (len x!32 a!21) ∧ (∀ u, ((((1 + a!21) == u) => (len v u)) ∧ ((mem v u) => (u == x!33))))))))))))))))][39m <: [36m[v:int list | ((len v size!0) ∧ (∀ u, ((mem v u) => (u == x0))))][39m

[1mQuery:[0m
[33m∀ size!0, ∀ x0, ∀ v, [39m[32m[39m
[36m((size!0 >= 0) ∧ (∃ x!0, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ (len v size!0) ∧ (∀ u, ((mem v u) => (u == x0))))))[39m [1m=>[0m
[95m(∃ x!0, (∃ x!30, (∃ x!31, (∃ x!32, (∃ a!19, (∃ x!33, (∃ x!34, (∃ a!20, (∃ a!21, ((x!0 <=> (0 == size!0)) ∧ ((size!0 > 0) <=> (¬ x!0)) ∧ ((x!0 ∧ (len x!30 0) ∧ (len v 0)) ∨ ((¬ x!0) ∧ (size!0 < size!0) ∧ (size!0 >= 0) ∧ (len x!31 size!0) ∧ (∀ u, ((mem x!31 u) => (u == x0))) ∧ (len x!31 a!19) ∧ (∀ u, ((((1 + a!19) == u) => (len x!32 u)) ∧ ((mem x!32 u) => (u == x0)))) ∧ ((size!0 - 1) == x!33) ∧ (len x!32 a!20) ∧ (∀ u, ((((1 + a!20) == u) => (len x!34 u)) ∧ ((mem x!34 u) => (u == x!33)))) ∧ (len x!32 a!21) ∧ (∀ u, ((((1 + a!21) == u) => (len v u)) ∧ ((mem v u) => (u == x!33))))))))))))))))[39m
[1mraw:[0m vc_head(15); vc_body(46)
[1madd_lemma:[0m vc_head(30); vc_body(6107)
[1mwithout_dt:[0m 13788
to_Z3: 0.1935s
[1mSolving time: 0.33[0m
model:
(define-fun size!0 () Int
  1)
(define-fun x0 () I
...
m ((x!0 Int) (x!1 Int)) Bool
  (= (k!471 x!1) 38))
[38;2;255;165;0mUnder Type Check failed:[39m[typecheck/undercheck.ml:447] Subtyping check: rejected by the verifier
[1m[31mTask 1, type check failed[39m[0m
& goal
 $3$ & $9$ & $3$ & $11$ & $(9, 9)$ & $0.97(0.09)$
