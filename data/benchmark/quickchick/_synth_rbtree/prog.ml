(*generated using Cobalt *) 

 (* Program *) 
 let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(*generated using Cobalt *) 

 (* Program *) 
 let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  height )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )  height  ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal  inv1 c  ( increment  s )  )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  s c  ( increment  s )  )  height  ( goal   ( increment  s )  c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )  height  ( goal  inv1 c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  b c b )   ( increment  b )   ( goal  inv1 c  ( increment  height )  )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  inv1 )  c  ( increment  inv1 )  )   ( increment  s )   ( goal   ( increment  inv )  c  ( increment  s )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  s c inv )   ( increment  a )   ( goal   ( increment  r )  c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  inv )  )   ( goal   ( increment  inv )  c  ( increment  inv1 )  )  height  ( goal   ( increment  height )  c  ( increment  inv1 )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv )   ( goal   ( increment  height )  c s )   ( increment  height )   ( goal  inv1 c  ( increment  inv1 )  )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c r )   ( increment  s )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal   ( increment  inv )  c  ( increment  s )  )   ( increment  height )   ( goal   ( increment  height )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c  ( increment  inv1 )  )  height  ( goal  height c height )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  height )   ( goal  a c s )   ( increment  r )   ( goal  n c inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  n )   ( goal  inv1 c height )   ( increment  s )   ( goal  inv1 c r )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  inv )  c height )   ( increment  s )   ( goal   ( increment  s )  c height )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  a )   ( goal  s c inv )   ( increment  inv )   ( goal  a c s )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  inv1 c  ( increment  s )  )   ( increment  s )   ( goal   ( increment  inv )  c height )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  inv )  c  ( increment  inv1 )  )  
else 
if ( c ) 
then 
  ( RbtNode   ( lt_eq_one  inv1 )   ( goal  s c  ( increment  s )  )   ( increment  inv1 )   ( goal  inv1 c inv )  )  
else 
 ( RbtNode   ( lt_eq_one   ( increment  s )  )   ( goal   ( increment  s )  c  ( increment  inv1 )  )   ( increment  inv )   ( goal  inv c s )  ) 
