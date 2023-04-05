(*generated using Cobalt *) 

 (* Program *) 
 let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) ,  ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  ) ,  ( goal   ( increment  inv )  c inv )  ) ),  ( int_range  inv inv ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) , height,  ( goal   ( increment  inv )  c inv )  ) ) ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs   ( increment  inv )  )  )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_range   ( increment   ( int_range   ( increment  inv )   ( increment   ( int_gen  () )  )  )  )  inv )  c  ( subs   ( increment   ( subs   ( increment   ( subs  height )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  true  ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  ) , inv,  ( goal   ( increment  inv )  c  ( subs   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment  inv )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( int_gen  () ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( subs  height ) ,  ( goal   ( int_gen  () )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment  height )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  true inv ) ,  ( subs   ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  ) ,  ( goal   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  false inv )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) , Rbtleaf ) ),  ( increment  height ) ,  ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  )  ) ),  ( int_range   ( subs  inv )   ( int_gen  () )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  ) ,  ( int_gen  () ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  )  ) ) ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( increment   ( subs  height )  ) ,  ( goal   ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  )  true  ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) , inv,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )  ) ,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  ) ) ) ) 
else 
 ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( increment   ( subs  height )  ) ,  ( goal   ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  )  true  ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( subs  height ) ,  ( goal   ( int_gen  () )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment  height )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment  height )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) , Rbtleaf ) ),  ( increment  height ) ,  ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  )  ) ),  ( int_range   ( subs  inv )   ( int_gen  () )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  ) ,  ( int_gen  () ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  )  ) ) ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  ) , inv,  ( goal   ( increment  inv )  c  ( subs   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment  inv )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) , inv,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )  ) ,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  ) ) ) ) 
else 
 ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) ,  ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  ) ,  ( goal   ( increment  inv )  c inv )  ) ),  ( int_range  inv inv ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) , height,  ( goal   ( increment  inv )  c inv )  ) ) ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs   ( increment  inv )  )  )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_range   ( increment   ( int_range   ( increment  inv )   ( increment   ( int_gen  () )  )  )  )  inv )  c  ( subs   ( increment   ( subs   ( increment   ( subs  height )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  true  ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  ) , inv,  ( goal   ( increment  inv )  c  ( subs   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment  inv )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ),  ( increment   ( int_gen  () )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )  false  ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs   ( increment  inv )  )  )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_range   ( increment   ( int_range   ( increment  inv )   ( increment   ( int_gen  () )  )  )  )  inv )  c  ( subs   ( increment   ( subs   ( increment   ( subs  height )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  true  ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( increment   ( subs  height )  ) ,  ( goal   ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  )  true  ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( int_gen  () ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( increment  inv )  c inv )  ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment   ( int_gen  () )  )   ( sizecheck   ( increment  inv )  )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( goal   ( int_range  height height )  c  ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  height )   ( increment  inv )  )  c  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  ) ,  ( goal   ( increment  inv )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( increment  inv )  c inv )  ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment   ( int_gen  () )  )   ( sizecheck   ( increment  inv )  )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( goal   ( int_range  height height )  c  ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( int_gen  () ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ),  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment   ( int_gen  () )  )   ( sizecheck   ( increment  inv )  )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) ,  ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  ) ,  ( goal   ( increment  inv )  c inv )  ) ),  ( int_range  inv inv ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) , height,  ( goal   ( increment  inv )  c inv )  ) ) ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs   ( increment  inv )  )  )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_range   ( increment   ( int_range   ( increment  inv )   ( increment   ( int_gen  () )  )  )  )  inv )  c  ( subs   ( increment   ( subs   ( increment   ( subs  height )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  true  ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  ) , inv,  ( goal   ( increment  inv )  c  ( subs   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment  inv )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( int_gen  () ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( subs  height ) ,  ( goal   ( int_gen  () )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment  height )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  true inv ) ,  ( subs   ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  ) ,  ( goal   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  false inv )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) , Rbtleaf ) ),  ( increment  height ) ,  ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  )  ) ),  ( int_range   ( subs  inv )   ( int_gen  () )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  ) ,  ( int_gen  () ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  )  ) ) ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( increment   ( subs  height )  ) ,  ( goal   ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  )  true  ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) , inv,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )  ) ,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  ) ) ) ) 
else 
 ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( increment   ( subs  height )  ) ,  ( goal   ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  )  true  ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( subs  height ) ,  ( goal   ( int_gen  () )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment  height )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment  height )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) , Rbtleaf ) ),  ( increment  height ) ,  ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  )  ) ),  ( int_range   ( subs  inv )   ( int_gen  () )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  ) ,  ( int_gen  () ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  )  ) ) ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  ) , inv,  ( goal   ( increment  inv )  c  ( subs   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment  inv )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) , inv,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )  ) ,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  ) ) ) ) 
else 
 ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) ,  ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  ) ,  ( goal   ( increment  inv )  c inv )  ) ),  ( int_range  inv inv ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) , height,  ( goal   ( increment  inv )  c inv )  ) ) ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs   ( increment  inv )  )  )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_range   ( increment   ( int_range   ( increment  inv )   ( increment   ( int_gen  () )  )  )  )  inv )  c  ( subs   ( increment   ( subs   ( increment   ( subs  height )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  true  ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  ) , inv,  ( goal   ( increment  inv )  c  ( subs   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment  inv )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ),  ( increment   ( int_gen  () )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )  false  ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs   ( increment  inv )  )  )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_range   ( increment   ( int_range   ( increment  inv )   ( increment   ( int_gen  () )  )  )  )  inv )  c  ( subs   ( increment   ( subs   ( increment   ( subs  height )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  true  ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( increment   ( subs  height )  ) ,  ( goal   ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  )  true  ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( int_gen  () ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( increment  inv )  c inv )  ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment   ( int_gen  () )  )   ( sizecheck   ( increment  inv )  )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( goal   ( int_range  height height )  c  ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  height )   ( increment  inv )  )  c  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  ) ,  ( goal   ( increment  inv )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( increment  inv )  c inv )  ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment   ( int_gen  () )  )   ( sizecheck   ( increment  inv )  )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( goal   ( int_range  height height )  c  ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( int_gen  () ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment   ( int_gen  () )  )   ( sizecheck   ( increment  inv )  )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) ,  ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  ) ,  ( goal   ( increment  inv )  c inv )  ) ),  ( int_range  inv inv ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) , height,  ( goal   ( increment  inv )  c inv )  ) ) ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs   ( increment  inv )  )  )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_range   ( increment   ( int_range   ( increment  inv )   ( increment   ( int_gen  () )  )  )  )  inv )  c  ( subs   ( increment   ( subs   ( increment   ( subs  height )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  true  ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  ) , inv,  ( goal   ( increment  inv )  c  ( subs   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment  inv )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( int_gen  () ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( subs  height ) ,  ( goal   ( int_gen  () )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment  height )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  true inv ) ,  ( subs   ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  ) ,  ( goal   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  false inv )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) , Rbtleaf ) ),  ( increment  height ) ,  ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  )  ) ),  ( int_range   ( subs  inv )   ( int_gen  () )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  ) ,  ( int_gen  () ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  )  ) ) ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( increment   ( subs  height )  ) ,  ( goal   ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  )  true  ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) , inv,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )  ) ,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  ) ) ) ) 
else 
 ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( increment   ( subs  height )  ) ,  ( goal   ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  )  true  ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( subs  height ) ,  ( goal   ( int_gen  () )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment  height )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment  height )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) , Rbtleaf ) ),  ( increment  height ) ,  ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  )  ) ),  ( int_range   ( subs  inv )   ( int_gen  () )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  ) ,  ( int_gen  () ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  )  ) ) ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  ) , inv,  ( goal   ( increment  inv )  c  ( subs   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment  inv )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) , inv,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )  ) ,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  ) ) ) ) 
else 
 ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) ,  ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  ) ,  ( goal   ( increment  inv )  c inv )  ) ),  ( int_range  inv inv ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) , height,  ( goal   ( increment  inv )  c inv )  ) ) ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs   ( increment  inv )  )  )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_range   ( increment   ( int_range   ( increment  inv )   ( increment   ( int_gen  () )  )  )  )  inv )  c  ( subs   ( increment   ( subs   ( increment   ( subs  height )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  true  ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  ) , inv,  ( goal   ( increment  inv )  c  ( subs   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment  inv )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ),  ( increment   ( int_gen  () )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )  false  ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs   ( increment  inv )  )  )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_range   ( increment   ( int_range   ( increment  inv )   ( increment   ( int_gen  () )  )  )  )  inv )  c  ( subs   ( increment   ( subs   ( increment   ( subs  height )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  true  ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( increment   ( subs  height )  ) ,  ( goal   ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  )  true  ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( int_gen  () ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( increment  inv )  c inv )  ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment   ( int_gen  () )  )   ( sizecheck   ( increment  inv )  )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( goal   ( int_range  height height )  c  ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  height )   ( increment  inv )  )  c  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  ) ,  ( goal   ( increment  inv )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( increment  inv )  c inv )  ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment   ( int_gen  () )  )   ( sizecheck   ( increment  inv )  )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( goal   ( int_range  height height )  c  ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( int_gen  () ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )  r ) ,  ( goal   ( int_range  inv inv )  false inv )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment   ( int_gen  () )  )   ( sizecheck   ( increment  inv )  )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) ,  ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  ) ,  ( goal   ( increment  inv )  c inv )  ) ),  ( int_range  inv inv ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) , height,  ( goal   ( increment  inv )  c inv )  ) ) ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs   ( increment  inv )  )  )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_range   ( increment   ( int_range   ( increment  inv )   ( increment   ( int_gen  () )  )  )  )  inv )  c  ( subs   ( increment   ( subs   ( increment   ( subs  height )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  true  ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  ) , inv,  ( goal   ( increment  inv )  c  ( subs   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment  inv )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( int_gen  () ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( subs  height ) ,  ( goal   ( int_gen  () )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment  height )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  true inv ) ,  ( subs   ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  ) ,  ( goal   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  false inv )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) , Rbtleaf ) ),  ( increment  height ) ,  ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  )  ) ),  ( int_range   ( subs  inv )   ( int_gen  () )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  ) ,  ( int_gen  () ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  )  ) ) ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( increment   ( subs  height )  ) ,  ( goal   ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  )  true  ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) , inv,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )  ) ,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  ) ) ) ) 
else 
 ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( increment   ( subs  height )  ) ,  ( goal   ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  )  true  ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( subs  height ) ,  ( goal   ( int_gen  () )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment  height )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment  height )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) , Rbtleaf ) ),  ( increment  height ) ,  ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  )  ) ),  ( int_range   ( subs  inv )   ( int_gen  () )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  ) ,  ( int_gen  () ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  )  ) ) ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  ) , inv,  ( goal   ( increment  inv )  c  ( subs   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment  inv )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) , inv,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )  ) ,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  ) ) ) ) 
else 
 ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) ,  ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  ) ,  ( goal   ( increment  inv )  c inv )  ) ),  ( int_range  inv inv ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) , height,  ( goal   ( increment  inv )  c inv )  ) ) ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs   ( increment  inv )  )  )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_range   ( increment   ( int_range   ( increment  inv )   ( increment   ( int_gen  () )  )  )  )  inv )  c  ( subs   ( increment   ( subs   ( increment   ( subs  height )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  true  ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  ) , inv,  ( goal   ( increment  inv )  c  ( subs   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment  inv )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ),  ( increment   ( int_gen  () )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )  false  ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs   ( increment  inv )  )  )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_range   ( increment   ( int_range   ( increment  inv )   ( increment   ( int_gen  () )  )  )  )  inv )  c  ( subs   ( increment   ( subs   ( increment   ( subs  height )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  true  ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( increment   ( subs  height )  ) ,  ( goal   ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  )  true  ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( int_gen  () ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( increment  inv )  c inv )  ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment   ( int_gen  () )  )   ( sizecheck   ( increment  inv )  )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( goal   ( int_range  height height )  c  ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  height )   ( increment  inv )  )  c  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  ) ,  ( goal   ( increment  inv )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( increment  inv )  c inv )  ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment   ( int_gen  () )  )   ( sizecheck   ( increment  inv )  )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( goal   ( int_range  height height )  c  ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( int_gen  () ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv inv )  false inv ) ,  ( increment   ( int_gen  () )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment   ( int_gen  () )  )   ( sizecheck   ( increment  inv )  )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) ,  ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  ) ,  ( goal   ( increment  inv )  c inv )  ) ),  ( int_range  inv inv ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) , height,  ( goal   ( increment  inv )  c inv )  ) ) ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs   ( increment  inv )  )  )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_range   ( increment   ( int_range   ( increment  inv )   ( increment   ( int_gen  () )  )  )  )  inv )  c  ( subs   ( increment   ( subs   ( increment   ( subs  height )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  true  ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  ) , inv,  ( goal   ( increment  inv )  c  ( subs   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment  inv )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( int_gen  () ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( subs  height ) ,  ( goal   ( int_gen  () )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment  height )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  true inv ) ,  ( subs   ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  ) ,  ( goal   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  false inv )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) , Rbtleaf ) ),  ( increment  height ) ,  ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  )  ) ),  ( int_range   ( subs  inv )   ( int_gen  () )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  ) ,  ( int_gen  () ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  )  ) ) ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( increment   ( subs  height )  ) ,  ( goal   ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  )  true  ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) , inv,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )  ) ,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  ) ) ) ) 
else 
 ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( increment   ( subs  height )  ) ,  ( goal   ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  )  true  ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( subs  height ) ,  ( goal   ( int_gen  () )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment  height )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment  height )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) , Rbtleaf ) ),  ( increment  height ) ,  ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  )  ) ),  ( int_range   ( subs  inv )   ( int_gen  () )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  ) ,  ( int_gen  () ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  )  ) ) ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  ) , inv,  ( goal   ( increment  inv )  c  ( subs   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment  inv )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) , inv,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )  ) ,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  ) ) ) ) 
else 
 ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) ,  ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  ) ,  ( goal   ( increment  inv )  c inv )  ) ),  ( int_range  inv inv ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) , height,  ( goal   ( increment  inv )  c inv )  ) ) ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs   ( increment  inv )  )  )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_range   ( increment   ( int_range   ( increment  inv )   ( increment   ( int_gen  () )  )  )  )  inv )  c  ( subs   ( increment   ( subs   ( increment   ( subs  height )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  true  ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  ) , inv,  ( goal   ( increment  inv )  c  ( subs   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment  inv )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ),  ( increment   ( int_gen  () )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )  false  ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs   ( increment  inv )  )  )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_range   ( increment   ( int_range   ( increment  inv )   ( increment   ( int_gen  () )  )  )  )  inv )  c  ( subs   ( increment   ( subs   ( increment   ( subs  height )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  true  ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( increment   ( subs  height )  ) ,  ( goal   ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  )  true  ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( int_gen  () ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( increment  inv )  c inv )  ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment   ( int_gen  () )  )   ( sizecheck   ( increment  inv )  )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( goal   ( int_range  height height )  c  ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  height )   ( increment  inv )  )  c  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  ) ,  ( goal   ( increment  inv )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( increment  inv )  c inv )  ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment   ( int_gen  () )  )   ( sizecheck   ( increment  inv )  )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( goal   ( int_range  height height )  c  ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( int_gen  () ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true, Rbtleaf,  ( int_range   ( subs  height )   ( increment  inv )  ) ,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment   ( int_gen  () )  )   ( sizecheck   ( increment  inv )  )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) ,  ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  ) ,  ( goal   ( increment  inv )  c inv )  ) ),  ( int_range  inv inv ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) , height,  ( goal   ( increment  inv )  c inv )  ) ) ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs   ( increment  inv )  )  )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_range   ( increment   ( int_range   ( increment  inv )   ( increment   ( int_gen  () )  )  )  )  inv )  c  ( subs   ( increment   ( subs   ( increment   ( subs  height )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  true  ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  ) , inv,  ( goal   ( increment  inv )  c  ( subs   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment  inv )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( int_gen  () ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( subs  height ) ,  ( goal   ( int_gen  () )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment  height )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  true inv ) ,  ( subs   ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  ) ,  ( goal   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  false inv )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) , Rbtleaf ) ),  ( increment  height ) ,  ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  )  ) ),  ( int_range   ( subs  inv )   ( int_gen  () )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  ) ,  ( int_gen  () ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  )  ) ) ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( increment   ( subs  height )  ) ,  ( goal   ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  )  true  ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) , inv,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )  ) ,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  ) ) ) ) 
else 
 ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( increment   ( subs  height )  ) ,  ( goal   ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  )  true  ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( subs  height ) ,  ( goal   ( int_gen  () )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment  height )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment  height )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) , Rbtleaf ) ),  ( increment  height ) ,  ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  )  ) ),  ( int_range   ( subs  inv )   ( int_gen  () )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  ) ,  ( int_gen  () ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  )  ) ) ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  ) , inv,  ( goal   ( increment  inv )  c  ( subs   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment  inv )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) , inv,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )  ) ,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  ) ) ) ) 
else 
 ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) ,  ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  ) ,  ( goal   ( increment  inv )  c inv )  ) ),  ( int_range  inv inv ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) , height,  ( goal   ( increment  inv )  c inv )  ) ) ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs   ( increment  inv )  )  )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_range   ( increment   ( int_range   ( increment  inv )   ( increment   ( int_gen  () )  )  )  )  inv )  c  ( subs   ( increment   ( subs   ( increment   ( subs  height )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  true  ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  ) , inv,  ( goal   ( increment  inv )  c  ( subs   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment  inv )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ),  ( increment   ( int_gen  () )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )  false  ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs   ( increment  inv )  )  )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_range   ( increment   ( int_range   ( increment  inv )   ( increment   ( int_gen  () )  )  )  )  inv )  c  ( subs   ( increment   ( subs   ( increment   ( subs  height )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  true  ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( increment   ( subs  height )  ) ,  ( goal   ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  )  true  ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( int_gen  () ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( increment  inv )  c inv )  ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment   ( int_gen  () )  )   ( sizecheck   ( increment  inv )  )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( goal   ( int_range  height height )  c  ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  height )   ( increment  inv )  )  c  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  ) ,  ( goal   ( increment  inv )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( increment  inv )  c inv )  ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment   ( int_gen  () )  )   ( sizecheck   ( increment  inv )  )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( goal   ( int_range  height height )  c  ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( int_gen  () ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal  inv false  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range  inv height ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment   ( int_gen  () )  )   ( sizecheck   ( increment  inv )  )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) ,  ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  ) ,  ( goal   ( increment  inv )  c inv )  ) ),  ( int_range  inv inv ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) , height,  ( goal   ( increment  inv )  c inv )  ) ) ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs   ( increment  inv )  )  )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_range   ( increment   ( int_range   ( increment  inv )   ( increment   ( int_gen  () )  )  )  )  inv )  c  ( subs   ( increment   ( subs   ( increment   ( subs  height )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  true  ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  ) , inv,  ( goal   ( increment  inv )  c  ( subs   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment  inv )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( int_gen  () ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( subs  height ) ,  ( goal   ( int_gen  () )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment  height )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  true inv ) ,  ( subs   ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  ) ,  ( goal   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  false inv )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) , Rbtleaf ) ),  ( increment  height ) ,  ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  )  ) ),  ( int_range   ( subs  inv )   ( int_gen  () )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  ) ,  ( int_gen  () ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  )  ) ) ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( increment   ( subs  height )  ) ,  ( goal   ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  )  true  ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) , inv,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )  ) ,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  ) ) ) ) 
else 
 ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( increment   ( subs  height )  ) ,  ( goal   ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  )  true  ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( subs  height ) ,  ( goal   ( int_gen  () )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment  height )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment  height )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) , Rbtleaf ) ),  ( increment  height ) ,  ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  )  ) ),  ( int_range   ( subs  inv )   ( int_gen  () )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  ) ,  ( int_gen  () ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  )  ) ) ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  ) , inv,  ( goal   ( increment  inv )  c  ( subs   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment  inv )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) , inv,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )  ) ,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  ) ) ) ) 
else 
 ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) ,  ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  ) ,  ( goal   ( increment  inv )  c inv )  ) ),  ( int_range  inv inv ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) , height,  ( goal   ( increment  inv )  c inv )  ) ) ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs   ( increment  inv )  )  )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_range   ( increment   ( int_range   ( increment  inv )   ( increment   ( int_gen  () )  )  )  )  inv )  c  ( subs   ( increment   ( subs   ( increment   ( subs  height )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  true  ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  ) , inv,  ( goal   ( increment  inv )  c  ( subs   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment  inv )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ),  ( increment   ( int_gen  () )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )  false  ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs   ( increment  inv )  )  )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_range   ( increment   ( int_range   ( increment  inv )   ( increment   ( int_gen  () )  )  )  )  inv )  c  ( subs   ( increment   ( subs   ( increment   ( subs  height )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  true  ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( increment   ( subs  height )  ) ,  ( goal   ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  )  true  ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( int_gen  () ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( increment  inv )  c inv )  ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment   ( int_gen  () )  )   ( sizecheck   ( increment  inv )  )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( goal   ( int_range  height height )  c  ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  height )   ( increment  inv )  )  c  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  ) ,  ( goal   ( increment  inv )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( increment  inv )  c inv )  ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment   ( int_gen  () )  )   ( sizecheck   ( increment  inv )  )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( goal   ( int_range  height height )  c  ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( int_gen  () ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  ) ,  ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment   ( int_gen  () )  )   ( sizecheck   ( increment  inv )  )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) ,  ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  ) ,  ( goal   ( increment  inv )  c inv )  ) ),  ( int_range  inv inv ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) , height,  ( goal   ( increment  inv )  c inv )  ) ) ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs   ( increment  inv )  )  )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_range   ( increment   ( int_range   ( increment  inv )   ( increment   ( int_gen  () )  )  )  )  inv )  c  ( subs   ( increment   ( subs   ( increment   ( subs  height )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  true  ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  ) , inv,  ( goal   ( increment  inv )  c  ( subs   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment  inv )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( int_gen  () ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( subs  height ) ,  ( goal   ( int_gen  () )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment  height )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  true inv ) ,  ( subs   ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  ) ,  ( goal   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  false inv )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) , Rbtleaf ) ),  ( increment  height ) ,  ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  )  ) ),  ( int_range   ( subs  inv )   ( int_gen  () )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  ) ,  ( int_gen  () ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  )  ) ) ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( increment   ( subs  height )  ) ,  ( goal   ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  )  true  ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) , inv,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )  ) ,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  ) ) ) ) 
else 
 ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( increment   ( subs  height )  ) ,  ( goal   ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  )  true  ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( subs  height ) ,  ( goal   ( int_gen  () )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment  height )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment  height )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) , Rbtleaf ) ),  ( increment  height ) ,  ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  )  ) ),  ( int_range   ( subs  inv )   ( int_gen  () )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  ) ,  ( int_gen  () ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  )  ) ) ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  ) , inv,  ( goal   ( increment  inv )  c  ( subs   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment  inv )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) , inv,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )  ) ,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  ) ) ) ) 
else 
 ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) ,  ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  ) ,  ( goal   ( increment  inv )  c inv )  ) ),  ( int_range  inv inv ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) , height,  ( goal   ( increment  inv )  c inv )  ) ) ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs   ( increment  inv )  )  )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_range   ( increment   ( int_range   ( increment  inv )   ( increment   ( int_gen  () )  )  )  )  inv )  c  ( subs   ( increment   ( subs   ( increment   ( subs  height )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  true  ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  ) , inv,  ( goal   ( increment  inv )  c  ( subs   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment  inv )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ),  ( increment   ( int_gen  () )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )  false  ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs   ( increment  inv )  )  )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_range   ( increment   ( int_range   ( increment  inv )   ( increment   ( int_gen  () )  )  )  )  inv )  c  ( subs   ( increment   ( subs   ( increment   ( subs  height )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  true  ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( increment   ( subs  height )  ) ,  ( goal   ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  )  true  ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( int_gen  () ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( increment  inv )  c inv )  ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment   ( int_gen  () )  )   ( sizecheck   ( increment  inv )  )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( goal   ( int_range  height height )  c  ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  height )   ( increment  inv )  )  c  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  ) ,  ( goal   ( increment  inv )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( increment  inv )  c inv )  ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment   ( int_gen  () )  )   ( sizecheck   ( increment  inv )  )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( goal   ( int_range  height height )  c  ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( int_gen  () ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment   ( int_gen  () )  )   ( sizecheck   ( increment  inv )  )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) ,  ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  ) ,  ( goal   ( increment  inv )  c inv )  ) ),  ( int_range  inv inv ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) , height,  ( goal   ( increment  inv )  c inv )  ) ) ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs   ( increment  inv )  )  )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_range   ( increment   ( int_range   ( increment  inv )   ( increment   ( int_gen  () )  )  )  )  inv )  c  ( subs   ( increment   ( subs   ( increment   ( subs  height )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  true  ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  ) , inv,  ( goal   ( increment  inv )  c  ( subs   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment  inv )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( int_gen  () ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( subs  height ) ,  ( goal   ( int_gen  () )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment  height )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  true inv ) ,  ( subs   ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  ) ,  ( goal   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  false inv )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) , Rbtleaf ) ),  ( increment  height ) ,  ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  )  ) ),  ( int_range   ( subs  inv )   ( int_gen  () )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  ) ,  ( int_gen  () ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  )  ) ) ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( increment   ( subs  height )  ) ,  ( goal   ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  )  true  ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) , inv,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )  ) ,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  ) ) ) ) 
else 
 ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( increment   ( subs  height )  ) ,  ( goal   ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  )  true  ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( subs  height ) ,  ( goal   ( int_gen  () )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment  height )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment  height )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) , Rbtleaf ) ),  ( increment  height ) ,  ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  )  ) ),  ( int_range   ( subs  inv )   ( int_gen  () )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  ) ,  ( int_gen  () ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  )  ) ) ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  ) , inv,  ( goal   ( increment  inv )  c  ( subs   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment  inv )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) , inv,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )  ) ,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  ) ) ) ) 
else 
 ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) ,  ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  ) ,  ( goal   ( increment  inv )  c inv )  ) ),  ( int_range  inv inv ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) , height,  ( goal   ( increment  inv )  c inv )  ) ) ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs   ( increment  inv )  )  )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_range   ( increment   ( int_range   ( increment  inv )   ( increment   ( int_gen  () )  )  )  )  inv )  c  ( subs   ( increment   ( subs   ( increment   ( subs  height )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  true  ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  ) , inv,  ( goal   ( increment  inv )  c  ( subs   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment  inv )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ),  ( increment   ( int_gen  () )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )  false  ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs   ( increment  inv )  )  )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_range   ( increment   ( int_range   ( increment  inv )   ( increment   ( int_gen  () )  )  )  )  inv )  c  ( subs   ( increment   ( subs   ( increment   ( subs  height )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  true  ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( increment   ( subs  height )  ) ,  ( goal   ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  )  true  ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( int_gen  () ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( increment  inv )  c inv )  ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment   ( int_gen  () )  )   ( sizecheck   ( increment  inv )  )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( goal   ( int_range  height height )  c  ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  height )   ( increment  inv )  )  c  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  ) ,  ( goal   ( increment  inv )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( increment  inv )  c inv )  ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment   ( int_gen  () )  )   ( sizecheck   ( increment  inv )  )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( goal   ( int_range  height height )  c  ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( int_gen  () ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( increment   ( int_range   ( increment   ( int_range  height inv )  )  r )  ) ,  ( goal   ( subs   ( int_range   ( increment   ( int_range  height inv )  )   ( increment   ( subs  inv )  )  )  )  c  ( int_range  inv  ( subs  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment   ( int_gen  () )  )   ( sizecheck   ( increment  inv )  )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) ,  ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  ) ,  ( goal   ( increment  inv )  c inv )  ) ),  ( int_range  inv inv ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) , height,  ( goal   ( increment  inv )  c inv )  ) ) ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs   ( increment  inv )  )  )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_range   ( increment   ( int_range   ( increment  inv )   ( increment   ( int_gen  () )  )  )  )  inv )  c  ( subs   ( increment   ( subs   ( increment   ( subs  height )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  true  ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  ) , inv,  ( goal   ( increment  inv )  c  ( subs   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment  inv )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( int_gen  () ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( subs  height ) ,  ( goal   ( int_gen  () )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment  height )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  true inv ) ,  ( subs   ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  ) ,  ( goal   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  false inv )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) , Rbtleaf ) ),  ( increment  height ) ,  ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  )  ) ),  ( int_range   ( subs  inv )   ( int_gen  () )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  ) ,  ( int_gen  () ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  )  ) ) ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( increment   ( subs  height )  ) ,  ( goal   ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  )  true  ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) , inv,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )  ) ,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  ) ) ) ) 
else 
 ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( increment   ( subs  height )  ) ,  ( goal   ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  )  true  ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( subs  height ) ,  ( goal   ( int_gen  () )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment  height )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment  height )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  ) ,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) , Rbtleaf ) ),  ( increment  height ) ,  ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  )  ) ),  ( int_range   ( subs  inv )   ( int_gen  () )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  ) ,  ( int_gen  () ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( goal   ( int_range  height height )  c  ( subs   ( increment  inv )  )  )  ) ) ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  ) , inv,  ( goal   ( increment  inv )  c  ( subs   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment  inv )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) , inv,  ( Rbtnode ( false,  ( goal   ( subs   ( increment  inv )  )  true  ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  ) ,  ( subs   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )  ) ,  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  ) ) ) ) 
else 
 ( goal   ( int_range   ( subs  inv )  height )   ( lt_eq_one   ( subs  height )  )   ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) ,  ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  ) ,  ( goal   ( increment  inv )  c inv )  ) ),  ( int_range  inv inv ) ,  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) , height,  ( goal   ( increment  inv )  c inv )  ) ) ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs   ( increment  inv )  )  )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_range   ( increment   ( int_range   ( increment  inv )   ( increment   ( int_gen  () )  )  )  )  inv )  c  ( subs   ( increment   ( subs   ( increment   ( subs  height )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  true  ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  ) , inv,  ( goal   ( increment  inv )  c  ( subs   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment  inv )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ),  ( increment   ( int_gen  () )  ) ,  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )  false  ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  inv  ( subs   ( increment  inv )  )  )  true  ( int_range  inv  ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_range   ( increment   ( int_range   ( increment  inv )   ( increment   ( int_gen  () )  )  )  )  inv )  c  ( subs   ( increment   ( subs   ( increment   ( subs  height )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  true  ( int_range   ( increment  inv )   ( increment   ( subs  inv )  )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( increment   ( subs  height )  ) ,  ( goal   ( int_range   ( increment   ( subs  height )  )   ( subs   ( int_range  inv inv )  )  )  true  ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( int_range  height height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ),  ( int_range   ( subs  inv )   ( increment   ( subs  inv )  )  ) ,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( int_gen  () ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  height )   ( bool_gen  () )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( increment  inv )  c inv )  ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment   ( int_gen  () )  )   ( sizecheck   ( increment  inv )  )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( goal   ( int_range  height height )  c  ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs  inv )  )  )  c  ( int_range   ( subs  inv )   ( increment  inv )  )  ) ,  ( subs   ( increment   ( subs  height )  )  ) ,  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment  inv )  c inv ) , inv,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  ) ,  ( subs   ( increment   ( int_range   ( subs   ( int_range   ( subs   ( increment  height )  )   ( increment  height )  )  )  inv )  )  ) ,  ( goal   ( increment   ( subs   ( increment  inv )  )  )  false  ( subs   ( increment  inv )  )  )  ) ) 
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  ) ,  ( int_range   ( subs  inv )   ( increment  height )  ) ,  ( goal   ( increment  inv )  false  ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  inv )  )   ( increment   ( subs   ( increment  height )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range  height  ( increment  inv )  )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( subs   ( int_range  inv  ( increment   ( subs  inv )  )  )  )  )  
else 
 ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  height )   ( increment  inv )  )  c  ( subs   ( int_range  inv inv )  )  ) ,  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  ) ,  ( goal   ( increment  inv )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal  inv false  ( increment   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs  height ) , Rbtleaf ) ),  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  ) ,  ( goal   ( int_gen  () )  false  ( subs   ( increment   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( int_range   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )   ( subs   ( increment   ( subs  inv )  )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ),  ( increment  height ) ,  ( goal   ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  true  ( subs   ( increment  inv )  )  )  ) ),  ( increment  inv ) ,  ( Rbtnode ( true,  ( goal   ( int_range  inv  ( subs  height )  )  true  ( increment  height )  ) ,  ( subs  height ) ,  ( Rbtnode ( false,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  ) , inv,  ( goal   ( increment   ( int_range  inv inv )  )  false  ( increment   ( subs   ( int_range  inv  ( increment  inv )  )  )  )  )  ) ) ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  inv )   ( lt_eq_one   ( subs  height )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( goal   ( subs   ( increment   ( subs   ( increment  inv )  )  )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( subs   ( int_range   ( increment  inv )   ( subs  inv )  )  ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( subs  inv )  )  c  ( increment   ( subs   ( increment  inv )  )  )  ) ,  ( increment   ( subs  inv )  ) ,  ( goal   ( int_range   ( increment   ( subs   ( increment  inv )  )  )   ( int_gen  () )  )  false  ( int_range  inv  ( increment  inv )  )  )  ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  ) ,  ( subs  height ) ,  ( goal   ( int_range  inv  ( subs   ( int_range  inv inv )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  ) , inv,  ( goal   ( increment  inv )  c inv )  ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment   ( int_gen  () )  )   ( sizecheck   ( increment  inv )  )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( goal   ( int_range  height height )  c  ( int_range  inv  ( increment  inv )  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( lt_eq_one   ( int_range  inv inv )  )   ( int_gen  () )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) , inv,  ( goal  r c  ( int_range  inv  ( increment  inv )  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs  inv )   ( bool_gen  () )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  ) ,  ( int_range   ( subs  inv )   ( increment   ( int_gen  () )  )  ) ,  ( goal   ( increment   ( int_range  inv  ( subs   ( increment  inv )  )  )  )  true  ( increment   ( subs  height )  )  )  ) ),  ( int_gen  () ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment  height )   ( sizecheck   ( subs   ( increment  inv )  )  )   ( increment   ( subs   ( int_range  height  ( increment  inv )  )  )  )  )  
else 
 ( Rbtnode ( false,  ( Rbtnode ( true,  ( goal   ( int_range  height height )   ( sizecheck   ( subs  inv )  )   ( int_range   ( subs  inv )   ( subs  inv )  )  ) ,  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( subs  inv )  )  ) ),  ( subs   ( increment   ( subs  inv )  )  ) ,  ( goal  inv false  ( increment   ( subs   ( increment  inv )  )  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( false,  ( Rbtnode ( true,  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv height )  )  false  ( subs   ( int_range  inv  ( subs  inv )  )  )  ) ,  ( subs   ( increment  inv )  ) ,  ( goal   ( int_range   ( increment   ( int_range  height inv )  )  r )  true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ),  ( int_range  inv height ) ,  ( Rbtnode ( false,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  ) ,  ( subs  inv ) ,  ( goal   ( subs  height )  true  ( subs   ( int_range  inv inv )  )  )  ) ) ) ),  ( subs   ( int_range  inv inv )  ) ,  ( goal  inv true  ( subs   ( int_range   ( subs  height )   ( increment  inv )  )  )  )  ) ) 
else 
if ( c ) 
then 
  ( goal   ( subs   ( increment   ( subs  inv )  )  )   ( bool_gen  () )   ( subs   ( int_range   ( increment  inv )   ( subs   ( int_range  inv inv )  )  )  )  )  
else 
if ( c ) 
then 
  ( Rbtnode ( false,  ( goal   ( subs   ( int_range  inv  ( increment  inv )  )  )  false  ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  ) ,  ( increment  height ) ,  ( Rbtnode ( true,  ( goal   ( int_range   ( increment  inv )   ( increment   ( subs  height )  )  )  false  ( increment  height )  ) ,  ( subs   ( int_range  inv inv )  ) ,  ( goal   ( subs   ( increment   ( subs  height )  )  )  false  ( increment  height )  )  ) ) ) ) 
else 
if ( c ) 
then 
  ( goal   ( increment   ( int_gen  () )  )   ( sizecheck   ( increment  inv )  )   ( subs   ( int_range   ( subs  inv )   ( subs   ( increment  height )  )  )  )  )  
else 
if ( c ) 
then 
  ( goal   ( int_range   ( subs   ( increment  height )  )   ( int_gen  () )  )   ( lt_eq_one   ( subs   ( increment  inv )  )  )   ( int_range   ( subs   ( increment  height )  )   ( increment   ( subs  inv )  )  )  )  
else 
 ( goal   ( subs   ( increment  inv )  )   ( sizecheck   ( subs  inv )  )   ( increment   ( int_range   ( subs  inv )   ( int_gen  () )  )  )  ) 
