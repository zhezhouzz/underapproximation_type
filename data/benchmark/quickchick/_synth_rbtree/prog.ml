(*generated using Cobalt *) 

 (* Program *) 
 let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c ( int_gen ()  ) ) , ( int_gen ()  ),  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  inv  false  ( increment  inv )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  inv  true  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  true ( int_gen ()  ) ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv  )  true  ( increment  r )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv false inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r c  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false inv  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv false inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv )  c inv ) ,  ( increment  height ) ,  ( goal  inv false  ( increment  ( int_gen ()  ) )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv  )  true  ( increment  r )  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c ( int_gen ()  ) ) , inv,  ( goal   ( increment  inv  )  true  ( increment  inv  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  r c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r true inv ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  true  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv true  ( increment  inv  )  ) ,  ( increment  inv  ) ,  ( goal  height true  ( increment  ( int_gen ()  ) )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r true inv ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  true  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  true  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  c  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  inv )  false  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  ( int_gen ()  ) )  c height ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  c inv  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  false r ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv )  false r )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) false inv ) , inv,  ( goal   ( increment  inv  )  false  ( increment  r )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false inv  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv false inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv )  c inv ) ,  ( increment  height ) ,  ( goal  inv false  ( increment  ( int_gen ()  ) )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv c  ( increment  ( int_gen ()  ) )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) c inv )  ) ) 
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv )  true  ( increment  inv )  ) ,  ( increment  inv  ) ,  ( goal  r true ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c ( int_gen ()  ) ) , ( int_gen ()  ),  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  r c inv  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  true  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  inv )  false height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  r c inv  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  true  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  inv )  false height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv  )  true  ( increment  r )  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  inv )  false height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c ( int_gen ()  ) ) , ( int_gen ()  ),  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  false r ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv )  false r )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) false inv ) , inv,  ( goal   ( increment  inv  )  false  ( increment  r )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c ( int_gen ()  ) ) , ( int_gen ()  ),  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  r c inv  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  true  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  inv )  false height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal  inv  c ( int_gen ()  ) ) ,  ( increment  inv ) ,  ( goal   ( increment  ( int_gen ()  ) )  true height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv  )  true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  false r ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  inv c inv )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  height ) ,  ( goal  inv true  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  r c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r true inv ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  true  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  r c inv  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  true  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  inv )  false height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false inv  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  inv )  false height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  height )  c  ( increment  inv  )  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  ( int_gen ()  ) )  true height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv c  ( increment  ( int_gen ()  ) )  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c inv )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r c  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  true  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  c  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  height true  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal  r c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) false inv ) , inv,  ( goal   ( increment  inv  )  false  ( increment  r )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  true ( int_gen ()  ) ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  r c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  height ) ,  ( goal  inv true  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c ( int_gen ()  ) ) , ( int_gen ()  ),  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false inv  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  inv  false  ( increment  inv )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  inv  true  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c ( int_gen ()  ) ) , ( int_gen ()  ),  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false inv  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  inv  false  ( increment  inv )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  inv  true  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal  inv  c ( int_gen ()  ) ) ,  ( increment  inv ) ,  ( goal   ( increment  ( int_gen ()  ) )  true height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  height )  c  ( increment  inv  )  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  ( int_gen ()  ) )  true height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  true ( int_gen ()  ) ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv  )  true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  height true  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal  r c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  height true  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  r )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  height )  c  ( increment  inv  )  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  ( int_gen ()  ) )  true height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  true  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  ( int_gen ()  ) )  false  ( increment  height )  ) ,  ( increment  height ) ,  ( goal  ( int_gen ()  ) false  ( increment  inv  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  true  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  c  ( increment  inv )  )  ) )
(* Program *)
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
  if ( sizecheck ( height ) )
  then 
  if ( c )
  then Rbtleaf
  else
  ( Rbtnode (true, Rbtleaf, ( int_gen () ), Rbtleaf) )  
  else
  if ( c )
  then 
  ( Rbtnode ( false, ( goal ( subs ( inv ) ) false  ( subs ( height )  ) ), ( int_gen () ),  ( goal ( subs ( inv ) ) false ( subs ( height ) ) ) ) )
  else
  if ( bool_gen () )   
  then 
  ( Rbtnode ( true, ( goal ( subs ( inv ) ) true height ), ( int_gen () ),  ( goal ( subs ( inv ) ) true height  ) ) )    
  else 
  ( Rbtnode ( false, ( goal ( subs ( subs ( inv ) ) ) false ( subs ( height ) ) ), ( int_gen () ),  ( goal ( subs ( subs ( inv ) ) ) false ( subs ( height ) ) ) ) )
 (* Program *) 
  let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
  if ( sizecheck ( height ) )
  then 
  if ( c )
  then ( Rbtnode (true, Rbtleaf, ( int_gen () ), Rbtleaf) )  
  else
  ( Rbtnode (true, Rbtleaf, ( int_gen () ), Rbtleaf) )  
  else
  if ( c )
  then 
  ( Rbtnode ( false, ( goal ( subs ( inv ) ) false  ( subs ( height )  ) ), ( int_gen () ),  ( goal ( subs ( inv ) ) false ( subs ( height ) ) ) ) )
  else
  if ( bool_gen () )   
  then 
  ( Rbtnode ( true, ( goal ( subs ( inv ) ) true height ), ( int_gen () ),  ( goal ( subs ( inv ) ) true height  ) ) )    
  else 
  ( Rbtnode ( false, ( goal ( subs ( subs ( inv ) ) ) false ( subs ( height ) ) ), ( int_gen () ),  ( goal ( subs ( subs ( inv ) ) ) false ( subs ( height ) ) ) ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
if ( sizecheck ( height ) )
then 
if ( c )
then ( Rbtnode (true, Rbtleaf, ( int_gen () ), Rbtleaf) )  
else
 ( Rbtleaf )
else
if ( c )
then 
( Rbtnode ( false, ( goal ( subs ( inv ) ) false  ( subs ( height )  ) ), ( int_gen () ),  ( goal ( subs ( inv ) ) false ( subs ( height ) ) ) ) )
else
if ( bool_gen () )   
then 
( Rbtnode ( true, ( goal ( subs ( inv ) ) true height ), ( int_gen () ),  ( goal ( subs ( inv ) ) true height  ) ) )    
else 
( Rbtnode ( false, ( goal ( subs ( subs ( subs ( inv ) ) ) ) false ( subs ( subs ( height ) ) ) ), ( int_gen () ),  ( goal ( subs ( subs ( inv ) ) ) false ( subs ( height ) ) ) ) )

 (* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv  )  true  ( increment  r )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv false inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r c  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  height )  c  ( increment  inv  )  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  ( int_gen ()  ) )  true height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  inv )  false  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  ( int_gen ()  ) )  c inv  ) , inv,  ( goal   ( increment  ( int_gen ()  ) )  c inv  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv c  ( increment  ( int_gen ()  ) )  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c inv )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r c  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  r c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  height true  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal  r c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  height true  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  r )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  false  ( increment  inv )  )  
else 
 ( goal   ( increment  inv )  false  ( increment  ( int_gen ()  ) )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c ( int_gen ()  ) ) , ( int_gen ()  ),  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  inv  false  ( increment  inv )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  inv  true  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  true ( int_gen ()  ) ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv  )  true  ( increment  r )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv false inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r c  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false inv  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv false inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv )  c inv ) ,  ( increment  height ) ,  ( goal  inv false  ( increment  ( int_gen ()  ) )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv  )  true  ( increment  r )  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c ( int_gen ()  ) ) , inv,  ( goal   ( increment  inv  )  true  ( increment  inv  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  r c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r true inv ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  true  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv true  ( increment  inv  )  ) ,  ( increment  inv  ) ,  ( goal  height true  ( increment  ( int_gen ()  ) )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r true inv ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  true  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  true  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  c  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  inv )  false  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  ( int_gen ()  ) )  c height ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  c inv  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  false r ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv )  false r )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) false inv ) , inv,  ( goal   ( increment  inv  )  false  ( increment  r )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false inv  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv false inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv )  c inv ) ,  ( increment  height ) ,  ( goal  inv false  ( increment  ( int_gen ()  ) )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv c  ( increment  ( int_gen ()  ) )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) c inv )  ) ) 
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv )  true  ( increment  inv )  ) ,  ( increment  inv  ) ,  ( goal  r true ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c ( int_gen ()  ) ) , ( int_gen ()  ),  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  r c inv  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  true  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  inv )  false height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  r c inv  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  true  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  inv )  false height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv  )  true  ( increment  r )  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  inv )  false height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c ( int_gen ()  ) ) , ( int_gen ()  ),  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  false r ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv )  false r )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) false inv ) , inv,  ( goal   ( increment  inv  )  false  ( increment  r )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c ( int_gen ()  ) ) , ( int_gen ()  ),  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  r c inv  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  true  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  inv )  false height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal  inv  c ( int_gen ()  ) ) ,  ( increment  inv ) ,  ( goal   ( increment  ( int_gen ()  ) )  true height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv  )  true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  false r ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  inv c inv )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  height ) ,  ( goal  inv true  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  r c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r true inv ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  true  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  r c inv  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  true  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  inv )  false height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false inv  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  inv )  false height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  height )  c  ( increment  inv  )  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  ( int_gen ()  ) )  true height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv c  ( increment  ( int_gen ()  ) )  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c inv )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r c  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  true  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  c  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  height true  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal  r c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) false inv ) , inv,  ( goal   ( increment  inv  )  false  ( increment  r )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  true ( int_gen ()  ) ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  r c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  height ) ,  ( goal  inv true  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c ( int_gen ()  ) ) , ( int_gen ()  ),  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false inv  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  inv  false  ( increment  inv )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  inv  true  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c ( int_gen ()  ) ) , ( int_gen ()  ),  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false inv  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  inv  false  ( increment  inv )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  inv  true  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal  inv  c ( int_gen ()  ) ) ,  ( increment  inv ) ,  ( goal   ( increment  ( int_gen ()  ) )  true height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  height )  c  ( increment  inv  )  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  ( int_gen ()  ) )  true height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  true ( int_gen ()  ) ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv  )  true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  height true  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal  r c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  height true  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  r )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  height )  c  ( increment  inv  )  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  ( int_gen ()  ) )  true height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  true  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  ( int_gen ()  ) )  false  ( increment  height )  ) ,  ( increment  height ) ,  ( goal  ( int_gen ()  ) false  ( increment  inv  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  true  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  c  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv  )  true  ( increment  r )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv false inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r c  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  height )  c  ( increment  inv  )  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  ( int_gen ()  ) )  true height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  inv )  false  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  ( int_gen ()  ) )  c inv  ) , inv,  ( goal   ( increment  ( int_gen ()  ) )  c inv  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv c  ( increment  ( int_gen ()  ) )  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c inv )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r c  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  r c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  height true  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal  r c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  height true  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  r )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  height )  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  false  ( increment  inv )  )  
else 
 ( goal   ( increment  inv )  false  ( increment  ( int_gen ()  ) )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c ( int_gen ()  ) ) , ( int_gen ()  ),  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  inv  false  ( increment  inv )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  inv  true  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  true ( int_gen ()  ) ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv  )  true  ( increment  r )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv false inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r c  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false inv  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv false inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv )  c inv ) ,  ( increment  height ) ,  ( goal  inv false  ( increment  ( int_gen ()  ) )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv  )  true  ( increment  r )  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c ( int_gen ()  ) ) , inv,  ( goal   ( increment  inv  )  true  ( increment  inv  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  r c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r true inv ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  true  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv true  ( increment  inv  )  ) ,  ( increment  inv  ) ,  ( goal  height true  ( increment  ( int_gen ()  ) )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r true inv ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  true  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  true  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  c  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  inv )  false  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  ( int_gen ()  ) )  c height ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  c inv  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  false r ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv )  false r )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) false inv ) , inv,  ( goal   ( increment  inv  )  false  ( increment  r )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false inv  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv false inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv )  c inv ) ,  ( increment  height ) ,  ( goal  inv false  ( increment  ( int_gen ()  ) )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv c  ( increment  ( int_gen ()  ) )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) c inv )  ) ) 
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv )  true  ( increment  inv )  ) ,  ( increment  inv  ) ,  ( goal  r true ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c ( int_gen ()  ) ) , ( int_gen ()  ),  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  r c inv  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  true  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  inv )  false height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  r c inv  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  true  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  inv )  false height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv  )  true  ( increment  r )  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  inv )  false height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c ( int_gen ()  ) ) , ( int_gen ()  ),  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  false r ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv )  false r )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) false inv ) , inv,  ( goal   ( increment  inv  )  false  ( increment  r )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c ( int_gen ()  ) ) , ( int_gen ()  ),  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  r c inv  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  true  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  inv )  false height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal  inv  c ( int_gen ()  ) ) ,  ( increment  inv ) ,  ( goal   ( increment  ( int_gen ()  ) )  true height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv  )  true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  false r ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  inv c inv )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  height ) ,  ( goal  inv true  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  r c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r true inv ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  true  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  r c inv  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  true  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  inv )  false height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false inv  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  inv )  false height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  height )  c  ( increment  inv  )  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  ( int_gen ()  ) )  true height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv c  ( increment  ( int_gen ()  ) )  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c inv )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r c  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  true  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  c  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  height true  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal  r c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) false inv ) , inv,  ( goal   ( increment  inv  )  false  ( increment  r )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  true ( int_gen ()  ) ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  r c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  height ) ,  ( goal  inv true  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c ( int_gen ()  ) ) , ( int_gen ()  ),  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false inv  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  inv  false  ( increment  inv )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  inv  true  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c ( int_gen ()  ) ) , ( int_gen ()  ),  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false inv  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  inv  false  ( increment  inv )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  inv  true  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal  inv  c ( int_gen ()  ) ) ,  ( increment  inv ) ,  ( goal   ( increment  ( int_gen ()  ) )  true height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  height )  c  ( increment  inv  )  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  ( int_gen ()  ) )  true height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  true ( int_gen ()  ) ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv  )  true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  height true  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal  r c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  height true  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  r )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  height )  c  ( increment  inv  )  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  ( int_gen ()  ) )  true height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  true  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  ( int_gen ()  ) )  false  ( increment  height )  ) ,  ( increment  height ) ,  ( goal  ( int_gen ()  ) false  ( increment  inv  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  true  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  c  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv  )  true  ( increment  r )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv false inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r c  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  height )  c  ( increment  inv  )  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  ( int_gen ()  ) )  true height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  inv )  false  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  ( int_gen ()  ) )  c inv  ) , inv,  ( goal   ( increment  ( int_gen ()  ) )  c inv  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv c  ( increment  ( int_gen ()  ) )  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c inv )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r c  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  r c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  height true  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal  r c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  height true  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  r )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  inv c inv  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  false  ( increment  inv )  )  
else 
 ( goal   ( increment  inv )  false  ( increment  ( int_gen ()  ) )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c ( int_gen ()  ) ) , ( int_gen ()  ),  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  inv  false  ( increment  inv )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  inv  true  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  true ( int_gen ()  ) ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv  )  true  ( increment  r )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv false inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r c  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false inv  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv false inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv )  c inv ) ,  ( increment  height ) ,  ( goal  inv false  ( increment  ( int_gen ()  ) )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv  )  true  ( increment  r )  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c ( int_gen ()  ) ) , inv,  ( goal   ( increment  inv  )  true  ( increment  inv  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  r c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r true inv ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  true  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv true  ( increment  inv  )  ) ,  ( increment  inv  ) ,  ( goal  height true  ( increment  ( int_gen ()  ) )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r true inv ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  true  ( increment  inv )  )  ) )
(* Program *)
  let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
  if ( sizecheck ( height ) )
  then 
  if ( c )
  then Rbtleaf
  else
  ( Rbtnode (true, Rbtleaf, ( int_gen () ), Rbtleaf) )  
  else
  if ( c )
  then 
  ( Rbtnode ( false, ( goal ( subs ( inv ) ) false  ( subs ( height )  ) ), ( int_gen () ),  ( goal ( subs ( inv ) ) false ( subs ( height ) ) ) ) )
  else
  if ( bool_gen () )   
  then 
  ( Rbtnode ( false, ( goal ( subs ( subs ( inv ) ) ) false ( subs ( height ) ) ), ( int_gen () ),  ( goal ( subs ( subs ( inv ) ) ) false ( subs ( height ) ) ) ) )  
  else 
  ( Rbtnode ( true, ( goal ( subs ( inv ) ) true height ), ( int_gen () ),  ( goal ( subs ( inv ) ) true height  ) ) )    
 (* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  true  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  c  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  inv )  false  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  ( int_gen ()  ) )  c height ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  c inv  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  false r ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv )  false r )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) false inv ) , inv,  ( goal   ( increment  inv  )  false  ( increment  r )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false inv  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv false inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv )  c inv ) ,  ( increment  height ) ,  ( goal  inv false  ( increment  ( int_gen ()  ) )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv c  ( increment  ( int_gen ()  ) )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) c inv )  ) ) 
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv )  true  ( increment  inv )  ) ,  ( increment  inv  ) ,  ( goal  r true ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c ( int_gen ()  ) ) , ( int_gen ()  ),  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  r c inv  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  true  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  inv )  false height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  r c inv  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  true  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  inv )  false height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv  )  true  ( increment  r )  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  inv )  false height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c ( int_gen ()  ) ) , ( int_gen ()  ),  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  false r ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv )  false r )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) false inv ) , inv,  ( goal   ( increment  inv  )  false  ( increment  r )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c ( int_gen ()  ) ) , ( int_gen ()  ),  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  r c inv  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  true  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  inv )  false height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal  inv  c ( int_gen ()  ) ) ,  ( increment  inv ) ,  ( goal   ( increment  ( int_gen ()  ) )  true height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv  )  true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  false r ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  inv c inv )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  height ) ,  ( goal  inv true  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  r c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r true inv ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  true  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  r c inv  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  true  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  inv )  false height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false inv  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  inv )  false height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  height )  c  ( increment  inv  )  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  ( int_gen ()  ) )  true height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv c  ( increment  ( int_gen ()  ) )  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c inv )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r c  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  true  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  c  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  height true  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal  r c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) false inv ) , inv,  ( goal   ( increment  inv  )  false  ( increment  r )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  true ( int_gen ()  ) ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  r c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  height ) ,  ( goal  inv true  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c ( int_gen ()  ) ) , ( int_gen ()  ),  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false inv  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  inv  false  ( increment  inv )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  inv  true  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c ( int_gen ()  ) ) , ( int_gen ()  ),  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false inv  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  inv  false  ( increment  inv )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  inv  true  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal  inv  c ( int_gen ()  ) ) ,  ( increment  inv ) ,  ( goal   ( increment  ( int_gen ()  ) )  true height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  height )  c  ( increment  inv  )  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  ( int_gen ()  ) )  true height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  true ( int_gen ()  ) ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv  )  true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  height true  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal  r c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  height true  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  r )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  height )  c  ( increment  inv  )  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  ( int_gen ()  ) )  true height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  true  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  ( int_gen ()  ) )  false  ( increment  height )  ) ,  ( increment  height ) ,  ( goal  ( int_gen ()  ) false  ( increment  inv  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  true  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  c  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv  )  true  ( increment  r )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv false inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r c  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  height )  c  ( increment  inv  )  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  ( int_gen ()  ) )  true height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  inv )  false  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  ( int_gen ()  ) )  c inv  ) , inv,  ( goal   ( increment  ( int_gen ()  ) )  c inv  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv c  ( increment  ( int_gen ()  ) )  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c inv )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r c  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  r c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  height true  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal  r c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  height true  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  r )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) ,  ( increment  inv  ) ,  ( goal  inv true  ( increment  r )  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  false  ( increment  inv )  )  
else 
 ( goal   ( increment  inv )  false  ( increment  ( int_gen ()  ) )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c ( int_gen ()  ) ) , ( int_gen ()  ),  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  inv  false  ( increment  inv )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  inv  true  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  true ( int_gen ()  ) ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv  )  true  ( increment  r )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv false inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r c  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false inv  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv false inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv )  c inv ) ,  ( increment  height ) ,  ( goal  inv false  ( increment  ( int_gen ()  ) )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv  )  true  ( increment  r )  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c ( int_gen ()  ) ) , inv,  ( goal   ( increment  inv  )  true  ( increment  inv  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  r c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r true inv ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  true  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv true  ( increment  inv  )  ) ,  ( increment  inv  ) ,  ( goal  height true  ( increment  ( int_gen ()  ) )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r true inv ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  true  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  true  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  c  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  inv )  false  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  ( int_gen ()  ) )  c height ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  c inv  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  false r ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv )  false r )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) false inv ) , inv,  ( goal   ( increment  inv  )  false  ( increment  r )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false inv  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv false inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv )  c inv ) ,  ( increment  height ) ,  ( goal  inv false  ( increment  ( int_gen ()  ) )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv c  ( increment  ( int_gen ()  ) )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) c inv )  ) ) 
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv )  true  ( increment  inv )  ) ,  ( increment  inv  ) ,  ( goal  r true ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c ( int_gen ()  ) ) , ( int_gen ()  ),  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  r c inv  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  true  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  inv )  false height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  r c inv  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  true  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  inv )  false height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv  )  true  ( increment  r )  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  inv )  false height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c ( int_gen ()  ) ) , ( int_gen ()  ),  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  false r ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv )  false r )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) false inv ) , inv,  ( goal   ( increment  inv  )  false  ( increment  r )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c ( int_gen ()  ) ) , ( int_gen ()  ),  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  r c inv  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  true  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  inv )  false height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal  inv  c ( int_gen ()  ) ) ,  ( increment  inv ) ,  ( goal   ( increment  ( int_gen ()  ) )  true height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv  )  true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  false r ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  inv c inv )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  height ) ,  ( goal  inv true  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  r c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r true inv ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  true  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  r c inv  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  true  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  inv )  false height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false inv  )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  true  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  inv )  false height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  height )  c  ( increment  inv  )  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  ( int_gen ()  ) )  true height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv c  ( increment  ( int_gen ()  ) )  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c inv )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r c  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  true  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  c  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  inv  )  ) ,  ( increment  inv ) ,  ( goal   ( increment  height )  c ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  height true  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal  r c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) false inv ) , inv,  ( goal   ( increment  inv  )  false  ( increment  r )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  true ( int_gen ()  ) ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  inv c inv ) , ( int_gen ()  ),  ( goal  r c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  height ) ,  ( goal  inv true  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c ( int_gen ()  ) ) , ( int_gen ()  ),  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false inv  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  inv  false  ( increment  inv )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  inv  true  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c ( int_gen ()  ) ) , ( int_gen ()  ),  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false inv  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  inv  false  ( increment  inv )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  inv  true  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal  inv  c ( int_gen ()  ) ) ,  ( increment  inv ) ,  ( goal   ( increment  ( int_gen ()  ) )  true height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  height )  c  ( increment  inv  )  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  ( int_gen ()  ) )  true height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  true ( int_gen ()  ) ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  c inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv  )  true  ( increment  r )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  height true  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal  r c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  height true  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  r )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  height )  c  ( increment  inv  )  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  ( int_gen ()  ) )  true height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  true  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  ( int_gen ()  ) )  false  ( increment  height )  ) ,  ( increment  height ) ,  ( goal  ( int_gen ()  ) false  ( increment  inv  )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  true  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal   ( increment  ( int_gen ()  ) )  c  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  true ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) , Rbtleaf ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r true inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv  )  true  ( increment  r )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv false inv  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r c  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  true  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal   ( increment  height )  c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  height )  true  ( increment  inv  )  )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  height )  c  ( increment  inv  )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  height )  c  ( increment  inv  )  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  ( int_gen ()  ) )  true height )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( goal   ( increment  inv )  false  ( increment  inv )  )  
else 
 ( Rbtnode ( true,  ( goal   ( increment  ( int_gen ()  ) )  c inv  ) , inv,  ( goal   ( increment  ( int_gen ()  ) )  c inv  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) , ( int_gen ()  ),  ( goal  r false inv )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  r c ( int_gen ()  ) ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) false height )  ) ) 
else 
if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  inv c  ( increment  ( int_gen ()  ) )  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c inv )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  r c  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  ( int_gen ()  ) c height )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) , inv ,  ( goal   ( increment  inv  )  false  ( increment  inv )  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false  ( increment  inv )  ) ,  ( increment  inv ) ,  ( goal  r c inv  )  ) ) 
else 
if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  height true  ( increment  ( int_gen ()  ) )  ) ,  ( increment  height ) ,  ( goal  r c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  height true  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  inv  )  false  ( increment  r )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if ( c ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false ( int_gen ()  ) )  ) ) 
else 
if (  ( lt_eq_one  height )  ) 
then 
  ( goal   ( increment  inv )  false  ( increment  inv )  )  
else 
 ( goal   ( increment  inv )  false  ( increment  ( int_gen ()  ) )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) false  ( increment  height )  ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal  inv  true r )  ) ) 
else 
 ( goal   ( increment  ( int_gen ()  ) )  true  ( increment  inv  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  ) ,  ( increment  inv ) ,  ( goal  inv  true inv  )  ) ) 
else 
 ( goal   ( increment  ( int_gen ()  ) )  true  ( increment  inv  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) false  ( increment  height )  ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false inv  )  ) ) 
else 
 ( goal   ( increment  ( int_gen ()  ) )  true  ( increment  inv  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false height ) , inv,  ( goal  ( int_gen ()  ) c  ( increment  ( int_gen ()  ) )  )  ) ) 
else 
 ( goal   ( increment  ( int_gen ()  ) )  true  ( increment  inv  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if (  ( lt_eq_one  inv )  ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv  )  false height ) ,  ( increment  inv ) ,  ( goal  ( int_gen ()  ) false inv  )  ) ) 
else 
 ( goal   ( increment  ( int_gen ()  ) )  true  ( increment  inv  )  ) 
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if (  ( lt_eq_one  height )  ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  c  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  height )  c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  inv true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false  ( increment  height )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if (  ( lt_eq_one  height )  ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  c  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  height )  c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal   ( increment  ( int_gen ()  ) )  true inv  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if (  ( lt_eq_one  height )  ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  c  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  height )  c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  false height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  ( int_gen ()  ) )  true inv  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if (  ( lt_eq_one  height )  ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  c  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  height )  c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) c  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if (  ( lt_eq_one  height )  ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  c  ( increment  inv  )  ) ,  ( increment  height ) ,  ( goal   ( increment  height )  c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  inv true inv ) ,  ( increment  inv  ) ,  ( goal  ( int_gen ()  ) c  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if (  ( lt_eq_one  height )  ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) false inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv )  false height )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  inv true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false  ( increment  height )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if (  ( lt_eq_one  height )  ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) false inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv )  false height )  ) ) 
else 
 ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal   ( increment  ( int_gen ()  ) )  true inv  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if (  ( lt_eq_one  height )  ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) false inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv )  false height )  ) ) 
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  false height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  ( int_gen ()  ) )  true inv  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if (  ( lt_eq_one  height )  ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) false inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv )  false height )  ) ) 
else 
 ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) c  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if (  ( lt_eq_one  height )  ) 
then 
  ( Rbtnode ( true,  ( goal  ( int_gen ()  ) false inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  inv )  false height )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  inv true inv ) ,  ( increment  inv  ) ,  ( goal  ( int_gen ()  ) c  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if (  ( lt_eq_one  height )  ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  ( int_gen ()  ) )  false inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  height )  c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  inv true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false  ( increment  height )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if (  ( lt_eq_one  height )  ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  ( int_gen ()  ) )  false inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  height )  c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal   ( increment  ( int_gen ()  ) )  true inv  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if (  ( lt_eq_one  height )  ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  ( int_gen ()  ) )  false inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  height )  c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  false height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  ( int_gen ()  ) )  true inv  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if (  ( lt_eq_one  height )  ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  ( int_gen ()  ) )  false inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  height )  c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) c  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if (  ( lt_eq_one  height )  ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  ( int_gen ()  ) )  false inv ) ,  ( increment  inv  ) ,  ( goal   ( increment  height )  c  ( increment  height )  )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  inv true inv ) ,  ( increment  inv  ) ,  ( goal  ( int_gen ()  ) c  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if (  ( lt_eq_one  height )  ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  false height ) ,  ( increment  height ) ,  ( goal   ( increment  inv )  false height )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  inv true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false  ( increment  height )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if (  ( lt_eq_one  height )  ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  false height ) ,  ( increment  height ) ,  ( goal   ( increment  inv )  false height )  ) ) 
else 
 ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal   ( increment  ( int_gen ()  ) )  true inv  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if (  ( lt_eq_one  height )  ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  false height ) ,  ( increment  height ) ,  ( goal   ( increment  inv )  false height )  ) ) 
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  false height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  ( int_gen ()  ) )  true inv  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if (  ( lt_eq_one  height )  ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  false height ) ,  ( increment  height ) ,  ( goal   ( increment  inv )  false height )  ) ) 
else 
 ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) c  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if (  ( lt_eq_one  height )  ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  false height ) ,  ( increment  height ) ,  ( goal   ( increment  inv )  false height )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  inv true inv ) ,  ( increment  inv  ) ,  ( goal  ( int_gen ()  ) c  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if (  ( lt_eq_one  height )  ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  false height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv )  false height )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  inv true inv ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) false  ( increment  height )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if (  ( lt_eq_one  height )  ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  false height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv )  false height )  ) ) 
else 
 ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal   ( increment  ( int_gen ()  ) )  true inv  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if (  ( lt_eq_one  height )  ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  false height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv )  false height )  ) ) 
else 
 ( Rbtnode ( true,  ( goal   ( increment  inv  )  false height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  ( int_gen ()  ) )  true inv  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if (  ( lt_eq_one  height )  ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  false height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv )  false height )  ) ) 
else 
 ( Rbtnode ( true,  ( goal   ( increment  height )  c height ) , ( int_gen ()  ),  ( goal  ( int_gen ()  ) c  ( increment  inv )  )  ) )
(* Program *) 
let rec goal    (inv : int)  (c : bool)  (height : int) : (int rbtree) = 
 if (  ( lt_eq_one  height )  ) 
then 
  ( Rbtnode ( true,  ( goal   ( increment  inv )  false height ) ,  ( increment  ( int_gen ()  ) ) ,  ( goal   ( increment  inv )  false height )  ) ) 
else 
 ( Rbtnode ( true,  ( goal  inv true inv ) ,  ( increment  inv  ) ,  ( goal  ( int_gen ()  ) c  ( increment  inv )  )  ) )
