(*generated using Cobalt *) 

 (* Program *) 
 let rec goal    (size : int)  (x0 : int) : (int list) = 
 if (  ( sizecheck  size )  ) 
then 
 [] 
else 
 ( subs  x0 )  ::  ( goal   ( subs  size )  x0 )  

 (* Program *) 
let rec goal    (size : int)  (x0 : int) : (int list) = 
 if (  ( sizecheck  size )  ) 
then 
 [] 
else 
 ( subs  x0 )  ::  ( goal   ( subs  size )   ( gt_eq_int_gen  x0 )  ) 
(* Program Typechecks *) 
let rec goal    (size : int)  (x0 : int) : (int list) = 
 if (  ( sizecheck  size )  ) 
then 
 [] 
else 
 ( subs  x0 )  :: []
(* Program Failed *) 
(let rec goal    (size : int)  (x0 : int) : (int list) = 
 if (  ( sizecheck  size )  ) 
then 
 [] 
else 
 ( subs  size )  ::  ( goal   ( subs  size )  x0 ) 
(* Program Failed *) 
let rec goal    (size : int)  (x0 : int) : (int list) = 
 if (  ( sizecheck  size )  ) 
then 
 [] 
else 
 ( subs  size )  ::  ( goal   ( subs  size )   ( gt_eq_int_gen  x0 )  )  
(* Program Failed *) 
let rec goal    (size : int)  (x0 : int) : (int list) = 
 if (  ( sizecheck  size )  ) 
then 
 [] 
else 
 ( subs  size )  :: []
(* Program Typechecks *) 
let rec goal    (size : int)  (x0 : int) : (int list) = 
 if (  ( sizecheck  size )  ) 
then 
 [] 
else 
 ( gt_eq_int_gen  x0 )  ::  ( goal   ( subs  size )  x0 ) 
(* Program Typechecks*) 
let rec goal    (size : int)  (x0 : int) : (int list) = 
 if (  ( sizecheck  size )  ) 
then 
 [] 
else 
 ( gt_eq_int_gen  x0 )  ::  ( goal   ( subs  size )   ( gt_eq_int_gen  x0 )  ) 
 
(* Program  Typechecks*) 
let rec goal    (size : int)  (x0 : int) : (int list) = 
 if (  ( sizecheck  size )  ) 
then 
 [] 
else 
 ( gt_eq_int_gen  x0 )  :: []
(* Program : Typechecks *) 
let rec goal    (size : int)  (x0 : int) : (int list) = 
 if (  ( sizecheck  size )  ) 
then 
 [] 
else 
x0 ::  ( goal   ( subs  size )  x0 ) 
(* Program *) 
let rec goal    (size : int)  (x0 : int) : (int list) = 
 if (  ( sizecheck  size )  ) 
then 
 [] 
else 
x0 ::  ( goal   ( subs  size )   ( gt_eq_int_gen  x0 )  ) 
(* Program *) 
let rec goal    (size : int)  (x0 : int) : (int list) = 
 if (  ( sizecheck  size )  ) 
then 
 [] 
else 
x0 :: []
(* Program *) 
let rec goal    (size : int)  (x0 : int) : (int list) = 
 if (  ( sizecheck  x0 )  ) 
then 
  ( subs  x0 )  ::  ( goal   ( subs  size )  x0 )  
else 
 ( goal   ( subs  size )   ( gt_eq_int_gen  x0 )  ) 
(* Program *) 
let rec goal    (size : int)  (x0 : int) : (int list) = 
 if (  ( sizecheck  x0 )  ) 
then 
  ( subs  x0 )  ::  ( goal   ( subs  size )  x0 )  
else 
 ( goal   ( subs  size )   ( gt_eq_int_gen  x0 )  ) 
(* Program *) 
let rec goal    (size : int)  (x0 : int) : (int list) = 
 if (  ( sizecheck  x0 )  ) 
then 
  ( subs  x0 )  :: [] 
else 
 ( goal   ( subs  size )   ( gt_eq_int_gen  x0 )  ) 
(* Program *) 
let rec goal    (size : int)  (x0 : int) : (int list) = 
 if (  ( sizecheck  x0 )  ) 
then 
  ( subs  x0 )  :: [] 
else 
 ( goal   ( subs  size )   ( gt_eq_int_gen  x0 )  ) 
(* Program *) 
let rec goal    (size : int)  (x0 : int) : (int list) = 
 if (  ( sizecheck  x0 )  ) 
then 
  ( subs  size )  ::  ( goal   ( subs  size )  x0 )  
else 
 ( goal   ( subs  size )   ( gt_eq_int_gen  x0 )  ) 
(* Program *) 
let rec goal    (size : int)  (x0 : int) : (int list) = 
 if (  ( sizecheck  x0 )  ) 
then 
  ( subs  size )  ::  ( goal   ( subs  size )  x0 )  
else 
 ( goal   ( subs  size )   ( gt_eq_int_gen  x0 )  ) 
(* Program *) 
let rec goal    (size : int)  (x0 : int) : (int list) = 
 if (  ( sizecheck  x0 )  ) 
then 
  ( subs  size )  :: [] 
else 
 ( goal   ( subs  size )   ( gt_eq_int_gen  x0 )  ) 
(* Program *) 
let rec goal    (size : int)  (x0 : int) : (int list) = 
 if (  ( sizecheck  x0 )  ) 
then 
  ( subs  size )  :: [] 
else 
 ( goal   ( subs  size )   ( gt_eq_int_gen  x0 )  ) 
(* Program *) 
let rec goal    (size : int)  (x0 : int) : (int list) = 
 if (  ( sizecheck  x0 )  ) 
then 
 x0 ::  ( goal   ( subs  size )  x0 )  
else 
 ( goal   ( subs  size )   ( gt_eq_int_gen  x0 )  ) 
(* Program *) 
let rec goal    (size : int)  (x0 : int) : (int list) = 
 if (  ( sizecheck  x0 )  ) 
then 
 x0 ::  ( goal   ( subs  size )  x0 )  
else 
 ( goal   ( subs  size )   ( gt_eq_int_gen  x0 )  ) 
(* Program *) 
let rec goal    (size : int)  (x0 : int) : (int list) = 
 if (  ( sizecheck  x0 )  ) 
then 
 x0 :: [] 
else 
 ( goal   ( subs  size )   ( gt_eq_int_gen  x0 )  )   
(* Program *) 
let rec goal    (size : int)  (x0 : int) : (int list) = 
 if (  ( sizecheck  x0 )  ) 
then 
 x0 :: [] 
else 
 ( goal   ( subs  size )   ( gt_eq_int_gen  x0 )  ) 
