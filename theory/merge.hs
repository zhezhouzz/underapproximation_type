{-@ LIQUID "--notermination" @-}
import Data.Set hiding (filter, split)
import Prelude  hiding (reverse, filter)

data Vec a = Nil | Cons a (Vec a)

{-@ measure elts @-}
elts :: (Ord a) => Vec a -> Set a
elts Nil = empty
elts (Cons x xs) = singleton x `union` elts xs

{-@ type ListS a S = {v:Vec a | elts v = S} @-}
{-@ type ListUn a X Y = ListS a {Set_cup (elts X) (elts Y)} @-}

{-@ merge  :: (Ord a) => l1:_ -> l2:_ -> ListUn a l1 l2  @-}
merge l1 l2 =
 case l1 of
   Nil -> l2
   (Cons h1 t1) ->
     case l2 of
       Nil -> l1
       (Cons h2 t2) ->
         if h1 < h2
         then Cons h1 (merge t1 l2)
         else if h2 < h1
         then Cons h2 (merge l1 t2)
         else merge t1 t2
