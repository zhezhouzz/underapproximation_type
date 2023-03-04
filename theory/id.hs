{-@ LIQUID "--notermination" @-} 
import Data.Set hiding (filter, split)
import Prelude  hiding (reverse, filter)

data Vec a = Nil | Cons a (Vec a)

{-@ measure elts @-}
elts :: (Ord a) => Vec a -> Set a
elts Nil = empty
elts (Cons x xs) = singleton x `union` elts xs


{-@ id  :: (Eq a) => x:a -> l:Vec a -> {v:Vec a | member x (elts v)}  @-}
id x l =
  let l' = Cons x l
  in
    case l' of
      Nil -> Nil
      (Cons h t) -> t
