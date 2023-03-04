{-@ LIQUID "--notermination" @-}
data IncList a =
    Emp
  | (:<) { hd :: a, tl :: IncList a }

infixr 9 :<

{-@ data IncList a =
        Emp
      | (:<) { hd :: a, tl :: IncList {v:a | hd <= v}}  @-}

merge         :: (Ord a) => IncList a -> IncList a -> IncList a
merge xs  Emp = xs
merge Emp ys  = ys
merge (x :< xs) (y :< ys)
  | x < y     = x :< merge xs (y :< ys)
  | x > y     = y :< merge (x :< xs) ys
  | otherwise = x :< merge xs ys
merge _ _     = Emp


merge         :: (Ord a) => IncList a -> IncList a -> IncList a
merge xs  Emp = xs
merge Emp ys  = ys
merge (x :< xs) (y :< ys)
  | x <= y    = x :< merge xs (y :< ys)
  | otherwise = x :< merge xs (y :< ys)
merge _ _     = Emp

merge         :: (Ord a) => IncList a -> IncList a -> IncList a
merge xs  Emp = Emp
merge Emp ys  = Emp
merge (x :< xs) (y :< ys) = x :< merge xs ys
merge _ _     = Emp
