module FoldTree where 

data Tree a = Leaf a | Trees [Tree a] deriving(Show)
cow = Trees [Leaf 1, Trees [ Leaf 2, Leaf 3], Leaf 4]

-- Using concatMap

flatten1 :: Tree a -> [a]
flatten1 (Leaf a) = [a]
flatten1 (Trees trees) = trees >>= flatten1


-- Using foldr

flatten2 :: Tree a -> [a]
flatten2 = foldrTree (:) []


-- It's possible a version with instance Foldable Tree where

foldlTree :: (b->a->b)->b->(Tree a)->b
foldlTree f acu (Leaf x) = f acu x
foldlTree f acu (Trees []) = acu
foldlTree f acu (Trees (t:ts)) = 
  let acu' = (foldlTree f acu t)
  in foldlTree f acu' (Trees ts)

foldrTree :: (a->b->b)->b->(Tree a)->b
foldrTree f acu (Leaf x) = f x acu
foldrTree f acu (Trees []) = acu
foldrTree f acu (Trees (t:ts)) = 
  let acu' = foldrTree f acu (Trees ts)
  in foldrTree f acu' t
