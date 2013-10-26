{-# LANGUAGE ExistentialQuantification,RankNTypes #-}
-- Ver http://en.wikibooks.org/wiki/Haskell/Polymorphism
-- sobre RankNTypes
module Mu where
import Control.Applicative
{--
Cada tipo inductivo regular puede considerarse solución 
de una ecuación fix-point construida sobre un functor base
(https://twitter.com/#!/joseanpg/status/156123214093881344)

Como ejemplo sencillo podemos coger el típico N = Zero | Succ N
(https://twitter.com/#!/joseanpg/status/156123394667053056)
--}
data Nat = Zero|Succ Nat
              deriving(Show)
two=Succ(Succ Zero)
{--
El functor base para dicho tipo es polinomico (más fácil): F t = Zero | Succ t
(https://twitter.com/#!/joseanpg/status/156123600645128192)
--}
data FNat t=FZero|FSucc t
               deriving(Show)
two'=FSucc(FSucc FZero)

{--
Es obvio que si N = Zero | Succ N entonces N = F N
https://twitter.com/#!/joseanpg/status/156123783336443904
--}
newtype Nat'=Nat'(FNat Nat')
           deriving(Show)
two''=Nat' (FSucc (Nat' (FSucc (Nat' FZero))))
{--
Dado un functor F entonces μ F 'denotará' la solución de la ecuación X = F X, 
es decir: μ F = F (μ F). 
(https://twitter.com/#!/joseanpg/status/156127030046490625)
Por cierto, N = F N → N = μ F.
(https://twitter.com/#!/joseanpg/status/156127681237352449)
[ #Haskell ] newtype μ f = In ( f (μ f))
(https://twitter.com/#!/joseanpg/status/156137618143322112)
--}
newtype Mu f=In(f (Mu f))

newtype Nat''=Nat''(Mu FNat)
two'''=Nat''(In (FSucc (In (FSucc (In FZero)))))            

-- from http://stackoverflow.com/questions/4434292/catamorphism-and-tree-traversing-in-haskell

data TreeNode a child
    = Leaf a | 
      Branch child child
      deriving(Show)
type IntNode = TreeNode Int      
type IntTree = Mu IntNode

instance Functor (TreeNode a) where
    fmap f (Leaf x) = Leaf x 
    fmap f (Branch l r)=Branch (f l) (f r)

cata:: (Functor node)=>(node r -> r) -> Mu node -> r
cata f (In t)=f (fmap (cata f) t)

data TreeAlgebra a r = 
  TreeAlgebra { leaf   :: a      -> r, 
                branch :: r -> r -> r }
  
foldFunction :: TreeAlgebra a r -> (TreeNode a r -> r)
foldFunction alg (Leaf a) = leaf alg a
foldFunction alg (Branch l r) = branch alg l r

type Tree a=Mu (TreeNode a)
treeCata:: TreeAlgebra a r -> (Tree a -> r)
treeCata alg = cata (foldFunction alg)

tree :: IntTree 
tree=In(Branch (In (Leaf 1)) (In (Leaf 2)))

treeAlg=TreeAlgebra{leaf=id,branch=(+)}

test=treeCata treeAlg tree 
-- tes
