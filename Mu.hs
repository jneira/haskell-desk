{-# LANGUAGE ExistentialQuantification,RankNTypes #-}
-- Ver http://en.wikibooks.org/wiki/Haskell/Polymorphism
-- sobre RankNTypes
module Mu where

{--
Cada tipo inductivo regular puede considerarse solución 
de una ecuación fix-point construida sobre un functor base
(https://twitter.com/#!/joseanpg/status/156123214093881344)

Como ejemplo sencillo podemos coger el típico N = Zero | Succ N
(https://twitter.com/#!/joseanpg/status/156123394667053056)
--}
data N = Zero|Succ N
              deriving(Show)
two=Succ(Succ Zero)
{--
El functor base para dicho tipo es polinomico (más fácil): F t = Zero | Succ t
(https://twitter.com/#!/joseanpg/status/156123600645128192)
--}
data F t=FZero|FSucc t
               deriving(Show)
ftwo=FSucc(FSucc FZero)
{--
Es obvio que si N = Zero | Succ N entonces N = F N
https://twitter.com/#!/joseanpg/status/156123783336443904
--}
newtype FN=FN(F FN)
           deriving(Show)
fntwo=FN (FSucc (FN (FSucc (FN FZero))))
{--
Dado un functor F entonces μ F 'denotará' la solución de la ecuación X = F X, 
es decir: μ F = F (μ F). 
(https://twitter.com/#!/joseanpg/status/156127030046490625)
Por cierto, N = F N → N = μ F.
(https://twitter.com/#!/joseanpg/status/156127681237352449)
[ #Haskell ] newtype μ f = In ( f (μ f))
(https://twitter.com/#!/joseanpg/status/156137618143322112)
--}
newtype Mu f=In (f (Mu f))

newtype MuN=MuN(Mu F)
muNTwo=MuN(In (FSucc (In (FSucc (In FZero)))))            
-- from http://en.wikibooks.org/wiki/Haskell/Fix_and_recursion
newtype Mu' f=Mu' (forall a.(f a->a)->a)
data Nu f=forall a.Nu a (a->f a)
