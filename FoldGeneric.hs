{-# LANGUAGE ExistentialQuantification,RankNTypes #-}
-- Ver http://en.wikibooks.org/wiki/Haskell/Polymorphism
-- sobre RankNTypes
module FoldGeneric where

-- from http://en.wikibooks.org/wiki/Haskell/Fix_and_recursion
newtype Mu f=Mu (forall a.(f a->a)->a)
data Nu f=forall a.Nu a (a->f a)

fold' :: (f a -> a) -> Mu f -> a
fold' g (Mu f)=f g
unfold' :: (a -> f a) -> a -> Nu f
unfold' f x=Nu x f
--refold' :: (a -> f a) -> (g a-> a) -> Mu f -> Nu g
--refold' f g=unfold' g . fold' f

newtype T=T (forall a.a->a)
t=T (id::a->a)

newtype T' f=T'(forall a.f a->a)
t'=T'(head::[a]->a)

f :: (forall a.([a]->a)->a)
f g=undefined
mu=Mu(f)

newtype Stream a=Stream (Nu ((,) a)) -- forsome b. (b,b->(a,b))
newtype Void a=Void (Mu ((,) a)) -- forall b.((a,b)->b)->b

-- Mu (forall a.(f a->a)->a)
newtype MuList=MuList(Mu [])

newtype Rank2OnList = MakeRank2OnList (forall a . [a]->a)
newtype Rank2Poly f = MakeRank2Poly (forall a . f a->a)

newtype PolyList=MkPolyList(forall a . [a])
