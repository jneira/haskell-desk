module Main where
-- http://bartoszmilewski.com/2014/01/14/functors-are-containers/

newtype Const b a = Const { getConst :: b }
instance Functor (Const b) where
    fmap _ (Const x) = Const x

newtype Identity a = Identity { runId :: a }
instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

newtype Reader e a = Reader (e -> a)
instance Functor (Reader e) where  
    fmap f (Reader g) = Reader (\x -> f (g x))

