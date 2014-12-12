{-# LANGUAGE MultiParamTypeClasses  #-}
module TypeClasses where

class Class1 a b where
      testA :: a -> b
      testB :: b -> a

class Class2 a b where
      testC :: a -> b
      testD :: b -> a

class (Class1 a b,Class2 a b) => Class3 a b c where
      testF :: a -> b -> c
  
class (Class1 a b,Class2 a b) => Class4 a b c where
      testG :: a -> b -> c