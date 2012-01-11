--The Trivial Monad
--http://blog.sigfpe.com/2007/04/trivial-monad.html

data Trivial a = Trivial a deriving Show

ret :: a -> Trivial a
ret x = Trivial x

fmp :: (a->b) -> Trivial a -> Trivial b
fmp f (Trivial x) = Trivial (f x)

f :: Int -> Trivial Int
--f x =  Trivial (x+1)
f x=ret (x+1)

bnd :: (a ->  Trivial b) -> Trivial a ->  Trivial b
bnd f (Trivial b) = f b

--{(1) define a function g :: Int -> W Int -> W Int so that g x (W y) = W (x+y). Obviously that definition won't do - the left hand side has a W y pattern so it's actually unwrapping. Rewrite this function so that the only unwrapping that happens is carried out by bind.}--

g :: Int -> Trivial Int -> Trivial Int
g x t = bnd (ret.(x+)) t  
