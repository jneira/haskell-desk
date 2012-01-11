import IO
import Monad
import System
import Char
--import Control.Monad.Cont


newtype Cont r a = Cont { runCont :: ((a -> r) -> r) } 
-- r is the final result type of the whole computation 
  
instance Monad (Cont r) where 
    return a       = Cont $ \k -> k a                       
    -- i.e. return a = \k -> k a 
    (Cont c) >>= f = Cont $ \k -> c (\a -> runCont (f a) k) 
    -- i.e. c >>= f = \k -> c (\a -> f a k) 

instance Functor (Cont r) where
  fmap f = \c -> Cont (\k -> runCont c (k . f))

class (Monad m) => MonadCont m where 
    callCC :: ((a -> m b) -> m a) -> m a 
 
instance MonadCont (Cont r) where 
    callCC f = Cont $ \k -> runCont (f (\a -> Cont $ \_ -> k a)) k

fun :: Int -> String
fun n = (`runCont` id) $ do
        str <- callCC $ \exit1 -> do                        
          -- define "exit1"
          when (n < 10) (exit1 (show n))
          let ns = map digitToInt (show (n `div` 2))
          n' <- callCC $ \exit2 -> do                       
            -- define "exit2"
            when ((length ns) < 3) (exit2 (length ns))
            when ((length ns) < 5) (exit2 n)
            when ((length ns) < 7) $ do let ns' = map intToDigit (reverse ns)
                                        exit1 (dropWhile (=='0') ns')  
                                          --escape 2 levels
            return $ sum ns
          return $ "(ns = " ++ (show ns) ++ ") " ++ (show n')
        return $ "Answer: " ++ str


add :: Int -> Int -> Int
add x y = x + y

square :: Int -> Int
square x = x * x

--A simple module, no continuations
-- We assume some primitives add and square for the example:
pythagoras :: Int -> Int -> Int
pythagoras x y = add (square x) (square y)

--A simple module, using continuations
-- We assume CPS versions of the add and square primitives,
-- (note: the actual definitions of add'cps and square'cps are not
-- in CPS form, they just have the correct type)

add'cps :: Int -> Int -> (Int -> r) -> r
add'cps x y k = k (add x y)

square'cps :: Int -> (Int -> r) -> r
square'cps x k = k (square x)

pythagoras'cps :: Int -> Int -> (Int -> r) -> r
pythagoras'cps x y k =
 square'cps x $ \x'squared ->
 square'cps y $ \y'squared ->
 add'cps x'squared y'squared $ \sum'of'squares ->
 k sum'of'squares


add'cont :: Int -> Int -> Cont r Int
add'cont x y = return (add x y)

square'cont :: Int -> Cont r Int
square'cont x = return (square x)

pythagoras'cont :: Int -> Int -> Cont r Int
pythagoras'cont x y =
    do x'squared <- square'cont x
       y'squared <- square'cont y
       sum'of'squares <- add'cont x'squared y'squared
       return sum'of'squares

-- *Main> runCont (pythagoras'cont 3 4) print
-- 25

addThree'cont :: Int -> Cont r Int
addThree'cont x = return (x + 3)

example = runCont (square'cont 4 >>= addThree'cont) print

