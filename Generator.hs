module Main where
import Data.List.Class (ListItem(..),fromList,toList,genericTake)
import Control.Monad.ListT
import Control.Monad.Generator
import Control.Monad.Trans

hanoi :: Monad m => Int -> Char -> Char -> Char -> GeneratorT (Char,Char) m ()  
hanoi 0 _ _ _ = return ()
hanoi n from to other = do
  hanoi (n-1) from other to
  yield (from, to)
  hanoi (n-1) other to from

{--
generate $ hanoi 3 'a' 'b' 'c' 
[[('a','b'),('a','c'),('b','c'),('a','b'),('c','a'),('c','b'),('a','b')]]
h 3 a b c
  h 2 a c b 
    h 1 a b c
      h 0 a c b
      (a,b)
      h 0 b c a
    (a,c)
    h 1 b c a
      h 0 b a c
      (b,c)
      h 0 a c b
--}

bits prev= do
  yield prev
  x <- lift "01"
  bits (prev ++ [x])

{--
genericTake 2 $ generate bits ""
ListT {runListT = 
 [Cons {headL = "", 
        tailL = ListT {runListT = 
                 [Cons {headL = "0", 
                        tailL = ListT {runListT = 
                                 [Cons {headL = "00", 
                                        tailL = ListT {runListT = [Nil]}},
                                  Cons {headL = "01", 
                                        tailL = ListT {runListT = [Nil]}}]}},
                  Cons {headL = "1", 
                        tailL = ListT {runListT = 
                                 [Cons {headL = "10", 
                                        tailL = ListT {runListT = [Nil]}},
                                  Cons {headL = "11", tailL = ListT {runListT = [Nil]}}]}}]}}]}
--}
