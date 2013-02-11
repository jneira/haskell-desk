module MonadTutorial
       where
import Control.Monad
import Control.Monad.State

-- example 1
type Sheep = String

father :: Sheep -> Maybe Sheep
father s= Just $ "dad of " ++ s

mother :: Sheep -> Maybe Sheep
mother s= Just $ "mum of " ++ s 

test :: String -> Maybe Int
test _=Just 0

type ParserT a = StateT String Maybe a
type Parser a=State String a

s1=state (\st->(length st,st))
psMult2 i=state $ (,) (2 * i)  

s2=get >>= \v-> s1 >>= psMult2 >>= \w->  put (v++"adios") >> return w  
t1=runState s2 "hola"
-- (8,"holaadios")
t2=runState (s2>>s2) "hola"
-- (18,"holaadiosadios")

-- If your monad has a *terminator* element such that 
-- *terminator >>= f = terminator*, then you can use >>= recursively 
badmany p = p >>= \x-> badmany p >>= \xs-> return (x:xs) 

