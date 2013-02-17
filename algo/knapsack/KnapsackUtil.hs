module KnapsackUtil where
import Data.List.Split

type Value=Int
type Size=Int

readWords :: Read a => String -> [a]
readWords s= ws
 where ws=map read $ words s

readItem :: String -> (Value,Size)
readItem str=(v,s)
 where [v,s]=readWords str

readData' file n= do
 (h:s) <-lines `fmap` readFile file
 let [c,m]=readWords h
     n'=if (n==0 || n > m) then m else n
     items=map readItem $ take n' s
 return ((c,n'),items)

readData f =readData' f 0
