module Queens where
import Data.List

mapIx f= (flip $ zipWith f) [1..]
 
unique xs=nub xs == xs

diagonal xs = any (not.unique) 
              [mapIx (+) xs,mapIx (-) xs]

all' n= permutations [1..n]

reinas n=filter (not.diagonal) $ all' n  
