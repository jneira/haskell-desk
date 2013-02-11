module Queens where
import Data.List
-- Based in http://www.solveet.com/exercises/Problema-de-las-ocho-reinas/50/solution-336
mapIx f= (flip $ zipWith f) [0..]
 
unique xs=nub xs == xs

diagonal xs = any (not.unique) 
              [mapIx (+) xs,mapIx (-) xs]

all' n= permutations [1..n]

reinas n=filter (not.diagonal) $ all' n  
