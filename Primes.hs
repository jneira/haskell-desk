module Primes where
import Data.Numbers.Primes
import Data.List

primesWith1=1:primes
combs xs=filter ((>1).length) $ subsequences xs
sumOf x xs=any (x==) $ map sum xs

sumOfPredecesors []=False
sumOfPredecesors xs=sumOf (last xs) $ combs (init xs)
 
s=filter (not.sumOfPredecesors) $  inits $  take 1000 primesWith1

