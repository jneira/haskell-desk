module Exercitium where
import Data.Ord
import Data.List

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

zeckendorf :: Integer -> [Integer]
zeckendorf n | n == h = [n]
             | otherwise = h:zeckendorf (n-h) 
   where h = last $ takeWhile (<=n) fibs

zeckendorf2 n=f n $ reverse $ takeWhile (<=n) fibs 
            where f 0 _=[] 
                  f x (h:hs) = h:f (x-h) (dropWhile(>(x-h))hs)

zeckendorf3 n = last $ sort $ filter ((n==).sum) $ subsequences fibs