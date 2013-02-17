module Main where
import Data.List (foldl')
import Data.Array.Diff
import qualified Data.MemoCombinators as Mem
import System.Environment

type Value=Int
type Size=Int
type Capacity=Int
type Items=Array Int (Value,Size)
type UAcc=DiffUArray Int Int

solve :: Items -> Capacity -> Value
solve v w=memo w ! n
  where (_,n)=bounds v
        memo=Mem.integral go :: (Int -> UAcc)
        go 0=listArray (0,n) $ repeat 0
        go j=foldl' (step j) (go 0) [1..n]
        step j a i=a//[(i,val)]
          where val | wi > j = a!(i-1)
                    | otherwise=max (a!(i-1)) (vi + (a'!(i-1)))
                (vi,wi)=v!i
                a'=memo (j-wi)

readItem :: String -> (Value,Size)
readItem str=(v,s)
 where [v,s]=map read $ words str

main= do
  [file] <- getArgs
  (h:s) <-lines `fmap` readFile file
  let [c,n]=map read $ words h
      items=map readItem $ take n s
      v=listArray (1,n) items
  print $ solve v c
