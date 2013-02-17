module Main where
import Data.Array.Diff
import Data.List (foldl')
import Data.List.Split
import System.Environment
import KnapsackUtil

type Capacity=Int
type Items=Array Int (Value,Size)
type UAcc=DiffUArray Int Int

solve :: Capacity -> Items -> Value
solve w v=solve' w v ! (((n+1)*(w+1))-1)
  where (_,n)=bounds v
        
solve' :: Capacity -> Items -> UAcc
solve' w v= foldl' (step w v) init 
                [i |i<-[li+(w+1)..hi],i `mod` (w+1)/=0]
 where (_,n)=bounds v
       (li,hi)=bounds init
       init=init' n w

init' :: Int -> Int -> UAcc
init' n w=listArray (0,((n+1)*(w+1))-1) $ repeat 0

step:: Capacity -> Items -> UAcc -> Int -> UAcc
step w v a k=a//[(k,val)]
 where val | wi>j = a!i'
           | otherwise = max (a!i') (vi + (a!ij'))
         where  w'=w+1
                (j,i) = ((k `mod` w'),k `div` w')
                (vi,wi)=v!i
                (i',ij')=(k-w',k-w'-wi)

showU :: Capacity -> UAcc -> String
showU w acc = unlines $ map unwords splited
  where xs=map (\e->show e ++ (spc $ show e)) $ elems acc
        splited=splitEvery (w+1) xs
        spc e= replicate (3-(length e)) ' '

main= do
  [file] <- getArgs
  ((c,n),its)<-readData file
  let arr=listArray (1,n) its
  print $ solve c arr
