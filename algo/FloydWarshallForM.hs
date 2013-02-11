module Main where
import Data.Array.IO
import Data.Map (Map,findWithDefault,fromList)
import Control.Monad
import System.Environment
import System.Time (getClockTime, diffClockTimes) 

type IOAcc = IOUArray Int Int
type Edge = (Int,Int)
type EdgeCost = (Edge,Int)

infinite=50000

solve :: Int -> [EdgeCost] -> IO (Maybe Int)
solve n ecs = do
  a <- init' n ecs
  solve' n (n-1,n-1,n-1) a 0 0 0
  withNegCycle <- hasNegativeCycle n a
  if (withNegCycle) then return Nothing
  else do sh <- shortestPath a
          return $ Just sh

init' :: Int -> [EdgeCost] -> IO IOAcc
init' n ec= do
  let ec'=fromList ec
      f i j | i==j = 0
  	    | otherwise=findWithDefault infinite (i+1,j+1) ec'
      items= [f i j|i<-[0..n-1],j<-[0..n-1]]
  newListArray (0,n*n-1) items

solve' :: Int -> (Int,Int,Int) -> IOAcc -> Int -> Int -> Int -> IO ()
solve' n l@(li,lj,lk) a ii ij ik = do
  forM_ [ik..lk] $ \k->
     forM_ [ii..li] $ \i-> do
       aik <- readArray a (k*n+i)
       forM_ [ij..lj] $ \j-> do
         let jn=j*n;ij=jn+i
         triv <- readArray a ij
         akj <- readArray a (jn+k)
         writeArray a ij $ min triv (aik+akj)

hasNegativeCycle :: Int -> IOAcc -> IO Bool
hasNegativeCycle n a= do
  diags <- mapIndices (0,n-1) (\i->i*n+i) a
  elems <- getElems diags
  return $ any (<0) $ elems

shortestPath :: IOAcc -> IO Int
shortestPath a=do
  elems <- getElems a
  return $ minimum elems

          
-- Actions
readWords :: Read a => String -> [a]
readWords s= ws
 where ws=map read $ words s

readItem :: String -> EdgeCost
readItem str=((i,j),c)
 where [i,j,c]=readWords str

readData :: FilePath -> IO (Int,[EdgeCost])
readData file= do
 (h:s) <-lines `fmap` readFile file
 let [n,_]=readWords h
     items=map readItem s
 return (n,items)
  
solution file= do
  (n,items) <- readData file
  putStrLn "Computing minimum distances..." 
  t1 <- getClockTime 
  sol <- solve n items
  print sol
  t2 <- getClockTime 
  putStrLn ("Time: " ++ show (diffClockTimes t2 t1)) 

main=do
  file:_ <- getArgs 
  solution file >>= print

