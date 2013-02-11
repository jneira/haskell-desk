module Main where
import Data.Graph
import Data.Array.Diff
import Data.Heap 
import Data.Set hiding (map,toList,fromList,fromAscList,size,insert)
import qualified Data.Set as Set (toList,fromList,fromAscList,size,insert)
import Data.List (minimumBy)
import Data.Ord (comparing)
import System.Environment
import System.Time (getClockTime, diffClockTimes) 

type Vertices=DiffUArray Vertex Vertex
type MinPaths=(Vertices,Vertices)
type GraphWithDists=Table [(Vertex,Int)] 

inf :: Int
inf= maxBound `div` 2

solve :: GraphWithDists -> Vertex -> MinPaths  
solve g v=step g (initMinPaths (Set.size q) v) q
  where q= Set.fromList $ vertices $ fmap (map fst) g 

initMinPaths :: Int -> Vertex -> MinPaths
initMinPaths n v= (dist//[(v,0)],previous)
  where dist=listArray (1,n) $ repeat inf   
        previous=listArray (1,n) $ repeat v

step :: GraphWithDists -> MinPaths -> Set Vertex -> MinPaths 
step g mps@(dists,previous) q
  | Set.size q < 2 = mps
  | otherwise = step g mps' q' -- O(n)
  where u = minVertexByDist dists q -- O(n) 
        q'= delete u q -- O(logn)
        mps'=foldr (updateDist u) mps (g!u) -- O(n)

minVertexByDist ::  Vertices -> Set Vertex -> Vertex
minVertexByDist dist q = fold min' h  q   
  where h=head.Set.toList $ q 
        min' n p=if (dist!p)<(dist!n) then p else n

updateDist :: Vertex -> (Vertex,Int) -> 
              MinPaths -> MinPaths 
updateDist u (v,d) ds@(dists,prevs)
  | dists!v > newDist = (dists//[(v,newDist)],prevs//[(v,u)]) 
  | otherwise = ds
  where newDist=(dists!u) + d

gwd  :: GraphWithDists
gwd=listArray (1,7) 
    [[(2,10),(5,6)],[(3,5),(4,2)],[(4,4)],
     [(1,5)],[(6,9),(7,1)],[(4,3),(7,8)],[]]

minPath :: Vertices -> Vertex -> Vertex -> [Vertex] 
minPath vs v w | w == v = []
               | prev == v = [v]
               | otherwise = prev:minPath vs v prev  
  where prev=vs!w

-- With heap
type DistsHeap=MinPrioHeap Int Vertex
type MinPaths'=((DistsHeap,Vertices),Vertices)

solveWithHeap g v=(dists,prevs)
  where ((_,dists),prevs)=solve' g v

solve' :: GraphWithDists -> Vertex -> MinPaths'  
solve' g v= step' g ((hp,dists),prevs) q
  where q= Set.fromList $ vertices $ fmap (map fst) g 
        n= Set.size q 
        allButRoot=Set.toList $ delete v q
        hp= fromList $ (1,v):zip (repeat inf) allButRoot
        (dists,prevs)=initMinPaths n v

step' :: GraphWithDists -> MinPaths' -> Set Vertex -> MinPaths' 
step' g mps@((hp,dists),prevs) q
  | Set.size q < 2 = mps
  | otherwise = step' g mps' q' -- O(n)
  where Just((d,u),hp') = view hp -- O(1) 
        q'= delete u q -- O(logn)
        mps'=foldr (updateDist' u) ((hp',dists),prevs) (g!u) -- O(n)

updateDist' :: Vertex -> (Vertex,Int) -> 
              MinPaths' -> MinPaths' 
updateDist' u (v,d) ds@((hp,dists),prevs)
  | dists!v > newDist = ((hp',dists'),prevs//[(v,u)]) 
  | otherwise = ds
  where newDist=(dists!u) + d
        dists'=dists//[(v,newDist)]
        hp'=insert (newDist,v) hp 

-- Actions
readWords :: Read a => String -> [a]
readWords s= ws
 where ws=map read $ words s

readItem :: String -> (Vertex,(Vertex,Int))
readItem str=(i,(j,abs c))
 where [i,j,c]=readWords str

readData :: FilePath -> IO (Int,[(Vertex,(Vertex,Int))])
readData file= do
 (h:s) <-lines `fmap` readFile file
 let [n,_]=readWords h
     items=map readItem s
 return (n,items)

buildGraph :: Int -> [(Vertex,(Vertex,Int))] -> GraphWithDists
buildGraph n items=accumArray (flip (:)) [] (1,n) items 

sol :: FilePath -> Vertex -> Vertex -> [String] -> IO (Int,[Vertex])
sol fn v w opts= do
  (n,edges) <- readData fn
  let g=buildGraph n edges
      (dists,prevs)=if elem "-h" opts then solveWithHeap g v 
                    else solve g v 
  return $ (dists!w,minPath prevs v w)

main=do
  fn:v:w:heap <- getArgs
  t0 <- getClockTime 
  res <- sol fn (read v) (read w) heap 
  t1 <- getClockTime 
  putStrLn ("Time: " ++ show (diffClockTimes t1 t0)) 
  print res
