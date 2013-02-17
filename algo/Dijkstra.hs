module Main where
import Data.Array.Diff
import Data.Heap 
import Data.Set hiding (map,toList,fromList,fromAscList,
                        singleton,size,insert)
import qualified Data.Set as Set (fromList,toList,size)
import Data.List (minimumBy)
import Data.Ord (comparing)
import System.Environment
import System.Time (getClockTime, diffClockTimes) 

type Vertex=Int
type Distance=Int
type Path=[Vertex]
type PrevsVertices=DiffUArray Vertex Vertex
type Distances=DiffUArray Vertex Distance
type MinPaths=(Distances,PrevsVertices)
type WeightedGraphAL=Array Vertex [(Vertex,Int)] 
type WeightedGraphM=Array (Vertex,Vertex) Distance

inf :: Int
inf= maxBound `div` 2

solve :: Int -> [((Vertex,Vertex),Distance)] -> Vertex -> MinPaths  
solve n g v=step g' (initMinPaths n v) q
  where g'=buildGraphM n g  
        q= Set.fromList $ [1..n]

buildGraphM :: Int -> [((Vertex,Vertex),Distance)] -> WeightedGraphM
buildGraphM n items=accum (const id) init items 
  where init=listArray ((1,1),(n,n)) $ repeat inf

initMinPaths :: Int -> Vertex -> MinPaths
initMinPaths n v= (dist//[(v,0)],previous)
  where dist=listArray (1,n) $ repeat inf   
        previous=listArray (1,n) $ repeat v

step :: WeightedGraphM -> MinPaths -> Set Vertex -> MinPaths 
step g mps@(dists,previous) q -- O(n*(2n+logn)) ~> O(n^2)
  | Set.size q < 2 = mps
  | otherwise = step g mps' q' 
  where u = minVertexByDist dists q -- O(n) 
        q'= delete u q -- O(logn)
        mps'=foldr (updateDist g u) mps [1..snd.snd $ bounds g] -- O(n)

minVertexByDist ::  Distances -> Set Vertex -> Vertex
minVertexByDist dist q = fold min' h  q   
  where h=head.Set.toList $ q 
        min' n p=if (dist!p)<(dist!n) then p else n

updateDist :: WeightedGraphM -> Vertex -> Vertex -> 
              MinPaths -> MinPaths 
updateDist g u v ds@(dists,prevs)
  | dists!v > newDist = (dists//[(v,newDist)],prevs//[(v,u)]) 
  | otherwise = ds
  where d=g!(u,v) 
        newDist=(dists!u) + d

minPath :: PrevsVertices -> Vertex -> Vertex -> Path 
minPath vs v w | w == v = []
               | prev == v = [v]
               | otherwise = prev:minPath vs v prev  
  where prev=vs!w

type DistsHeap=MinPrioHeap Distance Vertex

solveWithHeap :: Int -> [((Vertex,Vertex),Distance)] -> Vertex -> MinPaths
solveWithHeap n g v=solve' g' v
  where g'=buildGraphAL n g 

buildGraphAL :: Int -> [((Vertex,Vertex),Distance)] -> WeightedGraphAL
buildGraphAL n items=accumArray (flip (:)) [] (1,n) items' 
  where items'=map (\((x,y),z)->(x,(y,z))) items
        
solve' :: WeightedGraphAL -> Vertex -> MinPaths  
solve' g v= step' g (dists,prevs) hp -- O(n+aloga)??
  where n= rangeSize $ bounds g 
        hp= singleton (1,v) 
        (dists,prevs)=initMinPaths n v -- O(n)

step' :: WeightedGraphAL -> MinPaths -> DistsHeap -> MinPaths 
step' g mps@(dists,prevs) hp -- O(a*log a)??
  | size hp == 0 = mps
  | otherwise = step' g mps' hp'' --O(a)
  where Just((d,u),hp') = view hp  -- O(loga)
        (hp'',mps')=foldr (updateDist' u) --O(loga) 
                    (hp',(dists,prevs)) (g!u)

updateDist' :: Vertex -> (Vertex,Distance) -> 
              (DistsHeap,MinPaths) -> (DistsHeap,MinPaths) 
updateDist' u (v,d) ds@(hp,(dists,prevs))
  | dists!v > newDist = (hp',(dists',prevs//[(v,u)])) 
  | otherwise = ds
  where newDist=(dists!u) + d
        dists'=dists//[(v,newDist)]
        hp'=insert (newDist,v) hp 

gwd  :: [((Vertex,Vertex),Distance)]
gwd=[((1,2),10),((1,5),6),((2,3),5),((2,4),2),((3,4),4),((3,1),2),
     ((4,1),5),((5,6),9),((5,7),1),((6,4),3),((6,7),8)]

-- Actions
readWords :: Read a => String -> [a]
readWords s= ws
 where ws=map read $ words s

readItem :: String -> ((Vertex,Vertex),Distance)
readItem str=((i,j),abs c) -- No negative weights allowed
 where [i,j,c]=readWords str

readData :: FilePath -> IO (Int,[((Vertex,Vertex),Distance)])
readData file= do
 (h:s) <-lines `fmap` readFile file
 let [n,_]=readWords h
     items=map readItem s
 return (n,items)


sol :: FilePath -> Vertex -> Vertex -> [String] -> IO (Distance,[Vertex])
sol fn v w opts= do
  (n,edges) <- readData fn
  let (dists,prevs)=if elem "-h" opts then solveWithHeap n edges v 
                    else solve n edges v 
  return $ (dists!w,minPath prevs v w)

main=do
  fn:v:w:heap <- getArgs
  t0 <- getClockTime 
  res <- sol fn (read v) (read w) heap 
  print res
  t1 <- getClockTime 
  putStrLn ("Time: " ++ show (diffClockTimes t1 t0)) 
