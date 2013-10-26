module Main where
import System.Environment
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector,(!),(//))
import qualified Data.Vector as Vec

type Point=(Float,Float)
type S= Set Int
type A= Map S (Vector Float)
type M= Vector [S]
type D = Vector (Vector Float)

-- from http://stackoverflow.com/a/14286085/49554
choose :: [b] -> Int -> [[b]]
_      `choose` 0       = [[]]
[]     `choose` _       =  []
(x:xs) `choose` k       =  (x:) `fmap` (xs `choose` (k-1)) ++ xs `choose` k

subsets:: Int -> Int ->  [Set Int]
subsets n m = map (Set.fromDistinctAscList.(0:)) $ 
              choose [1..(n-1)] (m-1)  

inf=1/0

inits :: Int -> (M,A)
inits n = (Vec.fromList $ tail sss,a)
  where sss=[subsets n m|m<-[1..n]]
        a= Map.fromList [(s,dists $ Set.size s)|ss<-sss,s<-ss]
        dists 1 = Vec.fromList $ 0:(replicate (n-1) inf)
        dists _ = Vec.fromList $ replicate n inf
                
dist :: Point -> Point -> Float
dist (x,y) (z,w)=sqrt $ (x-z)^2+(y-w)^2

dists :: [Point] -> D
dists ps = Vec.fromList [Vec.fromList [d pi pj|pj<-ps]|pi<-ps]
   where d pi pj=if (pi==pj) then 0.0 else dist pi pj

solve :: D -> A -> M -> A
solve ds = Vec.foldl' (foldl' f)
   where f _A _S= Set.foldl'  rec _A _S
           where rec _A 0 = _A
                 rec _A j = Map.adjust u _S _A
                   where u v=v // [(j,nxt)]
                         _S'=Set.delete j _S
                         prevS=_A Map.! _S'
                         nxt=Set.foldl' rec' inf _S'
                         rec' d k=min d $ (prevS!k) + (ds!k)!j   

tsp n coords=Vec.ifoldl' f inf _D
    where (_M,_A)=inits n
          ds = dists coords
          _A'=solve ds _A _M
          _D =_A' Map.! head ( _M!(n-2))
          f d 0 _ = inf
          f d j v = min d $ v+((ds!0)!j)  
          
        
readWords :: Read a => String -> [a]
readWords s= ws
 where ws=map read $ words s

readCoords :: FilePath -> IO (Int,[Point])
readCoords file=do
 (h:css) <-lines `fmap` readFile file
 let csr=[(read x,read y)|cs<-css,let x:y:_=words cs]
 return (read h,csr) -- $ map readWords xs

main = do
  (file:_) <- getArgs
  (n,coords)<-readCoords file
  print $ tsp n coords 