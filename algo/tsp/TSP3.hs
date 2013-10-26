{-# LANGUAGE BangPatterns #-}
module Main where
import System.Environment
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntMap.Strict (IntMap,insert,insertWith,
                          fromDistinctAscList)
import qualified Data.IntMap.Strict as Map
import Data.Vector (Vector,(!))
import qualified Data.Vector as Vec
import Data.Bits
import Data.Word

type Point=(Float,Float)
type S= Word32
type A= IntMap  (IntMap Float)
type M= Vector [S]
type D = Vector (Vector Float)

{-
int set = (1 << k) - 1;
int limit = (1 << n);
// Gosper's hack:
int c = set & -set;
int r = set + c;
set = (((r^set) >>> 2) / c) | r; -}

(.^.)=xor
(>>>)=shiftR
(<<<)=shiftL
(./.)=div

set :: Int -> Word32
set k=(1 <<< k)-1
limit :: Int -> Word32
limit n=1 <<< n

fi = fromIntegral

gh :: Word32 -> Word32
gh set= (((r .^. set) >>> 2) ./. c) .|. r 
  where r=set + c 
        c=set .&. (-set)

subsets:: Int -> Int ->  [S]
subsets n m = f s
  where (s,l)=(set m,limit n)
        f s | s < l = s : f (gh s)
            | otherwise = []

inf=1/0

fromList n xs=fromDistinctAscList $ zip [0..n-1] xs 

initV n = fromList n $ replicate n inf

initA n = Map.fromList [(fi s,dists s)|s<-subsets n 1]
  where dists 1 = fromList n $ 0:(replicate (n-1) inf)
        dists _ = initV n

dist :: Point -> Point -> Float
dist (x,y) (z,w)=sqrt $ (x-z)^2+(y-w)^2

dists :: [Point] -> D
dists ps = Vec.fromList [Vec.fromList [d pi pj|pj<-ps]|pi<-ps]
   where d pi pj=if (pi==pj) then 0.0 else dist pi pj

prevSet :: S -> Int -> S
prevSet = clearBit 

members :: Int -> S -> [Int]
members n s= [x|x<-[0..n-1],testBit s x] 

foldSet :: Int -> (a -> Int -> a) -> a -> S -> a 
foldSet n f i set=foldl f i ms
  where ms=members n set

solve :: Int -> D -> A
solve n ds  = foldl f (initA n) [2..n]
  where iv = initV n
        f prevA m = foldl f Map.empty sets
          where sets = subsets n m 
                f _A _S= (foldSet n) rec _A _S
                  where rec !_A 0 = _A
                        rec !_A j = insertWith u (fi _S) iv'  _A
                          where iv' = insert j nxt iv
                                u _ pv = insert j nxt pv
                                _S'=prevSet _S j
                                preV=prevA Map.! (fi _S')
                                !nxt=(foldSet n) rec' inf _S'
                                rec' d k=min d $ (preV Map.! k) + (ds!k)!j   

tsp n coords=Map.foldlWithKey f inf _D
    where ds = dists coords
          _A'=solve n ds
          _D =_A' Map.! fi ((limit n)-1) 
          f d 0 _ = inf
          f d j v = min d $ v+((ds!0)!j)  
          
tspM n coords= do
 let ds=dists coords        
     _A'=solve n ds
 return $ tsp n coords

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
  tspM n coords >>= print 