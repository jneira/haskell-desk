{-# LANGUAGE BangPatterns #-}
module Main where
import System.Environment
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector,(!),(//))
import qualified Data.Vector as Vec
import Data.Bits
import Data.Word

type Point=(Float,Float)
type S= Word32
type A= Map S (Vector Float)
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

gh :: Word32 -> Word32
gh set= (((r .^. set) >>> 2) ./. c) .|. r 
  where r=set + c 
        c=set .&. (-set)

test l s=do 
  print $ s 
  let s'=gh s
  if s'<l then test l s' else print "end"

subsets:: Int -> Int ->  [S]
subsets n m = f s
  where (s,l)=(set m,limit n)
        f s | s < l = s : f (gh s)
            | otherwise = []

inf=1/0

inits :: Int -> (M,A)
inits n = (Vec.fromList $ tail sss,a)
  where sss=[subsets n m|m<-[1..n]]
        a= Map.fromList [(s,dists s)|ss<-sss,s<-ss]
        dists 1 = Vec.fromList $ 0:(replicate (n-1) inf)
        dists _ = Vec.fromList $ replicate n inf
                
dist :: Point -> Point -> Float
dist (x,y) (z,w)=sqrt $ (x-z)^2+(y-w)^2

dists :: [Point] -> D
dists ps = Vec.fromList [Vec.fromList [d pi pj|pj<-ps]|pi<-ps]
   where d pi pj=if (pi==pj) then 0.0 else dist pi pj

prevSet :: S -> Int -> S
prevSet = clearBit 

members :: S -> [Int]
members s= [x|x<-[0..(bitSize s)-1],testBit s x] 

foldSet :: (a -> Int -> a) -> a -> S -> a 
foldSet f i set=foldl' f i (members set) 

solve :: D -> A -> M -> A
solve ds = Vec.foldl' (foldl' f)
   where f _A _S= foldSet  rec _A _S
           where rec _A 0 = _A
                 rec _A j = Map.adjust u _S _A
                   where u v=v // [(j,nxt)]
                         _S'=prevSet _S j
                         !prevS=_A Map.! _S'
                         nxt=foldSet rec' inf _S'
                         rec' d k=min d $ (prevS!k) + (ds!k)!j   


tsp n coords=Vec.ifoldl' f inf _D
    where (_M,_A)=inits n
          ds = dists coords
          _A'=solve ds _A _M
          _D =_A' Map.! head ( _M!(n-2))
          f d 0 _ = inf
          f d j v = min d $ v+((ds!0)!j)  
          
tspM n coords= do
 let ds=dists coords
     (_M,_A)=inits n        
     _A'=solve ds _A _M
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