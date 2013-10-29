{-# LANGUAGE BangPatterns #-}
module Main where
import System.Environment
import Data.Vector (Vector,(!))
import qualified Data.Vector as V
import Data.Vector.Unboxed.Mutable (STVector,new)
import qualified Data.Vector.Unboxed.Mutable as M
import Data.IntMap.Strict (IntMap,insert,insertWith,
                          fromDistinctAscList)
import qualified Data.IntMap.Strict as Map
import Data.Bits
import Data.Word
import Data.STRef
import Control.Monad
import Control.Monad.ST

type Point = (Float,Float)
type S = Word32 
type D = Vector (Vector Float)
type V s= M.STVector s Float
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

dist :: Point -> Point -> Float
dist (x,y) (z,w)=sqrt $ (x-z)^2+(y-w)^2

dists :: [Point] -> D
dists ps = V.fromList [V.fromList [d pi pj|pj<-ps]|pi<-ps]
   where d pi pj=if (pi==pj) then 0.0 else dist pi pj

prevSet :: S -> Int -> S
prevSet = clearBit 

members :: Int -> S -> [Int]
members n s= [x|x<-[0..n-1],testBit s x] 

initV n = do
  v <- new n
  M.set v inf
  return v

initA n = do
  a <- newSTRef Map.empty
  forM_ (subsets n 1) $ \ s -> do
    v <- new n
    M.unsafeWrite v 0 (if (s==1) then 0.0 else inf)
    forM_ [1..n-1] $ \ j -> M.unsafeWrite v j inf
    a' <- readSTRef a
    modifySTRef' a $ insert (fi s) v
  return a
tsp' n coords= do 
  let ds=dists coords
  a <- initA n
  forM_ [2..n] $ \ m -> do
    a' <- newSTRef Map.empty
    forM_ (subsets n m) $ \ s -> do
      iv <- initV n
      va <- readSTRef a
      va' <- readSTRef a'
      let ms = members n s
          v=Map.findWithDefault iv (fi s) va'
      forM_ ms $ \ j -> do
        let preS=prevSet s j
            preV=va Map.! (fi preS)
        when (j /= 0) $ do
          min <- rec ds preV ms j
          M.unsafeWrite v j min
        modifySTRef' a' $ insert (fi s) v
    va' <- readSTRef a'
    writeSTRef a va'
  finalA <- readSTRef a
  let (_,finalV):_ = Map.toList finalA   
  rec ds finalV [1..n-1] 
rec :: D -> V s -> [Int] -> Int -> ST s Float 
rec ds preV  ms j  = do
  d <- newSTRef inf
  forM_ ms $ \ k -> do
    when (k /= j) $ do
      kv <-  M.unsafeRead preV k
      modifySTRef' d $ min (kv + (ds!k)!j)
  readSTRef d     

tsp :: Int -> [Point] -> Float
tsp n cs=runST $ tsp' n cs

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
  --(stToIO $ tsp' n coords) >>= print
  print $ tsp n coords
