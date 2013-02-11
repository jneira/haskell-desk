module Main where
import Data.Array.Diff
import Data.Map (Map,findWithDefault,fromList)
import Data.List (foldl',minimumBy)
import Data.Ord (comparing)
import System.Environment

type Edge = (Int,Int)
type EdgeCost = (Edge,Int)
type Path=(Edge,Int)

type Acc=DiffUArray (Int,Int) Int
type Index=(Int,Int,Int)
type Bounds=(Index,Index)

solve :: Int -> [EdgeCost] -> Maybe Path
solve n ecs | hasNegativeCycle s = Nothing
            | otherwise = Just $ shortestPath s
  where s=fw n ecs

fw :: Int -> [EdgeCost] -> Acc
fw n ec= fw' (init' n ec) ((1,1,1),(n,n,n)) 

init' :: Int -> [EdgeCost] -> Acc
init' n ec=empty//[((i,j),f i j)|i<-[1..n],j<-[1..n]]
  where ec'=fromList ec
        empty=listArray ((1,1),(n,n)) $ repeat 0
        f i j | i==j = 0
              | otherwise=findWithDefault maxBound (i,j) ec'

fw' :: Acc -> Bounds -> Acc
fw' acc= (foldl' step' acc).range 
   where step' a (k,i,j) = step a (i,j,k)

step a (i,j,k)=a//[((i,j),min triv calc)]
  where triv=a!(i,j)
        (t1,t2)=(a!(i,k),a!(k,j))
        calc | t1 == maxBound || t2==maxBound = maxBound
             | otherwise=t1+t2

shortestPath :: Acc -> Path
shortestPath=(minimumBy (comparing snd)).assocs

hasNegativeCycle :: Acc -> Bool
hasNegativeCycle a= any (<0) $ elems diags
  where (_,(n,_))=bounds a
        diags=ixmap (1,n) (\i->(i,i)) a

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
  return $ solve n items

main=do
  file:_ <- getArgs 
  solution file >>= print