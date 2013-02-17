module Main where
import Control.Applicative
import Data.List
import Data.List.Split
import Data.Map hiding (map,(!))
import qualified Data.Map as Map (map,(!))
import Data.Array
import Data.Ord
import Data.Graph
import System.Environment

type EdgesCost = Map Edge Int

primMST :: (Graph,EdgesCost) -> [Vertex] -> [Edge] 
primMST (g,ec) x
  | all (flip elem $ x) $ vertices g =[]
  | otherwise = cheapestEdge:(primMST (g,ec) (v:x))
  where edges'=[(u,v)|u<-x,v<-g!u,not$elem v x] 
        cheapestEdge@(u,v)=head $ sortBy cost edges'
        cost=comparing.(Map.!) $ ec

cost :: EdgesCost -> [Edge] -> Integer
cost costs edges=sum [toInteger $ costs Map.! edge|edge<-edges]

readEdge:: String -> (Edge,Int)
readEdge s=((u,v),c)    
  where [u,v,c]=map read $ splitOn " " s

buildGraphWithCost:: Bounds -> [(Edge,Int)] -> (Graph,EdgesCost)
buildGraphWithCost bounds edgesWithCost=
  (buildG bounds $ map fst withRevs,fromList withRevs) 
  where withRevs=concat [[((u,v),c),((v,u),c)]|
                         ((u,v),c)<-edgesWithCost]

readPair :: Read a => String -> (a,a)
readPair s= (w,l) 
  where [w,l]=map read $ splitOn " " s
        
primMSTSolution edgesFile= do
  s <-lines <$> readFile edgesFile
  let edgesWithCost=map readEdge $ tail s
      (nv,ne)=readPair $ head s
      (g,ec)=buildGraphWithCost (1,nv) edgesWithCost
      mst=primMST (g,ec) [1]
  print $ cost ec mst -- -3612829

main = do [edgesFile] <- getArgs
          primMSTSolution edgesFile

g=buildG (1,4) [(1,2),(2,1),(1,3),(3,1),
                (2,4),(4,2),(3,4),(4,3),(1,4),(4,1)]
ec:: Map Edge Int
ec=fromList [((1,2),1),((2,1),1),
             ((1,3),2),((3,1),2),
             ((1,4),1),((4,1),1),
             ((2,4),3),((4,2),3),
             ((3,4),4),((4,3),4)]
  
