module KruskalUtil where
import Data.Graph
import Data.List (sortBy)
import Data.Ord (comparing)

-- actions

sortEdges:: [(Edge,Int)] -> [Edge]
sortEdges xs=map fst $ sortBy (comparing snd) xs 

readWords :: Read a => String -> [a]
readWords s= ws 
  where ws=map read $ words s

readEdge:: String -> (Edge,Int)
readEdge s=((u,v),c)    
  where [u,v,c]=map read $ words s  

getKruskalInput:: FilePath -> IO (Int,[Edge])
getKruskalInput edgesFile= do
  (h:s) <-lines `fmap` readFile edgesFile
  let edgesWithCost=map readEdge s
      sorted=sortEdges edgesWithCost
      (bnds:_)=readWords h 
  print $ "Lines:"++(show $ length sorted)
  return (bnds,sorted)

-- examples

g=buildG (1,4) [(1,2),(2,1),(1,3),(3,1),
                (2,4),(4,2),(3,4),(4,3),(1,4),(4,1)]

ec:: [(Edge,Int)]
ec=[((1,2),1),((1,3),2),((1,4),1),    
    ((2,4),3),((3,4),4)]

sorted:: [Edge]
sorted=[(1,2),(1,4),(1,3),(2,4),(3,4)]

sorted':: [Edge]
sorted'=[(1,2),(3,4),(4,5),(3,5),(2,3),(1,5)]
