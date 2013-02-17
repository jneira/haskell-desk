module Main where
import KruskalUtil
import Data.Graph
import System.Environment

solve:: Bounds -> [Edge] -> [Edge]
solve bnds edges= rec (edges,[]) 
  where rec ([],mst)=mst
        rec ((x,y):es,t)=rec (es,t')
          where t' = if withCycle then t 
                     else (x,y):(y,x):t
                subgraph=buildG bnds t
                withCycle=path subgraph x y

main= do
  [path]<-getArgs
  (bnds,sorted)<-getKruskalInput path
  let mst=solve (1,bnds) sorted
  print $ length mst
