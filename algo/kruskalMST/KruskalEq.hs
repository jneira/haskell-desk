module Main where
import KruskalUtil
import Data.Maybe(fromJust,listToMaybe)
import Data.Equivalence.Persistent
import Data.List (find,delete)
import System.Environment

type Vertex=Int
type Edge=(Vertex,Vertex)
type Components=(Equivalence Vertex,[Vertex])
              
initComponents :: (Int,Int) -> Components
initComponents bs@(x,y)=(emptyEquivalence bs,[x..y])

solve :: Components -> [Edge] ->  [Edge]
solve _ []=[]
solve comps ((x,y):es) 
  | hasCycle comps (x,y) = nxt 
  | otherwise = (x,y):nxt 
  where nxt= solve comps' es
        comps'=update comps (x,y)

hasCycle :: Components -> Edge -> Bool
hasCycle comps (x,y)=
  case (leader comps x,leader comps y) of
    (Just lx,Just ly) -> lx==ly
    _ -> False

update :: Components -> Edge -> Components
update comps@(eqs,leaders) (x,y) 
  | lx == ly = (eqs',leaders)
  | otherwise = (eqs',delete ly leaders)
  where eqs'=equate x y eqs   
        (lx,ly) = (getLeader x,getLeader y)
        getLeader x=fromJust (leader comps x) 
       
leader :: Components -> Vertex -> Maybe Vertex
leader (eqs,leaders) x=find (equiv eqs x) leaders

main= do
  [file] <- getArgs
  (bnds,sorted)<-getKruskalInput file
  let comps=initComponents (1,bnds)
  print $ length $ solve comps sorted 
  
