module KruskalEq2 where
import KruskalUtil
import Data.Maybe(fromJust,listToMaybe)
import Data.Equivalence.Persistent
import Data.Set (Set,(\\))
import qualified Data.Set as Set
import System.Environment

type Leaders=Set Vertex
type AllEdges=Set Vertex
type Components=((Equivalence Vertex,Leaders),AllEdges)

type Vertex=Int
type Edge=(Vertex,Vertex)

initComponents :: (Int,Int) -> Components
initComponents bs@(x,y)=
  ((emptyEquivalence bs,Set.empty),
   Set.fromAscList [x..y])

solve:: Components -> [Edge] -> [Edge]
solve cs es=head [out|(cs,(in',out))<-all,null $ in']
  where all=iterate step (cs,(es,[]))

step :: (Components,([Edge],[Edge])) -> 
        (Components,([Edge],[Edge]))
step i@(_,([],t))=i
step (comps,(((x,y):es),t)) 
  | hasCycle comps (x,y) = (comps,(es,t))
  | otherwise = (comps',(es,(x,y):t))
  where comps'=update comps (x,y)

hasCycle :: Components -> Edge -> Bool
hasCycle comps (x,y)=
  case (leader comps x,leader comps y) of
    (Just lx,Just ly) -> lx==ly
    _ -> False

update :: Components -> Edge -> Components
update comps@((eqs,leaders),all) (x,y) = case ls of
  (Nothing,Nothing) -> ((eqs',Set.insert x leaders),all')
  (Just lx,Just ly) -> if (lx==ly) then ((eqs,leaders),all')  
                       else ((eqs',Set.delete ly leaders),all')
  (_,_)             -> ((eqs',leaders),all')
  where eqs'=equate x y eqs 
        all'=all \\ Set.fromList [x,y] 
        ls = (leader comps x,leader comps y)

leader :: Components -> Vertex -> Maybe Vertex
leader ((eqs,leaders),_) x=
  listToMaybe [l|l<-Set.toList leaders,equiv eqs x l]

solution file= do
  (bnds,sorted)<-getKruskalInput file
  let comps=initComponents (1,bnds)
      mst=solve comps sorted 
  print $ length mst

