module GraphWalk  where
import Data.Graph
import Data.Array
import Data.List
{-
fun RecProfundidadRecursivo(v: nodo, visitado: Vector)
  var
    w: nodo
  fvar
  visitado[v] <- cierto
  para cada w adyacente a v hacer
    si no visitado[ w] entonces
      RecProfundidadRecursivo( w, visitado)
    fsi
  fpara
ffun
-}

localDeepWalk :: Graph -> Vertex -> [Vertex] -> [Vertex]
localDeepWalk graph v vs=
  foldl (flip $ localDeepWalk graph) vs' ads
  where vs'=vs++[v] 
        ads=[x|x<-graph!v,not $ elem x vs']

{-
tipo Vector = matriz[O .. n] de booleano
fun RecorridoProfundidad(G = (N,A): grafo)
  var
   visitado: Vector
   v: nodo
  fvar
  para cada v de N hacer
    visitado[v] <- falso
  fpara
  para cada v de N hacer
    si no visitado[v] entonces
      RecProfundidadRecursivo(v, visitado)
    fsi
  fpara
ffun
-}

deepWalk :: Graph -> [Vertex]
deepWalk graph=foldl f [] $ vertices graph
  where f acc nxt=if (elem nxt acc) then acc
                  else localDeepWalk graph nxt acc

{--
fun RecProfundidadRecursivoNum(v: nodo, num: natural, visitado: Vector, numOrden:
VectorNat)
  var
    w: nodo
  fvar
  visitador[v] <- cierto
  num f <- num + 1
  numOrden[v] <- num
  para cada w adyacente a v hacer
    si no visitador[w] entonces
      RecProfundidadRecursivoNum(w, num, visitado, numOrden)
    fsi
  fpara
ffun

tipo Vector = matriz[O .. n] de booleano
tipo VectorNat = matriz[O .. n] de natural
fun RecorridoProfundidadNum(G = (N,A): grafo)
  var
    visitado: Vector
    numOrden: VectorNat
  fvar
  para cada v E N hacer
    visitado[v] <- falso
  fpara
  num <- O
  para cada v en N hacer
    si no visitador v] entonces
      RecProfundidadRecursivoNum(v, num, visitado, numOrden)
    fsi
  fpara
ffun
--}
                    
deepWalkNum :: Graph -> Table Int
deepWalkNum graph = array (bounds graph) $ zip inOrder [1..]
  where inOrder=deepWalk graph 

{--
fun RecAnchura(v: nodo, visitado: Vector)
  var
    u,w: nodo
    Q: TCola
  fvar
  Q <- Cola Vacía
  visitado[v] <- cierto
  Encolar(v,Q)
  mientras no vacia(Q) hacer
    u <- Primero(Q)
    Desencolar(u,Q)
    para cada w adyacente a u hacer
      si no visitado[w] entonces
        visitado[w] <- cierto
        Encolar(w,Q)
      fsi
    fpara
  mientras
ffun
--}
-- @__josejuan__ version 
localBreadthWalk :: Graph -> Vertex -> [Vertex] -> [Vertex]
localBreadthWalk graph v prev= walk (prev++[v]) [v] 
  where walk ws ps = if null a'  then ws 
                     else walk (ws ++ a') a' 
          where a' = [x|p<-ps,x<-graph!p,not $ elem x ws]
        
breadthWalk :: Graph -> [Vertex]
breadthWalk graph = foldl f [] $ vertices graph
  where f vs nxt=if (elem nxt vs) then vs
                 else localBreadthWalk graph nxt vs

{-
fun RecProfundidadRecursivoOrdenTopologico(v: nodo, visitado: Vector)
  var
    w: nodo
  fvar
  visitado[ v] <- cierto
  para cada w adyacente a v hacer
    si no visitado[w] entonces
      RecProfundidadRecursivoOrdenTopologico( w, visitado)
    fsi
  fpara
  escribir(v)
ffun
-}
localTopologicalSort:: Graph -> Vertex -> 
                       ([Vertex],[Vertex]) -> ([Vertex],[Vertex])
localTopologicalSort graph v (prev,vs)=
  foldl (flip $ localTopologicalSort graph) (prev,vs') ads
  where vs'=vs++[v] 
        ads=[x|x<-graph!v,not $ elem x (prev++vs')]

-- suponiendo que el primero de los vertices no tiene predecesor
topologicalSort :: Graph -> [Vertex]
topologicalSort graph= foldl f [] $ vertices graph
  where f acc nxt=if (elem nxt acc) then acc
                  else (snd lts)++acc  
          where lts=localTopologicalSort graph nxt (acc,[])

btree::Graph
btree=buildG (0,7) [(0,1),(0,2),(1,3),(1,4),(2,5),(2,6)]
cyclic::Graph
cyclic=buildG (0,5) [(0,1),(1,2),(1,5),(2,3),(3,4),(4,2),(2,5),(5,1)]

graph_2_14=buildG (1,6) [(1,3),(2,3),(2,4),(3,5),(4,5),(5,6)]
