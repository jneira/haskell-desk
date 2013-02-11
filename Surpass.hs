module Surpassing where
import qualified Data.Map as Map
import Data.List
        
count w=snd $ mapAccumL count' Map.empty $ reverse w
  where count' acc x=(update acc,calc acc x)
          where update=Map.insertWith (+) x 1

calc tokens x=Map.foldrWithKey f 0 tokens
  where f key val count | x<key=count+val
                        | otherwise=count            

max':: Ord t => [t]->Integer
max'=maximum.count

{-
Observaciones:
* El conjunto c es implicito y esta dado por los elementos unicos de s
* Por ejemplo para s="generating" c="aeginrt" o para s=[0,1,1,0,1] c=[0,1]
* Siendo n=length s, m=length c, el coste es O(g(m)*n) , para valores pequeños de m g(m)<logn
-} 
