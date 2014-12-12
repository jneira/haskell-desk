module Hashiwokakero where
import Data.Maybe (fromJust,isJust,isNothing)
import Data.Array (Array,Ix,(!),(//),listArray,bounds,elems,inRange,ixmap)
import Data.List (intersperse,sortBy)
import Data.Ord (comparing)
 
--Constants
_MAXBRIDGES= 8
_DIRECTIONS :: [Direction]
_DIRECTIONS = [(-1,0),(0,1),(1,0),(0,-1)]

type Puzzle =  [[Int]]
type Cell = (Int,Int)
type Direction= (Int,Int)
type Panel = Array Cell Int
type Bridge = (Direction,Int)
type BridgeExtend=[(Cell,Int)]
type IslandSet =  [(Cell,[[Bridge]])]

data PuzzleNode = PuzzleNode  {panel::Panel,islandsSet::IslandSet}

-- Show functions 
instance Show PuzzleNode
         where show a =unlines $ showFrame (x+3):
                               [showLine $ row i (panel a)| i<-[0..y]]
                               ++ [showFrame (x+3)]
                               where (y,x)=snd $ bounds $ panel a
 
showFrame n = "+" ++ (concat.(replicate n) $ "-") ++ "+"
showLine row = let elem idx | inRange (bounds row) idx = row!idx
			    | otherwise= 0 
		   iterate r=[(r!x,elem y,elem z)|
				x<-[fst $ bounds r..snd $ bounds r],
				y<-[x-1],z<-[x+1]]
	       in "| " ++ (concat $ map showCell $ iterate row) ++ " |"

showCell (x,y,z) | x==0 = " "  | x>0 = show x
	         | x<0 = showBridge x y z           

showBridge x y z
	   | hBridge = if x==(-1) then "-" else "=" 
	   | otherwise = if x==(-1) then "|" else "H" 
	   where hBridge = ((y<0) || (z<0)) || ((y>0) && (z>0))
 
-- backtracking functions

initPuzzle :: [[Int]]-> [[Int]]
initPuzzle p = let p'=map (intersperse 0) p 
	       in intersperse (replicate (length $ p'!!0)  0) p'
		
puzzleArray :: [[Int]]->Panel
puzzleArray a= let x=length a-1
                   y=length (head a)-1
               in listArray ((0,0),(x,y)) (concat a)
 
arrayPuzzle :: Panel -> [[Int]]
arrayPuzzle array= cellsList (bounds array) (elems array)

cellsList :: ((Int,Int),(Int,Int)) -> [Int] -> [[Int]]
cellsList bnds [] = []
cellsList ((x,y),(x',y')) elems =
                   take (y'+1) elems : cellsList ((x,y),(x',y'))
                                                 (drop (y'+1) elems)

islands :: Panel -> [Cell]
islands puzzle= [(y,x) | y<- [0..(fst.snd $ bounds puzzle)],
                                x<- [0..(snd.snd $ bounds puzzle)],
                                puzzle!(y,x)>0]

initRootNode ::  Int -> [[Int]] -> PuzzleNode 
initRootNode k puzzle =
              let panel=puzzleArray $ initPuzzle puzzle
                  aBridges is=maybeBridges (panel!is) [2] bridges [limitNumBridges]
                  wout0 is=map (filter ((/=0).snd)) $ aBridges is
		  pBridges is=filter (validBridges panel is) $ wout0 is 
                  iset=sortBy (comparing $ length.snd) $ 
                              map (\a->(a,pBridges a)) $ islands panel
              in PuzzleNode panel iset
 
validBridges :: Panel -> Cell -> [Bridge] -> Bool
validBridges p is brs= let p'=putBridges p is brs
		           fitBrd p=(remainBridges p is) == 0
                       in (not.isNothing $ p') &&  
		       		(fitBrd.fromJust $ p')
			
row :: (Ix t1, Ix t) => t1 -> Array (t1, t) e -> Array t e
row i x = ixmap (l',u') (\j->(i,j)) x where ((_,l'),(_,u')) = bounds x
 
backtracking :: (t -> Bool) -> (t -> [t]) -> t -> [t]
backtracking isSolution explore node
             | isSolution node = [node]
             | otherwise = concatMap
                         (backtracking isSolution explore)
                         (explore node)
 
apply :: [(a->Bool)] -> a -> Bool
apply preds x = and $ map ($ x) preds
 
bridges :: [Int] -> [Bridge]
bridges vals =let coords=_DIRECTIONS
              in [(coords!!x,-(vals!!x))
                   |x<-[0..(length coords)-1]]
                  
limitNumBridges :: (Int,[Int])->Bool
limitNumBridges (k,xs)= sum xs == k
 
maybeBridges :: Int -> [Int] -> ([Int] -> a) -> [(Int,[Int])->Bool] -> [a]
maybeBridges x [k] build preds = maybeBridges x [k,k,k,k] build preds
maybeBridges x [n',e',s',w'] build preds
               | x < 1 || x>_MAXBRIDGES = []
               | otherwise = [ build [n,e,s,w]
                              | n<-[0..n'],e<-[0..e'],
                                s<-[0..s'],w<-[0..w'],
                                apply preds (x,[n,e,s,w])]

explore :: PuzzleNode -> [PuzzleNode]
explore node | invalidNode node =[]
	     | otherwise = 
	  	let puzzle=panel node
                    filteredISet = map (\(x,y)->(x, filter (validBridges puzzle x) $ y)) 
                    		    $ islandsSet node
                    orderedISet= sortBy (comparing $ length.snd) filteredISet
                    (island,aBridges)=head orderedISet
                    panels=map fromJust $ filter isJust
                             $ map (putBridges puzzle island) aBridges
                    nodeBuilder=flip PuzzleNode $ tail orderedISet 
                    acc pans | null pans = [nodeBuilder puzzle]
                    	     | otherwise = foldl (flip $ (:) . nodeBuilder) [] pans 
		    in  acc panels
                              
invalidNode n = let is= islandsSet n
                    p= panel n
		in (null is) || (any (null.snd) is) 
                   -- || (not.and $ map (\a-> or $ map (validBridges p (fst a)) (snd a)) is)    			
              
putBridges :: Panel -> Cell -> [Bridge] -> Maybe Panel
putBridges puzzle island []=Just puzzle
putBridges puzzle island bridges
              | any isNothing exts = Nothing
              | otherwise = Just (puzzle//(concatMap fromJust exts))
              where exts= map fst $ bridgeExtends puzzle island bridges
 
bridgeExtends :: Panel -> Cell -> [Bridge] -> [(Maybe BridgeExtend,BridgeExtendResult)]
bridgeExtends puzzle island bridges = map (bridgeExtend puzzle island) bridges
 
data BridgeExtendResult = NoExtend | OutOfRange | CrossOtherBridge| 
                          DestIslandFull | PrevBridgeMatch | 
                          PrevBridgeMismatch |ExtendOk 
                          deriving (Show,Enum, Eq, Ord)
 
bridgeExtend :: Panel -> Cell -> Bridge -> (Maybe BridgeExtend,BridgeExtendResult)
bridgeExtend puzzle (y,x)((0,0),_)= (Just [],NoExtend)
bridgeExtend puzzle (y,x) ((_,_),0)= (Just [],NoExtend)
bridgeExtend puzzle (y,x) ((dy,dx),b)= 
             let reduce acc (y',x')
                        | not $ inRange (bounds puzzle) nextCell= (Nothing,OutOfRange)
                        | nextVal < 0 = if (y,x)/=(y',x') 
                                        then (Nothing,CrossOtherBridge)
                                        else if nextVal==b 
                                             then (Just [],PrevBridgeMatch)
                                             else (Nothing,PrevBridgeMismatch) 
                        | nextVal > 0 = if (fitBridge puzzle nextCell b)
                                        then (Just acc,ExtendOk)
                                        else (Nothing,DestIslandFull)
                        | nextVal == 0 = reduce ((nextCell,b):acc) nextCell
                          where nextCell =(y'+dy,x'+dx)
                                nextVal = puzzle!nextCell
             in reduce [] (y,x)

fitBridge :: Panel -> Cell -> Int -> Bool
fitBridge p c k= (remainBridges p c) - (abs k) >= 0
remainBridges :: Panel -> Cell -> Int
remainBridges p (y,x) =p!(y,x) + (numBridges p (y,x))

numBridges :: Panel -> Cell -> Int
numBridges p (y,x)= foldl1 (+) $ map ((!) p) $
                     cellsInRange p [(y-1,x),(y,x+1),(y+1,x),(y,x-1)]

cellsInRange :: Panel -> [Cell] -> [Cell]
cellsInRange puzzle cells = filter (inRange (bounds puzzle)) cells
 
isSolution :: PuzzleNode -> Bool
isSolution node =  islandsSet node == [] 
			&&  (allIslandsBridged $ panel node)

allIslandsBridged :: Panel -> Bool
allIslandsBridged panel=
		let rBridges=sum $ map (remainBridges panel) $  islands panel
		in rBridges==0
 
resuelveHashiwokakero puzzle=putStrLn $ showSolution puzzle
		
showSolution puzzle = let root=initRootNode 2 puzzle
			  sol=backtracking isSolution explore $ root
		      in unlines [show root,(showSolMessage $ length sol)++"\n",
				    unlines $ map show sol]

showSolMessage 0 = "Puzzle sin solucion"
showSolMessage 1 = "Puzzle con una unica solucion"
showSolMessage n = "Puzzle con " ++ show n ++ " soluciones"

exploreN :: Int -> [PuzzleNode] -> [PuzzleNode]
exploreN 0 nodes = nodes
exploreN k nodes = concatMap (exploreN (k-1)) $ map explore nodes
