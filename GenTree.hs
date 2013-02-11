module RandTree where 
import System.Random 

type Seed=(Int,Double,StdGen) 
type WalkTree a=([a],[a],[a]) 

randTree maxLevel prob= 
    do rndGen<-newStdGen 
       return $ gen (maxLevel,prob,rndGen) ([0],[],[])  

gen :: (Ord a,Enum a)=>Seed -> WalkTree a -> WalkTree a 
gen (level,prob,rndGen) acc@(pre,inn,post) 
    | level==0 || noBranch = acc 
    | otherwise = (pre'',inn'',next $ post++post'') 
    where noBranch=(fst $ random rndGen) > prob 
          nextSeed=(,,) (level-1) prob 
          next=(++[(succ.maximum) pre]) 
          (rndGen',rndGen'')=split rndGen 
          (pre',inn',post')=gen (nextSeed rndGen') (next pre,[],[]) 
          (pre'',inn'',post'')=gen (nextSeed rndGen'')  
                               (pre',next $ inn++inn',post')
