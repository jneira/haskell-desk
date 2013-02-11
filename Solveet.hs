module Solveet where
import Data.Numbers.Primes
import Data.List
import Data.Maybe
import Data.String.Utils
import qualified Data.Map as Map
import Data.String.Utils 

-- http://www.solveet.com/exercises/Numero-perfecto/76/solution-473

perfectNum n = head [x | x <- [n,n-1..], esPerfecto x] 
  where esPerfecto x = x == sum (divisores x) 
        divisores x  = [y | y <- [1..x-1], mod x y == 0] 

-- 1ª definición (por generación y prueba): 
        
coloraciones1 :: (Eq a, Eq b) => [(a,[a])] -> [b] -> [[(a,b)]] 
coloraciones1 [] _ = [[]] 
coloraciones1 ((p,vs):m) cs =  
  [(p,c):xs | c <- cs, 
   xs <- coloraciones1 m cs, 
   c `notElem` [c' | (v,c') <- xs, v `elem` vs]] 
  
-- 2ª definición (por generación y prueba con acumulador): 

coloraciones2 :: (Eq a, Eq b) => [(a,[a])] -> [b] -> [[(a,b)]] 
coloraciones2 m cs = aux m cs [[]] 
  where aux [] _ pss = pss 
        aux ((p,vs):m) cs pss = 
          aux m cs [(p,c):ps | c <- cs, ps <- pss, 
                    c `notElem` [c' | (v,c') <- ps, v `elem` vs]]

primesSquareCube=[(fromJust p,s,c,c-s)|
                  prs<-tail $ inits primes,
                  let c=(last prs)^3,
                  s<-takeWhile (<c) $ map (^2) primes ,
                  let p=find (==(s-(c-s))) $ takeWhile (<s) primes,
                  isJust p]
-- http://www.solveet.com/exercises/Numeros-primos-de-Mersenne/94/solution-593  
primosMersenne=filter(\n->not$any(\d->n`mod`d==0)[2..n-1])$map(\n->2^n-1)[1..]

primosMersenne'=[x|n<-[1..],let x=2^n-1,all(/=0)$map(mod x)[2..x-1]]
primosMersenne''=filter(\n->all(\d->mod n d/=0)[2..n-1])$map(\n->2^n-1)[1..]

-- http://www.solveet.com/exercises/Elegancia-cadena-a-tabla-hash/96
s="1,pepe,2012;4,juan,2024;2,andres,2009"
fromStr s r c=Map.fromList [(h,t)|r<-split r s,(h:t)<-[split c r]]
fromStr' c f =Map.fromList.map((\(x:y)->(x,y)).split c).split f
                              
-- http://www.solveet.com/exercises/Primos-en-promedio/107
pp=3:11:23:(map (head.fst) $ iterate f ([71,23,11,3],drop 20 primes))     
   where 
     test p acc=all (isPrime.(\x->div(x+p)2)) acc 
     f (acc,(p:ps)) | test p acc =  (p:acc,ps) 
                    | otherwise = f (acc,ps)
                                
-- http://http://www.solveet.com/exercises/Todas-las-combinaciones-ordenadas-de-un-conjunto/111
comb' xs = n $ map (:[]) xs 
  where n ys = ys ++ (n $ concatMap (\y -> map (y:) ys) xs) 
     
comb'' xs = tail (concat aux)  
  where aux = [[]] : [[x:ys | x <- xs, ys <- yss] | yss <- aux]         
        
comb xs=concat $ iterate (\ys->[y++[x]|y<-ys,x<-xs]) $  map (:[]) xs 

-- http://www.solveet.com/exercises/Descomposiciones-como-suma-de-dos-cuadrados/112

sum2Squares n=[(x,y)|x<-[ini n..lim n],let r=n-x*x,
               y<-[ini r..min (lim r) x],y*y==r]
  where lim=floor.sqrt.fromIntegral
        ini x=lim $ x `div` 2
