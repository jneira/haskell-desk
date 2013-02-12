module Heap 
       (fromList,allElems,empty,isEmpty,insert,first,top)
       where
import Data.Array 
import Data.Maybe (fromJust,isJust,isNothing,catMaybes)
import Data.List (sort,maximumBy)
import Data.Ord (comparing)
import Control.Applicative hiding (empty)
import Test.QuickCheck hiding  (shrink)
import qualified Test.QuickCheck as TQ (shrink)

newtype Heap a= Heap ((Array Int (Maybe a)),Int,Int)
              deriving (Show,Eq)
type Children a=(Maybe a,Maybe a)

instance (Arbitrary a,Ord a) => Arbitrary (Heap a) where
  arbitrary=sized heap
    where heap n=do 
          max <- choose (1,n)
          elms <- (reverse.sort) `fmap` vectorOf max arbitrary 
          let c = length $ filter isJust $ elms
          return $ Heap (listArray (1,max) elms,max,c)

shapeProp:: Heap a -> Bool
shapeProp (Heap (_,_,0))= True
shapeProp (Heap (_,_,1))= True
shapeProp heap@(Heap (t,m,c))=
  all complete butlast && (even c && single last' || 
                           odd c && complete last')  
  where childs=[ch|i<-[1..c],
                let ch=children heap i,withChild ch]
        (butlast,last')=(init childs,last childs)
        withChild (f,s)=any isJust [f,s]
        complete (f,s)=all isJust [f,s]
        single (f,s)=isJust f && isNothing s               
        
heapProp:: Ord a => Heap a -> Bool
heapProp h@(Heap (t,m,c))=
  all greaterThanChilds parents
  where parents=[1..c] 
        greaterThanChilds i=(and $ (>=) <$> [p] <*> [chl,chr]) || 
                            isNothing p
          where p=get h i
                (chl,chr)=children h i
        
implProp:: Heap a -> Bool
implProp h@(Heap (t,m,c))=c<=m && low==1 && up==m && c==n 
  where (low,up)=bounds t
        n=length $ filter isJust $ elems t

correct heap=and $ [implProp,shapeProp,heapProp] <*> [heap]

fromList :: Ord a => Int -> [a] -> Heap a
fromList max xs=Heap(listArray (1,max) xs'',max,length xs')
  where xs'= take max xs 
        xs''=map Just (reverse $ sort xs')++
            replicate (max-length xs') Nothing

allElems :: Heap a -> [a]
allElems (Heap(t,m,c))=catMaybes $ elems t

prop_fromList xs=
  forAll (choose (1,100)) $ \max-> correct $ heap max
  where heap m=fromList m xs
        types=(xs::[Int])
                            
swap :: Heap a -> Int -> Int -> Heap a
swap heap@(Heap (t,m,c)) from to 
  | c <= 1 = heap 
  | otherwise = Heap (t',m,c) 
  where t'=t//[(to,t!from),(from,t!to)]


prop_reverseSwap heap@(Heap (t,m,c))=
  forAll bounds $ \[x,y]-> swap (swap heap x y) y x == heap
  where bounds=vectorOf 2 (choose (1,m))
        types= (heap::Heap Int)

prop_trivialSwap heap@(Heap (t,m,c))=
  forAll (choose (1,m)) $ \x-> swap heap x x == heap
  where types=(heap::Heap Int)

get :: Heap a -> Int -> Maybe a
get (Heap (_,0,_)) _=Nothing
get (Heap (t,_,_)) i=t!i

set :: Heap a -> Int -> a -> Heap a
set h@(Heap (_,0,_)) _ _=h
set (Heap (t,m,c)) i x=Heap (t//[(i,Just x)],m,c) 

reset :: Heap a -> Int -> Heap a
reset h@(Heap (_,0,_)) _ =h
reset (Heap (t,m,c)) i =Heap (t//[(i,Nothing)],m,c)

prop_invGetSet h@(Heap (t,m,c)) x=
  forAll (choose (1,m)) $ \i-> get (set h i x) i == expected  
  where types=(heap::Heap Int,x::Int)
        expected | m==0 = Nothing
                 | otherwise=Just x

prop_idemSet h@(Heap (t,m,c)) x=
  forAll (choose (1,m)) $ \i-> set (set h i x) i x== set h i x
  where types=(heap::Heap Int,x::Int)

parent:: Heap a -> Int -> Maybe a
parent _ 1=Nothing
parent heap@(Heap (t,m,c)) i=get heap pi
  where pi=i `div` 2

children:: Heap a -> Int -> Children a
children heap@(Heap (t,m,c)) i
  | il<=m && ir<=m = (get heap il,get heap ir) 
  | il<=m =(get heap il,Nothing)
  | otherwise = (Nothing,Nothing)
  where (il,ir)=(2*i,2*i+1)

empty :: Ord a=>Int -> Heap a
empty max=fromList max []

isEmpty :: Heap a -> Bool
isEmpty (Heap (_,_,c))=c==0

prop_empty m=isEmpty $ (empty m:: Heap Int) 

float:: Ord a => Heap a -> Int -> Heap a 
float heap i 
  | i > 1 && p < x = float heap' pi
  | otherwise = heap 
  where pi=i `div` 2
        [p,x]=get heap <$> [pi,i]
        heap'= swap heap i pi
        
shrink:: Ord a => Heap a -> Int -> Heap a
shrink heap@(Heap (_,max,_)) i 
  | i' /= i= shrink heap' i'
  | otherwise=heap
  where idxs=i:filter (<= max) [2*i,2*i+1]
        i' = maximumBy (comparing$(get heap)) idxs
        heap'=swap heap i i'


insert:: Ord a => Heap a -> a -> Heap a 
insert heap@(Heap (t,m,c)) e 
  | m==c = error "Full heap"
  | otherwise=float heap'' $ c+1
  where heap'=Heap(t,m,c+1) 
        heap''=set heap' (c+1) e

prop_Insert heap@(Heap (t,m,c)) e=
  c<m ==> expected (elems t') && correct heap'
  where heap'@(Heap (t',_,_))=insert heap e
        expected=all (flip elem $ Just e:elems t)
        types=(heap::Heap Int,e::Int)

first:: Heap a -> a
first h@(Heap (_,_,0))=error "Empty head"
first h=fromJust $ get h 1 

top:: Ord a => Heap a -> Maybe (Heap a,a)
top h@(Heap (_,_,0))=Nothing
top h@(Heap (t,m,c))=Just (Heap (t',m,c-1),first h)
  where last=fromJust $ get h c 
        h'=reset (set h 1 last) c
        Heap (t',_,_)=shrink h' 1

prop_Top heap@(Heap (t,m,c))=
  c>0 ==> correct heap' && 
          Just e == (maximum $ elems t)  
  where Just (heap',e)=top heap
        types=(heap::Heap Int)

heap:: Heap Int
heap=fromList 10 [1,2,3,4,5]

