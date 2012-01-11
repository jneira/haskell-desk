module HaskellTutor where

butlast (h1:h2:[])=h1
butlast (h:t)=butlast t

butlast2=last.init
butlast3=head.tail.reverse

compress [] = []
compress [a] = [a]
compress (h1:h2:t)
  | h1==h2=h1:compress t
  | otherwise=h1:compress (h2:t)

compress2 (h:t) = foldl skip [h] t  
  where skip acc x   
          | last acc == x = acc 
          | otherwise = acc ++ [x]

compress3 []=[]
compress3 (h:t) = h:(compress3 $ dropWhile (==h) t) 

dropevery xs n=r xs (n-1) 
   where r [] _=[]
         r (h:t) 0=r t (n-1)
         r (h:t) n=h:r t (n-1) 
        
dropevery2 xs n= take (n-1) xs ++ dropevery (drop n xs) n

dropevery3 xs n= map fst $ filter (\x-> 0/=mod (snd x) n) $ zip xs [1..]

dupli []=[]
dupli (h:t)=h:h:dupli t

dupli2=concatMap $ replicate 2
dupli3=concatMap (\x->[x,x]) 
dupli4 xs=xs >>= replicate 2
dupli5= foldr (\x acc->x:x:acc) []
