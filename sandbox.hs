module Sandbox where
-- Ejemplos y ejercicios siguiendo el libro: "Programacion Funcional" de Jeroem Fokker
-- http://people.cs.uu.nl/jeroen/-}


import Data.List as List 
import Data.Array as Array
import Control.Monad.Writer

fac n = product [1..n]

comb n k = fac n / (fac k * fac (n-k))
n !^! k=comb n k 

formulaWhere a b c = [(-b+d)/n, (-b-d)/n ]
		     where d= sqrt (b*b-4.0*a*c)
			   n= 2.0*a
facrec n |n==0 = 1
	 |n>0 = n * fac (n-1)

--Ejemplos de patrones en la declaracion (destructuracion en la llamada)

facrec2 0 = 1
facrec2 (n+1)=(n+1)*fac n

{-Es posible crear listas de funciones, si estas funciones (como numeros, valores booleanos y listas) son de un
mismo tipo, es posible hacer listas de funciones.
:type [sin,cos,tan]-}

{-Esta permitido escribir el tipo de una funcion en el
programa. La definicion de funcion se realizarla de la siguiente forma:-}
mysum :: [Int] -> Int
mysum [] = 0
mysum (x:xs) = x + mysum xs

{-Aunque la declaracion del tipo es superflua, tiene dos ventajas:
* se comprueba si la funcion tiene el tipo que esta declarado.
* la declaracion del tipo ayuda a entender la funcion.-}

add5 :: [Int] -> [Int]
add5 [] = []
add5 (x:xs) = (x+5):add5(xs)       

add6 :: Int -> Int
add6 n = n+6

lpad :: Int -> Char -> [Char] -> [Char]
lpad 0 c cad = cad
lpad n c cad = lpad (n-1) c (c:cad)
 
{- Ejercicio 1.1. 
   Escriba una funcion que cuente cuantos numeros negativos existen en una lista.-}

countNeg0 xs = foldl (\x y -> if y<0 then x+1 else x) 0  xs
countNeg1 xs=sum [1 | x <- xs,x<0]
countNeg2  [] =  0
countNeg2  (h:t) | h < 0 = 1 + countNeg2 t
                 | otherwise  = countNeg2 t

{- Ejercicio 1.2
   Escriba una funcion diag que tenga una lista de caracteres como parametro 
   y que de como resultado los caracteres
   en una diagonal.-}

diag str=putStrLn $ reduce 0 str
	 where 	reduce n []=""
		reduce n (h:t)=	replicate  n  ' ' 
			        ++ [h] ++ reduce (n+1) t

{- Ejercicio 1.3
   Escriba una funcion cuadrado que dada una lista de caracteres, presente tantas 
   copias de esta serie de caracteres (cada copia en una nueva linea), de manera 
   que el numero de las letras en horizontal sea igual al numero de las letras
   que hay verticalmente. Tenga en cuenta que una cadena de caracteres es en realidad 
   una lista de caracteres -}

cuadrado str= putStrLn.unlines $ replicate (length str) str 

{- Ejercicio 1.4
   Escriba una funcion dividir, de manera que dada una lista de caracteres de como 
   resultado otra lista, pero ahora  dividida en lineas. Cada vez que haya dos 
   caracteres seguidos que sean iguales se insertar en el resultado una
   nueva linea (entre los dos caracteres iguales) -}

dividir lst=let line x (h:t) |x==h=x:'\n':h:t
                             |otherwise=x:h:t
                f (h:t)=foldr line [last (h:t)] $ init (h:t)
                f []=[]    
            in putStrLn $ f lst      

{- Ejercicio 3.1
   Escriba una funcion aproxseno que, dados dos numeros eps y x 
   (el primero mayor que 0, el segundo cualquiera),
   de como resultado el numero y con la propiedad de que
   | sin x - y | < eps
   Use la siguiente regla matematica: 
   (-1)^n * x ^(2*n+1) /(fromIntegral $ fac (2*n+1))
   Escriba dos veces una definiciÃÂ¯ÃÂ¿ÃÂ½on para aproxseno: 
   una vez usando la funcion iterate y otra con until.
-}
aproxseno x eps = head $ until (\(y:_)-> abs (sin x-y) < eps) 
                        (\(fst:snd:tail) -> fst+snd:tail)
                        $ termsTaylor x                               

termsTaylor x= map (term x) [0..]    
term x n=  (-1)^n * x ^(2*n+1) /(fromIntegral $ fac (2*n+1))

aproxseno2 x eps= head $ dropWhile (\y-> abs (sin x-y) >= eps)
                       $ map head $ iterate (\(fst:snd:tail) -> fst+snd:tail) 
                                  $ termsTaylor x

{- Ejercicio 3.4
Â¿Que funcion f y que lista a cumplen la siguiente regla?
map (+1) . reverse = foldl f a -}
assert 3.4 f lst=(map (+ 1).reverse $ lst)
                  == (f lst)
test 3.41 = assert 3.4 (foldl (\a b->b+1:a) []) [0,1,2,3]  

{- Ejercicio 3.5
Defina una funcion esta que controle si existe cierto elemento en una lista de elementos. Defina la funcion de las
siguientes maneras:
1 Tome todos los elementos iguales al elemento buscado y coloque estos en una lista. Compruebe despues si
esta lista esta vacia o no.
2 Haga una nueva lista en la que todos los elementos iguales al elemento buscado sean reemplazados por 1 y los
otros elementos por 0. Sume los elementos de la lista resultante y compruebe si el resultado es igual a 0 o no.
3 Compruebe para cada elemento si es igual al elemento buscado o no. Despues compruebe si uno de estos tests
devolvio True.-}

esta 0 x lst = not.null.(filter (== x)) $ lst
esta 1 x lst = 0 < (sum $ map (\y->if y==x then 1 else 0) lst)
esta 2 x lst = or $ map (== x) lst
esta 3 x lst = any (== x) lst

{-Ejercicio 3.6
Escriba una funcion posiciones que devuelva una lista de indices de las posiciones de un elemento determinado en
una lista de elementos.
Por ejemplo:
? posiciones 4 [1,4,3,7,4,2]
[2,5]
? posiciones [3,5] [[3,6],[2,5]]
[] -}

posiciones x lst= let acc (i,is) y | x==y = (i+1,is++[i])
                                   | otherwise=(i+1,is)
                  in snd $ foldl acc (0,[]) lst


{-Ejercicio 3.7
Escriba una funcion ndedc (numero de elementos distintos creciente), que dada una lista no decreciente de numeros,
decida cuantos numeros distintos hay en la lista.
Use el dato de que la lista esta ordenada.-}
ndedc:: (Eq a) => [a] -> Int
ndedc lista = let  norep [] n=[n]
                   norep (x:xs) n | x == n    = x:xs
                                  | otherwise = n:x:xs
              in length $ foldl norep [] lista 

{-Ejercicio 3.8
Escriba una funcion nded (numero de elementos distintos), que, dada una lista cualquiera de numeros, devuelva
cuantos numeros distintos existen en la lista.
Una posibilidad de resolver este problema es contar solamente la primera ocurrencia de cada numero en la lista.
-}
nded :: (Eq a)=>[a]->Int
nded = length.nub 
nded2::(Eq a)=>[a]->Int
nded2 = let cont (x,xs) y | elem y xs = (x,xs)
                          | otherwise = (x+1,y:xs) 
        in fst .(foldl cont (0,[])) 

{-Ejercicio 3.9
Escriba una funcion segmento, que, dados una lista xs y dos numeros i y j, devuelva una sublista de xs desde el
indice i+1 hasta el indice j.
No se puede usar el operador !!.
Antes de contestar esta pregunta, se debe especificar que pasa si j <= i, j > #xs y si i > #s.-}

segmento2 i j lst 
         | j > (length lst)=segmento i (length lst) lst
         | i>=j = segmento j i lst
         | otherwise= let acc (c,xs) x | c>=i && c<=j = (c+1,xs++[x])
                                       | otherwise = (c+1,xs)
                      in snd $ foldl acc (0,[]) lst    

segmento:: Int -> Int ->  [a] -> [a]
segmento i j lista | i >= j = segmento2 j i lista
                    | i < 0=segmento2  0 j lista
                    | j > l=segmento2  i l lista
                    | otherwise=take (j-i) (drop i lista)
                    where l=length lista

{-Ejercicio 3.10
Escriba una funcion esSegmento, que, dadas dos listas xs y ys devuelva True si xs es segmento de ys, 
y False si no.
Una lista xs es sublista de ys cuando ys = hs ++ xs ++ ts, con hs, ts listas de cero o mas elementos.
Se puede usar la funcion segmento del ejercicio anterior.-}
esSegmento xs ys 
           | length xs > length ys = False
           | h1==h2 && xs == take (length xs) ys = True 
           | otherwise = esSegmento xs t2
           where (h1:_,h2:t2)=(xs,ys)

esSegmento2 xs ys | length xs > (length ys)   = False
                  | xs == take (length xs) ys = True
                  | otherwise                 = esSegmento2 xs (tail ys)

{-Ejercicio 3.11
Escriba una funcion scdosa (sigue concatenando los dos anteriores), que, dadas dos listas xs y ys 
del mismo tipo, devuelva una lista infinita de listas, con las siguientes propiedades:
*Los primeros dos elementos son respectivamente xs y ys.
*Para cada n > 0 el n+2-esimo elemento es la concatenacion del n-esimo elemento con el n+1-esimo elemento.
Use la funcion iterate.-}
 
scdosa xs ys =xs:(map last $  iterate  (\lst -> lst++[(last.init $ lst) ++ (last lst)]) [xs,ys])
-- La buena
acdosa2 xs ys = map fst $ iterate (\(xs,ys)->(ys,xs++ys)) (xs, ys)

{-Ejercicio 3.12
Escriba una funcion sssp (sigue sumando el segmento previo), que, dada una lista finita de numeros ns con un
tamaño k > 0, devuelva una lista infinita ms que cumpla con las siguientes propiedades:
* ns = take k ms
* Para todo n >=  k : ms!!(n+1) = (sum . drop (n-k) . take n) ms
Por ejemplo:
sssp [0,0,1] = [0,0,1,1,2,4,7,13,24,44..
Use la funcion iterate.-}
sssp lst = let k=length lst
               f xs=let n=length xs
                    in xs++[(sum.drop(n-k).take n) xs] 
           in init lst ++ (map last $ iterate f lst)

sssp2:: [Int] -> [Int]
sssp2 xs = xs ++ map fst ( iterate f (sum xs, xs) )
          where f (suma, y:ys) = (sum zs, zs) where zs = ys ++ [suma]

-- La buena 
sssp3 ns = map head (iterate f ns)
  where f ns = (tail ns) ++ [sum ns] 

{-Ejercicio 3.13
Escriba una funcion elimDobles, que, dada una lista (que puede ser infinita), devuelva una nueva lista, con solamente
una ocurrencia de cada elemento de la lista original. El problema en este ejercicio es que la lista puede ser infinita.
Por eso, no puede usar las funciones foldr y foldl.-}

elimDobles []=[]
elimDobles (x:xs) = x:(elimDobles (filter (/=x) xs))

{-Ejercicio 3.14
Un valor x se denomina extremo interno con indice i en la lista xs, si i es un indice con las siguientes propiedades:
• 1 < i < length xs
• xs!!i = x
• existen una j y una k , con j < i y k > i con xs!!j /= x y xs!!k /= x
• la mayor j (j < i) y la menor k (k > i) con xs!!j /= x y xs!!k /= x cumplen con la condicion que
o xs!!j > x y xs!!k > x
o xs!!j < x y xs!!k < x
Dos extremos internos con indices i y j en una lista son vecinos si no existe otro extremo con indice k y i < k < j
o j < k < i.
Escriba una funcion extremos, que calcule los extremos internos de una lista.
Use la funcion foldl.-}
-- La mia

extremos [h]= []
extremos [h,t]=[]
extremos (h:t)= 
         let f (acc,p) x 
               | x>p && (head acc)>p ||  
                 x<p && (head acc)<p=(p:acc,x)
               | otherwise= (acc,x)
         in init.fst $ foldl f ([h],h) t

extremos1 [h]= []
extremos1 [h,t]=[]
extremos1 (h:t)=let f (acc,p) x = (acc++e,x) 
                     where e | x>p && (last acc)>p ||  
                               x<p && (last acc)<p=[p]
                             | otherwise=[]
               in tail.fst $ foldl f ([h],h) t
--Otra
extremos2:: [Int] -> [Int]
extremos2 lista = fst (foldl f ([], []) lista)
                 where f ([], []) n     = ([], [n])
                       f (extremos, (y:ys)) n | ys == [] && y == n =(extremos, (y:ys ))
                                              | ys == [] && y /= n =(extremos, (y:[n]))
                                              | head ys == n =(extremos, (y:ys))
                                              | (y < head ys)==((head ys) < n)=(extremos, y:[n])
                                              | otherwise =(extremos ++ [head ys], (head ys):[n])

{-Ejercicio 3.15
Escriba una funcion distanciaExtr que calcule la maxima distancia entre dos extremos vecinos. (Ver el ejercicio
3.14 para la definicion de extremos.) Si no existen dos extremos vecinos en la lista, entonces el resultado sera 0.-}
distanciaExtr (h:t)= 
         let f ((i,max),(acc,p)) x 
                | x>p && (head acc)>p ||  
                  x<p && (head acc)<p=((0,max'),(p:acc,x))
                | otherwise= ((i',max),(acc,x))
               where i'=i+1
                     max' | max<0=0 | i'>max=i'  
                          |otherwise=max
         in  snd.fst $ foldl f ((0,-1),([h],h)) t

-- Entendi mal no es la distancia segun el indice del extremo sino la diferencia entre valores:
distanciaExtr2:: [Int] -> Int
distanciaExtr2 lista = maxDif (extremos lista)

-- Maxima diferencia absoluta entre dos elementos consecutivos
maxDif:: [Int] -> Int
maxDif (x:xs) | length (x:xs) < 2=0
              | otherwise=fst ( foldl f (0, x) xs )
                 where f (a, b) n=( max (abs(b-n))a, n )

{-Ejercicio 3.16
Escriba una funcion recursiva sc (sublistas crecientes), que, dada una lista, devuelva una lista de listas que existan
en todas las sublistas no decrecientes de la lista. Escriba tambien una definicion de sc usando foldr.
Por ejemplo:
? sc [6,1,4,8] = [[],[6],[1],[1,4],[4],
[1,4,8],[4,8],[1,8],[6,8],[8]]-}

sc lista = combinacion lista
           where combinacion     [] = []
                 combinacion (x:xs) = combinacion xs 
                                      ++ (combina x $ combinacion xs) 

combina elemento []     = [[elemento]]
combina elemento (x:xs) | x == [] = combina elemento xs
                        | elemento <=head x=(elemento:x):(combina elemento xs )
                        | otherwise=combina elemento xs  

--Soluci�n con foldr
sc2:: [Int] -> [[Int]]
sc2 lista = foldr f [] lista
            where f elemento xs = xs ++ (combina elemento xs)

--solucion mia no correcta..solo consecutivos
x <=: (h:t)| x<=h = x:(h:t)
           | otherwise=(h:t)
x <=: []=[x]
combs [] = [[]]
combs (x:xs) = [[]] ++ map (x<=:) (combs xs)
sc3 :: (Ord a) => [a] -> [[a]]
sc3=nub.(concatMap combs).subs
{-Ejercicio 3.17
Escriba una funcion dividir, que, dados una lista no decreciente xs y un elemento x, devuelva una tupla de dos
listas (ys,zs), con xs = ys ++ zs, donde todos los elementos de ys sean menores o iguales que x, y todos los
elementos de zs sean mayores que x.
Escriba una funcion insertar, que, dados una lista no decreciente ys y un elemento y, devuelva una lista no
decreciente igual a ys mas el elemento y insertado en el lugar correspondiente.
dividir :: a -> [a] -> ([a],[a])-}
dividir x xs=let acc (xs,(h:t))=
             in

-- Useful tips
infix 8 $>
--($>) :: a-> [(a->b)]  -> [b]
fs $> x = map ($ x) fs

-- Monads
tellMe  :: Int -> Writer String Int
tellMe x= do 
          let y ="2"  -- <- getLine
          tell $ "You have written:" ++ y
          let r=x+(read y :: Int)
          tell $ "The result is:" ++  show  r
          return r

