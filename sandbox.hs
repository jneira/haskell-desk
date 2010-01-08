
import Data.List as List 
import Data.Array as Array
import Control.Monad.Writer

fac n = product [1..n]

comb n k = fac n / (fac k * fac (n-k))
n !^! k=comb n k 

formulaWhere a b c = [ (-b+d)/n
		     , (-b-d)/n
		     ]
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
 
mkArray :: (Ix a) => (a -> b) -> (a,a) -> Array a b
mkArray f bnds = array bnds [(i, f i) | i <- range bnds]
squares= mkArray (\i -> i * i) (1,100)

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

cuadrado str= putStrLn.concat $ replicate (length str) 
              $ str ++ "\n"

{- Ejercicio 1.4
   Escriba una funcion dividir, de manera que dada una lista de caracteres de como 
   resultado otra lista, pero ahora  dividida en líneas. Cada vez que haya dos 
   caracteres seguidos que sean iguales se insertar en el resultado una
   nueva linea (entre los dos caracteres iguales) -}

dividir ""= putStrLn ""
dividir (h:t)=putStrLn (concat (reverse (foldl acc [h:"\n"] t)))
		where acc ((h:ht):t) ch | h==ch =(ch:h:ht):t 
					| otherwise= (ch:"\n"):(h:ht):t

dividir2 str=putStrLn $ concatMap (\x->x ++ "\n") $ group str


-- Ejercicio 3.1
--aproxseno :: (Num a) => a -> a -> a

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
ndedc lista = let  norepe [] n=[n]
                   norepe (x:xs) n | x == n    = x:xs
                                    | otherwise = n:x:xs
              in length $ foldl norepe [] lista 
           
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

