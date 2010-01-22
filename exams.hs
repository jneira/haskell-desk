module Examen where
import Data.List as List
import Data.Numbers.Primes
-- EXAMEN SEPT 2009

{-1. Una regla de reescritura puede ser vista como una tupla formada por una
cadena de entrada y una lista de cadenas de salida. Por ejemplo:
reglas = [ ("DESPEDIDA",["ADIOS","NOS VEREMOS"]),
("HOLA",["ENCANTADO"]),
("SALUDO",["HOLA","QUE TAL?"]) ]
Asumiremos que en una lista de reglas de reescritura no existen dos reglas
con la misma cadena de entrada. Se pide programar (en HUGS) las
siguientes funciones:
(a) (1 punto) Una funcion reescribe que, dada una cadena c y una lista
de reglas de reescritura r, si en r existe una regla cuya cadena de
entrada coincida con l devuelva la lista de cadenas de salida que establece
dicha regla. Si no existiese una regla tal en r debera devolver
una lista vacia. Por ejemplo:
> reescribe "DESPEDIDA" reglas
["ADIOS","NOS VEREMOS"]
> reescribe "ADIOS" reglas
[]
-}
reglas = [ ("DESPEDIDA",["ADIOS","NOS VEREMOS"]),
		("HOLA",["ENCANTADO"]),("SALUDO",["HOLA","QUE TAL?"]) ]
reescribe c r
  | res==[]=[]
  | otherwise=snd.head $ res
  where res=(filter ((== c).fst)) r
               
reescribe1 sent [] = []
reescribe1 sent ((ent,sal):resto)
	| sent == ent = sal
	| otherwise = reescribe1 sent resto
reescribe2 c r= concat [s | (e,s)<-r,e==c]

{-(b) (1,5 puntos) Diremos que una lista de cadenas es irreducible segun una
lista de reglas de reescritura r, si ninguna de sus cadenas es cadena de
entrada para una regla de r. Se desea, pues, una funcion reescritura
que, dada una lista de reglas de reescritura r y una lista de cadenas
l, devuelva la lista de cadenas irreducible resultado de reescribir todas
las cadenas de l segun r (se valorara eficiencia). Por ejemplo:
> reescritura reglas ["SALUDO","SOY UN PROGRAMA","DESPEDIDA"]
["ENCANTADO","QUE TAL?","SOY UN PROGRAMA","ADIOS","NOS VEREMOS"]-}

reescritura [] r=[]
reescritura (h:s) r 
  | out==[]=h:reescritura s r
  | otherwise=reescritura (out++s) r
  where out=reescribe h r 
                        
test1=reescritura ["SALUDO","SOY UN PROGRAMA","DESPEDIDA"] reglas
         == ["ENCANTADO","QUE TAL?","SOY UN PROGRAMA","ADIOS","NOS VEREMOS"]
{-2. Se pide programar en HUGS las siguientes funciones:
(a) (1 punto) Una funcion extiende que, dada una funcion f::a -> a ->
a y una lista l::[a] extienda la funcion f sobre todos los elementos
de l. Por ejemplo:
>extiende (+) [1..10]
55
> extiende (++) [[1],[2],[23],[34]]
[1,2,23,34]-}

extiende f (h:[])=h
extiende f (h:t)=f h  (extiende f t)  

extiende2 f (h:t)=foldr f h t

{-(b) (1 punto) Una funcion extiendeCola que recibe una funcion f::a ->
a -> a, una lista l::[a] y un dato d de tipo a y comprueba si extendiendo
f sobre alguna de las colas de la lista l se puede obtener d.
Por ejemplo:
> extiendeCola (+) [1..10] 15
False (ya que extiende (+) [a..10] != 15 para todo a entre 1 y 10)
> extiendeCola (+) [1..10] 19
True (ya que extiende (+) [9,10] == 19)-}

extiendeCola _ [] _= False
extiendeCola f (h:t) d
  | extiende f (h:t)==d =True
  | otherwise= extiendeCola f t d 
               
extiendeCola2 _ [x] d = x == d
extiendeCola2 f (x:xs) d = (extiende f (x:xs) == d) || (extiendeCola2 f xs d)               

t2 =not $ extiendeCola2 (+) [1..10] 15
t3 =extiendeCola2 (+) [1..10] 19

{-3. Sea la funcion: funcion c p l1 l2 = [c x y| x<-l1 , p x , y<-l2]
(a) (1 punto) Deduzca, razonadamente, el tipo de funcion.-}

funcion :: (a->b->c)->(a->Bool)->[a]->[b]->[c]
funcion c p l1 l2 = [c x y| x<-l1 , p x , y<-l2]

{-(b) (1 punto) Si primo es una funcion que nos dice si un numero natural
es, o no, un numero primo. Calcule, razonadamente, el resultado de
evaluar la siguiente expresion:
funcion (*) primo [1..10] [1..3]

[2,3,5,7,4,6,10,14,6,9,15,21]

(c) (0.5 puntos) Explique, razonadamente, que concepto propio de la programacion 
funcional ejemplifica la funcion funcion.

listas de comprehension? funciones de orden superior

4. Sean las funciones: suma a = foldr (+) 0 [1..a] y fact a = foldr
(*) 1 [1..a]
Consideremos la lista infinita l=[(suma x,fact x)|x<-[0..]]. Se pide
escribir las siguientes funciones en HUGS:
(a) (0.5 puntos) Una funcion siguiente que dado el termino k-esimo (x,y)
de l y k, calcule el siguiente termino de l.-}
suma a = foldr (+) 0 [1..a]
fact a = foldr (*) 1 [1..a]
l=[(suma x,fact x)|x<-[0..]]
sig (x,y) k = (x+(k+1),y*(k+1))

{-(b) (1 punto) Una funcion ll que calcule la lista l utilizando la funcion
siguiente.-}
 
--ll=[sig (0,0


