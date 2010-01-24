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
 
aux (x,y) z= let res=sig (x,y) z
             in (x,y): aux res (z+1)
ll=aux (0,1) 0

lll=map fst $ iterate (\(x,y)->(sig x y,y+1)) ((0,1),0)

{-5. (1.5 puntos) Defina una funcion analizaMaximo que, dada una lista l de
naturales devuelva una tupla con el maximo elemento de l y el numero de
veces que se repite. Por ejemplo:
> analizaMaximo [1,2,3,4,2,3,3,4,2,3,3,1]
(4,2)-}

analizaMaximo xs=
  let a (x,y) z | x==z = (x,y+1)
                | x<z = (z,1) 
                | otherwise=(x,y)
  in foldl a (0,0) xs
     
t4 = (4,2)==analizaMaximo [1,2,3,4,2,3,3,4,2,3,3,1]

{-Convocatoria Febrero 2009 - Segunda Semana-}

{-1. (1â5 puntos) El siguiente codigo es una forma basica de implementar una
funcion que calcule el enesimo numero de fibonacci :-}
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = ( fibonacci (n-1) ) + ( fibonacci (n-2) )
{-Sin embargo, el coste temporal de esta funcion es exponencial. Se pide
escribir una implementacion equivalente que calcule el enesimo numero de
fibonacci de forma eficiente.-}

fib 0 = 1
fib 1 = 1
fib n=let a (h:h':t) _=(h+h':h:h':t)
      in head $ foldl a [1,1] [2..n] 

fib2 n = ifibonacci n 1 1
  where ifibonacci 0 nm1 nm2 = nm2
        ifibonacci n nm1 nm2 = ifibonacci (n-1) (nm1+nm2) nm1
        
{-2. (1â5 puntos) Las ternas pitagoricas son aquellas tuplas (a,b,c) que cumplen
el teorema de pitagoras: a2 +b2 = c2. Se pide programar en HUGS, en una
unica linea y utilizando listas por comprension una funcion que nos devuelva
todas las ternas pitagoricas.-}

pit=[(b,c,a)|a<-[1..],b<-[1..a],c<-[1..b],a^2==b^2+c^2]
pit2 = [(x,y,z) | x <- [1..] , y <- [1..x] , z <- [1..x+y], z*z == x*x + y*y ]

{-3. (1 punto) Dos funciones son equivalentes cuando a igual entrada, devuelven
igual salida. Comprobar esto de forma automatica es, en general, semidecidible,
aunque si restringimos el conjunto de los datos de entrada es perfectamente
posible hacerlo. Se pide, por tanto, programar en HUGS una
funcion equivalentes que recibe dos funciones f y g y una lista de datos l
y comprueba si f y g son equivalentes sobre el conjunto de datos contenido
en l.-}

eqs f g l=(map f l)==(map g l)

{-4. Una funcion muy util a la hora de trabajar con cadenas de caracteres es la
funcion tr, que recibe dos listas de caracteres y una cadena y devuelve la
cadena recibida en la que se han sustituido los caracteres presentes en la
primera lista por los de la segunda (siempre que esten en la misma posicion).
Por ejemplo, para intercambiar los caracteres âaâ y âoâ seria:
> tr "ao" "oa" "hola mundo"
"halo munda"
(a) (1 punto) Se pide implementar una funcion tr generica, que realice lo
anteriormente descrito.-}

tr' x y []=[]
tr' x y (h:t)
  | x==h = y: tr' x y  t 
  | otherwise=h:tr' x y  t

tr [] ys cad=cad
tr xs [] cad=cad
tr (hx:tx) (hy:ty) cad=
   tr tx ty (tr' hx hy cad)
              
tr2 xs ys cad=
    let itr [] ys x=x
        itr xs [] x=x
        itr (h:t) (h':t') x 
            | h==x = h'
            | otherwise=itr t t' x
    in map (itr xs ys) xs


{-(b) (1 punto) ¿Que concepto de la programacion funcional se esta utilizando
en la funcion anterior? Expliquelo y ponga algun otro ejemplo.

Se supone que la recursividad en mi caso, patrones estructurales en fin.

-}

{-5. (1 punto) Dada una lista de numeros, se pide programar en HUGS una
funcion diferencias que devuelva la lista de diferencias entre dos elementos
consecutivos de la lista de entrada. Por ejemplo:
> diferencias [1,4,9,16,25,36]
[3,5,7,9,11]-}

diferencias xs=
            let itr x (0,[])=(x,[])
                itr x (pre,acc)=(x,(pre-x):acc)
            in snd $ foldr itr (0,[]) xs

diferencias2 (h:t)=
            let dif [] x=[]
                dif (h:t) x=(h-x):dif t h
            in dif t h


t6= diferencias2 [1,4,9,16,25,36]

{-6. Se pide programar en HUGS las siguientes funciones:
(a) (0’75 puntos) Una funcion decimales que, dados dos numeros naturales
a y b nos devuelva la lista infinita de los decimales (incluyendo
la parte entera) de la division a/b. Por ejemplo:
> decimales 146 7
[20,8,5,7,1,4,2,8,5,7,1,4,2,...]-}

dec a b=
   let (d,r)=(div a b,(rem a b)*10)
   in d:dec r b 
