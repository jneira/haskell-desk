module Examen where
import Data.List as List
import Data.Numbers.Primes
import Random
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
   let (d,r)=(div a b,(mod a b)*10)
   in d:dec r b 

{-(b) (0’75 puntos) Una funcion restos que, dados dos numeros naturales
a y b nos devuelva la lista infinita de los sucesivos restos obtenidos al
realizar la division a/b. Por ejemplo:
> restos 146 7
[6,4,5,1,3,2,6,4,5,1,3,2,6,...]-}

restos a b=
  let r=mod a b
  in r:restos (r*10) b
     
{-(c) (1’5 puntos) Una funcion periodo que, dados dos numeros naturales
a y b nos devuelva el periodo de la expresion decimal a/b. Para ello
utilice las funciones decimales y restos realizadas anteriormente.
El periodo comienza cuando aparece por primera vez el primer resto
repetido y termina justo antes de aparecer dicho resto por segunda
vez. Fijandonos en los ejemplos anteriores, el primer resto repetido es
el 6 y la longitud del periodo es, tambien, 6. Por lo tanto, el periodo
seria:
[20,8,5,7,1,4,2,8,5,7,1,4,2,...]
> periodo 146 7
[8,5,7,1,4,2]-}

patron _ xs []=[]
patron max [] (h:t)=patron max [h] t 
patron max xs (h:t)  
  | l>max =[]
  | xs==next=xs 
  | otherwise= patron max (xs++[h]) t  
    where l=length xs
          next=take l (h:t)
          
periodo a b=
  let (hd:td)=dec a b
      itr h (h':t') 
        | null p=itr h' t' 
        | otherwise=p
          where p=patron 10 [h] (h':t')
  in itr hd td
      
{-Convocatoria Febrero 2009 - Primera Semana

1. Diremos que un numero es “especial” si dicho numero es igual a la suma
de los factoriales de sus cifras. Por ejemplo, tenemos que 145 = 1!+4!+5!.
Supongamos que tenemos ya una funcion fact que nos devuelve el factorial
de un numero. Se pide programar (en HUGS) las siguientes funciones:
(a) (1’5 puntos) Una funcion especial que, dado un numero natural n nos
indica si dicho numero es, o no, especial. (Nota: Se pueden utilizar
tantas funciones auxiliares como se consideren necesarias)-}

numLista 0=[]
numLista n=
  let (d,r)=(div n 10,mod n 10)
  in  numLista d++[r] 
 
especial n=n==(sum $ map fact $ numLista n)

especiales=filter especial [1..]

{-(b) (0’5 puntos) Una funcion especiales que, dados dos numeros naturales
a y b, nos devuelve todos los n´umeros especiales entre a y b ambos
inclusive.
Para resolver este apartado utilizaremos la funcion filter que nos
filtra los elementos de una lista que cumplen una cierta condicion. En
este caso la de ser numeros “especiales”:-}

esps a b=filter especial [a..b]

{-2. (1 punto) Se pide programar en HUGS una funcion aplicar, que recibe
un numero natural n, una funcion f :: a -> a y un valor x :: a y
devuelve el resultado de aplicar n veces la funcion f a x. Por ejemplo, si f
x = x + 1, entonces:
> aplicar 0 f 4
4
> aplicar 1 f 4
5
> aplicar 5 f 4
9-}
aplicar n f a=last.(take (n+1))$ (iterate f a)

aplicar2 0 f a=a    
aplicar2 n f a=f (aplicar2 (n-1) f a)    

{-3. (1’5 puntos) El siguiente codigo es una posible forma de implementar el
algoritmo de ordenacion Quicksort:-}
quicksort [] = []
quicksort (a:x) = (quicksort menores) ++ [a] ++ (quicksort mayores)
                  where menores = filter (< a) x
                        mayores = filter (not . (< a)) x
{-Se pide escribir una implementacion equivalente que calcule las listas mayores
y menores de una forma mas eficiente que el codigo aqui presentado.-}

quicksort2 [] = []
quicksort2 (a:x) = (quicksort2 menores) ++ [a] ++ (quicksort2 mayores)
                  where (menores,mayores) = minmax a x
minmax a xs=
  let acc (min,may) x
        | x<a=(x:min,may)
        | otherwise=(min,x:may)
  in foldl acc ([],[]) xs
     
rands x=take x $ randomRs (1,20) (mkStdGen 42)

{-4. Las Maquinas Pila son maquinas virtuales que permiten ejecutar instrucciones
sencillas que operan sobre una pila. Por ejemplo, para sumar dos
numeros a y b, primero se apilaria uno, despues el otro y luego se ejecutarıa
la instruccion Sumar, que desapilarıa los dos ultimos numeros apilados en
la pila, los sumarıa y apilarıa el resultado nuevamente en la pila.
Se pide programar en HUGS una sencilla maquina pila que permita ejecutar
programas con sumas, restas, multiplicaciones y divisiones sobre numeros
enteros. Para ello:
(a) (1’5 puntos) Diseñe una implementacion de una pila utilizando listas.
Implemente las operaciones basicas para trabajar con pilas utilizando
la implementacion que ha diseñado: pilavacia, apilar, desapilar
y cima.-}
pilavacia=[]
apilar h t=h:t
desapilar []=(0,[])
desapilar (h:t)=(h,t)
cima (h:t)=h

{-(b) (1’5 puntos) Implemente las funciones sumar, restar, multiplicar
y dividir que dada una pila p devuelven otra pila en la que se han
desapilado los dos ultimos elementos apilados en p y se ha apilado
la operacion correspondiente (suma, resta, multiplicacion o division)
entre ellos.-}

eval op p=
  let (op1,p1)=desapilar p
      (op2,p2)=desapilar p1
  in apilar (op op1 op2) p2
     
psuma p=eval (+) p
presta p=eval (-) p
pmult p=eval (*) p
pdiv p=eval div p

{-(c) (0’5 puntos) Defina un tipo de datos Instrucciones que contenga las
instrucciones de la maquina pila. Dichas instrucciones deben permitir
apilar un numero entero y realizar las cuatro operaciones basicas
anteriormente descritas.-}

data Instr=Sumar | Restar | Mult | Div | Apilar Int
          deriving (Show)
{-(d) (1 punto) Implemente una funcion ejecutarcodigo a la cual se le
pase una lista de Instrucciones c y una pila p, y devuelva la pila
resultante de ejecutar el codigo almacenado en c. Por ejemplo, si la
lista fuese [Apilar 4,Apilar 3,Apilar 2,Sumar,Multiplicar], se
devolveria una pila en cuya cima se encontraria el resultado de evaluar
la expresion 4  *  (3 + 2).-}

ejecutarCodigo [] p=p
ejecutarCodigo (h:t) p=
  let op Sumar=psuma ; op Restar=presta  
      op Mult=pmult ; op Div=pdiv ;
      op (Apilar x)=(apilar x) 
  in ejecutarCodigo t (op h $ p)
  
test=ejecutarCodigo [Apilar 4,Apilar 3,Apilar 2,Sumar,Mult] []

{-5. (1 punto) Explique el concepto de evaluacion perezosa utilizando como
ejemplo alguna funcion de las que se pide implementar en este examen.

*La eval perezosa consiste en que la evaluacion se pospone hasta que el
resultado de la misma es necesaria. Eso posibilita crear funciones 
que representen valores infinitos

Convocatoria Septiembre 2008 - Original

1. El juego del Tetris consiste en ir colocando una serie de piezas que caen en
un tablero bidimensional, de forma que al llenar una fila esta desaparece.
El juego termina cuando la fila superior del tablero no esta vacia. Para representar
el tablero se puede utilizar una matriz bidimensional de naturales,
representando cada numero el color de las piezas salvo el 0 que representar´ıa
un espacio en blanco. Se pide programar en HUGS:
(a) (1 punto) Una funcion nuevoTablero que, dados dos numeros enteros
ancho y alto mayores que cero, cree un tablero vacio para jugar al
Tetris.-}

nuevoTablero x y=replicate x (replicate y 0)

{-(b) (1’5 puntos) Las funciones filaLlena, filaNoLlena, filaVacia, filaNoVacia
que dada una fila del tablero nos digan, respectivamente, si dicha
fila esta llena, no lo esta, esta vacia o no lo esta.-}

filaLlena=and.(map (/= 0))
filaVacia=and.(map (== 0))
filaNoLlena=not.filaLlena
filaNoVacia=not.filaVacia

{-(c) (0’5 puntos) Una funcion gameOver que, dado un tablero, nos diga si
el juego debe o no terminar.-}

gameOver=filaLlena.head

{-(d) (1 punto) Una funcion numeroDeLineas que, dado un tablero, nos diga
cuantas filas estan llenas en dicho tablero.-}

numLineasLlenas tb=length $ filter (filaLlena) tb

{-(e) (1’5 puntos) Una funcion cambiaTablero que, dado un tablero, nos
devuelva otro tablero (del mismo tamaño) en el que se hayan eliminado
las filas llenas, desplazando hacia abajo las que estuvieran por encima
de estas.-}

cambioTablero []=[]
cambioTablero (h:t) 
  | filaLlena h = fv:cambioTablero t 
   | otherwise=h:cambioTablero t 
    where fv=replicate (length h) 0
          
{-2. Se dice que un numero es omirp cuando se trata de un numero primo y
ademas al invertir sus digitos tambien se obtiene un numero primo. Por
ejemplo 31 es omirp pues 13 tambien es primo, de igual forma 1597 y 7951
son tambien numeros omirp. Se pide programar en HUGS:
(a) (1 punto) Una funcion invierteNumero que dado un numero natural x
mayor que cero, devuelva el numero natural resultado de invertir los
digitos de x.-}
numLista2 0=[]
numLista2 x=
  let (d,r)=(div x 10, mod x 10)
  in (numLista2 d)++[r]

calc x y z=x*y+z  
listaNum2 =foldl (calc 10) 0  

invNum n=listaNum2 $ reverse $ numLista2 n

invierteNumero x = iInvierteNumero 0 x
  where iInvierteNumero ac 0 = ac
        iInvierteNumero ac x = iInvierteNumero (ac*10+u) d
          where u = mod x 10
                d = div x 10
                
{-(b) (0’5 puntos) Suponiendo que tenemos una funcion primo que nos dice
si un numero es primo, se desea una funcion omirp que dado un numero
natural x, nos diga si dicho numero es, o no, omirp.-}

omirp n=isPrime n && (isPrime $ invNum n) 

{-3. Un fichero puede ser vacio o bien contener un documento de texto o una
carpeta, la cual puede contener a su vez un numero arbitrario de ficheros.
Se pide realizar en HUGS:
(a) (0’5 puntos) Definir una estructura que sea capaz de representar un
fichero.-}

data Fichero=Vacio | Texto [Char] | Carpeta [Fichero]

{-(b) (1’5 puntos) Las funciones numTextos y numCarpetas que devuelven,
respectivamente, el numero de documentos de texto y el numero de
carpetas que contiene un fichero.-}

numTextos Vacio = 0
numTextos (Texto x) = 1
numTextos (Carpeta fics)= sum $ map numTextos fics 

numCarpetas Vacio=0
numCarpetas (Texto x)=0
numCarpetas (Carpeta fics)=1+(sum $ map numCarpetas fics)

{-(c) (1 punto) Una funcion sacaTextos que dado un fichero, nos devuelva
otro fichero obtenido eliminando todos los documentos de texto del
fichero de entrada.-}

--sacaTextos fic=