module Examen where
import Data.List as List
import Data.Numbers.Primes
import Random
import Control.Arrow
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
                                    deriving (Show)
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

sacaTextos (Texto x)=Vacio
sacaTextos (Carpeta fics)=Carpeta (map sacaTextos fics)
sacaTextos fic=fic

test6=sacaTextos (Carpeta [Vacio,Texto "hola",Carpeta [Texto "adios"]])
-- >Carpeta [Vacio,Vacio,Carpeta [Vacio]]

{-Convocatoria Febrero 2008 - Segunda Semana-}

{-1. Queremos disponer en HUGS de un tipo de datos que permita expresar funciones
como combinaciones de sumas, restas, productos y divisiones de polinomios.
Para ello:
(a) (1 punto) Defina el tipo de datos Funcion que permita expresar una
funcion como combinacion de sumas, restas, productos o divisiones
de polinomios con coeficientes reales (para representar un polinomio
utilice una lista de numeros reales que contenga sus coeficientes).-}

data OperP=SumaP|RestaP|ProdP|DivP deriving (Show)
data Funcion= Polinomio [Float] | Funcion OperP (Funcion) (Funcion)
                                  deriving (Show)

{-(b) (1’5 puntos) Diseñe una funcion evalua que, dada f de tipo Funcion
y un numero real x, evalue f en x.-}

evalua (Funcion SumaP f g) x= (evalua f x) + (evalua g x) 
evalua (Funcion RestaP f g) x= (evalua f x) - (evalua g x) 
evalua (Funcion ProdP f g) x= (evalua f x) * (evalua g x) 
evalua (Funcion DivP f g) x= (evalua f x) / (evalua g x) 
evalua (Polinomio []) x=0
evalua (Polinomio (h:t)) x=h+ x * evalua (Polinomio t) x 

{-(c) (1’5 puntos) Diseñe una funcion derivada que, dada f de tipo Funcion,
devuelva la funcion derivada de f como un dato de tipo Funcion.
Recuerde que:
(f + g)' = f' + g' 
(f − g)' = f' − g' 
(f * g)' = f' * g + f * g'
(f / g)' = (f'*g+f*g')/g2 
(k * x^n)' = n * k * x^(n−1) -}

derivada (Funcion SumaP f g)=Funcion SumaP (derivada f) (derivada g)
derivada (Funcion RestaP f g)=Funcion RestaP (derivada f) (derivada g)
derivada (Funcion ProdP f g)=Funcion SumaP (Funcion ProdP (derivada f) g)
                             (Funcion ProdP f (derivada g))
derivada (Funcion DivP f g)=
  Funcion DivP 
  (Funcion SumaP 
   (Funcion ProdP (derivada f) g)
   (Funcion ProdP f (derivada g)))
  (Funcion ProdP g g)
derivada (Polinomio xs)= Polinomio ( derivadaPol xs 0 [])


derivadaPol [] _ d
  | d == [] = [0]
  | deriv == [] = [0]
  | otherwise = deriv
    where deriv = tail d
derivadaPol (x:xs) g d = derivadaPol xs (g+1) (d++[x*g])

{-2. Se desea una funcion en HUGS que, dada una lista l nos devuelva una de las
siguientes cadena de caracteres que describa correctamente a l :
• “La lista no tiene elementos”
• “La lista tiene un unico elemento”
• “La lista tiene mas de un elemento”

(a) (1 punto) Diseñe una funcion que realice eficientemente este cometido.-}
 
mensaje []="La lista no tiene elementos"
mensaje [a]="La lista tiene un unico elemento"
mensaje xs="La lista tiene mas de un elemento"

{-(b) (0’5 puntos) Para realizar eficientemente este calculo, ¿que concepto
propio de la programacion funcional estamos aprovechando? Explique
en que consiste dicho concepto comparando la funcion del apartado
anterior con otra funcion que realice el mismo calculo de forma poco
eficiente.

La evaluacion segun patrones estructurales de los datos en lugar de sus valores
En este caso aprovechando la estructura de defincion de una lista
Evaluacion perezosa ya que no se evalua mas que la parte de la
lista necesaria.

3. Realice las siguientes funciones en HUGS en una unica linea de codigo:
(a) (0’5 puntos) Una funcion aListas que, dada una lista de naturales,
devuelva otra lista en la que cada elemento sea una lista que contenga
tantas listas vacias como indique el elemento correspondiente de la
lista de entrada. Por ejemplo:
> aLista [0,1,2,3]
[[],[[]],[[],[]],[[],[],[]]]-}

aLista=map (flip replicate$ [])

{-(b) (0’5 puntos) Una funcion aNaturales que realice el proceso inverso al
realizado por aListas.-}

aNaturales=map length 

{-(c) (0’5 puntos) Una funcion que, dada una lista l como las producidas
por la funcion aListas, realice el mismo calculo que:-}

calc1 l=foldr (+) 0 (aNaturales l)
calc2 l=foldl (+) 0 (aNaturales l)

calc3 l=length ( concat l )

{-4. Un punto en un espacio n-dimensional se puede representar como una lista
de n numeros reales con las coordenadas de dicho punto respecto de un
sistema de referencia dado. Se pide programar las siguientes funciones en
HUGS:
(a) (0’5 puntos) Una funcion igualdimension que, dadas dos listas, nos
diga si ambas tienen la misma longitud. Para realizar esta funcion no
debera utilizarse la funcion predefinida length ni programar
otra que realice su funcion. -}

igualdim [] []=True
igualdim [] ys=False
igualdim xs []=False
igualdim (h:t) (h':t')=igualdim t t'

{-(b) (1 punto) Una funcion distancia que, dados dos puntos en un espacio
n-dimensional, calcule la distancia euclidea (por ejemplo, en dimension
2, la distancia entre [2,1] y [3,4] es la raiz cuadrada de (2−3)^2+(1−4)^2)
entre dichos puntos. Esta funcion debera devolver -1 en el caso de que
los puntos no pertenezcan al mismo espacio (es decir, que la longitud
de ambas listas sea diferente).-}

distancia xs ys
  | (igualdim xs ys)=sum $ map (\[x,y]->(x-y)^2) (transpose [xs,ys])
  | otherwise=(-1)                                 
              
distancia2 [] []=0
distancia2 xs []=(-1)              
distancia2 [] ys=(-1)
distancia2 (h:t) (h':t')=((h-h')^2)+distancia2 t t'
  
{-(c) (1’5 puntos) Una funcion longitud que, dada una lista de puntos en
un espacio n-dimensional, calcule la longitud del camino que forman,
como suma de las distancias entre dos puntos consecutivos. Si alguno
de los puntos no estuviera en el mismo espacio que los demas (alguna
lista es de diferente longitud), esta funcion debera devolver -1.-}
long []=0
long (h:t)=
  let r (acc,p) p'
        | (igualdim p p' && acc>=0)=(acc+distancia p p',p')
        | otherwise= (acc,p)
  in fst $ foldl r (0,h) t 
     
long1 []=0
long1 [a]=0
long1 (p:p':t)  
  | (d>=0) && (l>=0) =d+l
  | otherwise=(-1)
    where d= distancia p p'
          l=long1 t
          
{-Convocatoria Febrero 2008 - Primera Semana-}

{-1. Supongamos tener una funcion mayor (que define un orden parcial) que
dados dos elementos x e y de tipo a nos dice si x es mayor que y. Nuestro
objetivo es, dada una lista de elementos de tipo a, obtener dicha lista
ordenada segun el orden definido por la funcion mayor, de forma que si se
cumple que mayor x y, entonces y debe preceder a x en la lista resultado.
Se pide programar en HUGS las siguientes funciones:
(a) (1’5 puntos) Una funcion inserta que, dada una funcion mayor, un elemento
x de tipo a y una lista l de elementos del mismo tipo (ordenada
segun el orden establecido por la funcion mayor), inserte ordenadamente
x en l, siguiendo el orden que establece la funcion mayor.-}

inserta _ x []=[x]
inserta mayor x (h:t)
  | mayor x h=h:(inserta mayor x t)
  | otherwise=x:h:t

{-(b) (1’5 puntos) Una funcion ordena que, dada una funcion mayor y una
lista l de elementos, devuelva la lista l ordenada segun el orden establecido
por la funcion mayor. Utilice, para ello, la funcion inserta
del apartado anterior-}

ordena mayor []=[]
ordena mayor xs=ordena' mayor [] xs

ordena' mayor xs []=xs
ordena' mayor xs (h:t)=ordena' mayor (inserta mayor h xs) t

{-2. (1’5 puntos) ¿Que conceptos propios de la programacion funcional ilustran
las funciones del ejercicio anterior? Expliquelo tomando como ejemplos las
funciones inserta y ordena.

funciones de orden superior
Definicion de funciones con patrones
El patr´on subrayado
Uso de listas-}

{-3. Se dice que dos numeros son amigos si la suma de los divisores propios (todos
sus divisores salvo el mismo) del primero es igual al segundo y viceversa.
Por ejemplo:
• los divisores propios de 220 son 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 y 110, que
suman 284
• los divisores propios de 284 son 1, 2, 4, 71 y 142, que suman 220
Se pide programar las siguientes funciones en HUGS, cada una en una
´unica linea de codigo:
(a) (0’5 puntos) Una funcion suma que sume los elementos de una lista
de numeros.-}
  
sumaN xs=foldl1 (+) xs
 
{-(b) (0’5 puntos) Una funcion divisores que genere la lista de los divisores
propios de un numero.-}

divs n=filter (\x->(mod n x)==0) [1..(div n 2)] 
divs2 n=[x | x<-[1..(div n 2)],(mod n x)== 0]

{-(c) (0’5 puntos) Una funcion amigos que dados dos numeros nos diga si
son numeros amigos.-}

amigos x y=(sumaN $ divs x) == y &&
           (sumaN $ divs y) == x
           
{-4. (1 punto) Realice una funcion esFactorial (en HUGS) que dado un numero
natural, nos diga si es, o no, el factorial de otro numero y, en caso de serlo,
nos devuelva dicho numero.-}

esFactorial f x 
  | factorial x==f=f
  | otherwise=(-1)

factorial 0=1
factorial 1=1
factorial x=x*(factorial (x-1))

iesFactorial n c fc
  | n == fc = (True,c)
  | n < fc = (False,0)
  | otherwise = iesFactorial n (c+1) (fc*(c+1))
                
esFactorial2 n = iesFactorial n 1 1

{-5. (1 punto) La programacion funcional pura nos permite escribir un programa
sin que importe el orden en el que se definen las funciones. Sin embargo,
en algunas ocasiones el orden de escritura si resulta importante. Ilustre con
un ejemplo como podria variar la evaluacion de una funcion si cambiasemos
el orden de escritura de su definicion.

Una funcion que genere numeros aleatorios sera diferente en un orden u otro

6. Las Maquinas de Turing y otros modelos computacionales utilizan estructuras
de datos denominadas cintas, que consisten en secuencias de valores
arbitrariamente largas. Para acceder a estas cintas existe un puntero que
indica en que posicion de la cinta se encuentra el siguiente dato a leer o
escribir.
(a) (0’5 puntos) Defina un tipo de datos Cinta que permita almacenar,
de forma eficiente, una cinta de datos y el puntero de la misma.-}
data Cinta a = Cinta [a] [a] deriving (Show)

{-(b) (1 punto) Implemente dos funciones avanza y retrocede que dada una
cinta c devuelvan una cinta en las que el puntero se haya movido al
siguiente dato y al anterior respectivamente.-}

avanza (Cinta [a] [])=Cinta [a] []
avanza (Cinta [a] [hp:tp])=Cinta [hp:a] [tp]

retrocede (Cinta [] [p])=Cinta [] [p]
retrocede (Cinta [ha:ta] [p])=Cinta [ta] [ha:p]

{-(c) (0’5 puntos) Implemente una funcion nuevoDato que dada una cinta c
y un dato d, devuelva otra cinta igual a c salvo que el dato apuntado
por el puntero haya sido substituido por d.
S-}

nuevoDato (Cinta [a] [p]) d=Cinta [a] [d:p]

{-Convocatoria Septiembre 2007 - Original

1. (1’5 puntos) La especificacion de una funcion se compone de:
• Precondicion: predicado que define las condiciones que deben cumplir
los datos de entrada
• Postcondicion: predicado que define la relacion entre los datos de entrada
y los datos de salida
Se desea una funcion test en HUGS que, dada una funcion, su especificacion
y un dato de entrada para dicha funcion, nos diga si la funcion, para ese
dato de entrada, cumple con su especificacion.-}

testf f (pre,post) d=not.pre $ d || (post d $ f d)

{-2. (1’5 puntos) Se desea una funcion quitaPareja en HUGS que, dados dos
elementos a y b y una lista xs, devuelva la lista resultante de eliminar de
xs toda aparicion consecutiva (y en el mismo orden) de los elementos a y
b. Por ejemplo:
> quitaPareja 12 10 [1,2,10,12,10,20,12,10,10,12]
[1,2,10,20,10,12]-}

quitaPareja a b []=[]
quitaPareja a b [c]=[c]
quitaPareja a b (h:h':t)
  | a==h && b==h' =quitaPareja a b t  
  | otherwise=h:quitaPareja a b (h':t)
              
t5=quitaPareja 12 10 [1,2,10,12,10,20,12,10,10,12]==[1,2,10,20,10,12]

{-3. Se desea una funcion en HUGS que, dado un numero natural, obtenga su
descomposicion segun unos factores determinados. Para ello:
(a) (1 punto) Programe una funcion factor que, dado un numero n y un
factor x, devuelva una tupla formada por el numero de veces que x
divide a n y el resultado de dividir n por x elevado a dicho numero.
Por ejemplo:
> factor 3 162
(4,2)
Ya que 162 = 2*3^4.-}

factor a n=
  let nxt (x,y)=(x+1,div y a)
  in until (\(x,y)->(mod y a)/=0) nxt (0,n) 

factor2 :: (Num a, Integral b) => (b,b) -> (a,b)
factor2 (x,n) = ifactor x (0,n)
  where ifactor x (e,n)
          | rem n x == 0 = ifactor x (e+1,div n x)
          | otherwise = (e,n)

q3a (x,n) = x > 0 && n > 0
r3a (x,n) (p,r) = x^p*r == n

{-(b) (1 punto) Utilizando la funcion factor del apartado anterior, programe
una funcion factoriza que, dado un numero y una lista infinita de
factores, devuelva una lista conteniendo la factorizacion del numero
segun los factores de la lista. Por ejemplo, si listaPrimos devuelve
la lista infinita de los numeros primos, entonces:
> factoriza 327675 listaPrimos
[(3,1),(5,2),(17,1),(257,1)]
Ya que 327675 = 3^1* 5^2* 17^1* 257^1.-}

factoriza x (h:t) 
  | x==1 = []
  | exp==0 = factoriza x t
  | otherwise=(h,exp):factoriza r t
    where (exp,r)= factor h x   

{-4. (1’5 puntos) Se desea una funcion que dada una lista xs y un predicado
p sobre los elementos de dicha lista, devuelva una tupla formada por dos
listas: la de aquellos elementos que cumplen p y la de los que no lo cumplen.
Por ejemplo, si esPrimo es una funcion que nos dice si un numero es, o no,
primo, entonces:
> separa [1..10] esPrimo
([2,3,5,7],[1,4,6,8,9,10])
No debe usar ninguna funcion predefinida en HUGS para resolver
el ejercicio. Se valorara la eficiencia de la funcion obtenida.-}

separa [] pred=([],[])
separa xs pred=
  let aux (fs,ts) []=(fs,ts)
      aux (fs,ts) (h:t)
        | pred h=aux (h:fs,ts) t
        | otherwise=aux (fs,h:ts) t
  in aux ([],[]) xs 
     
{-5. La sucesion de Farey de orden n es la sucesion creciente de todas 
las fracciones irreducibles con valores entre 0 y 1 que tienen un denominador
menor o igual a n. Para calcular la sucesion de Farey de orden n + 1
partimos de la sucesion de orden n y añadimos entre cada dos elementos
consecutivos una nueva fraccion cuyo numerador y denominador sean, respectivamente,
la suma de los numeradores y la suma de los denominadores
de dichos elementos, siempre que el nuevo denominador sea menor o igual
a n.
Por ejemplo, representando las fracciones como una tupla (numerador, denominador),
las sucesiones de Farey de orden 1, 2, 3 y 4 serian:
• Orden 1: [(0,1),(1,1)] (por definicion)
• Orden 2: [(0,1),(1,2),(1,1)]
• Orden 3: [(0,1),(1,3),(1,2),(2,3),(1,1)]
• Orden 4: [(0,1),(1,4),(1,3),(1,2),(2,3),(3,4),(1,1)]
indicando en negrita aquellas fracciones que aparecen por primera vez.
(a) (2 puntos) Programe una funcion trFarey que, dado un orden n y la
sucesion de Farey de orden n−1, calcule la sucesion de Farey de orden
n.-}
trFarey [] _=[]
trFarey [a] _=[a]
trFarey ((a,b):(c,d):t) n
  | n==0 = (a,b): r
  | otherwise=(a,b):(x,y):r 
    where (x,y)=(a+c,b+d)                 
          r=trFarey ((c,d):t) (n-1)

{-(b) (1’5 puntos) Programe una funcion farey que, dado un numero n,
calcule la sucesion de Farey de orden n. Utilice, para ello, la funcion
trFarey del apartado anterior.-}

farey n=
  let nxt (xs,x)=(trFarey xs (x+1),x+1)
  in fst $ until ((== n).snd) nxt ([(0,1),(1,1)],1) 
     
{-Convocatoria Febrero 2007 - Segunda Semana

1. (1’5 puntos) Un numero natural se dice polidivisible si es divisible por su
longitud y al eliminar la cifra de las unidades volvemos a obtener un numero
polidivisible. Por ejemplo:
1024 sera polidivisible si es divisible por 4 (lo es) y 102 es polidivisible
102 sera polidivisible si es divisible por 3 (lo es) y 10 es polidivisible
10 sera polidivisible si es divisible por 2 (lo es) y 1 es polidivisible
1 es, trivialmente, polidivisible
Por lo tanto 10, 102 y 1024 son numeros polidivisibles
Se desea una funcion en HUGS que, dado un numero natural n > 0 nos diga
si n es, o no, polidivisible.-}

lengthNum x=
  let nxt (i,n)=(i+1,div n 10)
  in fst $ until ((== 0).snd) nxt (0,x)
lengthNum2:: Integer -> Int
lengthNum2=length.show
lengthNum3 n
  | n < 10 = 1
  | otherwise = 1 + lengthNum3 ( div n 10 )

esPolidiv 1=True
esPolidiv x=(0==(mod x $ lengthNum x)) && 
            (esPolidiv $ div x 10) 
            
{-2. Los numeros expansivos se definen de la siguiente forma:
• el primer numero expansivo es el 1
• dado un numero expansivo, para calcular el siguiente cada vez que
en el primero aparezcan n cifras consecutivas iguales (p.ej. 3333) se
sustituiran por n seguido de la cifra que se repite (en el ejemplo 43).
Asi, los primeros numeros expansivos son:
1 -> ‘‘un uno’’: 11
11 -> ‘‘dos unos’’: 21
21 -> ‘‘un dos, un uno’’: 1211
1211 -> ‘‘un uno, un dos, dos unos’’: 111221
...
Se pide programar en HUGS las siguientes funciones:
(a) (1’5 puntos) Una funcion expand que, dada una lista con las cifras de
un numero expansivo genere el siguiente.-}

{-Examen Febrero 2010
Se pide programar en HUGS las siguientes funciones que puedan dar una
respuesta incluso si una (pero no las dos) de las listas que se les pasa como
parámetros es una lista infinita:

(a) (1 punto) Una función masCortata que, dadas dos listas a y b nos diga
si la lista a es más corta que la lista b.-}

masCorta ::[a]->[b]->Bool
masCorta xs []=False
masCorta [] ys=True
masCorta (_:tx) (_:ty)=masCorta tx ty 

{-(b) Utilizando la función anterior, una función masLarga que
dadas dos listas a y b nos diga si la lista a es mas larga que la lista b-}
masLarga ::[a]->[b]->Bool
masLarga a b=masCorta b a

{-(C) Utilizando las dos funciones anteriores, una función igualLongitud 
que, dadas dos listas a y b nos diga si ambas listas tienen la
misma longitud.-}

igualLongitud xs ys= (not $ masCorta xs ys) && (not $ masLarga xs ys)

{-d) (1 punto~ ¿Qué permite que estas funciones admitan que una de las
listas sea infinita? Explique que sucedería si los dos argumentos de
estas funciones fuesen listas infinitas.

La evaluacion perezosa. Se quedaria en un bucle infinito

Una agenda puede ser vista como una lista de tuplas (Fecha,Cita) ordenada
de forma Creciente según las fechas, donde, a su vez, una Fecha, puede ser
vista, como una tupla, de tres enteros que indiquen el día, mes y año de la
fecha y una cita como una cadena de caracteres que la describe.

Se pide programar en HUGS las siguientes funciones para gestionar una
agenda:

(am) (1 punto) Una función nuevaCita que dada una fecha, una Cita, y
una agenda, inserte (de manera ordenada) una nueva Cita en nuestra
agenda.-}
type Agenda=[(Fecha,Cita)]
type Fecha=(Int,Int,Int)
type Cita=[Char]

nuevaCita :: Fecha -> Cita -> Agenda -> Agenda
nuevaCita f c []= [(f,c)]
nuevaCita f c ((fh,ch):t) 
  | f>fh=(fh,ch):(nuevaCita f c t) 
  | otherwise=(f,c):(fh,ch):t

-- Funcionaria si el orden es (aaaa,mm,dd)
{-b) Una función citas que dadas dos fechas fi y ff y una
agenda devuelva todas las Citas que haya en la agenda, que tengan
lugar entre ambas fechas.-}

citas :: Fecha -> Fecha -> Agenda -> Agenda
citas fi ff a=filter (\(f,c)->f>=fi && f<=ff) a 

{-Una matriz bidimensional se puede implementar en HUGS
mediante una lista en la que cada elemento representa una fila
de la matriz en forma de lista de los elementos que contiene. 
Se desea una función columna que dada una matriz y un indice
de columna (que se asumirá válido para la matriz), 
devuelva una lista con los elementos de la matriz que están en
esa Columna. Esta función deberá ser hecha mediante una lista por Com-
prensión.-}

columna:: (Eq a)=>[[a]] -> Int -> [a]
columna1 mx i=[a|l<-mx,a<-l,a==(l!!i)] 
columna mx i=[l!!j|l<-mx,j<-[0..(length l)],j==i] 

{-4. En matemáticas. una sucesión alícuota es una sucesión en la que cada
término es la suma de los divisores propios (todos sus divisores salvo él
mismo) del término anterior. Se pide:

(3) (1 punto) Una función siguiente tal que dado un término de una
sucesión alícuota, Calcule el siguiente término de dicha sucesión. Por
ejemplo:

> siguiente 10
8 (pues los divisores estrictos de 10 son 1, 2 y 5)
> siguiente 6
6 (pues los divisores estrictos de 6 son 1, 2 y 3)-}


divs1 n=filter (\x->(mod n x)==0) [1..(div n 2)]
sig1 n=sum $ divs1 n

{-b) Una función alícuota tal que dado un número calcule la
sucesión alícuota, que dicho número genera. Si algún término de la
sucesión se repite, la sucesión es cíclica. La función deberá, detectar
este hecho y detener el Cálculo mostrando Como último término el
primer término repetido. Por ejemplo:-}

alicuota n=until (\l->elem (last l) (init l))
           (\l->l++[sig1 $ last l]) [n]