module Examen where
import Data.List as List
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
reescribe c r=(filter ((== c).fst)) r
reescribe1 sent [] = []
reescribe1 sent ((ent,sal):resto)
	| sent == ent = sal
	| otherwise = reescribe1 sent resto
reescribe2 c r= concat [s | (e,s)<-r,e==c]

{-(b) (1’5 puntos) Diremos que una lista de cadenas es irreducible segun una
lista de reglas de reescritura r, si ninguna de sus cadenas es cadena de
entrada para una regla de r. Se desea, pues, una funcion reescritura
que, dada una lista de reglas de reescritura r y una lista de cadenas
l, devuelva la lista de cadenas irreducible resultado de reescribir todas
las cadenas de l segun r (se valorara eficiencia). Por ejemplo:
> reescritura reglas ["SALUDO","SOY UN PROGRAMA","DESPEDIDA"]
["ENCANTADO","QUE TAL?","SOY UN PROGRAMA","ADIOS","NOS VEREMOS"]-}

