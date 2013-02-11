module Logic where
import Data.List 
-- 
-- Las fórmulas proposicionales se definen por:
-- * Las fórmulas atómicas son fórmulas proposicionales.
-- * Si F es una fómula proposicional, entonces -F también los es.
-- * Si F y F son fórmulas proposicionales, entonces (F /\ G), (F \/ G), 
--   (F --> G) y (F <--> G) también lo son.
data Prop = Atom Char
          | Neg Prop
          | Conj Prop Prop
          | Disj Prop Prop
          | Impl Prop Prop
          | Equi Prop Prop
          deriving Show

-- Ejemplos de fórmulas atómicas.
p, q, r :: Prop
p  = Atom 'p'
q  = Atom 'q'
r  = Atom 'r'

-- Definiciones de las conectivas en notación habitual:
--    no f        es la negación de f 
--    f /\ g      es la conjunción de f y g
--    f \/ g      es la disyunción de f y g
--    f --> g     es la implicación de f a g
--    f <--> g    es la equivalencia entre f y g
no :: Prop -> Prop
no = Neg

infixr 5 \/
infixr 4 /\
infixr 3 -->
infixr 2 <-->
(/\), (\/), (-->), (<-->) :: Prop -> Prop -> Prop
(/\)   = Conj
(\/)   = Disj
(-->)  = Impl
(<-->) = Equi

-- Las interpretaciones son listas de pares cuya primera componente es
-- el nombre de una variable proposicional y la segunda es un valor de verdad. 
type Interpretacion = [(Char,Bool)]

-- (busca c t) es la segunda componente del primer par de la lista de
-- pares t cuya primera componente es igual a c. Por ejemplo,
--    busca 2 [(1,'a'),(3,'d'),(2,'c')]  ==  'c'
busca :: Eq c => c -> [(c,v)] -> v
busca c t = head [v | (c',v) <- t, c == c']

-- (valor i f) es el valor de la fórmula f en la interpretación
-- i. Por ejemplo, 
--    valor [('p',False),('q',True)]  (p --> (p /\ q))  ==  True
--    valor [('p',True), ('q',False)] (p --> (p /\ q))  ==  False
valor :: Interpretacion -> Prop -> Bool
valor i (Atom x)   = busca x i
valor i (Neg f)    = not (valor i f)
valor i (Conj f g) = valor i f && valor i g
valor i (Disj f g) = valor i f || valor i g
valor i (Impl f g) = valor i f <= valor i g
valor i (Equi f g) = valor i f == valor i g

-- (variables f) es el conjunto formado por todos los símbolos
-- proposicionales que aparecen en la fórmula f. Por ejemplo,  
--    variables (p /\ q --> p)  == ['p','q']
variables :: Prop -> [Char]
variables (Atom x)   = [x]
variables (Neg f)    = variables f
variables (Conj f g) = variables f `union` variables g
variables (Disj f g) = variables f `union` variables g
variables (Impl f g) = variables f `union` variables g
variables (Equi f g) = variables f `union` variables g

-- (interpretaciones f) es la lista de las interpretaciones de la
-- fórmula f. Por ejemplo, 
--    ghci> interpretaciones'' (p /\ q --> p)
--    [[('p',True), ('q',True)],
--     [('p',True), ('q',False)],
--     [('p',False),('q',True)],
--     [('p',False),('q',False)]]
interpretaciones :: Prop -> [Interpretacion]
interpretaciones f = aux (variables f)
    where aux []     = [[]]
          aux (v:vs) = [(v,True):i | i <- is] ++ [(v,False):i | i <- is] 
                       where is = aux vs

-- (modelos f) es la lista de todas las interpretaciones de la
-- fórmula f que son modelo de f. Por ejemplo, 
--    ghci> modelos ((p \/ q) /\ ((no q) \/ r))
--    [[('p',True), ('q',True), ('r',True)],
--     [('p',True), ('q',False),('r',True)],
--     [('p',True), ('q',False),('r',False)],
--     [('p',False),('q',True), ('r',True)]]
modelos :: Prop -> [Interpretacion]
modelos f = [i | i <- interpretaciones f, valor i f == True]

-- (esSatisfacible f) se verifica si la fórmula f es satisfacible. Por
-- ejemplo,  
--    esSatisfacible ((p --> q) /\ (q --> r))  ==  True
--    esSatisfacible (p /\ (no p))             ==  False
esSatisfacible :: Prop -> Bool
esSatisfacible f = modelos f /= []
