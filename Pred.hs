module Pred where

type Proposicion = String
data Constante = Falso | Verdad
data Atom =  Constante | Proposicion
data Operacion= No Atom | Y Atom Atom | 
                O Atom Atom | Cond Atom Atom | 
                Bicond Atom Atom
data Letra = Proposicion | Atom

data Expresion = Atom | 
