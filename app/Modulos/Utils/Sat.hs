{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use notElem" #-}

module Sat (resolver'') where

import Control.Monad ()
import Data.Maybe ( listToMaybe )
import Tipos (Individuo(genes, Individuo))

-- tipo para representar um literal
type Literal = Int

-- tipo para representar uma cláusula como uma lista de literais
type Clausula = [Literal]

-- tipo para representar uma fórmula como uma lista de cláusulas
type Formula = [Clausula]

-- tipo para representar um registro de atribuições como uma lista de literais
type Registro = [Literal]

--  O estado de um solucionador em qualquer momento é um subconjunto da fórmula original e um registro de atribuições; isto é, uma lista de literais considerados verdadeiros.
data EstadoSolucionador = EstadoSolucionador
  { formula :: !Formula,
    record :: !Registro
  }
  deriving (Show)

--  O algoritmo principal, uma simples busca com retorno atrás e propagação unitária.
dpll :: EstadoSolucionador -> Maybe Registro
dpll s
  | null f = return r
  | otherwise = do
    l <- escolhaLiteral f
    case dpll (EstadoSolucionador (simplificar f l) (l : r)) of
      Just record -> return record
      Nothing -> dpll $! EstadoSolucionador (simplificar f (- l)) ((- l) : r)
  where
    s' = propagador s
    !f = formula s'
    !r = record s'

--  propagador simplifica a fórmula para cada variável em uma cláusula unitária (ou seja, uma cláusula com apenas uma unidade).
propagador :: EstadoSolucionador -> EstadoSolucionador
propagador (EstadoSolucionador f r) =
  case getUnit f of
    Nothing -> EstadoSolucionador f r
    Just u -> propagador $ EstadoSolucionador (simplificar f u) (u : r)

--  Retorna um `Just Literal` ou Nothing se a fórmula não tiver literais restantes. Como o argumento foi verificado para ver se era nulo em uma etapa anterior, a falha em encontrar um literal significa que a fórmula contém apenas cláusulas vazias, implicando que o problema é insatisfatório e `dpll` fará o backtracking.
escolhaLiteral :: Formula -> Maybe Literal
escolhaLiteral !f = listToMaybe . concat $! f

--  Se uma cláusula unitária (lista de um único elemento) existir na fórmula, retorne o literal dentro dela, ou Nothing.
getUnit :: Formula -> Maybe Literal
getUnit !xs = listToMaybe [x | [x] <- xs]

--  Simplificar uma fórmula `f` em relação a um literal `l` significa que, para cada cláusula em que `-l` é um membro, remova `-l`, e remova cada cláusula de f que contenha `l`.
--   Raciocínio: uma disjunção com um valor falso não precisa considerar esse valor, e uma disjunção com um valor verdadeiro é trivialmente satisfatória.
simplificar :: Formula -> Literal -> Formula
simplificar !f !l = [simpClausula x l | x <- f, not (l `elem` x)]
  where
    simpClausula c l = filter (/= - l) c

--  A função de alto nível que envolve `dpll` e esconde as internas da biblioteca. Aceita uma lista de listas de Inteiros, tratando a lista externa como uma conjunção e as listas internas como disjunções.
resolver :: Formula -> Maybe Registro
resolver = dpll . flip EstadoSolucionador []

resolver' :: Individuo [Int] -> Maybe Registro
resolver' input = resolver $ genes input

resolver'' :: Individuo [Int] -> Individuo [Int]
resolver'' input = case resolver' input of
  Nothing -> Individuo (genes input) 0
  Just vetor -> Individuo (genes input) (fromIntegral (length vetor) / fromIntegral (length (genes input)))