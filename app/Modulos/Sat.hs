{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use notElem" #-}

module Sat (solve) where

import Control.Monad ()
import Data.Maybe ( listToMaybe )

-- tipo para representar um literal
type Literal = Int

-- tipo para representar uma cláusula como uma lista de literais
type Clause = [Literal]

-- tipo para representar uma fórmula como uma lista de cláusulas
type Formula = [Clause]

-- tipo para representar um registro de atribuições como uma lista de literais
type Record = [Literal]

-- | O estado de um solucionador em qualquer momento é um subconjunto da fórmula original e um registro de atribuições; isto é, uma lista de literais considerados verdadeiros.
data SolverState = SolverState
  { formula :: !Formula,
    record :: !Record
  }
  deriving (Show)

-- | O algoritmo principal, uma simples busca com retorno atrás e propagação unitária.
dpll :: SolverState -> Maybe Record
dpll s
  | null f = return r
  | otherwise = do
    l <- chooseLiteral f
    case dpll (SolverState (simplify f l) (l : r)) of
      Just record -> return record
      Nothing -> dpll $! SolverState (simplify f (- l)) ((- l) : r)
  where
    s' = unitpropagate s
    !f = formula s'
    !r = record s'

-- | unitpropagate simplifica a fórmula para cada variável em uma cláusula unitária (ou seja, uma cláusula com apenas uma unidade).
unitpropagate :: SolverState -> SolverState
unitpropagate (SolverState f r) =
  case getUnit f of
    Nothing -> SolverState f r
    Just u -> unitpropagate $ SolverState (simplify f u) (u : r)

-- | Retorna um `Just Literal` ou Nothing se a fórmula não tiver literais restantes. Como o argumento foi verificado para ver se era nulo em uma etapa anterior, a falha em encontrar um literal significa que a fórmula contém apenas cláusulas vazias, implicando que o problema é insatisfatório e `dpll` fará o backtracking.
chooseLiteral :: Formula -> Maybe Literal
chooseLiteral !f = listToMaybe . concat $! f

-- | Se uma cláusula unitária (lista de um único elemento) existir na fórmula, retorne o literal dentro dela, ou Nothing.
getUnit :: Formula -> Maybe Literal
getUnit !xs = listToMaybe [x | [x] <- xs]

-- | Simplificar uma fórmula `f` em relação a um literal `l` significa que, para cada cláusula em que `-l` é um membro, remova `-l`, e remova cada cláusula de f que contenha `l`.
--
--   Raciocínio: uma disjunção com um valor falso não precisa considerar esse valor, e uma disjunção com um valor verdadeiro é trivialmente satisfatória.
simplify :: Formula -> Literal -> Formula
simplify !f !l = [simpClause x l | x <- f, not (l `elem` x)]
  where
    simpClause c l = filter (/= - l) c

-- | A função de alto nível que envolve `dpll` e esconde as internas da biblioteca. Aceita uma lista de listas de Inteiros, tratando a lista externa como uma conjunção e as listas internas como disjunções.
solve :: Formula -> Maybe Record
solve = dpll . flip SolverState []