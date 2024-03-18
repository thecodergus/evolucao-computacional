module Avaliacoes where
import Tipos (Individuo(genes, Individuo))
import Sat (replaceWithBools)
import Control.Parallel.Strategies (parMap, rpar)


avaliarSAT :: Individuo Bool -> [[Int]] -> Individuo Bool
avaliarSAT individuo disjuncao = Individuo (genes individuo) $ avaliar $ contaBools (map or $ replaceWithBools disjuncao $ genes individuo)
    where
        avaliar :: (Int, Int) -> Float
        avaliar (quantidade_True, quantidade_total) = fromIntegral quantidade_True / fromIntegral quantidade_total

        contaBools :: [Bool] -> (Int, Int)
        contaBools = foldl (\(trueCount, totalCount) x -> if x then (trueCount + 1, totalCount + 1) else (trueCount, totalCount + 1)) (0, 0)


avaliarSATs :: [Individuo Bool] -> [[Int]] -> [Individuo Bool]
avaliarSATs individuos disjuncao = parMap rpar (`avaliarSAT` disjuncao) individuos