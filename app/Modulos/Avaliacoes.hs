module Avaliacoes where
import Tipos (Individuo(genes, Individuo))
import Control.Parallel.Strategies (parMap, rpar)


avaliarSAT :: Individuo Bool -> [[Int]] -> Individuo Bool
avaliarSAT individuo disjuncao = Individuo (genes individuo) $ avaliar $ contaBools (map or $ replaceWithBools disjuncao $ genes individuo)
    where
        avaliar :: (Int, Int) -> Float
        avaliar (quantidade_True, quantidade_total) = fromIntegral quantidade_True / fromIntegral quantidade_total

        contaBools :: [Bool] -> (Int, Int)
        contaBools = foldl (\(trueCount, totalCount) x -> if x then (trueCount + 1, totalCount + 1) else (trueCount, totalCount + 1)) (0, 0)

        replaceWithBools :: [[Int]] -> [Bool] -> [[Bool]]
        replaceWithBools intLists boolList = map (map (replace boolList)) intLists
            where
                replace :: [Bool] -> Int -> Bool
                replace boolList i
                    | i < 0 = not (boolList !! (abs i - 1))
                    | otherwise = boolList !! (i - 1)

                    
avaliarSATs :: [Individuo Bool] -> [[Int]] -> [Individuo Bool]
avaliarSATs individuos disjuncao = parMap rpar (`avaliarSAT` disjuncao) individuos