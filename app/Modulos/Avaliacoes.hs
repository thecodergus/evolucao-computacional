module Avaliacoes where
import Tipos (Individuo(genes, Individuo, fitness), Populacao)
import Control.Parallel.Strategies (parMap, rpar)


avaliarSAT :: Individuo Bool -> [[Int]] -> Individuo Bool
avaliarSAT individuo disjuncao = Individuo (genes individuo) $ avaliar $ contaBools (map or $ replaceWithBools disjuncao $ genes individuo)
    where
        avaliar :: (Int, Int) -> Float
        avaliar (quantidade_True, quantidade_total) = fromIntegral quantidade_True / fromIntegral quantidade_total

        contaBools :: [Bool] -> (Int, Int)
        contaBools = foldl (\(trueCount, totalCount) x -> if x then (trueCount + 1, totalCount + 1) else (trueCount, totalCount + 1)) (0, 0)

        replaceWithBools :: [[Int]] -> [Bool] -> [[Bool]]
        replaceWithBools intLists boolList = map (map replace) intLists
            where
                replace :: Int -> Bool
                replace i
                    | i < 0 = not (boolList !! (abs i - 1))
                    | otherwise = boolList !! (i - 1)


avaliarSATs :: Populacao Bool -> [[Int]] -> Populacao Bool
avaliarSATs individuos disjuncao = parMap rpar (`avaliarSAT` disjuncao) individuos


melhorIndividuo :: Ord a => Populacao a -> Maybe (Individuo a)
melhorIndividuo [] = Nothing
melhorIndividuo (x:xs) = Just (foldl1 maxIndividuo (x:xs))
  where
    maxIndividuo :: Ord a => Individuo a -> Individuo a -> Individuo a
    maxIndividuo i1 i2
      | fitness i1 > fitness i2 = i1
      | otherwise = i2
