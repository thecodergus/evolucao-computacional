module Avaliacoes where
import Tipos (Individuo(genes, Individuo, fitness), Populacao)
import Control.Parallel.Strategies (parMap, rpar)


avaliarSAT :: Individuo Bool -> [[Int]] -> Individuo Bool
-- avaliarSAT recebe um indivíduo booleano e uma lista de disjunções (representadas por listas de inteiros)
-- e retorna o indivíduo booleano com uma nova avaliação de aptidão (fitness)
avaliarSAT individuo disjuncao = Individuo (genes individuo) $ avaliar $ contaBools $ map or $ replaceWithBools disjuncao $ genes individuo
    where
        -- avaliar calcula a aptidão (fitness) como a razão entre a quantidade de Trues e o total de valores
        avaliar :: (Int, Int) -> Float
        avaliar (trueCount, totalCount) = fromIntegral trueCount / fromIntegral totalCount

        -- contaBools conta o número de Trues e o total de valores booleanos em uma lista de booleanos
        contaBools :: [Bool] -> (Int, Int)
        contaBools = foldl contaBools' (0, 0)
          where
            contaBools' :: (Int, Int) -> Bool -> (Int, Int)
            contaBools' (trueCount, totalCount) True = (trueCount + 1, totalCount + 1)
            contaBools' (trueCount, totalCount) False = (trueCount, totalCount + 1)

        -- replaceWithBools substitui os inteiros nas listas de disjunções pelos correspondentes valores booleanos
        replaceWithBools :: [[Int]] -> [Bool] -> [[Bool]]
        replaceWithBools intLists boolList = map (map replace) intLists
            where
                replace :: Int -> Bool
                replace i
                    | i < 0 = not (boolList !! (abs i - 1))
                    | otherwise = boolList !! (i - 1)

-- avaliarSATs avalia uma população de indivíduos booleanos em relação a uma lista de disjunções
avaliarSATs :: Populacao Bool -> [[Int]] -> Populacao Bool
avaliarSATs individuos disjuncao = parMap rpar (`avaliarSAT` disjuncao) individuos

-- melhorIndividuo encontra o indivíduo com a maior aptidão (fitness) em uma população, se houver algum
melhorIndividuo :: Ord a => Populacao a -> Maybe (Individuo a)
melhorIndividuo [] = Nothing
melhorIndividuo (x:xs) = Just (foldl1 maxIndividuo (x:xs))
  where
    -- maxIndividuo retorna o indivíduo com a maior aptidão (fitness) entre dois indivíduos
    maxIndividuo :: Ord a => Individuo a -> Individuo a -> Individuo a
    maxIndividuo i1 i2
      | fitness i1 > fitness i2 = i1
      | otherwise = i2