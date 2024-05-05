module Avaliacoes.Sat where

import Tipos (Individuo (Individuo, genes))

-- avaliacao recebe um indivíduo booleano e uma lista de disjunções (representadas por listas de inteiros)
-- e retorna o indivíduo booleano com uma nova avaliação de aptidão (fitness)
avaliacao :: Individuo Bool -> [[Int]] -> Individuo Bool
avaliacao individuo disjuncao = Individuo (genes individuo) $ avaliar $ contaBools $ replaceWithBools disjuncao $ genes individuo
  where
    -- avaliar calcula a aptidão (fitness) como a razão entre a quantidade de Trues e o total de valores
    avaliar :: (Int, Int) -> Float
    avaliar (trueCount, totalCount) = fromIntegral trueCount / fromIntegral totalCount

    -- contaBools conta o número de Trues e o total de valores booleanos em uma lista de booleanos
    contaBools :: [[Bool]] -> (Int, Int)
    contaBools = foldl contaBools' (0, 0)
      where
        contaBools' :: (Int, Int) -> [Bool] -> (Int, Int)
        contaBools' (trueCount, totalCount) uniao
          | or uniao = (trueCount + 1, totalCount + 1)
          | otherwise = (trueCount, totalCount + 1)

    -- replaceWithBools substitui os inteiros nas listas de disjunções pelos correspondentes valores booleanos
    replaceWithBools :: [[Int]] -> [Bool] -> [[Bool]]
    replaceWithBools intLists boolList = map (map replace) intLists
      where
        replace :: Int -> Bool
        replace i
          | i < 0 = not (boolList !! (abs i - 1))
          | otherwise = boolList !! (i - 1)