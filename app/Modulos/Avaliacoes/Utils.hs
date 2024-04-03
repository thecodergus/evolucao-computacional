module Avaliacoes.Utils where

import Tipos (Individuo (fitness), Populacao)

-- melhorIndividuo encontra o indivíduo com a maior aptidão (fitness) em uma população, se houver algum
melhorIndividuo :: Ord a => Populacao a -> Maybe (Individuo a)
melhorIndividuo [] = Nothing
melhorIndividuo (x : xs) = Just (foldl1 maxIndividuo (x : xs))
  where
    -- maxIndividuo retorna o indivíduo com a maior aptidão (fitness) entre dois indivíduos
    maxIndividuo :: Ord a => Individuo a -> Individuo a -> Individuo a
    maxIndividuo i1 i2
      | fitness i1 > fitness i2 = i1
      | otherwise = i2
