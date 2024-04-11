module Utils.Avaliacoes where


import Tipos (Individuo (fitness), Populacao)
import Data.Foldable (maximumBy)
import Data.Ord (comparing)

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

vencedorDoTorneio :: (Ord a) => Populacao a -> [Int] -> IO (Individuo a)
-- A função vencedorDoTorneio recebe uma população e uma lista de índices como parâmetros
vencedorDoTorneio populacao' indices = return $ maximumBy (comparing fitness) [populacao' !! i | i <- indices]
-- Esta linha faz o seguinte:
-- * [populacao' !! i | i <- indices] é uma lista comprehension que seleciona os indivíduos da população correspondentes aos índices fornecidos
-- * maximumBy (comparing fitness) encontra o indivíduo com a maior aptidão na lista de indivíduos selecionados
-- * return envolve o resultado em um IO para que a função possa ser usada em um contexto de IO
