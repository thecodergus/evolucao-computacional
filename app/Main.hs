module Main where
import GerarPopulacao (gerarPopulacaoBooleana)
import Sat (funcaoObjetivo)
import Data.Foldable (maximumBy)
import Data.Ord (comparing)
import Tipos (Individuo(fitness))


main :: IO ()
main = do
  pop_d <- gerarPopulacaoBooleana 30 5
  let clausulas = [[True, False, False], [False, True, False], [False, False, True], [True, True, True], [False, True, True], [True, False, True], [True, True, False],
                  [False, False, False], [True, True, True], [False, True, False], [True, False, True], [False, False, True], [True, True, False], [False, True, True],
                  [True, False, False], [False, True, True], [True, True, False], [False, False, True], [True, False, True], [False, True, False]]
  let pop = map (`funcaoObjetivo` clausulas) pop_d
  let melhor_individuo = maximumBy (comparing fitness) pop

  print pop
  print $ "Melhor individuo: " ++ show melhor_individuo