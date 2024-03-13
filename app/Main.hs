module Main where
import GerarPopulacao (gerarPopulacaoForSAT)
import Sat (resolver'')
import Avaliacoes (maiorFitness)

main :: IO ()
main = do
  pop <- gerarPopulacaoForSAT 30 10 (-10, 10)

  let pop_2 = map resolver'' pop

  print pop_2
  print "Melhor avaliacao: "
  print $ maiorFitness pop_2