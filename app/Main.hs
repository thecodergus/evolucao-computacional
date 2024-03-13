module Main where
import GerarPopulacao (gerarPopulacaoForSAT)
import Sat (resolver')

main :: IO ()
main = do
  pop <- gerarPopulacaoForSAT 30 5 (-10, 10)

  print pop

  print $ resolver' $ head pop