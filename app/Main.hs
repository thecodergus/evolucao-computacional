module Main where
import GerarPopulacao (gerarPopulacaoForSAT)
import Sat(solve')

main :: IO ()
main = do
  pop <- gerarPopulacaoForSAT 30 5 (-10, 10)

  print pop

  print $ solve' $ head pop