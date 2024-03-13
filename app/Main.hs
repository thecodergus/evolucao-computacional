module Main where
import GerarPopulacao (gerarPopulacaoForSAT)
import Sat (resolver'')

main :: IO ()
main = do
  pop <- gerarPopulacaoForSAT 30 10 (-10, 10)

  let pop_2 = map resolver'' pop

  print pop_2