module Main where
import GerarPopulacao (gerarPopulacaoForSAT)

main :: IO ()
main = do
  pop <- gerarPopulacaoForSAT 30 5 (-100, 100)

  print pop