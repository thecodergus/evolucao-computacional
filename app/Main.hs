module Main where

import GerarIndividuos

main :: IO ()
main = do
  print $ "Ola"
  individuos_booleanos <- gerarIndividuoBooleano 10
  print $ individuos_booleanos
  individos_inteiros_a <- gerarIndividuoInteiroBound 10 (-10, 10)
  print $ individos_inteiros_a
  individo_flutuantes <- gerarIndividuoFlutuante 10 (-10, 10)
  print $ individo_flutuantes