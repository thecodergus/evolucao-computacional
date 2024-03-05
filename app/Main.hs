module Main where

-- import GerarIndividuos
import GerarPopulacao

main :: IO ()
main = do
  print "Ola"

  -- individuos_booleanos <- gerarIndividuoBooleano 10
  -- print individuos_booleanos

  -- individos_inteiros_a <- gerarIndividuoInteiroBound 10 (-10, 10)
  -- print individos_inteiros_a

  -- individo_flutuantes <- gerarIndividuoFlutuante 10 (-10, 10)
  -- print individo_flutuantes

  -- individos_inteiros_b <- gerarIndividuoInteiroPermutado 10
  -- print individos_inteiros_b

  pop_a <- gerarPopulacaoInteiroBound 10 10 (-10, 10)
  print pop_a

  -- pop_b <- gerarPopulacaoInteiroPermutado 10 10
  -- print pop_b

  pop_c <- gerarPopulacaoFlutuante 10 10 (-10, 10)
  print pop_c

  pop_d <- gerarPopulacaoBooleana 10 10
  print pop_d