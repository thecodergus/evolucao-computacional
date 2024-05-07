module Main where

import Utils.Arquivo (fileToIntLists)
import GerarPopulacao (gerarPopulacaoBooleana, gerarPopulacaoInteiroPermutado)
import RotinaEvolutiva (loopEvolutivoEnumerado)
import Utils.Grafico (gravarHistorico)
import System.CPUTime ( getCPUTime )
import Selecao (roleta)
import Avaliacoes.NRainhas(avaliacao)
import Crosssover (pmx)
import Mutacao (swap, mutacao)

main :: IO ()
main = do
  let n = 128
  pop_incial <- gerarPopulacaoInteiroPermutado 20 n (1, n)

  startTime <- getCPUTime

  geracaoInfo <- loopEvolutivoEnumerado pop_incial (n `avaliacao`) roleta (`mutacao` 0.05) (`pmx` 0.9) 0.01 20

  endTime <- getCPUTime

  let execTime = fromIntegral (endTime - startTime) / (10 ** 12)

  print $ "Tempo de execucao: " ++ show execTime ++ " segundos"

  gravarHistorico geracaoInfo "Grafico.png"