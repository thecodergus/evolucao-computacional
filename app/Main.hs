module Main where

import Utils.Arquivo (fileToIntLists)
import GerarPopulacao (gerarPopulacaoBooleana, gerarPopulacaoInteiroPermutado)
import RotinaEvolutiva (loopEvolutivoEnumerado)
import Utils.Grafico (gravarHistorico)
import System.CPUTime ( getCPUTime )
import Selecao (roletaViciada)
import Avaliacoes.NRainhas(avaliacao)
import Crosssover (umPontoAleatorio)

main :: IO ()
main = do
  let n = 8
  pop_incial <- gerarPopulacaoInteiroPermutado 8 n (1, n)

  startTime <- getCPUTime

  geracaoInfo <- loopEvolutivoEnumerado pop_incial (n `avaliacao`) roletaViciada 0.05 (`umPontoAleatorio` 0.9) 0.01 10000

  endTime <- getCPUTime

  let execTime = fromIntegral (endTime - startTime) / (10 ** 12)

  print $ "Tempo de execucao: " ++ show execTime ++ " segundos"

  gravarHistorico geracaoInfo "Grafico.png"