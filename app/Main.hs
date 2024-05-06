module Main where

import Utils.Arquivo (fileToIntLists)
import GerarPopulacao (gerarPopulacaoBooleana, gerarPopulacaoInteiroPermutado)
import RotinaEvolutiva (loopEvolutivoEnumerado)
import Utils.Grafico (gravarHistorico)
import System.CPUTime ( getCPUTime )
import Selecao (roletaViciada)
import Avaliacoes.NRainhas(avaliacao)
import Mutacao (swap)

main :: IO ()
main = do
  let n = 32
  pop_incial <- gerarPopulacaoInteiroPermutado 8 n (1, n)

  -- print pop_incial
  -- print $ map (n `avaliacao`) pop_incial
  startTime <- getCPUTime

  geracaoInfo <- loopEvolutivoEnumerado pop_incial (n `avaliacao`) roletaViciada (`swap` 0.05)  0.9 0.01 10000

  endTime <- getCPUTime

  let execTime = fromIntegral (endTime - startTime) / (10 ** 12)

  print $ "Tempo de execucao: " ++ show execTime ++ " segundos"

  gravarHistorico geracaoInfo "Grafico.png"