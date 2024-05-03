module Main where

import Utils.Arquivo (fileToIntLists)
import GerarPopulacao (gerarPopulacaoBooleana)
import RotinaEvolutiva (loopEvolutivoEnumerado)
import Avaliacoes.Sat (avaliarSAT)
import Utils.Grafico (gravarHistorico)
import System.CPUTime ( getCPUTime )
import Selecao (roletaViciada)


main :: IO ()
main = do
  arquivo <- fileToIntLists "arquivoSAT.cnf"
  pop_incial <- gerarPopulacaoBooleana 50 100

  startTime <- getCPUTime

  geracaoInfo <- loopEvolutivoEnumerado pop_incial (`avaliarSAT` arquivo) roletaViciada 0.05 0.8 0.1 10000

  endTime <- getCPUTime

  let execTime = fromIntegral (endTime - startTime) / (10 ** 12)

  print $ "Tempo de execucao: " ++ show execTime ++ " segundos"

  gravarHistorico geracaoInfo "Grafico.png"