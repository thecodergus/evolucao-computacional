module Main where

import Avaliacoes.Matematica (fitnessMax, fitnessMin)
import Aleatoriedades (randomBoolLista)
import Arquivo (fileToIntLists)
import GerarPopulacao (gerarPopulacaoBooleana)
import RotinaEvolutiva (loopEvolutivoEnumerado)
import Avaliacoes.Sat (avaliarSAT)
import Grafico (gravarHistorico)
import Tipos (Individuo(fitness))
import System.CPUTime


main :: IO ()
main = do
  arquivo <- fileToIntLists "/home/udesc/Documentos/evolucao-computacional-main/arquivoSAT.cnf"
  pop_incial <- gerarPopulacaoBooleana 50 100

  startTime <- getCPUTime

  geracaoInfo <- loopEvolutivoEnumerado pop_incial (`avaliarSAT` arquivo) 0.05 0.8 10

  endTime <- getCPUTime

  let execTime = fromIntegral (endTime - startTime) / (10 ** 12)

  print $ "Tempo de execução: " ++ show execTime ++ " segundos"

  gravarHistorico geracaoInfo "Grafico.png"