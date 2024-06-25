module Main where


import RotinaEvolutiva (loopEvolutivoEnumerado)

import Complexidade.Tipos (Trajeto)
import Complexidade.Algoritmos (avaliacao)
import System.IO (hSetEncoding, stdin, stdout, utf8)
import Complexidade.Parser (parser)
import GerarPopulacao (gerarPopulacaoInteiroPermutado)
import Selecao (roletaSemReposicao)
import Mutacao (swap)
import Crosssover (pmx)
import Utils.Grafico (gravarHistorico)
import System.CPUTime (getCPUTime)


main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8
  arquivo <- readFile "distancias.txt"

  let trajetos = parser arquivo
  let num_geracoes = 20

  pop_inicial <- gerarPopulacaoInteiroPermutado 30 244 (1, 244)

  startTime <- getCPUTime

  geracaoInfo <- loopEvolutivoEnumerado pop_inicial (trajetos `avaliacao`) roletaSemReposicao (`swap` 0.05) (`pmx` 0.75) 0.2 num_geracoes

  endTime <- getCPUTime

  let execTime = fromIntegral (endTime - startTime) / (10 ** 12)

  print $ "Tempo de execucao: " ++ show execTime ++ " segundos"

  gravarHistorico geracaoInfo "Grafico-Trajeto.png"  
