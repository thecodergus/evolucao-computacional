module Main where

import Utils.Arquivo (fileToIntLists)
import GerarPopulacao (gerarPopulacaoBooleana, gerarPopulacaoInteiroPermutado)
import RotinaEvolutiva (loopEvolutivoEnumerado)
import Utils.Grafico (gravarHistorico)
import System.CPUTime ( getCPUTime )
import Selecao (roleta)
import qualified Avaliacoes.Radio as Radio
import qualified Avaliacoes.NRainhas as Rainhas
import Crosssover (pmx, doisPontosAleatorios, cx)
import Mutacao (bitflip, mutacao, swap)
import Tipos (Individuo(fitness, genes, Individuo))
import Data.Maybe (maybeToList, fromMaybe)

-- radios :: IO ()
-- radios = do

nRainhas :: IO ()
nRainhas = do
  let n = 128
  let numIndividuos = 10
  let numGeracoes = 2000

  pop_incial <- gerarPopulacaoInteiroPermutado numIndividuos n (1, n)

  startTime <- getCPUTime

  geracaoInfo <- loopEvolutivoEnumerado pop_incial (n `Rainhas.avaliacao`) roleta (`swap` 0.05) (`doisPontosAleatorios` 0.8) 0.2 numGeracoes

  endTime <- getCPUTime

  let execTime = fromIntegral (endTime - startTime) / (10 ** 12)

  print $ "Tempo de execucao: " ++ show execTime ++ " segundos"

  gravarHistorico geracaoInfo "Grafico-NRainhas.png"



main :: IO ()
main = nRainhas
  

