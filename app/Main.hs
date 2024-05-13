module Main where

import Utils.Arquivo (fileToIntLists)
import GerarPopulacao (gerarPopulacaoBooleana, gerarPopulacaoInteiroPermutado)
import RotinaEvolutiva (loopEvolutivoEnumerado)
import Utils.Grafico (gravarHistorico)
import System.CPUTime ( getCPUTime )
import Selecao (roleta)
import qualified Avaliacoes.Radio as Radio
import qualified Avaliacoes.NRainhas as Rainhas
import qualified Avaliacoes.Sat as Sat
import Crosssover (pmx, doisPontosAleatorios, cx)
import Mutacao (bitflip, mutacao, swap)
import Tipos (Individuo(fitness, genes, Individuo), GeracaoInfo (melhorIndividuo))
import Data.Maybe (maybeToList, fromMaybe)
import qualified Utils.Avaliacoes as Avaliacoes


sat :: IO ()
sat = do
  let numIndividuos = 30
  let numGeracoes = 2000

  disjuncao <- fileToIntLists "arquivoSAT.cnf"

  pop_incial <- gerarPopulacaoBooleana numIndividuos 100

  startTime <- getCPUTime

  geracaoInfo <- loopEvolutivoEnumerado pop_incial (`Sat.avaliacao` disjuncao) roleta (`swap` 0.05) (`doisPontosAleatorios` 0.8) 0.2 numGeracoes

  endTime <- getCPUTime

  let execTime = fromIntegral (endTime - startTime) / (10 ** 12)

  print $ "Tempo de execucao: " ++ show execTime ++ " segundos"

  print $ "Melhor individuo: " ++ show (Avaliacoes.melhorIndividuo $ melhorIndividuo geracaoInfo)

  gravarHistorico geracaoInfo "Grafico-Sat.png"

radios :: IO ()
radios = do
  let numIndividuos = 30
  let numGeracoes = 100

  pop_incial <- gerarPopulacaoBooleana numIndividuos 10

  startTime <- getCPUTime

  geracaoInfo <- loopEvolutivoEnumerado pop_incial (fromMaybe (error "Invalid individual") . Radio.avaliacao) roleta (`bitflip` 0.05) (`cx` 0.8) 0.2 numGeracoes

  endTime <- getCPUTime

  let execTime = fromIntegral (endTime - startTime) / (10 ** 12)

  print $ "Tempo de execucao: " ++ show execTime ++ " segundos"

  print $ "Melhor individuo: " ++ show (Avaliacoes.melhorIndividuo $ melhorIndividuo geracaoInfo)

  gravarHistorico geracaoInfo "Grafico-Radios.png"

nRainhas :: IO ()
nRainhas = do
  let n = 32
  let numIndividuos = 10
  let numGeracoes = 20000

  pop_incial <- gerarPopulacaoInteiroPermutado numIndividuos n (1, n)

  startTime <- getCPUTime

  geracaoInfo <- loopEvolutivoEnumerado pop_incial (n `Rainhas.avaliacao`) roleta (`swap` 0.05) (`doisPontosAleatorios` 0.8) 0.2 numGeracoes

  endTime <- getCPUTime

  let execTime = fromIntegral (endTime - startTime) / (10 ** 12)

  print $ "Tempo de execucao: " ++ show execTime ++ " segundos"

  print $ "Melhor individuo: " ++ show (Avaliacoes.melhorIndividuo $ melhorIndividuo geracaoInfo)

  gravarHistorico geracaoInfo "Grafico-NRainhas.png"



main :: IO ()
main = nRainhas


