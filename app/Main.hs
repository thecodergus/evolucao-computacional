module Main where

import Utils.Arquivo (fileToIntLists)
import GerarPopulacao (gerarPopulacaoBooleana, gerarPopulacaoInteiroPermutado, gerarPopulacaoInteiroBound)
import RotinaEvolutiva (loopEvolutivoEnumerado)
import Utils.Grafico (gravarHistorico)
import System.CPUTime ( getCPUTime )
import Selecao (roletaComReposicao, roletaSemReposicao, torneio, torneioEstocastico)
import qualified Avaliacoes.Radio as Radio
import qualified Avaliacoes.NRainhas as Rainhas
import qualified Avaliacoes.Sat as Sat
import qualified Avaliacoes.NRainhasOtmizado as NRainhasOtmizado
import Crosssover (pmx, doisPontosAleatorios, cx, umPontoAleatorio)
import Mutacao (bitflip, mutacao, swap)
import Tipos (Individuo(fitness, genes, Individuo), GeracaoInfo (melhorIndividuo))
import Data.Maybe (maybeToList, fromMaybe)
import qualified Utils.Avaliacoes as Avaliacoes


sat :: IO ()
sat = do
  let numIndividuos = 30
  let numGeracoes = 2000

  disjuncao <- fileToIntLists "/home/udesc/Documentos/evolucao-computacional-main/arquivoSAT.cnf"

  pop_incial <- gerarPopulacaoBooleana numIndividuos 100

  startTime <- getCPUTime

  geracaoInfo <- loopEvolutivoEnumerado pop_incial (`Sat.avaliacao` disjuncao) roletaSemReposicao (`bitflip` 0.05) (`doisPontosAleatorios` 1) 0 numGeracoes

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

  geracaoInfo <- loopEvolutivoEnumerado pop_incial (fromMaybe (error "Invalid individual") . Radio.avaliacao) (torneioEstocastico 3 0.41) (`bitflip` 0.05) (`cx` 0.8) 0.2 numGeracoes

  endTime <- getCPUTime

  let execTime = fromIntegral (endTime - startTime) / (10 ** 12)

  print $ "Tempo de execucao: " ++ show execTime ++ " segundos"

  print $ "Melhor individuo: " ++ show (Avaliacoes.melhorIndividuo $ melhorIndividuo geracaoInfo)

  gravarHistorico geracaoInfo "Grafico-Radios.png"

nRainhas :: IO ()
nRainhas = do
  let n = 16
  let numIndividuos = 30
  let numGeracoes = 10000

  pop_incial <- gerarPopulacaoInteiroPermutado numIndividuos n (1, n)

  startTime <- getCPUTime

  geracaoInfo <- loopEvolutivoEnumerado pop_incial (n `NRainhasOtmizado.avaliacao`) (4 `torneioEstocastico` 0.2) (`swap` 0.05) (`cx` 1) 0 numGeracoes

  endTime <- getCPUTime

  let execTime = fromIntegral (endTime - startTime) / (10 ** 12)

  print $ "Tempo de execucao: " ++ show execTime ++ " segundos"

  print $ "Melhor individuo: " ++ show (Avaliacoes.melhorIndividuo $ melhorIndividuo geracaoInfo)

  gravarHistorico geracaoInfo "Grafico-NRainhas.torneioEstocastico.swap.cx.png"



main :: IO ()
main = nRainhas

-- main :: IO ()
-- main = do
--   pop_incial <- gerarPopulacaoBooleana 20 10

--   let pop_avaliada = map (fromMaybe (error "Invalid individual") . Radio.avaliacao) pop_incial

--   pop_selecionada <- torneioEstocastico 2 0.3 pop_avaliada

--   print $ pop_selecionada


