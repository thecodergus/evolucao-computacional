module Main where

import Utils.Arquivo (fileToIntLists)
import GerarPopulacao (gerarPopulacaoBooleana, gerarPopulacaoInteiroPermutado, gerarPopulacaoInteiroBound)
import RotinaEvolutiva (loopEvolutivoEnumerado)
import Utils.Grafico (gravarHistorico)
import System.CPUTime ( getCPUTime )
import Selecao (roletaComReposicao, roletaSemReposicao, torneio, torneioEstocastico)
import qualified Avaliacoes.Radio as Radio
import qualified Avaliacoes.NRainhas as Rainhas
import qualified Avaliacoes.NRainhasValorada as NRainhasValorada
import qualified Avaliacoes.Sat as Sat
import Crosssover (pmx, doisPontosAleatorios, cx, umPontoAleatorio)
import Mutacao (bitflip, mutacao, swap)
import Tipos (Individuo(fitness, genes, Individuo), GeracaoInfo (melhorIndividuo), Populacao)
import Data.Maybe (maybeToList, fromMaybe)
import qualified Utils.Avaliacoes as Avaliacoes
import Utils.Outros (tratamento)
import Control.Parallel.Strategies (parMap, rpar)


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
  let numGeracoes = 1

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
  let numGeracoes = 10

  pop_incial <- gerarPopulacaoInteiroPermutado numIndividuos n (1, n)
  
  startTime <- getCPUTime

  geracaoInfo <- loopEvolutivoEnumerado (otimizacao pop_incial) (n `Rainhas.avaliacao`) roletaSemReposicao (`swap` 0.05) (`cx` 1) 0 numGeracoes

  endTime <- getCPUTime

  let execTime = fromIntegral (endTime - startTime) / (10 ** 12)

  print $ "Tempo de execucao: " ++ show execTime ++ " segundos"

  print $ "Melhor individuo: " ++ show (Avaliacoes.melhorIndividuo $ melhorIndividuo geracaoInfo)

  gravarHistorico geracaoInfo "Grafico-NRainhas.roletaSemReposicao.swap.cx.png"

  where
    otimizacao :: Populacao Int -> Populacao Int
    otimizacao pop = parMap rpar (\(Individuo gene _) -> Individuo (tratamento gene) 0) pop

nRainhasValorada :: IO ()
nRainhasValorada = do
  let n = 8
  let numIndividuos = 30
  let numGeracoes = 100

  pop_incial <- gerarPopulacaoInteiroPermutado numIndividuos (n * n) (1, n * n)
  
  startTime <- getCPUTime

  geracaoInfo <- loopEvolutivoEnumerado (otimizacao pop_incial) NRainhasValorada.avaliacao roletaSemReposicao (`swap` 0.05) (`cx` 1) 0 numGeracoes

  endTime <- getCPUTime

  let execTime = fromIntegral (endTime - startTime) / (10 ** 12)

  print $ "Tempo de execucao: " ++ show execTime ++ " segundos"

  print $ "Melhor individuo: " ++ show (Avaliacoes.melhorIndividuo $ melhorIndividuo geracaoInfo)

  gravarHistorico geracaoInfo "Grafico-NRainhas.roletaSemReposicao.swap.cx.png"

  where
    otimizacao :: Populacao Int -> Populacao Int
    otimizacao pop = parMap rpar (\(Individuo gene _) -> Individuo (tratamento gene) 0) pop


main :: IO ()
main = nRainhasValorada

-- main :: IO ()
-- main = do
--   pop_incial <- gerarPopulacaoBooleana 20 10

--   let pop_avaliada = map (fromMaybe (error "Invalid individual") . Radio.avaliacao) pop_incial

--   pop_selecionada <- torneioEstocastico 2 0.3 pop_avaliada

--   print $ pop_selecionada


