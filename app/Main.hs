module Main where

import Avaliacoes.NRainhas (fitness')
import qualified Avaliacoes.NRainhas as Rainhas
import qualified Avaliacoes.Radio as Radio
import qualified Avaliacoes.Sat as Sat
import Control.Monad (void)
import Control.Parallel.Strategies (parMap, rpar)
import Crosssover (cx, doisPontosAleatorios, pmx, umPontoAleatorio)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Time
import GerarPopulacao (gerarPopulacaoBooleana, gerarPopulacaoInteiroBound, gerarPopulacaoInteiroPermutado)
import Mutacao (bitflip, mutacao, swap)
import RotinaEvolutiva (loopEvolutivoEnumerado)
import Selecao (roletaComReposicao, roletaSemReposicao, torneio, torneioEstocastico)
import System.CPUTime (getCPUTime)
import Tipos (GeracaoInfo (melhorIndividuo), Individuo (Individuo, fitness, genes), Populacao)
import Utils.Arquivo (fileToIntLists)
import qualified Utils.Avaliacoes as Avaliacoes
import Utils.Grafico (gravarHistorico)

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

tranformarParaValorado :: Int -> [(Int, Int)] -> [Int]
tranformarParaValorado _ [] = []
tranformarParaValorado n' ((x, y) : ts) = (n' * x) + y : tranformarParaValorado n' ts

nRainhas :: [(Int, Int)] -> IO ()
nRainhas [] = return ()
nRainhas ((n, numGeracoes) : ss) = do
  let numIndividuos = 30

  pop_incial <- gerarPopulacaoInteiroPermutado numIndividuos n (0, n - 1)

  startTime <- getCPUTime

  geracaoInfo <- loopEvolutivoEnumerado pop_incial (Rainhas.avaliacao True n) roletaSemReposicao (`swap` 0.05) (`pmx` 0.8) 0.3 numGeracoes

  endTime <- getCPUTime

  let execTime = fromIntegral (endTime - startTime) / (10 ** 12)

  currentTime <- getCurrentTime

  putStrLn $ "N" ++ show n ++ "-G" ++ show numGeracoes ++ " Tempo de execucao: " ++ show execTime ++ " segundos"

  gravarHistorico geracaoInfo ("N" ++ show n ++ "-G" ++ show numGeracoes ++ "-" ++ formatTime defaultTimeLocale "%Y-%m-%d_%H-%M-%S" currentTime ++ "-Grafico-NRainhas.roletaSemReposicao.swap.cx.png")

  appendFile ("N" ++ show n ++ "-G" ++ show numGeracoes ++ "-NRainhas.csv") (show n ++ ";" ++ show numGeracoes ++ ";" ++ show (genes $ head $ maybeToList $ Avaliacoes.melhorIndividuo $ melhorIndividuo geracaoInfo) ++ ";" ++ show (Rainhas.fo $ genes $ head $ maybeToList $ Avaliacoes.melhorIndividuo $ melhorIndividuo geracaoInfo) ++ ";" ++ show (Rainhas.fo $ genes (fromMaybe (error "Invalid individual") $ Avaliacoes.melhorIndividuo $ melhorIndividuo geracaoInfo)) ++ ";" ++ show execTime ++ ";" ++ show (length (Rainhas.rainhasAtacando (genes $ head $ maybeToList $ Avaliacoes.melhorIndividuo $ melhorIndividuo geracaoInfo))) ++ "\n")

  void (nRainhas ss)

main :: IO ()
main =
  nRainhas
    [ (64, 20000),
      (64, 20000),
      (64, 20000),
      (64, 20000),
      (64, 20000),
      (64, 20000),
      (64, 20000),
      (64, 20000),
      (64, 20000),
      (64, 20000)
    ]