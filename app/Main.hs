module Main where


import RotinaEvolutiva (loopEvolutivoEnumerado)

import Complexidade.Tipos (Trajeto)
import Complexidade.Algoritmos (avaliacao, intParaString, somarDistancias, escreverFinal)
import System.IO (hSetEncoding, stdin, stdout, utf8)
import Complexidade.Parser (parser)
import GerarPopulacao (gerarPopulacaoInteiroPermutado)
import Selecao (roletaSemReposicao)
import Mutacao (swap)
import Crosssover (pmx, cx)
import Utils.Grafico (gravarHistorico)
import System.CPUTime (getCPUTime)
import Tipos (Individuo (Individuo, genes), GeracaoInfo (melhorIndividuo))
import qualified Utils.Avaliacoes as Avaliacoes
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import Control.Parallel
import Control.Parallel.Strategies
import Control.Concurrent.ParallelIO.Global
import System.IO
import System.Random (randomRIO)
import Data.Time (formatTime, defaultTimeLocale)
import Data.Time.Clock (getCurrentTime)

gerarNumeroAleatorio :: IO Int
gerarNumeroAleatorio = randomRIO (1, 1000)

-- Obtenha o timestamp atual
obterTimestampAtual :: IO String
obterTimestampAtual = do
  formatTime defaultTimeLocale "%Y-%m-%d_%H-%M-%S" <$> getCurrentTime

-- Adicione o número aleatório e o timestamp ao nome do arquivo
adicionarNumeroETimestamp :: String -> IO String
adicionarNumeroETimestamp nomeArquivo = do
  numeroAleatorio <- gerarNumeroAleatorio
  timestamp <- obterTimestampAtual
  return $ nomeArquivo ++ "_" ++ show numeroAleatorio ++ "_" ++ timestamp


runs :: (Int, Int) -> IO ()
runs (tamanho_populacao, num_geracoes) = do
  arquivo <- readFile "distancias.txt"

  let trajetos = parser arquivo
  pop_inicial <- gerarPopulacaoInteiroPermutado tamanho_populacao 244 (1, 244)

  startTime <- getCPUTime

  geracaoInfo <- loopEvolutivoEnumerado pop_inicial (trajetos `avaliacao`) roletaSemReposicao (`swap` 0.06) (`cx` 0.8) 0.3 num_geracoes

  endTime <- getCPUTime

  let execTime = fromIntegral (endTime - startTime) / (10 ** 12)

  let melhorCaminho = intParaString (genes $ fromMaybe (error "Problema") (Avaliacoes.melhorIndividuo $ melhorIndividuo geracaoInfo)) trajetos

  trace ("Tempo de execucao: " ++ show execTime ++ " segundos") (return ())
  trace ("Menor distancia: " ++ show (somarDistancias melhorCaminho trajetos)) (return ())

  data_atual <- obterTimestampAtual
  numero_random <- gerarNumeroAleatorio

  escreverFinal ("caminhoFinal." ++ show num_geracoes ++ "." ++ show tamanho_populacao ++ "." ++ data_atual ++ "." ++ show numero_random ++ ".txt") (("Tempo de execucao: " ++ show execTime ++ " segundos\nMenor distancia: " ++ show (somarDistancias melhorCaminho trajetos) ++ "\ngi") : melhorCaminho)

  gravarHistorico geracaoInfo ("Grafico-Trajeto." ++ show num_geracoes ++ "." ++ show tamanho_populacao ++ "." ++ data_atual ++ "." ++ show numero_random ++ ".png")

  return ()


main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8

  parallel_ $
    map
      runs
      [ (30, 1500),
        (30, 1500),
        (30, 1500),
        (30, 1500),
        (30, 1500)
      ]