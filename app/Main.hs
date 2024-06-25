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


main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8
  arquivo <- readFile "distancias.txt"

  let trajetos = parser arquivo
  let num_geracoes = 1000
  let tamanho_populacao = 30

  pop_inicial <- gerarPopulacaoInteiroPermutado tamanho_populacao 244 (1, 244)

  startTime <- getCPUTime

  geracaoInfo <- loopEvolutivoEnumerado pop_inicial (trajetos `avaliacao`) roletaSemReposicao (`swap` 0.06) (`cx` 0.8) 0.3 num_geracoes

  endTime <- getCPUTime

  let execTime = fromIntegral (endTime - startTime) / (10 ** 12)

  let melhorCaminho = intParaString (genes $ fromMaybe (error "Problema") (Avaliacoes.melhorIndividuo $ melhorIndividuo geracaoInfo)) trajetos

  escreverFinal ("caminhoFinal."++ show num_geracoes  ++ "." ++ show tamanho_populacao ++ ".txt") melhorCaminho

  print $ "Tempo de execucao: " ++ show execTime ++ " segundos"
  print $ "Menor distancia: " ++ show (somarDistancias melhorCaminho trajetos)



  gravarHistorico geracaoInfo ("Grafico-Trajeto." ++ show num_geracoes ++ "." ++ show tamanho_populacao ++ ".png")
