module Selecao where

import Utils.Aleatoriedades (randomInt, randomFloat)
import Utils.Outros(shuffle)
import Control.Monad (replicateM)
import Utils.Avaliacoes (vencedorDoTorneio)
import Tipos (Populacao, Individuo(fitness, Individuo))
import Data.List (sort)
import Debug.Trace(trace)
import Control.Parallel.Strategies (parMap, rpar)


roleta :: (Ord a, Show a) => Populacao a -> IO (Populacao a)
roleta populacao = girarRoleta (fitnessRelativo populacao) (length populacao)
  where
    fitnessTotal :: Populacao a -> Float
    fitnessTotal pop = sum $ map fitness pop

    fitnessRelativo pop = sort $ parMap rpar calcularFitnessRelativo pop
      where
        calcularFitnessRelativo :: Individuo a -> Individuo a
        calcularFitnessRelativo individuo = individuo {fitness = 360 * (fitness individuo / (if fitnessTotal pop /= 0 then fitnessTotal pop else 1))}

    girarRoleta :: (Ord a, Show a) => Populacao a -> Int -> IO (Populacao a)
    girarRoleta [] _ = return []
    girarRoleta _ 0 = return []
    girarRoleta pop n =
      randomFloat (0, 360) >>= \sorteio -> (encontrarIndividuo pop sorteio 0 :) <$> girarRoleta pop (n - 1)
        where
          encontrarIndividuo :: Populacao a -> Float -> Float -> Individuo a
          encontrarIndividuo [] _ _ = error "Individuo nao encontrado"
          encontrarIndividuo (p : ps) sorteio acc
            | sorteio <= acc + fitness p = p
            | otherwise = encontrarIndividuo ps sorteio (acc + fitness p)



-- A função torneioEstocastico recebe o tamanho do torneio (k), a taxa de seleção (kp) e uma população como parâmetros
torneioEstocastico :: (Eq a, Ord a) => Int -> Float -> Populacao a -> IO (Populacao a)
torneioEstocastico k kp populacao
  -- Se k for menor que 2, retorna um erro informando que o valor mínimo é 2
  | k < 2 = error "O valor minimo de k eh igual a 2"
  | otherwise = do
    -- Calcula o tamanho da população e a quantidade de indivíduos a serem selecionados com base na taxa de seleção
    let tamanhoPopulacao = length populacao
        qtdSelecionados = ceiling (fromIntegral tamanhoPopulacao * kp)

    -- Para cada indivíduo a ser selecionado, seleciona k índices aleatoriamente para formar um torneio
    torneios <- replicateM qtdSelecionados (replicateM k (randomInt (0, tamanhoPopulacao - 1)))

    -- Realiza o torneio para cada grupo de índices selecionados e retorna a lista de vencedores
    mapM (vencedorDoTorneio populacao) torneios

torneio :: (Ord a) => Int -> Populacao a -> IO (Populacao a)
torneio k populacao
  | k < 2 = error "O valor minimo de k eh igual a 2"
  | otherwise = do
    -- Calcula o tamanho da população
    let tamanhoPopulacao = length populacao

    -- Para cada indivíduo da população, seleciona k índices aleatoriamente para formar um torneio
    torneios <- replicateM tamanhoPopulacao (replicateM k (randomInt (0, tamanhoPopulacao - 1)))

    -- Realiza o torneio para cada grupo de índices selecionados e retorna a lista de vencedores
    mapM (vencedorDoTorneio populacao) torneios
