module Selecao where

import Utils.Aleatoriedades (randomFloat, randomInt)
import Control.Monad (replicateM)
import Utils.Avaliacoes (vencedorDoTorneio)
import Tipos (Populacao, Individuo (fitness))


roletaViciada :: Eq a => Populacao a -> IO (Populacao a)
roletaViciada populacao = do
    -- Gerando valor limite 1
    valorAleatorio <- randomFloat (0, 1)
    let valorAleatorio' = valorAleatorio * somarFitness

    -- Criando população Intermediária
    popIntermediaria <- selecao valorAleatorio' 0 populacao

    -- Gerando valor limite 2
    valorAleatorio'' <- randomFloat (0, 1)
    let valorAleatorio''' = valorAleatorio'' * somarFitness

    -- Criando população final
    selecao valorAleatorio''' 0 popIntermediaria
    where
        -- Função que soma o fitness de todos os indivíduos
        somarFitness :: Float
        somarFitness = foldl (\b acc -> b + fitness acc) 0.0 populacao

        -- Função que seleciona os indivíduos que irão para a próxima geração
        selecao :: Eq a => Float -> Float -> Populacao a -> IO (Populacao a)
        selecao _ _ [] = return []
        selecao valorLimite contador pop
            | contador > valorLimite = return []
            | otherwise = do
                individuoAleatorio <- randomInt (0, length pop - 1)

                let individuo = pop !! individuoAleatorio

                restante <- selecao valorLimite (contador + fitness individuo) (filter (/= individuo) pop)

                return (individuo : restante)


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
