module Selecao where

import Tipos (Individuo(fitness, genes), Populacao)
import Aleatoriedades (randomFloat, randomInt)
import Data.Foldable (maximumBy)
import Data.Ord (comparing)
import Control.Monad (replicateM)


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


torneioEstocastico :: Eq a => Int -> Float -> Populacao a -> IO (Populacao a)
torneioEstocastico k kp populacao = do
  let tamanhoPopulacao = length populacao
      qtdSelecionados = ceiling (fromIntegral tamanhoPopulacao * kp)

  torneios <- replicateM qtdSelecionados (replicateM k (randomInt (0, tamanhoPopulacao - 1)))

  mapM (selecionarMelhor populacao) torneios
  where
    selecionarMelhor :: Eq a => Populacao a -> [Int] -> IO (Individuo a)
    selecionarMelhor populacao' indices = return $ maximumBy (comparing fitness) [populacao' !! i | i <- indices]
