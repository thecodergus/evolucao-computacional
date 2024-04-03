module Selecao where

import Tipos (Individuo(fitness, Individuo), Populacao)
import Aleatoriedades (randomFloat)
import Data.List(sort)

roletaViciada :: Populacao a -> IO (Populacao a)
roletaViciada pop = do
    -- Gerando valor limite 
    valorAleatorio <- randomFloat (0, 1)
    let valorAleatorio = valorAleatorio * somarFitness

    -- Criando população intermediaria
    let poplacaIntermediaria = selecionar valorAleatorio 0 pop

    -- Gerando valor limite da população intermediaria
    valorAleatorio <- randomFloat (0, 1)
    let valorAleatorio = valorAleatorio * somarFitness

    return $ selecionar valorAleatorio 0 poplacaIntermediaria
        where
            -- Somando todos os valores das fitness
            somarFitness :: Float
            somarFitness = foldl (\b acc -> b + fitness acc) 0.0 pop

            -- Faz a seleção de individuos
            selecionar :: Float -> Float -> Populacao a -> Populacao a
            selecionar _ _ [] = []
            selecionar valorAleatorio contador todos = selecionar' valorAleatorio contador todos []
                where
                    selecionar' :: Ord a => Float -> Float -> Populacao a -> Populacao a -> Populacao a
                    selecionar' valorAleatorio contador todos@(indi:individuos) selecionados
                                | contador > valorAleatorio = selecionados
                                | length (filter (== indi) selecionados) > 2 = selecionar' valorAleatorio contador individuos selecionados
                                | contador > fitness indi = selecionar' valorAleatorio contador individuos selecionados
                                | otherwise = selecionar' valorAleatorio (contador + fitness indi) todos (selecionados ++ [indi])