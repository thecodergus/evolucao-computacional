module Selecao where

import Tipos (Individuo(fitness, genes), Populacao)
import Aleatoriedades (randomFloat)



roletaViciada :: Eq a => Populacao a -> IO (Populacao a)
roletaViciada pop = do
    -- Gerando valor limite 
    valorAleatorio <- randomFloat (0, 1)
    let valorAleatorio = valorAleatorio * somarFitness

    -- Criando população Intermediária
    let popIntermediaria = selecao valorAleatorio 0 pop []

    -- Gerando novo valor limite
    valorAleatorio2 <- randomFloat (0, 1)
    let valorAleatorio2 = valorAleatorio2 * somarFitness

    -- Criando população final
    let popFinal = selecao valorAleatorio2 0 popIntermediaria []

    return popFinal
    where
        -- Função que soma o fitness de todos os indivíduos
        somarFitness :: Float
        somarFitness = foldl (\b acc -> b + fitness acc) 0.0 pop

        -- Função que seleciona os indivíduos que irão para a próxima geração
        selecao :: Eq a => Float -> Float -> Populacao a -> Populacao a -> Populacao a
        selecao _ _ [] _ = []
        selecao valorLimite contador todos@(um:resto) pop
            | contador > valorLimite = pop
            | length (filter (\x -> genes x == genes um) pop) > 1 = selecao valorLimite contador resto pop
            | otherwise = selecao valorLimite (contador + fitness um) todos pop
