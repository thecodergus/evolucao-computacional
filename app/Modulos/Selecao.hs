module Selecao where

import Tipos (Individuo(fitness, Individuo), Populacao)
import Aleatoriedades (randomFloat)
import Data.List(sort)

roletaViciada :: Populacao (*) -> IO (Populacao (*))
roletaViciada pop = do
    valorAleatorio <- randomFloat (0, 1)
    let valorAleatorio = valorAleatorio * somarFitness

    return $ selecionar valorAleatorio 0 pop
        where
            somarFitness :: Float
            somarFitness = foldl (\b acc -> b + fitness acc) 0.0 pop

            selecionar :: Float -> Float -> Populacao * -> Populacao *
            selecionar _ _ [] = []
            selecionar valorAleatorio contador todos@(indi:individuos)  | contador > valorAleatorio =  []
                                                                        | contador > fitness indi = selecionar valorAleatorio contador individuos
                                                                        | otherwise = indi : selecionar valorAleatorio (contador + fitness indi) todos
