module Main where

import Avaliacoes.Matematica (fitnessMax, fitnessMin)
import Aleatoriedades (randomBoolLista)



main :: IO ()
main = do
    inputBinary <- randomBoolLista 16
    putStrLn $ "Fitness para maximização: " ++ show (fitnessMax inputBinary)
    putStrLn $ "Fitness para minimização: " ++ show (fitnessMin inputBinary)