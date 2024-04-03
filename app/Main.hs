module Main where

import Avaliacoes.Matematica (fitnessMax, fitnessMin)
import GerarAleatoriedades (randomBoolLista)

f :: Float -> Float
f x = cos (20 * x) - abs x / 2 + x ** 3 / 4

main :: IO ()
main = do
    inputBinary <- randomBoolLista 16
    putStrLn $ "Fitness para maximização: " ++ show (fitnessMax f inputBinary)
    putStrLn $ "Fitness para minimização: " ++ show (fitnessMin f inputBinary)