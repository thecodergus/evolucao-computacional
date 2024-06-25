module Complexidade.Algoritmos where

import Complexidade.Tipos
import Data.List (nub)
import GHC.Base (Int)
import Tipos (Individuo (Individuo))

nomesCidades :: [Trajeto] -> [String]
nomesCidades input = nub $ concatTrajeto input
    where
        concatTrajeto :: [Trajeto] -> [String]
        concatTrajeto [] = [] 
        concatTrajeto ((Trajeto de para _) : xs) = de : para : concatTrajeto xs

descobrirDistancia :: String -> String -> [Trajeto] -> Integer
descobrirDistancia _ _ [] = 999999999
descobrirDistancia a b ((Trajeto de para m) : lista)
    | a == de && b == para = 
        case m of
            Just x -> x
            Nothing -> 999999999
    | otherwise = descobrirDistancia a b lista


somarDistancias :: [String] -> [Trajeto] -> Integer
somarDistancias [] _ = 0
somarDistancias _ [] = 0
somarDistancias (a : b : resto) lista = (descobrirDistancia a b lista) + (somarDistancias (b : resto) lista)

numeroTotalCidades :: [Trajeto] -> Int
numeroTotalCidades trajetos = length $ nomesCidades trajetos

mapearCidadesNumeros :: [Trajeto] -> [(String, Int)]
mapearCidadesNumeros trajetos = zip (nomesCidades trajetos) [1 ..]

funcaoObjetivo :: [Int] -> [Trajeto] -> Integer
funcaoObjetivo entrada lista = somarDistancias (map (\x -> procurarPorString x (mapearCidadesNumeros lista)) entrada) lista
    where
        procurarPorString :: Int -> [(String, Int)] -> String
        procurarPorString _ [] = error "Algo deu ruim"
        procurarPorString n ((a, b) : lista)
            | n == b = a
            | otherwise = procurarPorString n lista

fitness :: [Int] -> [Trajeto] -> Float
fitness pop lista
    | (funcaoObjetivo pop lista) /= 0 = 1 / fromIntegral (funcaoObjetivo pop lista)
    | otherwise = 1

avaliacao :: [Trajeto]  -> Individuo Int -> Individuo Int
avaliacao lista (Individuo gene _) = Individuo gene (fitness gene lista)