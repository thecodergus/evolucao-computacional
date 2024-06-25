module Complexidade.Algoritmos where

import Complexidade.Tipos
import Data.List (nub)

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