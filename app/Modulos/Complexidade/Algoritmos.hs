module Complexidade.Algoritmos where

import Complexidade.Tipos
import Data.List (nub)
import GHC.Base (Int)
import Tipos (Individuo (Individuo))
import Data.Maybe

punicao :: Integer
punicao = 1000000

nomesCidades :: [Trajeto] -> [String]
nomesCidades input = nub $ concatTrajeto input
    where
        concatTrajeto :: [Trajeto] -> [String]
        concatTrajeto [] = []
        concatTrajeto ((Trajeto de' para' _) : xs) = de' : para' : concatTrajeto xs

descobrirDistancia :: String -> String -> [Trajeto] -> Integer
descobrirDistancia _ _ [] = punicao
descobrirDistancia a b ((Trajeto de' para' m) : lista)
    | a == de' && b == para' =
        fromMaybe punicao m
    | otherwise = descobrirDistancia a b lista


somarDistancias :: [String] -> [Trajeto] -> Integer
somarDistancias [] _ = 0
somarDistancias _ [] = 0
somarDistancias [_] (_ : _) = 0
somarDistancias (a : b : resto) lista = descobrirDistancia a b lista + somarDistancias (b : resto) lista

numeroTotalCidades :: [Trajeto] -> Int
numeroTotalCidades trajetos = length $ nomesCidades trajetos

mapearCidadesNumeros :: [Trajeto] -> [(String, Int)]
mapearCidadesNumeros trajetos = zip (nomesCidades trajetos) [1 ..]

intParaString :: [Int] -> [Trajeto] -> [String]
intParaString entrada lista = map (\x -> procurarPorString x (mapearCidadesNumeros lista)) entrada
    where
        procurarPorString :: Int -> [(String, Int)] -> String
        procurarPorString _ [] = error "Algo deu ruim"
        procurarPorString n ((a, b) : lista')
            | n == b = a
            | otherwise = procurarPorString n lista'

funcaoObjetivo :: [Int] -> [Trajeto] -> Integer
funcaoObjetivo entrada lista = somarDistancias (intParaString entrada lista) lista

fitness :: [Int] -> [Trajeto] -> Float
fitness pop lista = - fromIntegral (funcaoObjetivo pop lista)

avaliacao :: [Trajeto]  -> Individuo Int -> Individuo Int
avaliacao lista (Individuo gene _) = Individuo gene (fitness gene lista)


-- Função para escrever uma lista de strings em um arquivo
escreverFinal :: FilePath -> [String] -> IO ()
escreverFinal filename strings = do
  let content = unwords (map (++ " -> ") strings)
  writeFile filename content