module GerarAleatoriedades where


import System.Random ( randomRIO )
import Control.Monad (replicateM)

-- Função auxiliar para gerar um valor inteiro aleatório entre dois valores
randomInt :: (Int, Int) -> IO Int
randomInt = randomRIO

-- Função auxiliar para gerar um valor float aleatório entre dois valores
randomFloat :: (Float, Float) -> IO Float
randomFloat = randomRIO

-- Função auxiliar para gerar um valor booleano aleatório
randomBool :: IO Bool
randomBool = randomRIO (False, True)

-- Função auxiliar para gerar um vetor de booleanos aleatorios
randomBoolLista :: Int -> IO [Bool]
randomBoolLista num = replicateM num randomBool

-- Função auxiliar para gerar uma matriz de booleanos aleatorios
randomBoolMatriz :: Int -> Int -> IO [[Bool]]
randomBoolMatriz num_listas tam_lista = replicateM num_listas (randomBoolLista tam_lista)