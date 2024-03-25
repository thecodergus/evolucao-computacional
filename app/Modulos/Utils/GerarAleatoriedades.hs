module GerarAleatoriedades where


import System.Random ( randomRIO )
import Control.Monad (replicateM)

-- Função auxiliar para gerar um valor inteiro aleatório entre dois valores
randomInt :: (Int, Int) -> IO Int
randomInt = randomRIO

-- Função auxiliar para gerar uma lista de inteiros aleatórios
randomIntLista :: (Int, Int) -> Int -> IO [Int]
randomIntLista range num = replicateM num (randomRIO range)

-- Função auxiliar para gerar uma matriz de inteiros aleatórios
randomIntMatriz :: (Int, Int) -> Int -> Int -> IO [[Int]]
randomIntMatriz range num_listas tam_lista = replicateM num_listas (randomIntLista range tam_lista)

-- Função auxliar para gear uma lista de listas com elementos aleatorios
randomSublistas :: (Int, Int) -> Int -> IO [[Int]]
randomSublistas range num_sublistas = do
  num_elementos <- randomRIO (1, num_sublistas)
  replicateM num_sublistas (randomIntLista range num_elementos)

-- Função auxliar para gear uma lista de listas com tamanhos variaveis e elementos aleatorios
randomSublistas' :: (Int, Int) -> Int -> IO [[Int]]
randomSublistas' range num_sublistas = do
  replicateM num_sublistas $ do
    num_elementos <- randomRIO (1, num_sublistas)
    randomIntLista range num_elementos

-- Função auxiliar para gerar um valor float aleatório entre dois valores
randomFloat :: (Float, Float) -> IO Float
randomFloat = randomRIO

-- Função auxiliar para gerar uma lista de floats aleatórios
randomFloatLista :: (Float, Float) -> Int -> IO [Float]
randomFloatLista range num = replicateM num (randomRIO range)

-- Função auxiliar para gerar uma matriz de floats aleatórios
randomFloatMatriz :: (Float, Float) -> Int -> Int -> IO [[Float]]
randomFloatMatriz range num_listas tam_lista = replicateM num_listas (randomFloatLista range tam_lista)

-- Função auxiliar para gerar um valor booleano aleatório
randomBool :: IO Bool
randomBool = randomRIO (False, True)

-- Função auxiliar para gerar um vetor de booleanos aleatorios
randomBoolLista :: Int -> IO [Bool]
randomBoolLista num = replicateM num randomBool

-- Função auxiliar para gerar uma matriz de booleanos aleatorios
randomBoolMatriz :: Int -> Int -> IO [[Bool]]
randomBoolMatriz num_listas tam_lista = replicateM num_listas (randomBoolLista tam_lista)