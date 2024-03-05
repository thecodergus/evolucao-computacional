module GerarAleatoriedades where


import System.Random

-- Função auxiliar para gerar um valor inteiro aleatório entre dois valores
randomInt :: (Int, Int) -> IO Int
randomInt = randomRIO

-- Função auxiliar para gerar um valor float aleatório entre dois valores
randomFloat :: (Float, Float) -> IO Float
randomFloat = randomRIO

-- Função auxiliar para gerar um valor booleano aleatório
randomBool :: IO Bool
randomBool = randomRIO (False, True)
