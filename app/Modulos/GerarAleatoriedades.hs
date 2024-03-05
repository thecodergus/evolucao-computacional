module GerarAleatoriedades where


import System.Random

-- Função auxiliar para gerar um valor inteiro aleatório entre dois valores
randomRInt :: (Int, Int) -> IO Int
randomRInt range = randomRIO range

-- Função auxiliar para gerar um valor float aleatório entre dois valores
randomRFloat :: (Float, Float) -> IO Float
randomRFloat range = randomRIO range

-- Função auxiliar para gerar um valor booleano aleatório
randomRBool :: IO Bool
randomRBool = randomRIO (False, True)
