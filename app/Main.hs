module Main where

import Control.Parallel.Strategies
import Data.List.Split
import System.Random

data TipagemGene = 
    INTEIRO Int | 
    REAL Float | 
    BOLEANO Bool 
    deriving (Show)
type Gene = [TipagemGene]
data Individuo = Individuo {gene :: Gene, fitness :: Float} deriving (Show)
type Populacao = [Individuo]

-- Função auxiliar para converter um TipagemGene em um inteiro
toInt :: TipagemGene -> Int
toInt (INTEIRO i) = i
toInt (REAL _) = error "Não é possível converter um REAL em inteiro"
toInt (BOLEANO _) = error "Não é possível converter um BOLEANO em inteiro"

-- Função auxiliar para converter um TipagemGene em um float
toFloat :: TipagemGene -> Float
toFloat (INTEIRO i) = fromIntegral i
toFloat (REAL r) = r
toFloat (BOLEANO _) = error "Não é possível converter um BOLEANO em float"

-- Função auxiliar para converter um TipagemGene em um BOLEANOeano
toBool :: TipagemGene -> Bool
toBool (INTEIRO _) = error "Não é possível converter um Int em booleano"
toBool (REAL _) = error "Não é possível converter um Float em booleano"
toBool (BOLEANO b) = b


-- Função auxiliar para gerar um valor inteiro aleatório entre dois valores
randomRInt :: (Int, Int) -> IO Int
randomRInt range = randomRIO range

-- Função auxiliar para gerar um valor float aleatório entre dois valores
randomRFloat :: (Float, Float) -> IO Float
randomRFloat range = randomRIO range

-- Função auxiliar para gerar um valor booleano aleatório
randomBool :: IO Bool
randomBool = randomRIO (False, True)

-- Gerar Gene binário aleatório
gerarGeneBinarioAleatorio :: IO Gene
gerarGeneBinarioAleatorio = do
  value <- randomBool
  return [BOLEANO value]

-- Gerar Gene inteiro aleatório
gerarGeneInteiroAleatorioBounds :: (Int, Int) -> IO Gene
gerarGeneInteiroAleatorioBounds range = do
  value <- randomRInt range
  return [INTEIRO value]

-- Gerar Gene real aleatório
gerarGeneRealAleatorioBounds :: (Float, Float) -> IO Gene
gerarGeneRealAleatorioBounds range = do
  value <- randomRFloat range
  return [REAL value]


-- Gerar Gene
gerarGene :: TipagemGene -> Int -> Gene
gerarGene (INTEIRO n) = gerarGeneInteiroAleatorioBounds (0, n)
gerarGene (REAL n) = gerarGeneRealAleatorioBounds (0, n)
gerarGene (BOLEANO n) = gerarGeneBinarioAleatorio n

-- Gerar uma população aleatória
gerarPopulacaoAleatoria :: TipagemGene -> Int -> IO Populacao
gerarPopulacaoAleatoria rep popSize = do
  genes <- replicate popSize (gerarGene rep)
  return $ map (\g -> Individuo g 0) genes

main :: IO ()
main = do
  populacao <- gerarPopulacaoAleatoria (REAL 10) 100
  print populacao
