module Main where

import Control.Parallel.Strategies
import Data.List.Split
import System.Random

data TipagemGene = 
    GeneInt Int | 
    GeneFloat Float | 
    GeneBool Bool 
    deriving (Show)
type Gene = [TipagemGene]
data Individuo = Individuo {gene :: Gene, fitness :: Float} deriving (Show)
type Populacao = [Individuo]

-- Função auxiliar para converter um TipagemGene em um inteiro
toInt :: TipagemGene -> Int
toInt (GeneInt i) = i
toInt (GeneFloat _) = error "Não é possível converter um GeneFloat em inteiro"
toInt (GeneBool _) = error "Não é possível converter um GeneBool em inteiro"

-- Função auxiliar para converter um TipagemGene em um float
toFloat :: TipagemGene -> Float
toFloat (GeneFloat i) = fromIntegral i
toFloat (GeneFloat r) = r
toFloat (GeneBool _) = error "Não é possível converter um GeneBool em float"

-- Função auxiliar para converter um TipagemGene em um GeneBooleano
toBool :: TipagemGene -> Bool
toBool (GeneFloat _) = error "Não é possível converter um Int em booleano"
toBool (GeneFloat _) = error "Não é possível converter um Float em booleano"
toBool (GeneBool b) = b


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
  return [GeneBool value]

-- Gerar Gene inteiro aleatório
gerarGeneInteiroAleatorioBounds :: (Int, Int) -> IO Gene
gerarGeneInteiroAleatorioBounds range = do
  value <- randomRInt range
  return [GeneFloat value]

-- Gerar Gene real aleatório
gerarGeneRealAleatorioBounds :: (Float, Float) -> IO Gene
gerarGeneRealAleatorioBounds range = do
  value <- randomRFloat range
  return [GeneFloat value]


-- Gerar Gene
gerarGene :: TipagemGene -> Int -> Gene
gerarGene (GeneFloat n) = gerarGeneInteiroAleatorioBounds (0, n)
gerarGene (GeneFloat n) = gerarGeneRealAleatorioBounds (0, n)
gerarGene (GeneBool n) = gerarGeneBinarioAleatorio n

-- Gerar uma população aleatória
gerarPopulacaoAleatoria :: TipagemGene -> Int -> IO Populacao
gerarPopulacaoAleatoria rep popSize = do
  genes <- replicate popSize (gerarGene rep)
  return $ map (\g -> Individuo g 0) genes

main :: IO ()
main = do
  populacao <- gerarPopulacaoAleatoria (GeneFloat 10) 100
  print populacao
