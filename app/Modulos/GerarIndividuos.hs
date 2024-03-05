module GerarIndividuos where


import Control.Monad (replicateM)
import GerarAleatoriedades
    ( randomInt, randomFloat, randomBool )
import Tipos ( Individuo(..) )
import Data.List (permutations)

-- Função para gerar um individo de tamanho N com alelos aleatorios
gerarIndividuoBooleano :: Int -> IO Individuo
gerarIndividuoBooleano num_genes = do
  bools <- replicateM num_genes randomBool
  return $ IndividuoBooleano bools

-- Função para gerar um individuo de tamaho N com alelos aleatorios entre (a, b)
gerarIndividuoInteiroBound :: Int -> (Int, Int) -> IO Individuo
gerarIndividuoInteiroBound num_genes por_entre = do
    inteiros <- replicateM num_genes (randomInt por_entre)
    return $ IndividuoInteiroBound inteiros

-- Função para gerar um individuo de tamaho N com alelos aleatorios entre (a, b)
gerarIndividuoInteiroPermutado :: Int -> IO Individuo
gerarIndividuoInteiroPermutado num_genes = do
    let por_entre = (1, num_genes)
    let inteiros = permutations [fst por_entre .. snd por_entre]
    return $ IndividuoInteiroPermutado inteiros
    
-- Função para gerar um individuo de tamaho N com alelos aleatorios entre (a, b)
gerarIndividuoFlutuante :: Int -> (Float, Float) -> IO Individuo
gerarIndividuoFlutuante num_genes por_entre = do
    flutuantes <- replicateM num_genes (randomFloat por_entre)
    return $ IndividuoFlutuante flutuantes