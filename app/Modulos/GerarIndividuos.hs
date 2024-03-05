module GerarIndividuos where


import Control.Monad (replicateM)
import GerarAleatoriedades
import Tipos

-- Função para gerar um individo de tamanho N com alelos aleatorios
gerarIndividuoBooleano :: Int -> IO Individuo
gerarIndividuoBooleano num_genes = do
  bools <- replicateM num_genes randomRBool
  return $ IndividuoBooleano bools

-- Função para gerar um individuo de tamaho N com alelos aleatorios entre (a, b)
gerarIndividuoInteiroBound :: Int -> (Int, Int) -> IO Individuo
gerarIndividuoInteiroBound num_genes por_entre = do
    inteiros <- replicateM num_genes (randomRInt por_entre)
    return $ IndividuoInteiro inteiros

-- Função para gerar um individuo de tamaho N com alelos aleatorios entre (a, b)
gerarIndividuoFlutuante :: Int -> (Float, Float) -> IO Individuo
gerarIndividuoFlutuante num_genes por_entre = do
    flutuantes <- replicateM num_genes (randomRFloat por_entre)
    return $ IndividuoFlutuante flutuantes