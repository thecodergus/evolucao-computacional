module GerarIndividuos where


import Control.Monad (replicateM)
import GerarAleatoriedades
    ( randomInt, randomFloat, randomBool )
import Tipos ( Individuo(..), Intervalo (Intervalo) )

-- Função para gerar um individuo de tamanho N com alelos aleatórios booleanos
gerarIndividuoBooleano :: Int -> IO (Individuo Bool)
gerarIndividuoBooleano num_genes = do
  bools <- replicateM num_genes randomBool
  let intervalo' = Intervalo False True
  return $ Individuo bools num_genes intervalo' 0

-- Função para gerar um individuo de tamanho N com alelos aleatórios entre (a, b)
gerarIndividuoInteiroBound :: Int -> (Int, Int) -> IO (Individuo Int)
gerarIndividuoInteiroBound num_genes por_entre = do
  inteiros <- replicateM num_genes (randomInt por_entre)
  let intervalo' = uncurry Intervalo por_entre
  return $ Individuo inteiros num_genes intervalo' 0

-- Função para gerar um individuo de tamanho N com alelos aleatórios entre (a, b)
gerarIndividuoFlutuante :: Int -> (Float, Float) -> IO (Individuo Float)
gerarIndividuoFlutuante num_genes por_entre = do
  flutuantes <- replicateM num_genes (randomFloat por_entre)
  let intervalo' = uncurry Intervalo por_entre
  return $ Individuo flutuantes num_genes intervalo' 0