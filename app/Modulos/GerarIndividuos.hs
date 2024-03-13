module GerarIndividuos where


import GerarAleatoriedades
    (randomBoolLista, randomIntLista, randomFloatLista)
import Tipos ( Individuo(..), Intervalo (Intervalo) )

-- Função para gerar um individuo de tamanho N com alelos aleatórios booleanos
gerarIndividuoBooleano :: Int -> IO (Individuo Bool)
gerarIndividuoBooleano num_genes = do
  bools <- randomBoolLista num_genes
  let intervalo' = Intervalo False True
  return $ Individuo bools num_genes intervalo' 0

-- Função para gerar um individuo de tamanho N com alelos inteiros aleatórios entre (a, b)
gerarIndividuoInteiroBound :: Int -> (Int, Int) -> IO (Individuo Int)
gerarIndividuoInteiroBound num_genes por_entre = do
  inteiros <- randomIntLista por_entre num_genes
  let intervalo' = uncurry Intervalo por_entre
  return $ Individuo inteiros num_genes intervalo' 0

-- Função para gerar um individuo de tamanho N com alelos floats aleatórios entre (a, b)
gerarIndividuoFlutuante :: Int -> (Float, Float) -> IO (Individuo Float)
gerarIndividuoFlutuante num_genes por_entre = do
  flutuantes <- randomFloatLista por_entre num_genes
  let intervalo' = uncurry Intervalo por_entre
  return $ Individuo flutuantes num_genes intervalo' 0
