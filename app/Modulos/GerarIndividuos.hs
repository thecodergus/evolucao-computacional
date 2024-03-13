module GerarIndividuos where


import GerarAleatoriedades
    (randomBoolLista, randomIntLista, randomFloatLista)
import Tipos ( Individuo(..))

-- Função para gerar um individuo de tamanho N com alelos aleatórios booleanos
gerarIndividuoBooleano :: Int -> IO (Individuo Bool)
gerarIndividuoBooleano num_genes = do
  bools <- randomBoolLista num_genes
  return $ Individuo bools 0

-- Função para gerar um individuo de tamanho N com alelos inteiros aleatórios entre (a, b)
gerarIndividuoInteiroBound :: Int -> (Int, Int) -> IO (Individuo Int)
gerarIndividuoInteiroBound num_genes por_entre = do
  inteiros <- randomIntLista por_entre num_genes
  return $ Individuo inteiros 0

-- Função para gerar um individuo de tamanho N com alelos floats aleatórios entre (a, b)
gerarIndividuoFlutuante :: Int -> (Float, Float) -> IO (Individuo Float)
gerarIndividuoFlutuante num_genes por_entre = do
  flutuantes <- randomFloatLista por_entre num_genes
  return $ Individuo flutuantes  0
