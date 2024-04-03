module GerarIndividuos where


import Aleatoriedades
    (randomBoolLista, randomIntLista, randomFloatLista, randomSublistas')
import Tipos ( Individuo(..))

-- Função para gerar um individuo de tamanho N com alelos aleatórios booleanos
gerarIndividuoBooleano :: Int -> IO (Individuo Bool)
gerarIndividuoBooleano num_genes = 
  Individuo <$> randomBoolLista num_genes <*> pure 0

-- Função para gerar um individuo de tamanho N com alelos inteiros aleatórios entre (a, b)
gerarIndividuoInteiroBound :: Int -> (Int, Int) -> IO (Individuo Int)
gerarIndividuoInteiroBound num_genes por_entre = 
  Individuo <$> randomIntLista por_entre num_genes <*> pure 0

-- Função auxliar para gerar um individuo que se adeque ao problema SAT
gerarIndividuoInteiroForSAT :: Int -> (Int, Int) -> IO (Individuo [Int])
gerarIndividuoInteiroForSAT num_genes por_entre = 
  Individuo <$> randomSublistas' por_entre num_genes <*> pure 0

-- Função para gerar um individuo de tamanho N com alelos floats aleatórios entre (a, b)
gerarIndividuoFlutuante :: Int -> (Float, Float) -> IO (Individuo Float)
gerarIndividuoFlutuante num_genes por_entre = 
  Individuo <$> randomFloatLista por_entre num_genes <*> pure 0

