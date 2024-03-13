module GerarPopulacao where


import Tipos (Populacao, Individuo (Individuo))
import GerarIndividuos (gerarIndividuoBooleano, gerarIndividuoInteiroBound, gerarIndividuoFlutuante, gerarIndividuoInteiroForSAT)
import Control.Monad (replicateM)
import Data.List (permutations)


-- Função para gerar uma população de indivíduos booleanos
gerarPopulacaoBooleana :: Int -> Int -> IO (Populacao Bool)
gerarPopulacaoBooleana num_individuos num_genes = replicateM num_individuos (gerarIndividuoBooleano num_genes)

-- Função para gerar uma população de indivíduos inteiros bounded
gerarPopulacaoInteiroBound :: Int -> Int -> (Int, Int) -> IO (Populacao Int)
gerarPopulacaoInteiroBound num_individuos num_genes intervalo = replicateM num_individuos (gerarIndividuoInteiroBound num_genes intervalo)

-- Função para gerar uma população de indivíduos inteiros permutados
gerarPopulacaoInteiroPermutado :: Int -> Int -> IO (Populacao Int)
gerarPopulacaoInteiroPermutado num_individuos num_genes | num_individuos > num_genes =  error "Número de indivíduos deve ser menor ou igual ao número de genes"
                                                        | num_individuos < 0 = error "Número de indivíduos deve ser maior que zero"
                                                        | num_genes < 0 = error "Número de genes deve ser maior que zero"
                                                        | otherwise = do
  let por_entre = (1, num_genes)
  let inteiros = permutations [fst por_entre .. snd por_entre]
  let elementos = take num_individuos inteiros

  return $ map (`Individuo` 0) elementos


-- Função para gerar uma população de indivíduos flutuantes
gerarPopulacaoFlutuante :: Int -> Int -> (Float, Float) -> IO (Populacao Float)
gerarPopulacaoFlutuante num_individuos num_genes intervalo = replicateM num_individuos (gerarIndividuoFlutuante num_genes intervalo)

-- Função para gerar uma população que satisfaça o problema do 3-SAT
gerarPopulacaoForSAT :: Int -> Int -> (Int, Int) -> IO (Populacao [Int])
gerarPopulacaoForSAT num_individuos num_genes intervalo = replicateM num_individuos (gerarIndividuoInteiroForSAT num_genes intervalo)