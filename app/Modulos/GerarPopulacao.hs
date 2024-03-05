module GerarPopulacao where


import Tipos (Populacao)
import GerarIndividuos (gerarIndividuoBooleano, gerarIndividuoInteiroBound, gerarIndividuoFlutuante, gerarIndividuoInteiroPermutado)
import Control.Monad (replicateM)


-- Função para gerar uma população de indivíduos booleanos
gerarPopulacaoBooleana :: Int -> Int -> IO Populacao
gerarPopulacaoBooleana num_individuos num_genes = do
  replicateM num_individuos (gerarIndividuoBooleano num_genes)

-- Função para gerar uma população de indivíduos inteiros bounded
gerarPopulacaoInteiroBound :: Int -> Int -> (Int, Int) -> IO Populacao
gerarPopulacaoInteiroBound num_individuos num_genes intervalo = do
  replicateM num_individuos (gerarIndividuoInteiroBound num_genes intervalo)

-- Função para gerar uma população de indivíduos inteiros permutados
gerarPopulacaoInteiroPermutado :: Int -> Int -> IO Populacao
gerarPopulacaoInteiroPermutado num_individuos num_genes = do
  replicateM num_individuos (gerarIndividuoInteiroPermutado num_genes)

-- Função para gerar uma população de indivíduos flutuantes
gerarPopulacaoFlutuante :: Int -> Int -> (Float, Float) -> IO Populacao
gerarPopulacaoFlutuante num_individuos num_genes intervalo = do
  replicateM num_individuos (gerarIndividuoFlutuante num_genes intervalo)