module Main where
import GerarPopulacao (gerarPopulacaoInteiroBound)
import Sat (solve)
import Tipos (Individuo(fitness, genes))
import GerarAleatoriedades (randomBoolMatriz)

main :: IO ()
main = do
  pop <- gerarPopulacaoInteiroBound 1 30 (-100, 100)
  let um_gene = genes (head pop)
  print um_gene
  print $ solve (map (: []) um_gene)