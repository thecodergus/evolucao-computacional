module Main where
import GerarPopulacao (gerarPopulacaoBooleana)
import Avaliacoes (maiorFitness)
import Arquivo (stringToIntList, stringsToIntLists, fileToIntLists)
import Tipos(genes)
import Sat (replaceWithBools)

main :: IO ()
main = do
  pop <- gerarPopulacaoBooleana 10 100

  intLists <- fileToIntLists "/home/udesc/Documentos/evolucao-computacional-main/arquivoSAT.txt"
  
  print "Ola"


  let resultado  = replaceWithBools intLists (map genes pop)
  print resultado