module Main where
import GerarPopulacao (gerarPopulacaoBooleana)
import Arquivo (stringToIntList, stringsToIntLists, fileToIntLists)
import Tipos(genes)
import Sat (replaceWithBools)

main :: IO ()
main = do
  pop <- gerarPopulacaoBooleana 10 100

  intLists <- fileToIntLists "/home/udesc/Documentos/evolucao-computacional-main/arquivoSAT.txt"
  
  print "Ola"


  -- let resultado  = replaceWithBools intLists (map genes pop)
  print $ replaceWithBools intLists (genes (head pop))