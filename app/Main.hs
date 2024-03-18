module Main where
import GerarPopulacao (gerarPopulacaoBooleana)
import Arquivo (fileToIntLists)
import Avaliacoes (avaliarSATs)

main :: IO ()
main = do
  pop <- gerarPopulacaoBooleana 10 100

  intLists <- fileToIntLists "/home/udesc/Documentos/evolucao-computacional-main/arquivoSAT.cnf"
  
  print "Ola"
  -- print intLists


  -- let resultado  = replaceWithBools intLists (map genes pop)
  print $ avaliarSATs pop intLists