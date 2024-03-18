module Main where
import GerarPopulacao (gerarPopulacaoBooleana)
import Arquivo (fileToIntLists)
import Avaliacoes (avaliarSATs, melhorIndividuo)

main :: IO ()
main = do
  pop <- gerarPopulacaoBooleana 10 100
  intLists <- fileToIntLists "/home/udesc/Documentos/evolucao-computacional-main/arquivoSAT.cnf"
  
  print "Individuos:"

  let individuos_avaliados = avaliarSATs pop intLists

  print individuos_avaliados

  print "Melhor individuo:"

  print $ melhorIndividuo individuos_avaliados