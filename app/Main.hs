module Main where

import Avaliacoes.Matematica (fitnessMax, fitnessMin)
import Aleatoriedades (randomBoolLista)
import Arquivo (fileToIntLists)
import GerarPopulacao (gerarPopulacaoBooleana)
import RotinaEvolutiva (loopEvolutivoEnumerado)
import Avaliacoes.Sat (avaliarSAT)



main :: IO ()
main = do
    arquivo <- fileToIntLists "/home/udesc/Documentos/evolucao-computacional-main/arquivoSAT.cnf"
    pop_incial <- gerarPopulacaoBooleana 30 100

    (pop_final, historico) <- loopEvolutivoEnumerado pop_incial (`avaliarSAT` arquivo) 0.05 500

    print historico