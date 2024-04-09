module RotinaEvolutiva where


import Mutacao(mutacao)
import Crosssover(crossoverUmPontoAleatorio, crossover)
import Tipos(Populacao, Individuo)
import Avaliacoes.Utils(melhorIndividuo)
import Selecao(roletaViciada)
import Control.Parallel.Strategies (parMap, rpar)
import Data.Maybe (maybeToList)

-- Retorna a ultima População e o historico de melhores individuos
loopEvolutivoEnumerado :: Ord a => Populacao a -> (Individuo a -> Individuo a) -> Float -> Int -> IO (Populacao a, Populacao a)
loopEvolutivoEnumerado populacao _ _ 0 = return (populacao, [])
loopEvolutivoEnumerado populacao funcaoAvaliacao taxaMutacao contador = do
    print $ "-----------" ++ show contador ++ "-----------"

    -- Avaliacao
    let populacaoAvaliada = parMap rpar funcaoAvaliacao populacao

    -- Encontrar o melhor individuo
    let individuoEletista = maybeToList $ melhorIndividuo populacaoAvaliada
    
    -- Selecao
    selecaoIndividuos <- roletaViciada populacao

    -- Crossover
    novaPopulacao <- crossover selecaoIndividuos crossoverUmPontoAleatorio

    -- Mutacao
    let novaPopulacao' = parMap rpar (`mutacao` taxaMutacao) novaPopulacao
    novaPopulacao'' <- sequence novaPopulacao'


    (retorno, elitistas) <- loopEvolutivoEnumerado (individuoEletista ++ novaPopulacao'') funcaoAvaliacao taxaMutacao (contador - 1)

    return (retorno, elitistas ++ individuoEletista)