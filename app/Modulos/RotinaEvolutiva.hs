module RotinaEvolutiva where


import Mutacao(mutacao)
import Crosssover(crossoverUmPontoAleatorio, crossover)
import Tipos(Populacao, Individuo)
import Avaliacoes.Utils(melhorIndividuo)
import Selecao(roletaViciada)
import Control.Parallel.Strategies (parMap, rpar)

evolutivaEnumerada :: Ord a => Populacao a -> (Individuo a -> Individuo a) -> Float -> Int -> IO (Populacao a)
evolutivaEnumerada populacao funcaoAvaliacao taxaMutacao contador = do
    -- Avaliacao
    let populacaoAvaliada = parMap rpar funcaoAvaliacao populacao

    -- Encontrar o melhor individuo
    let individuoEletista = melhorIndividuo populacao

    -- Selecao
    selecaoIndividuos <- roletaViciada populacao

    -- Crossover
    novaPopulacao <- crossover selecaoIndividuos crossoverUmPontoAleatorio

    -- Mutacao
    let novaPopulacao = parMap rpar (mutacao taxaMutacao) novaPopulacao

    return populacao


