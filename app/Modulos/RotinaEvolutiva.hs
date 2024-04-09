module RotinaEvolutiva where


import Mutacao(mutacao)
import Crosssover(crossoverUmPontoAleatorio, crossover)
import Tipos(Populacao, Individuo)
import Avaliacoes.Utils(melhorIndividuo)
import Selecao(roletaViciada)
import Control.Parallel.Strategies (parMap, rpar)
import Data.Maybe (maybeToList)

evolutivaEnumerada :: Ord a => Populacao a -> (Individuo a -> Individuo a) -> Float -> Int -> IO (Populacao a)
evolutivaEnumerada populacao _ _ 0 = return populacao
evolutivaEnumerada populacao funcaoAvaliacao taxaMutacao contador = do
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


    evolutivaEnumerada (individuoEletista ++ novaPopulacao'') funcaoAvaliacao taxaMutacao (contador - 1)